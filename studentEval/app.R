library(shiny)
library(shinyjs)
library(shinythemes)
library(googlesheets)
library(dplyr)
library(stringr)
library(DT)

# Define UI for application
ui <- navbarPage(
  
  
  # Theme
  theme = shinytheme("cerulean"),
  
  # Application title
  title = "Student Evaluation System",
  
  # navbarPage ID
  id = "tabs",
  
  # Tab for login
  tabPanel(
    "Login",
    
    wellPanel(
      useShinyjs(),
      textInput("email", "Email"),
      passwordInput("password", "Password"),
      actionButton("submit","Submit"),
      textOutput("error")
    )
  ),
  
  # Tab for Students
  tabPanel(
    "Students",
    
    flowLayout(
      textInput(
        "fname",
        "Student First Name"
      ),
      textInput(
        "lname",
        "Student Last Name"
      ),
      textInput(
        "grade",
        "Student Grade"
      ),
      actionButton("submit_stu","Submit")
    )
  ),
  
  # Tab for Exams
  tabPanel(
    "Exam",
    
    tabsetPanel(
      tabPanel(
        "Examination",
        textInput(
          "exam_name",
          "Exam Title"
        ),
        dateInput(
          "exam_date",
          "Exam Date"
        ),
        textInput(
          "exam_grade",
          "Grade"
        ),
        actionButton("submit_exam","Submit")
      ),
      tabPanel(
        "Question Types",
        textInput(
          "qtype",
          "Question Type"
        ),
        actionButton("submit_qtype","Submit")
      ),
      tabPanel(
        "Question",
        selectInput(
          "equestions",
          "Select the Exam",
          choices = c(1:4)
        ),
        textInput(
          "q_name",
          "Question"
        ),
        selectInput(
          "q_qtype",
          "Select Question Type",
          choices = c(1:4)
        ),
        textInput(
          "total",
          "Total"
        ),
        actionButton("submit_q","Submit")
      )
    )
  ),
  
  # Tab for Data Entry
  tabPanel(
    "Results"
  ),

  # Tab for report 
  tabPanel(   
    "Report",
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "exam_choice",
          "Choose the Exam",
          choices = c(1:4)
        ),
        actionButton("display","Display")
      ),
      mainPanel(
        fluidRow(DT::dataTableOutput("report"),
                 actionButton("refresh", "Refresh"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  hideTab(inputId = "tabs",
          target = "Report"
  )
  hideTab(inputId = "tabs",
          target = "Students"
  )
  hideTab(inputId = "tabs",
          target = "Exam"
  )
  hideTab(inputId = "tabs",
          target = "Report"
          )
  hideTab(inputId = "tabs",
          target = "Results")
  
  # Login
  observeEvent(input$submit,{
    
    "1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
      gs_key() %>%
      gs_add_row(ws="Log",
                 input = c(paste(Sys.time()),
                           input$email,
                           input$password)
      )
    
    if(str_detect(input$email, "^[[:graph:]]+@immaculatehigh.edu.jm$") & 
       input$password == "Password"){
      showTab(inputId = "tabs",
              "Report")
      showTab(inputId = "tabs",
              "Students")
      showTab(inputId = "tabs",
              "Exam")
      showTab(inputId = "tabs",
              "Results")
      hideTab(inputId = "tabs",
              target = "Login")
    } else {
      output$error <- renderText({"Email/Password incorrect. Please try again."})
    }
  })
  
  # Display report
  observeEvent(input$display,{
    
    ws <- reactive({
      ifelse(
        input$choice == 1,
        2,
        1
      )
    })
    
    input$refresh
    
    output$report <- renderDataTable({
      
      "1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
        gs_key() %>%
        gs_read(ws=ws()) %>% 
        select(-Timestamp) %>%
        datatable(
          rownames = F,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            buttons = c("excel", "pdf","print")
          )
        )
      
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)