library(shiny)
library(shinyjs)
library(shinythemes)
library(googlesheets)
library(dplyr)
library(stringr)
library(DT)

# Initialize Exam and Qtype tables for selecti


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
      actionButton("login_submit","Submit"),
      textOutput("error")
    )
  ),
  
  # Tab for Students
  tabPanel(
    "Students",
    
    flowLayout(
      textInput(
        "studentid",
        "Student ID"
      ),
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
          "Exam Date",
          format = "dd/mm/yyyy"
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
          "qtypeid",
          "Question Type ID"
        ),
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
          choices = NULL
        ),
        textInput(
          "qno",
          "Question Number"
        ),
        
        textInput(
          "q_name",
          "Question"
        ),
        selectInput(
          "q_qtype",
          "Select Question Type",
          choices = NULL
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
    "Results",
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "performance_exam",
          "Choose the exam",
          choices = NULL
        ),
        actionButton("exam_select","Select")
      ),
      mainPanel(
        textInput(
          "performance_student",
          "Enter the Student ID"),
        textInput(
          "performance_questions",
          "Question"
        ),
        textInput(
          "performance_score",
          "Score"
        ),
        actionButton("performance","Submit")
      )
    )
  ),

  # Tab for report 
  tabPanel(   
    "Report",
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "exam_choice",
          "Choose the Exam",
          choices = NULL
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
server <- function(input, output, session) {
  
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
  
  exams <- reactive({
    invalidateLater(100)
    exams <- "1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
      gs_key() %>%
      gs_read(ws="Exam") %>%
      select(Ename) %>%
      unlist() %>%
      unique()
    return(exams)
  })
  qtypes <- reactive({
    invalidateLater(100)
    qtypes<-"1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
      gs_key() %>%
      gs_read(ws="Type") %>%
      select(Qtype) %>%
      unlist() %>%
      unique()
    return(qtypes)
  })
  
  # Login
  observeEvent(input$login_submit,{
    
    "1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
      gs_key() %>%
      gs_add_row(ws="Log",
                 input = c(paste(Sys.time()),
                           input$email,
                           input$password)
      )
    
    if(str_detect(input$email, "^[[:graph:]]+@+[[:graph:]]+.com$") & 
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
    updateSelectInput(
      session = session,
      inputId = "equestions",
      choices = exams()
    )
    updateSelectInput(
      session,
      inputId = "performance_exam",
      choices = exams()
    )
    updateSelectInput(
      session,
      inputId = "exam_choice",
      choices = exams()
    )
    updateSelectInput(
      session,
      inputId = "q_qtype",
      choices = qtypes()
    )
  })
  
  # Student Update
  observeEvent(input$submit_stu,{
    "1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
      gs_key() %>%
      gs_add_row(ws="Student",
                 input = c(input$studentid,
                           input$fname,
                           input$lname,
                           input$grade))
    reset(input$studentid)
    reset(input$fname)
    reset(input$lname)
    reset(input$grade)
  })
  
  # Exam Update
  observeEvent(input$submit_exam,{
    "1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
      gs_key() %>%
      gs_add_row(ws="Exam",
                 input = c(paste0(input$exam_name,
                                  input$exam_date,
                                  input$exam_grade),
                           input$exam_name,
                           input$exam_date,
                           input$exam_grade))
    updateSelectInput(
      session = session,
      inputId = "equestions",
      choices = exams()
    )
    updateSelectInput(
      session,
      inputId = "performance_exam",
      choices = exams()
    )
    updateSelectInput(
      session,
      inputId = "exam_choice",
      choices = exams()
    )

    reset(input$exam_name)
    reset(input$exam_date)
    reset(input$exam_grade)
    })
  
  # Question Type update
  observeEvent(input$submit_qtype,{
    "1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
      gs_key() %>%
      gs_add_row(ws="Type",
                 input = c(input$qtypeid,
                           input$qtype))
    updateSelectInput(
      session,
      inputId = "q_qtype",
      choices = qtypes()
    )
    reset(input$qtypeid)
    reset(input$qtype)
  })
  
  # Question Update
  observeEvent(input$submit_q,{
    "1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
      gs_key() %>%
      gs_add_row(ws="Question",
                 input = c(paste0(input$equestions, input$qno),
                           input$qno,
                           input$q_name,
                           input$q_qtype,
                           input$total,
                           input$equestions))
    reset(input$equestions)
    reset(input$qno)
    reset(input$q_name)
    reset(input$q_qtype)
    reset(input$total)
  })
  
  observeEvent(input$performance,{
    "1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
      gs_key() %>%
      gs_add_row(ws="Performance",
                 input = c(input$performance_exam,
                           input$performance_student,
                           paste0(input$performance_exam,
                                  input$performance_questions),
                           input$performance_score))
    reset(input$performance_student)
    reset(input$performance_questions)
    reset(input$performance_score)
    
  })
  
  # Display report
  observeEvent(input$display,{
    
    report_table <- reactive({
      performance <- "1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
        gs_key() %>%
        gs_read(ws="Performance") %>%
        filter(Exam == input$exam_choice)
      questype <- "1d_Qu2Aq9hVRheOnw0hisauXMJkT9RH0k5qMmpZs25_U" %>%
        gs_key() %>%
        gs_read(ws="Question") %>%
        select(QuesID,Qname,QType,QTotal)
      report <- performance %>%
        left_join(questype,by =c("Question"="QuesID"))
      return(report)
    })
    
    input$refresh
    
    output$report <- renderDataTable({
      
      report_table() %>%
        select(Student,Qname,QType,QTotal,Score) %>%
        rename(Question = Qname,
               Type = QType,
               Total = QTotal,
               `Student Score` = Score) %>%
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