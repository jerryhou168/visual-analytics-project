# Define the control panel on the left
source("./correlation/r_correlation.R", local = TRUE)

t3_ui_loanType <- radioButtons(inputId = "t3_loanType", 
                               label = "Type of Loans",
                               choices = c("New Loan" = "S",
                                           "Repeat Loan" = "R"),
                               selected = "S", inline = TRUE)

t3_ui_variable <- uiOutput(outputId = "t3_o_variables")

correlation_nav <- fluidRow(
  t3_ui_loanType,
  t3_ui_variable
)


correlation_main <-fluidRow(
  fluidRow(
    column(12,
           plotOutput(outputId = "corrplot1",
                      width = "100%",
                      height = 600)
    )
  )
)

corr <- function(input, output, session){
  
  corr_opts <- reactive({
    if (input$t3_loanType == "S") {
      return(newloan_factor_options)
    } else {
      return(repeatloan_factor_options)
    }
  })
  
  corr_def_opts <- reactive({
    if (input$t3_loanType == "S") {
      return(c("age_at_loan","age_at_loan_25th_pctile", "approval_duration_group"))
    } else {
      return(c("avg_age_at_loan","bank_account_type", "bank_name_clients"))
    }
  })
  
  output$t3_o_variables <- renderUI({
    checkboxGroupInput(inputId = "t3_variables", 
                       label = "Variables",
                       choices = corr_opts(),
                       selected = corr_def_opts(), 
                       inline = FALSE)
  })
  
  observeEvent(input$t3_loanType, {
    print("plot correlation started at 1")
    plot_correlation(input, output, session, 1, corr_def_opts())
  })
  
  observeEvent(input$t3_variables, {
    print("plot correlation started at 2")
    plot_correlation(input, output, session, 2, NULL)
  })
}