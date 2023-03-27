# Define the control panel on the left
source("./multicollinearity/r_multicollinearity.R", local = TRUE)

t4_ui_loanType <- selectInput(inputId = "t4_loanType", 
                           label = "Type of Loans",
                           choices = c("New Loan" = "S",
                                       "Repeat Loan" = "R"),
                           selected = "S")

t4_ui_variable <- uiOutput(outputId = "t4_o_variables")

t4_ui_notes <- htmlOutput(outputId = "t4_o_reminder")

multicollinearity_nav <- fluidRow(
  t4_ui_loanType,
  t4_ui_variable,
  t4_ui_notes)


multicollinearity_main <-fluidRow(
  fluidRow(
    column(12,
           plotOutput(outputId = "vifplot",
                      width = "100%",
                      height = 500)
    )
  )
  ,
  fluidRow(
    column(12,
           dataTableOutput(outputId="t4_o_vif_table")
    )
  )
)

multi <- function(input, output, session){
  
  output$t4_o_reminder <- renderText({
    "(Minimum three variables required)"
  })
  
  multicli_opts <- reactive({
    if (input$t4_loanType == "S") {
      return(newloan_factors)
    } else {
      return(repeatloan_factors)
    }
  })
  
  multicli_def_opts <- reactive({
    if (input$t4_loanType == "S") {
      return(c("age_at_loan","bank_account_type", "credit_rating"))
    } else {
      return(c("pct_ontime","avg_age_at_loan", "max_approval_duration"))
    }
  })
  
  output$t4_o_variables <- renderUI({
    checkboxGroupInput(inputId = "t4_variables", 
                       label = "Variables",
                       choices = multicli_opts(),
                       selected = multicli_def_opts(), 
                       inline = FALSE)
  })
  
  observeEvent(input$t4_loanType, {
    print("plot_multicollinearity started at 1")
    plot_multicollinearity(input, output, session, 1, multicli_def_opts())
  })
  
  observeEvent(input$t4_variables, {
    print("plot_multicollinearity started at 2")
    plot_multicollinearity(input, output, session, 2, NULL)
  })
}