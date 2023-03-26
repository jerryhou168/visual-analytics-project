##############################################################################
### Prediction Details
##############################################################################

source("prediction_loan_default.R", local = TRUE)

##############################################################################
### UI
##############################################################################

### Prediction Panel

newloan_predictor_options <- setNames(newloan_factors, 
                                      newloan_factors)

repeatloan_predictor_options <- setNames(repeatloan_factors, 
                                         repeatloan_factors)


t6_slt_loanType <- selectInput(inputId = "t6_loanType", 
                                 label = "Type of Loans",
                                 choices = c("New Loan" = "S",
                                             "Repeat Loan" = "R"),
                                 selected = "S")

t6_slt_predictors <- uiOutput(outputId = "output_t6_predictors")

t6_slt_algorithm <- selectInput(inputId = "t6_algorithm", 
                             label = "Prediction Algorithm",
                             choices = c("Nominal Logistic Regression" = "NL",
                                         "Fit Stepwise" = "FS",
                                         "Boosted Tree" = "BT",
                                         "Bootstrap Forest" = "BF"),
                             selected = "NL")


t6_btn_predict <- actionButton("go", "Predict")
t6_btn_reset <- actionButton("reset", "Clear")

### Panel elements
prediction_nav <- fluidRow(
  t6_slt_loanType,
  t6_slt_predictors,
  t6_slt_algorithm,
  t6_btn_predict,
  t6_btn_reset
)

prediction_main <-fluidRow(
  h2("main prediction")
)

##############################################################################
### Server
##############################################################################

prediction <- function(input, output, session) {
  
  observeEvent(input$go, {
    loan_default_prediction(input$t6_loanType, 
                            input$t6_predictors, 
                            input$t6_algorithm)
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "t6_loanType", 
                      selected = "S")
    
    updateSelectInput(session, "t6_predictors", 
                      choices = newloan_predictor_options)
    
    updateSelectInput(session, "t6_algorithm", 
                      selected = "NL")
    
  })
  
  output$output_t6_predictors <- renderUI({
    if(input$t6_loanType == "S"){
      options <- newloan_predictor_options
    } else {
      options <- repeatloan_predictor_options
    }
    selectInput(inputId = "t6_predictors",
                label = "Variables",
                choices = options,
                multiple = TRUE)
  })
}