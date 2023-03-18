# Define the control panel on the left
t1_loanType <- selectInput(inputId = "t1_loanType", 
                           label = "Type of Loans",
                           choices = c("New Loan" = "S",
                                       "Repeat Loan" = "R"),
                           selected = "S")

# Define the variables to be studied in this tab

uv_newloan_predictor_options <- setNames(newloan_factors, 
                            newloan_factors)

uv_repeatloan_predictor_options <- setNames(repeatloan_factors, 
                               repeatloan_factors)

t1_variable_x <- uiOutput(outputId = "Uni_Variable_X")




# Define the navigation bar
univariate_nav <- fluidRow(
  t1_loanType,
  t1_variable_x
)


univariate_main <-fluidRow(
  plotOutput("Univarplot",
             width = "100%",
             height = 400)
  
)


# Server

univar <- function(input, output) {

univar_opt <- reactive({
    if (input$t1_loanType == "S") {
      return(newloan_options)
    } else {
      return(repeatloan_options)
    }
  })  




output$Uni_Variable_X <- renderUI({
  if(input$t1_loanType == "S"){
    options <- uv_newloan_predictor_options
  } else {
    options <- uv_repeatloan_predictor_options
  }
  selectInput(inputId = "t1_variables",
              label = "Variables",
              choices = options,
              multiple = TRUE)
})



output$Uni_Variable_X <- renderUI({
   
  if (input$t1_loanType == "S") {
    options = newloan_options 
  } else {
    options = repeatloan_options
  }
    selectInput(inputId = "Uni_Variable_X",
                label = "Select variable X from below",
                choices = options,
                multiple = FALSE)
  
  })
  

univar_data <- reactive({
    if (input$t1_loanType == "S") {
      return(newloan)
    } else {
      return(repeatloan)
    }
  })

output$Univarplot <- renderPlot({

  if (input$Uni_Variable_X %in% c("bank_name_clients",
                                 "approval_duration_group",
                                 "age_at_loan_25th_pctile",
                                 "credit_rating",
                                 "termdays",
                                 "employment_status_risk",
                                 "level_of_education_risk",
                                 "referral",
                                 "bank_account_type_recode",
                                 "loanamount_group",
                                 "loanamount",
                                 "Interestrate",
                                 "bank_account_type",
                                 "employment_status",
                                 "total_referrrals",
                                 "max_interest_rate",
                                 "mean_interest_rate",
                                 "mean_referrals",
                                 "max_churn_flag",
                                 "loanamount"))
  {
  p <- ggplot(univar_data(), aes(x = data[[input$Uni_Variable_X]])) +
      geom_histogram()

  p
  }


})

}
        

  

  
  
  
  