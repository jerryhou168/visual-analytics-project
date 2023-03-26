# Define the control panel on the left
t1_loanType <- selectInput(inputId = "t1_loanType", 
                           label = "Type of Loans",
                           choices = c("New Loan" = "S",
                                       "Repeat Loan" = "R"),
                           selected = "S")

# Define the variables to be studied in this tab

newloan_factors_subset = c(
     "bank_name_clients",
     "approval_duration_group",
     "age_at_loan_25th_pctile",
     "credit_rating",
     "employment_status_risk",
     "level_of_education_risk",
     "referral",
     "bank_account_type_recode",
     "termdays",
     "age_at_loan",
     "bank_account_type"
  )

repeatloan_factors_subset = c(
    "pct_ontime",
    "total_ontime",
    "max_active_of_loans",
    "bank_name_clients",
    "max_age_at_loan",
    "avg_age_at_loan",
    "employment_status",
    "bank_account_type",
    "total_num_of_loans",
    "max_approval_duration",
    "termdays"
  )  
  
  
repeatloan_factors_subset 



uv_newloan_predictor_options <- setNames(newloan_factors_subset, 
                                         newloan_factors_subset)

uv_repeatloan_predictor_options <- setNames(repeatloan_factors_subset, 
                                            repeatloan_factors_subset)

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

global_univariate_chart_rendering = 0

univar <- function(input, output, session) {
  
  univar_data <- reactive({
    if (input$t1_loanType == "S") {
      return(newloan)
    } else {
      return(repeatloan)
    }
  })
  
  univar_variables <- reactive({
    if (input$t1_loanType == "S") {
      return(uv_newloan_predictor_options)
    } else {
      return(uv_repeatloan_predictor_options)
    }
  })
  
  observeEvent(input$t1_loanType, {
    default_variable = 'pct_ontime'
    if(input$t1_loanType == "S"){
      default_variable = 'bank_name_clients'
    }
    selectInput(inputId = "t1_variable",
                label = "Variables",
                choices = univar_variables(),
                selected = default_variable)
    
  })

  output$Uni_Variable_X <- renderUI({
    selectInput(inputId = "t1_variable",
                label = "Variables",
                choices = univar_variables(),
                multiple = FALSE)
  })
  
  output$Univarplot <- renderPlot({
  p = ggplot(univar_data(), aes(x = .data[[input$t1_variable]])) +
      geom_bar()
  p + theme_economist() +
  labs(title = "Univariate Analysis") +
  theme(axis.title.y = element_text(vjust = 2.5),
          axis.text.x = element_text(angle = 60,
                                     vjust = 0.5))
  
  })

}
        

  

  
  
  
  