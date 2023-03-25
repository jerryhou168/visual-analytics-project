# Define the control panel on the left
t5_loanType <- selectInput(inputId = "t5_loanType", 
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




qs_newloan_predictor_options <- setNames(newloan_factors_subset, 
                                         newloan_factors_subset)

qs_repeatloan_predictor_options <- setNames(repeatloan_factors_subset, 
                                            repeatloan_factors_subset)

t5_variable_x <- uiOutput(outputId = "qs_variable_X")



# Define the navigation bar



quasiseparation_nav <- fluidRow(
  t5_loanType,
  t5_variable_x
)


quasiseparation_main <-fluidRow(
  plotOutput("quasiplot",
             width = "100%",
             height = 400)
  
)
  
  
  

# Server

global_qscomplete_chart_rendering = 0

quasicomplete <- function(input, output, session) {
  
  qs_data <- reactive({
    if (input$t5_loanType == "S") {
      return(newloan)
    } else {
      return(repeatloan)
    }
  })
  
  qs_filter <- reactive({
    if (input$t5_loanType == "S") {
      return(newloan$good_bad_flag)
    } else {
      return(repeatloan$good_bad_flag)
    }
  })
  
  qs_variables <- reactive({
    if (input$t5_loanType == "S") {
      return(qs_newloan_predictor_options)
    } else {
      return(qs_repeatloan_predictor_options)
    }
  })
  
  observeEvent(input$t5_loanType, {
    default_variable = 'pct_ontime'
    if(input$t5_loanType == "S"){
      default_variable = 'bank_name_clients'
    }
    selectInput(inputId = "t5_variable",
                label = "Variables",
                choices = qs_variables(),
                selected = default_variable)
    
  })
  
  output$qs_variable_X <- renderUI({
    selectInput(inputId = "t5_variable",
                label = "Variables",
                choices = qs_variables(),
                multiple = FALSE)
  })
  
  output$quasiplot <- renderPlot({
  ggplot(qs_data(), aes(x = .data[[input$t5_variable]] , 
                               fill = qs_filter() )) + 
  geom_bar(bins = 20)+
  theme_economist()
  
  })
  






}