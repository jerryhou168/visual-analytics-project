# Define the control panel on the left
t4_loanType <- selectInput(inputId = "t4_loanType", 
                           label = "Type of Loans",
                           choices = c("New Loan" = "S",
                                       "Repeat Loan" = "R"),
                           selected = "S")

# Define the variables to be studied in this tab
newloan_Options <- c("approval_duration",
                     "totaldue",
                     "age_at_loan")

repeatloan_Options <- c("pct_ontime",
                        "total_ontime",
                        "max_active_of_loans",
                        "max_approval_duration",
                        "max_age_at_loan",
                        "avg_age_at_loan",
                        "total_num_of_loans",
                        "total_approval_duration",
                        "mean_approval_duration",
                        "totaldue")

newloan_options <- setNames(newloan_Options,
                            newloan_Options)

repeatloan_options <- setNames(repeatloan_Options,
                               repeatloan_Options)



t4_variable <- uiOutput(outputId = "Multi_Variables")


multicollinearity_nav <- fluidRow(
  t4_loanType,
  t4_variable)


multicollinearity_main <-fluidRow(
  plotOutput(outputId = "vifplot",
             width = "100%",
             height = 400))





multi <- function(input, output, session){
  
  multi_data <- reactive({
    if (input$t4_loanType == "S") {
      return(newloan)
    } else {
      return(repeatloan)
    }
  })  
  
  multi_variables <- reactive({
    if (input$t4_loanType == "S") {
      return(newloan_options)
    } else {
      return(repeatloan_options)
    }
  })
 
  multi_filter <- reactive({
    if (input$t4_loanType == "S") {
      return(newloan$good_bad_flag)
    } else {
      return(repeatloan$good_bad_flag)
    }
  })

  
  
  observeEvent(input$t4_loanType, {
    default_variable = 'pct_ontime' 
    if(input$t4_loanType == "S"){
      default_variable = 'approval_duration'
    }
    selectInput(inputId = "Multi_Variables",
                label = "Select variables from below",
                choices = multi_variables(),
                selected = default_variable,
                multiple = TRUE)
    
  })
  
  output$Multi_Variables <- renderUI({
    selectInput(inputId = "Multi_Variables",
                label = "Select variables from below",
                choices = multi_variables(),
                multiple = TRUE)
  })
  
  
  # create_recipe <- function(data){
  #   data_recipe <- recipe(multi_filter() ~ .,
  #                         data = data)
  #   data_recipe <- step_log(data_recipe, all_numeric())
  #   data_recipe <- step_novel(data_recipe, all_nominal(),
  #                -all_outcomes())
  #   data_recipe <- step_dummy(data_recipe, all_nominal(),
  #                -all_outcomes())
  #   return (data_recipe)
  # }


  output$vifplot <- renderPlot({  
    req(input$Multi_Variables)
    data_recipe <- recipe(multi_filter() ~ .,
                          data = multi_data())
    baking_recipe <- data_recipe %>% 
      step_log(all_numeric()) %>% 
      step_novel(all_nominal(), -all_outcomes()) %>% 
      step_dummy(all_nominal(), -all_outcomes())
    
    baked_data <- bake(baking_recipe, new_data = multi_data())
    
    model <- glm(multi_filter() ~ ., family = binomial("logit"), data = baked_data)
    
    plot <- check_collinearity(model)
    plot(plot)
  })
  }