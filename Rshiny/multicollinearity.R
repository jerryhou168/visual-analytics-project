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
  
  
  subds <- reactive({
    subds1 <- multi_data() %>% 
      dplyr::select(good_bad_flag , bank_name_clients )
    return(subds1)
  })

  
  set.seed(123)
  
  
  data_split <- reactive({
    initial_split(subds(), prop = 3/4, strata = good_bad_flag)
  })
  
  train_data <- reactive({
    training(data_split())
  })
  
  test_data <- reactive({
    testing(data_split())
  })
  
  
  create_recipe <- function(ds, ratio){
    ds_recipe <- recipe(good_bad_flag ~ ., data = train_data) %>%
      ## log transform data on numeric variables
      step_log(all_numeric()) %>%
      ## converts all nominal variables to factors related to categorical variables
      step_novel(all_nominal(), -all_outcomes()) %>%
      ##  converts all nominal variables into numeric variables
      step_dummy(all_nominal(), -all_outcomes())
    
    ## return recipe
    return (ds_recipe)
  }

  
  train_recipe <- reactive({
    create_recipe(train_data(), 5)
  })
  
  test_recipe <- reactive({
    create_recipe(test_data(), 5)
  })
  
  create_juice <- function(ds_recipe){
    ds_juice <- ds_recipe %>%
      prep() %>%
      juice()
    return (ds_juice)
  }
  

  prepared_train_data <- reactive({
    create_juice(train_recipe)
  })
  
  prepared_test_data <- reactive({
    create_juice(test_recipe)
  })  
  
  
  m1  <- reactive({
    glm(good_bad_flag ~ ., family=binomial("logit"), data = prepared_train_data())
  })  
  
  # chart <- performance::check_collinearity(m1())
  # 
  # output$vifplot <- renderPlot({
  #   p = plot(chart()) +
  #     
  #     p + theme_economist()
  #   
  # })
  
  }