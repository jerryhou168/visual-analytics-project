analysis_multicollinearity <- function(input, output, session, loanType, variables){
  
  multi_data <- reactive({
    tgt <- c(Global_Prediction_TGT_Flag, variables)
    if (loanType == "R") {
      seleted_data1 <- repeatloan[tgt]
      return(seleted_data1)
    }else{
      seleted_data2 <- newloan[tgt]
      return(seleted_data2)
    }
  })
  
  set.seed(123)
  
  data_split <- initial_split(multi_data(), prop = 0.9, strata = good_bad_flag)
  
  ## 9 / 10
  train_data <- training(data_split) 
  
  ## 1 / 10
  test_data <- testing(data_split)
  
  train_recipe <- data_multicollinearity_recipe(train_data, 
                                         "no_sample", 
                                         0.90, 
                                         0.90, 
                                         TRUE, 
                                         TRUE, 
                                         TRUE)
  print("step1")
  normalized_train_data <- data_processing_juice(train_recipe)
  print("step2")
  
  fit_model <- glm(good_bad_flag ~ ., family=binomial("logit"), data = normalized_train_data)
  
  print("step3")
  chart <- check_collinearity(fit_model)
  
  print("step4")
  output$vifplot <- renderPlot({
    plot(chart)
  })
  
  print("step5")
  output$t4_o_vif_table <- renderDataTable(chart, 
                                  options=list(
                                    title = "VIF",
                                    pageLength = 10,
                                    lengthChange = FALSE, 
                                    searching = FALSE,
                                    info = FALSE,
                                    pagingType = "simple"))
  
  print('multi-collinearity plot rendered')
  
}

plot_multicollinearity <- function(input, output, session, eventType, defaultVariablelist){
  
  loanType <- input$t4_loanType
  
  variables <- input$t4_variables
  
  if(length(variables) <= 1){
    output$vifplot <- renderPlot({
      ggplot()
    })
    output$t4_o_vif_table <- renderDataTable({})
    print('empty multi-collinearity plot rendered')
  }else{
    if(eventType == 1){
      analysis_multicollinearity(input, output, session, loanType, defaultVariablelist)
    }else{
      analysis_multicollinearity(input, output, session, loanType, variables)
    }
  }
}