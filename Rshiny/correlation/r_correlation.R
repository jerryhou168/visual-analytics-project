analysis_correlation <- function(input, output, session, loanType, variables){
  
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
  
  train_recipe <- data_processing_recipe(train_data, 
                                         "no_sample", 
                                         1, 
                                         0.95, 
                                         TRUE, 
                                         TRUE, 
                                         TRUE)
  
  normalized_train_data <- data_processing_juice(train_recipe)
  
  output$corrplot1 <- renderPlot({
    ggstatsplot::ggcorrmat(
      data = normalized_train_data,
      title = "Correlogram"
    )
  })
  
  print('correlation plot rendered')
  
}

plot_correlation <- function(input, output, session, eventType, defaultVariablelist){
  
  loanType <- input$t3_loanType
  
  variables <- input$t3_variables
  
  if(length(variables) <= 1){
    output$corrplot <- renderPlot({
      ggplot()
    })
    output$t3_o_vif_table <- renderDataTable({})
    print('empty correlation plot rendered')
  }else{
    if(eventType == 1){
      analysis_correlation(input, output, session, loanType, defaultVariablelist)
    }else{
      analysis_correlation(input, output, session, loanType, variables)
    }
  }
}