samplingCompleted <- function(input, output, session){
  
  Global_Prediction_WIP_Flag = FALSE
  
  mtd <- input$t0_method
  
  if(mtd == 'no_sample'){
    common_hide_all('hide')
    return(NULL)
  }
  
  common_display_msg(output, TRUE, 'Data sampling completed')
  common_display(output, 'S', 'show', NULL)
  
}

sampling <- function(input, output, session){
  
  typ <- input$t0_loanType
  mtd <- input$t0_method
  rat <- input$t0_ratio
  cor <- input$t0_correlation
  nrm <- input$t0_normalize
  rna <- input$t0_remove_nan
  rzv <- input$t0_remove_zv
  spt <- input$t0_split
  
  if(mtd == 'no_sample'){
    common_hide_all('hide')
    return(NULL)
  }
  
  drs_data <- reactive({
    if (typ == "R") {
      return(repeatloan)
    }else{
      return(newloan)
    }
  })
  
  set.seed(123)
  
  data_split <- initial_split(drs_data(), prop = spt, strata = good_bad_flag)
  
  tain_data <- training(data_split)
  
  train_recipe <- data_processing_recipe(tain_data, mtd, rat, cor, nrm, rna, rzv)
  
  train_juice <- data_processing_juice(train_recipe)
  
  ## original data attributes
  df_compare_vars <- data_attribute_variables(drs_data(), train_juice)
  df_compare_size <- data_attribute_sizing(drs_data(), train_juice)
  
  original_factors_table <- data.frame(Variable = names(drs_data()))
  
  output$sto11 <- renderDataTable(original_factors_table, 
                                  options=list(
                                    title = "Variables Before Sampling",
                                    pageLength = 10, 
                                    lengthChange = FALSE, 
                                    searching = FALSE,
                                    info = FALSE,
                                    pagingType = "simple"))
  
  ## view comparison
  output$spo11 <- renderPlot({
    ggplot(data = df_compare_vars, aes(x = Name, y = Count)) +
      geom_bar(stat="identity", fill = c("#F2B0AC","#75D5D7")) +
      ggtitle("Predictors") +
      xlab("Dataset Name") +
      ylab("No. of Predictors") +
      theme_bw()
  })
  
  sampled_factors_table <- data.frame(Variable = names(train_juice))
  
  output$sto12 <- renderDataTable(sampled_factors_table, 
                                  options=list(
                                    title = "Variables After Sampling",
                                    pageLength = 10,
                                    lengthChange = FALSE, 
                                    searching = FALSE,
                                    info = FALSE,
                                    pagingType = "simple"))
  
  output$spo12 <- renderPlot({
    ggplot(data = df_compare_size, aes(x = Name, y = Size)) +
      geom_bar(stat="identity", fill = c("#F2B0AC","#75D5D7")) +
      ggtitle("Data Size") +
      xlab("Dataset Name") +
      ylab("No. of Records") +
      theme_bw()
  })
}
