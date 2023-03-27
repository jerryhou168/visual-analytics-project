
tidy_predict_model <- function(algorithm, no_of_trees, treedpeth){
  
  alg_spec <- rand_forest(trees = no_of_trees, 
                          mode = "classification") %>%
    set_engine(engine = "randomForest") %>%
    set_mode("classification")
  
  if(algorithm == 'BT'){
    ## Boosted Tree
    alg_spec <- boost_tree(tree_depth = treedpeth, 
                           trees = no_of_trees, 
                           mode = "classification") %>%
      set_engine(engine = "xgboost") %>%
      set_mode("classification")
  }
  
  if(algorithm == 'LR'){
    ## Logistic Regression
    alg_spec <- logistic_reg() %>%
      set_engine(engine = "glm") %>%
      set_mode("classification")
  }
  
  return(alg_spec)
}

predict_algorithm <- function(alg){
  if(alg == 'BT'){
    return("Boosted Tree")
  }else if(alg == 'RF'){
    return("Random Forest")
  }else if(alg == 'LR'){
    return("Logistic Regression")
  }else{
    return("<N/A>")
  }
}

common_predicting <- function(input, output, session, alg, container){
  
  ## collecting all variables
  typ <- input$t0_loanType
  mtd <- input$t0_method
  pdl <- input$t0_predictors
  
  spt <- input$t0_split
  vfd <- input$t0_vfolds
  
  rat <- input$t0_ratio
  cor <- input$t0_correlation
  nrm <- input$t0_normalize
  rna <- input$t0_remove_nan
  rzv <- input$t0_remove_zv
  
  tre <- input$t0_trees
  tdp <- input$t0_treedepth
  
  ldp_data <- reactive({
    tgt <- c(Global_Prediction_TGT_Flag, pdl)
    if (typ == "R") {
      seleted_data1 <- repeatloan_prediction_ds[tgt]
      return(seleted_data1)
    }else{
      seleted_data2 <- newloan_prediction_ds[tgt]
      return(seleted_data2)
    }
  })
  
  ## set seed 
  set.seed(123)
  
  ## split data based on split parameters
  data_split <- initial_split(ldp_data(), prop = spt, strata = good_bad_flag)
  
  ## obtain train data from data split
  train_data <- training(data_split)
  test_data  <- testing(data_split)
  
  ## create recipe
  train_recipe <- data_processing_recipe(train_data, mtd, rat, cor, nrm, rna, rzv)
  
  ## juiced data
  train_juice <- data_processing_juice(train_recipe)
  
  ## creating cross-validation dataset
  cv_folds <- vfold_cv(data = train_data, v = vfd, strata = good_bad_flag)
  
  
  ## create prediction model
  alg_spec <- tidy_predict_model(alg, tre, tdp)
  
  ## create prediction workflow
  wkflow <- workflow() %>% add_recipe(train_recipe) %>% add_model(alg_spec)
  
  ###########################################################################################################
  ## Training
  ###########################################################################################################
  
  print("training")
  
  train_model_result <- wkflow %>% 
    fit_resamples(
      resamples = cv_folds, 
      metrics = metric_set(recall, precision, f_meas, accuracy, kap, roc_auc, sens, yardstick::spec),
      control = control_resamples(save_pred = TRUE)
    )
  
  train_model_predictions <- train_model_result %>% collect_predictions()
  
  ## model training results - ROC curve
  output[[container[1]]] <- renderPlot({
    train_model_predictions %>% group_by(id) %>% roc_curve(good_bad_flag, .pred_Bad) %>% autoplot() + 
      ggtitle(paste0("Training ROC Curve of ", predict_algorithm(alg)))
  })
  
  ## model training results - density chart
  output[[container[2]]] <- renderPlot({
    train_model_predictions %>% ggplot() + geom_density(aes(x = .pred_Bad, fill = good_bad_flag), alpha = 0.5) +
      ggtitle(paste0("Training Density of ", predict_algorithm(alg)))
  })
  
  ## model training results - confusion matrix chart
  output[[container[3]]] <- renderPlot({
    update_geom_defaults(geom = "rect", new = list(fill = c("#F2B0AC", "#75D5D7")))
    train_model_predictions %>% conf_mat(good_bad_flag, .pred_class) %>% autoplot(type = "mosaic") + 
      ggtitle(paste0("Training Confusion Matrix of ", predict_algorithm(alg)))
  })
  
  ## model training results - confusion matrix chart
  output[[container[4]]] <- renderPlot({
    update_geom_defaults(geom = "rect", new = list(fill = c("#F2B0AC", "#75D5D7")))
    train_model_predictions %>% conf_mat(good_bad_flag, .pred_class) %>% autoplot(type = "heatmap") + 
      ggtitle(paste0("Training Confusion Matrix of ", predict_algorithm(alg)))
  })
  
  ###########################################################################################################
  ## Testing
  ###########################################################################################################
  
  print("testing")
  
  test_model_result <- last_fit(wkflow, 
                                split = data_split, 
                                metrics = metric_set(recall, precision, f_meas, accuracy, kap, roc_auc, sens, yardstick::spec)
  )
  
  test_model_predictions <- test_model_result %>% collect_predictions()
  
  
  ## model testing results - ROC curve
  output[[container[5]]] <- renderPlot({
    test_model_predictions %>% group_by(id) %>% roc_curve(good_bad_flag, .pred_Bad) %>% autoplot() + 
      ggtitle(paste0("Testing ROC Curve of ", predict_algorithm(alg)))
  })
  
  ## model testing results - density chart
  output[[container[6]]] <- renderPlot({
    test_model_predictions %>% ggplot() + geom_density(aes(x = .pred_Bad, fill = good_bad_flag), alpha = 0.5) +
      ggtitle(paste0("Testing Density of ", predict_algorithm(alg)))
  })
  
  ## model testing results - confusion matrix chart
  output[[container[7]]] <- renderPlot({
    update_geom_defaults(geom = "rect", new = list(fill = c("#F2B0AC", "#75D5D7")))
    test_model_predictions %>% conf_mat(good_bad_flag, .pred_class) %>% autoplot(type = "mosaic") + 
      ggtitle(paste0("Testing Confusion Matrix of ", predict_algorithm(alg)))
  })
  
  ## model testing results - confusion matrix chart
  output[[container[8]]] <- renderPlot({
    update_geom_defaults(geom = "rect", new = list(fill = c("#F2B0AC", "#75D5D7")))
    test_model_predictions %>% conf_mat(good_bad_flag, .pred_class) %>% autoplot(type = "heatmap") + 
      ggtitle(paste0("Testing Confusion Matrix of ", predict_algorithm(alg)))
  })
  
  ## column contribution
  prediction_fit <- test_model_result %>% extract_fit_parsnip() %>% vip()
  
  ## model testing results - column contribution chart
  output[[container[9]]] <- renderPlot({
    update_geom_defaults(geom = "rect", new = list(fill = c("#75D5D7", "lightblue", "skyblue")))
    prediction_fit +
      ggtitle(paste0("Variable Contribution of ", predict_algorithm(alg)))
  })
  
  ## model testing results - column contribution table
  output[[container[10]]] <- renderDataTable(data.frame(prediction_fit$data), 
                                             options=list(
                                               title = paste0("Variable Contribution of ", predict_algorithm(alg)),
                                               pageLength = 10, 
                                               lengthChange = FALSE, 
                                               searching = FALSE,
                                               info = FALSE,
                                               pagingType = "simple"))
  
  print("prediction completed")
}

