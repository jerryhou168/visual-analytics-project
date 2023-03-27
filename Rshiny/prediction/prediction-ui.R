prediction_alert_panel <- fluidRow(
  id="coalert",
  fluidRow(
    column(12, htmlOutput(outputId = "costatus")),
    style = "background-color: white; padding:0px 17px 0px 17px;"
  )
)

prediction_panel_list = c(1,2,3,4,5,6,7,8,9)

prediction_main <- fluidRow(
  prediction_alert_panel,
  sampling_output_panel,
  prediction_output_panel_1,
  prediction_output_panel_2,
  prediction_output_panel_3
)

common_display <- function(output, type, toggle, section){
  if(type == 'S'){
    if(toggle == 'show'){
      shinyjs::show('sorow')
    }
    if(toggle == 'hide'){
      shinyjs::hide('sorow')
      sampling_display(output, 'hide')
    }
  }
  if(type == 'A'){
    if(toggle == 'show'){
      shinyjs::show('coalert')
    }
    if(toggle == 'hide'){
      shinyjs::hide('coalert')
    }
  }
  if(type == 'P'){
    if(toggle == 'show'){
      if(section == 1){
        prediction_display_1(output, 'show')
      }
      if(section == 2){
        prediction_display_2(output, 'show')
      }
      if(section == 3){
        prediction_display_3(output, 'show')
      }
    }
    if(toggle == 'hide'){
      prediction_display_1(output, 'hide')
      prediction_display_2(output, 'hide')
      prediction_display_3(output, 'hide')
    }
  }
}

common_hide_all <- function(output, toggle){
  ## hide all charts
  common_display(output, 'S', toggle, NULL)
  common_display(output, 'A', toggle, NULL)
  common_display(output, 'P', toggle, NULL)
}

prediction_parameter_reset <- function(input, output, session){
  
  ## hide all
  common_hide_all(output, 'hide')
  
  output$costatus <- renderText({ "<div></div>" })
  
  updateSelectInput(session, "t0_loanType",  selected = "S")
  updateSelectInput(session, "t0_algorithm",  selected = "BT")
  updateSelectInput(session, "t0_method", selected = "step_smote")
  updateSelectInput(session, 't0_predictors', selected = c("credit_rating",
                                                           "age_at_loan",
                                                           "bank_account_type"))
  
  updateSliderInput(session, "t0_ratio", value = 2)
  updateSliderInput(session, "t0_trees", value = 50)
  updateSliderInput(session, "t0_vfolds", value = 5)
  updateSliderInput(session, "t0_treedepth", value = 7)
  updateSliderInput(session, "t0_split", value = 0.75)
  
  updateSliderInput(session, "t0_correlation", value = 0.85)
  updateCheckboxInput(session, "t0_normalize", value = TRUE)
  updateCheckboxInput(session, "t0_remove_nan", value = TRUE)
  updateCheckboxInput(session, "t0_remove_zv", value = TRUE)
}

common_display_msg <- function(output, type, content){
  
  colourName <- '#75D5D7';
  
  if(type == TRUE){
    colourName <- '#75D5D7';
  }else{
    colourName <- '#F2B0AC';
  }
  
  html =paste0("<div style='margin:0px 15px 0px 15px; font-size: 15px; ",
               "padding: 5px 8px 8px 5px; background-color: #F0F0F0; ",
               "font-weight:bold; color: ", colourName, "'>", content, "</div>")
  
  output$costatus <- renderText({ html })
  common_display(output, 'A', 'show', 1)
}

prediction_parameter_loading <- function(input, output, session){
  
  common_hide_all(output, 'hide')
  
  predictors_opts <- reactive({
    if (input$t0_loanType == "S") {
      return(newloan_factor_options)
    } else {
      return(repeatloan_factor_options)
    }
  })
  
  predictors_def_opts <- reactive({
    if (input$t0_loanType == "S") {
      return(c("age_at_loan","bank_account_type", "credit_rating"))
    } else {
      return(c("pct_ontime","avg_age_at_loan", "max_approval_duration"))
    }
  })
  
  output$o0_predictors <- renderUI({
    selectInput(inputId = "t0_predictors",
                label = "Variables",
                choices = predictors_opts(),
                selected = predictors_def_opts(),
                multiple = TRUE)
  })
  
  observeEvent(input$reset1, {
    prediction_parameter_reset(input, output, session)
  })
  
  observeEvent(input$reset2, {
    prediction_parameter_reset(input, output, session)
  })
  
  observeEvent(input$sample, {
    
    common_hide_all(output, 'hide')
    
    if(Global_Prediction_WIP_Flag){
      common_display_msg(output, FALSE, 'An existing task is working in progress, please wait...')
      return(NULL)
    }
    
    ## asynchronous processing started
    tryCatch(
      Global_Prediction_WIP_Flag = TRUE,
      sampling(input, output, session),
      samplingCompleted(input, output, session),
      warning = function(warn_msg){
        print(paste0("warning in sampling: ", warn_msg))
      },
      error = function(error_msg) {
        ## rest prediction flag
        Global_Prediction_WIP_Flag = FALSE
        print(paste0("error in sampling: ", error_msg))
        common_display_msg(output, FALSE, paste0('ERROR:',error_msg))
      }
    )
  })
  
  observeEvent(input$predict, {
    
    common_hide_all(output, 'hide')
    
    if(Global_Prediction_WIP_Flag){
      common_display_msg(output, FALSE, 'An existing task is working in progress, please wait...')
      return(NULL)
    }
    
    selected_alogrithms <- length(input$t0_algorithm)
    
    if(selected_alogrithms <= 0){
      common_display_msg(output, FALSE, 'Please choose at least one predicting algorithm to proceed.')
      return(NULL)
    }
    
    selected_predictors <- length(input$t0_predictors)
    
    if(selected_predictors <= 2){
      common_display_msg(output, FALSE, 'Please choose at least three variables to proceed.')
      return(NULL)
    }
    
    tryCatch(
      Global_Prediction_WIP_Flag = TRUE,
      predicting(input, output, session),
      predictingCompleted(input, output, session),
      warning = function(warn_msg){
        print(paste0("warning in predicting: ", warn_msg))
      },
      error = function(error_msg) {
        ## rest prediction flag
        Global_Prediction_WIP_Flag = FALSE
        print(error_msg)
        print(paste0("error in predicting: ", error_msg))
        common_display_msg(output, FALSE, paste0('ERROR:',error_msg))
      }
    )
  })
}

