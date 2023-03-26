predictingCompleted <- function(input, output, session){
  
  ## asynchronous processing ended
  Global_Prediction_WIP_Flag = FALSE
  
  
  selected_alogrithm_count <- length(input$t0_algorithm)
  
  if(selected_alogrithm_count > 0){
    common_display_msg(output, TRUE, 'Loan default prediction completed')
    common_display(output, 'P', 'show', length(input$t0_algorithm))
  }else{
    common_display_msg(output, FALSE, 'Please choose at least one predicting algorithm to proceed.')
    common_display(output, 'P', 'hide', NULL)
  }
}

alg_html <- function(name){
  
  fullname <- predict_algorithm(name)
  
  html =paste0("<div style='font-size: 18px; padding: 10px 0px 10px 0px;",
               "font-weight:bold; color: white; ", 
               "'><div style='text-align: center;'>", 
               paste0(fullname, " Prediction"), 
               "</div></div>")
  
  return(html)
}

predicting <- function(input, output, session){
  
  alg <- input$t0_algorithm
  alg_count <- length(alg)
  
  if(alg_count == 1){
    print("single algorithm prediction started ...")
    container <- c("p1co11","p1co21","p1co31","p1co41","p1co12",
                   "p1co22","p1co32","p1co42","p1co51","p1to52")
    
    common_predicting(input, output, session, alg, container)
    print("single algorithm prediction finished")
  }
  
  if(alg_count == 2){
    print("two algorithms prediction started ...")
    containerA <- c("p2co11","p2co21","p2co31","p2co41","p2co51",
                   "p2co61","p2co71","p2co81","p2co91","p2to101")
    
    output$p2so01 <- renderText({ alg_html(alg[1]) })
    
    common_predicting(input, output, session, alg[1], containerA)
    
    containerB <- c("p2co12","p2co22","p2co32","p2co42","p2co52",
                    "p2co62","p2co72","p2co82","p2co92","p2to102")
    
    output$p2so02 <- renderText({ alg_html(alg[2]) })
    
    common_predicting(input, output, session, alg[2], containerB)
    print("two algorithm prediction finished")
  }
  
  if(alg_count == 3){
    print("three algorithms prediction started ...")
    containerA <- c("p3co11","p3co21","p3co31","p3co41","p3co51",
                    "p3co61","p3co71","p3co81","p3co91","p3to101")
    
    output$p3so01 <- renderText({ alg_html(alg[1]) })
    
    common_predicting(input, output, session, alg[1], containerA)
    
    containerB <- c("p3co12","p3co22","p3co32","p3co42","p3co52",
                    "p3co62","p3co72","p3co82","p3co92","p3to102")
    
    output$p3so02 <- renderText({ alg_html(alg[2]) })
    
    common_predicting(input, output, session, alg[2], containerB)
    
    containerC <- c("p3co13","p3co23","p3co33","p3co43","p3co53",
                    "p3co63","p3co73","p3co83","p3co93","p3to103")
    
    output$p3so03 <- renderText({ alg_html(alg[3]) })
    
    common_predicting(input, output, session, alg[3], containerC)
    print("three algorithm prediction finished")
  }
}
