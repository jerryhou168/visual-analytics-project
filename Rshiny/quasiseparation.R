# Define the control panel on the left
t5_loanType <- radioButtons(inputId = "t5_loanType", 
                           label = "Type of Loans",
                           choices = c("New Loan" = "S",
                                       "Repeat Loan" = "R"),
                           selected = "S", inline = TRUE)

# Define the variables to be studied in this tab

qs_newloan_predictor_options <- setNames(newloan_factors, 
                                         newloan_factorNames)

qs_repeatloan_predictor_options <- setNames(repeatloan_factors, 
                                            repeatloan_factorNames)

t5_variable_x <- uiOutput(outputId = "qs_variable_X")

# Define the navigation bar
quasiseparation_nav <- fluidRow(
  t5_loanType,
  t5_variable_x
)

quasiseparation_main <-fluidRow(
  plotOutput("quasiplot",
             width = "100%",
             height = 600)
)

# Server

renderQuasiSeparation <- function(input, output, session, factor) {
  
  tgtdata <- repeatloan
  
  if(input$t5_loanType == "S") {
    tgtdata <- newloan
  }
  
  output$quasiplot <- renderPlot({
    ggplot(tgtdata, aes(x = .data[[factor]] , 
                        fill = good_bad_flag )) + 
      geom_bar()+
      theme_economist()+
      labs(title = "Quasi Complete Seperataion",
           fill = "Loans Quality") +
      xlab(common_variable_name(input$t5_loanType, factor))+
      ylab("Count")+
      theme(axis.title.y = element_text(vjust = 2.5),
            axis.text.x = element_text(angle = 60,
                                       vjust = 0.5))
  })
  
}

quasicomplete <- function(input, output, session) {
  
  qs_variables <- reactive({
    if (input$t5_loanType == "S") {
      return(qs_newloan_predictor_options)
    } else {
      return(qs_repeatloan_predictor_options)
    }
  })
  
  output$qs_variable_X <- renderUI({
    radioButtons(inputId = "t5_variable",
                label = "Variables",
                choices = qs_variables())
  })
  
  observeEvent(input$t5_loanType, {
    default_variable = 'pct_ontime'
    if(input$t5_loanType == "S"){
      default_variable = 'bank_name_clients'
    }
    renderQuasiSeparation(input, output, session, default_variable)
  })
  
  observeEvent(input$t5_variable, {
    renderQuasiSeparation(input, output, session, input$t5_variable)
  })
}