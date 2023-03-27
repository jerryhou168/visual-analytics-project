# Define the control panel on the left
t1_loanType <- radioButtons(inputId = "t1_loanType", 
                           label = "Type of Loans",
                           choices = c("New Loan" = "S",
                                       "Repeat Loan" = "R"),
                           selected = "S", inline = TRUE)

# Define the variables to be studied in this tab

t1_variable_x <- uiOutput(outputId = "Uni_Variable_X")

# Define the navigation bar
univariate_nav <- fluidRow(
  t1_loanType,
  t1_variable_x
)

univariate_main <-fluidRow(
  id="univariate_chart",
  plotOutput("univarplot",
             width = "100%",
             height = 600)
)

# Server

renderUnivariate <- function(input, output, session, factor) {
  
  if(input$t1_loanType == "S") {
    tgtdata <- newloan
  }else{
    tgtdata <- repeatloan
  }
  
  output$univarplot <- renderPlot({
    
    update_geom_defaults(geom = "rect", new = list(fill = c("#75D5D7")))
    
    ggplot(tgtdata, aes(x = .data[[factor]], fill = .data[[factor]])) +
      geom_bar() + 
      xlab(common_variable_name(input$t1_loanType, factor))+
      ylab("Count")+
      theme_economist() +
      labs(title = "Univariate Analysis") +
      theme(axis.title.y = element_text(vjust = 2.5),
            axis.text.x = element_text(angle = 60,
                                       vjust = 0.5))
  })
}

univar <- function(input, output, session) {
  
  univar_variables <- reactive({
    if (input$t1_loanType == "S") {
      return(newloan_factor_options)
    } else {
      return(repeatloan_factor_options)
    }
  })
  
  output$Uni_Variable_X <- renderUI({
    radioButtons(inputId = "t1_variable",
                label = "Variables",
                choices = univar_variables())
  })
  
  observeEvent(input$t1_loanType, {
    #default_variable = 'pct_ontime'
    #if(input$t1_loanType == "S"){
    #  default_variable = 'bank_name_clients'
    #}
    renderUnivariate(input, output, session, 'bank_name_clients')
  })
  
  observeEvent(input$t1_variable, {
    renderUnivariate(input, output, session, input$t1_variable)
  })
  
}
        

  

  
  
  
  