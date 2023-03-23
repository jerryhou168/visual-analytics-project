# Define the control panel on the left
t3_loanType <- selectInput(inputId = "t3_loanType", 
                           label = "Type of Loans",
                           choices = c("New Loan" = "S",
                                       "Repeat Loan" = "R"),
                           selected = "S")

# Define the variables to be studied in this tab
newloan_options <- setNames(newloan_factors, 
                            newloan_factors)

repeatloan_options <- setNames(repeatloan_factors, 
                               repeatloan_factors)



t3_variable <- uiOutput(outputId = "Corr_Variables")





correlation_nav <- fluidRow(
  t3_loanType,
  t3_variable
  
)


correlation_main <-fluidRow(
  plotOutput(outputId = "Corrplot",
             width = "100%",
             height = 400)
)


corr <- function(input, output){
  
  corr_opt <- reactive({
    if (input$t3_loanType == "S") {
      corr_choices <- c("approval_duration",
                        "totaldue",
                        "age_at_loan")
    } else {
      corr_choices <- c("pct_ontime",
                        "total_ontime",
                        "max_active_of_loans",
                        "max_approval_duration",
                        "max_age_at_loan",
                        "avg_age_at_loan",
                        "total_num_of_loans",
                        "total_approval_duration",
                        "mean_approval_duration",
                        "totaldue")
    }
  })  
  
  output$Corr_Variables <- renderUI({
    selectInput(inputId = "Corr_Variables",
                label = "Select variables from below",
                choices = corr_opt(),
                multiple = TRUE)
  })
  
  
   
  corr_data <- reactive({
    if (input$t3_loanType == "S") {
      return(newloan)
    } else {
      return(repeatloan)
    }
  })

  
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
    # usr <- par("usr")
    # on.exit(par(usr))
    r <- abs(cor(x, y, use = "complete.obs"))
    txt <- format(c(r, 0.123456789),
                  digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5,
         0.5,
         txt,
         cex = cex.cor * (1 + r) /2)
  }
  
  
  # Correlation plot  
  output$Corrplot <- renderPlot({
    # To check if inputs are available before generation of plots
    req(input$t3_loanType)
    # To display warning message when less than two variables are chosen
    if (length(input$Corr_Variables) < 2){
      plot.new()
      text(x = 0.5,
           y = 0.5,
           labels = "Select two or more variables for the correlation plot",
           col = "red",
           cex = 1.5)
      return(NULL)
    } else {
      var_data <- corr_data()[, input$Corr_Variables]
      
      cor_data <- cor(var_data)
      par(usr = c(0, 1, 0, 1))
      
      # Using corrplot from corrplot library
      pairs(cor_data,
            upper.panel = panel.cor)
    }
  })

}
