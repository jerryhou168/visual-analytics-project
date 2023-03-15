# Define the control panel on the left
t1_loanType <- selectInput(inputId = "t1_loanType", 
                           label = "Type of Loans",
                           choices = c("New Loan" = "S",
                                       "Repeat Loan" = "R"),
                           selected = "S")

# Define the variables to be studied in this tab
newloan_options <- setNames(newloan_factors, 
                            newloan_factors)

repeatloan_options <- setNames(repeatloan_factors, 
                               repeatloan_factors)



t1_variable_x <- uiOutput(outputId = "Uni_Variable_X")




# Define the navigation bar
univariate_nav <- fluidRow(
  t1_loanType,
  t1_variable_x
)


univariate_main <-fluidRow(
  plotOutput(outputId = "Univarplot",
             width = "100%",
             height = 400)
  
)


# Server

univar <- function(input, output, session) {

  
  univar_opt <- reactive({
      if (input$t1_loanType == "S") {
        return(newloan_options)
      } else {
        return(repeatloan_options)
      }
    })
  
  output$Uni_Variable_X <- renderUI({
      selectInput(inputId = "Uni_Variable_X",
                  label = "Select variable X from below",
                  choices = names(univar_opt()),
                  multiple = FALSE)
    })

  
  univar_data <- reactive({
      if (input$t1_loanType == "S") {
        return(newloan)
      } 
    else {
         return(repeatloan)
       }
  })  

  output$Univarplot <- renderPlot({
    # Barchart for 1 variable   
    {
    p1 <- ggplot(univar_data(), aes(x =input$Uni_Variable_X)) +
        geom_histogram()
      
      p1
    }
    
    })
  }
        

  
  
  
  
  