# Define the control panel on the left
t4_loanType <- selectInput(inputId = "t4_loanType", 
                           label = "Type of Loans",
                           choices = c("New Loan" = "S",
                                       "Repeat Loan" = "R"),
                           selected = "S")

# Define the variables to be studied in this tab
newloan_options <- c("approval_duration",
                     "totaldue",
                     "age_at_loan")

repeatloan_options <- c("pct_ontime",
                       "total_ontime",
                       "max_active_of_loans",
                       "max_approval_duration",
                       "max_age_at_loan",
                       "avg_age_at_loan",
                       "total_num_of_loans",
                       "total_approval_duration",
                       "mean_approval_duration",
                       "totaldue")


t4_variable <- uiOutput(outputId = "Multi_Variables")


multi_flag <- c("good_bad_flag")




multicollinearity_nav <- fluidRow(
  t4_loanType,
  t4_variable
  
)


multicollinearity_main <-fluidRow(
  plotOutput(outputId = "Multiliplot",
             width = "100%",
             height = 400)
  
)

multi <- function(input, output, session){

  multi_data <- reactive({
    if (input$t4_loanType == "S") {
      return(newloan)
    } else {
      return(repeatloan)
    }
  })  

  multi_variables <- reactive({
    if (input$t4_loanType == "S") {
      return(newloan_options)
    } else {
      return(repeatloan_options)
    }
  })

  
  observeEvent(input$t4_loanType, {
    default_variable = 'pct_ontime'
    if(input$t4_loanType == "S"){
      default_variable = 'approval_duration'
    }
    selectInput(inputId = "t4_variables",
                label = "Select variables from below",
                choices = multi_variables(),
                selected = default_variable)
#                 multiple = TRUE)
    
  })

  output$Multi_Variables <- renderUI({
    selectInput(inputId = "t4_variables",
                label = "Select variables from below",
                choices = multi_variables(),
                multiple = TRUE)
  })
  
 
  model <- reactive({lm(reformulate(input$Multi_Variables),
                                    multi_flag,
    data = multi_data())}) 
  # summary(reformulate(input$Multi_Variables,
  #                   multi_flag))
  
  # VIF plot  
  output$Multiliplot <- renderPlot({
    # To check if inputs are available before generation of plots
    req(input$t4_variables)
    # To display warning message when less than two variables are chosen
    # if (length(input$t4_variables) < 2){
    #   plot.new()
    #   text(x = 0.5,
    #        y = 0.5,
    #        labels = "Select two or more variables for the VIF plot",
    #        col = "red",
    #        cex = 1.5)
    #   return(NULL)
    # } else {


    # check_collinearity(model)
      check_c <- check_collinearity(model())
      plot(check_c)
#     }
  })
  
  
}




