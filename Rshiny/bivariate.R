# Define the control panel on the left
source("./bivariate/r_bivariate.R", local = TRUE)

# Define the control panel on the left
t2_loanType <- radioButtons(inputId = "t2_loanType", 
                            label = "Type of Loans",
                            choices = c("New Loan" = "S",
                                        "Repeat Loan" = "R"),
                            selected = "S", inline = TRUE)

# Define the variables to be studied in this tab

t2_variable_x <- uiOutput(outputId = "outputBiVarX")
t2_variable_y <- uiOutput(outputId = "outputBiVarY")

# Define the navigation bar
bivariate_nav <- fluidRow(
  t2_loanType,
  t2_variable_x,
  t2_variable_y
)

bivariate_main <-fluidRow(
  plotOutput(outputId = "bivarplot",
             width = "100%",
             height = 600)
  
)

# Server

bivar <- function(input, output, session) {
  
  bivar_opt <- reactive({
    if (input$t2_loanType == "S") {
      return(newloan_factor_options)
    } else {
      return(repeatloan_factor_options)
    }
  })
  
  default_x_opt <- reactive({
    if (input$t2_loanType == "S") {
      return("age_at_loan")
    }else{
      return("pct_ontime")
    }
  })
  
  default_y_opt <- reactive({
    if (input$t2_loanType == "S") {
      return("age_at_loan_25th_pctile")
    }else{
      return("employment_status")
    }
  })
  
  output$outputBiVarX <- renderUI({
    radioButtons(inputId = "biVarX",
                 label = "Select variable X",
                 choices = bivar_opt(),
                 selected = default_x_opt())
  })
  
  output$outputBiVarY <- renderUI({
    radioButtons(inputId = "biVarY",
                 label = "Select variable Y",
                 choices = bivar_opt(),
                 selected = default_y_opt())
  })
  
  observeEvent(input$t2_loanType, {
    renderBivariateChart(input, output, session, default_x_opt(), default_y_opt())
  })
  
  observeEvent(input$biVarX, {
    renderBivariateChart(input, output, session, input$biVarX, input$biVarY)
  })
  
  observeEvent(input$biVarY, {
    renderBivariateChart(input, output, session, input$biVarX, input$biVarY)
  })
  
}
