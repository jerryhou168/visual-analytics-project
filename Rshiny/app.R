library(shiny)
library(corrplot)
library(ggplot2)
library(tidyverse)


# Datset loading
newloan <- read_csv("data/loan_data_v2/new_loans_cleaned.csv")

repeatloan <- read_csv("data/loan_data_v2/repeated_loans_cleaned.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # CSS style rules to customise tab formatting usage
  tags$head(
    tags$style(".nav.nav-tabs {
                       display: flex;
                       border: none;
                       justify-content: space-between;
                       padding: 0 15px;
                       font-size: 18px;
                       color: #4472C4;
                       font-family: Helvetica, Helvetica}
                       ")),
  
  
  
  # Application title
  titlePanel("Loan Default Prediction"),
  
  
  
  
  
  # Tabset panel with three options at the top of the main panel
  tabsetPanel(
    tabPanel("Explore",
             tabsetPanel(
               tabPanel("Bivariate Analysis",
                        # Input selections on the left panel
                        
                        # New or repeat loan
                        sidebarPanel(
                          selectInput(inputId = "LoanType",
                                      label = "Select type of loans",
                                      choices = c("Single loan" = "SINGLE LOAN",
                                                  "Repeat loans" = "REPEAT LOANS"),
                                      selected = "SINGLE LOAN"),
                          # Allow selection of variables from the chosen loan's dataset    
                          uiOutput(outputId = "Bi_Variable_Y"),
                          uiOutput(outputId = "Bi_Variable_X")),
                        mainPanel(plotOutput(outputId = "histogram"))
               ),
               tabPanel("Correlation Analysis",
                        # Input selections on the left panel
                        
                        # New or repeat loan
                        sidebarPanel(
                          selectInput(inputId = "LoanType",
                                      label = "Select type of loans",
                                      choices = c("Single loan" = "SINGLE LOAN",
                                                  "Repeat loans" = "REPEAT LOANS"),
                                      selected = "SINGLE LOAN"),
                          # Allow selection of variables from the chosen loan's dataset    
                          uiOutput(outputId = "Corr_Variables")),
                        mainPanel(plotOutput(outputId = "Correlationplot"))
                        
               )
             )),
    tabPanel("Analysis",
             mainPanel("Testing")),
    tabPanel("Predict",
             mainPanel("Testing2"))
  )
)

# Define server logic required to allow reading of different dataset
server <- function(input, output) {
  data <- reactive({
    if (input$LoanType == "SINGLE LOAN") {
      return(newloan)
    } else if (input$LoanType == "REPEAT LOANS") {
      return(repeatloan)
    }
  })
  
  output$Corr_Variables <- renderUI({
    selectInput(inputId = "Corr_Variables",
                label = "Select variables from below",
                choices = c("approval_duration",
                            "loanamount",
                            "totaldue",
                            "age_at_loan",
                            "longitude_gps",
                            "latitude_gps"),
                multiple = TRUE)
  })
  output$Bi_Variable_Y <- renderUI({
    selectInput(inputId = "Bi_Variable_Y",
                label = "Select variable Y from below",
                choices = names(data()),
                multiple = TRUE)
  })
  output$Bi_Variable_X <- renderUI({
    selectInput(inputId = "Bi_Variable_X",
                label = "Select variable X from below",
                choices = names(data()),
                multiple = TRUE)
  })

  # selected_bivars <- reactive({
  #   c(input$Bi_Variable_Y,
  #     input$Bi_Variable_X)
  # })
  
  
  
  # Correlation plot  
  output$Correlationplot <- renderPlot({
    # To check if inputs are available before generation of plots
    req(input$LoanType)
    req(input$Corr_Variables)
    var_data <- data()[, input$Corr_Variables]
    corr_data <- cor(var_data)
    # Using corrplot from corrplot library
    corrplot(corr_data,
             method = "ellipse",
             diag = FALSE,
             type = "lower")
    
  })
  
  # Below is not updated yet
#   output$histogram <- renderPlot({
#     vars <- selected_bivars()
# if (length(vars) > 0) {
#   hist_data_numeric <- data() %>%
#     dplyr::select(all_of(vars)) %>%
#     mutate(value = as.numeric(value))
#   p1 <- ggplot(hist_data_numeric,
#                aes(x = value,
#                    fill = key)) +
#     geom_histogram()
# 
#       if (length(vars) > 1) {
#         hist_data_numeric2 <- as.numeric(data()[vars[2]])
#         p2 <- p1 + geom_histogram(data = hist_data_numeric2,
#                                   aes(x = .data),
#                                   alpha = 0.5,
#                                   fill = 'red')
#       }
#       print(p2)
# 
#     }
#   })
  output$histogram <- renderPlot({
    req(input$LoanType)
    req(input$Bi_Variable_Y)
    ggplot(data(),
           aes(x = factor(.data[[input$Bi_Variable_Y]]))) +
      geom_bar()
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

