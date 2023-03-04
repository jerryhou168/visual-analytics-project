library(shiny)
library(tidyverse)

# Datset loading
newloan_working <- read_csv("data/newloan/New_loan_working_dataset.csv")
newloan <- read_csv("data/newloan/New_loan_final_dataset.csv")

repeatloan_working <- read_csv("data/repeatloan/Repeat_Loan_working_dataset.csv")
repeatloan <- read_csv("data/repeatloan/Repeat_Loan_final_dataset.csv")


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
  # Input selections on the left panel

  # New or repeat loan
  sidebarPanel(
    selectInput(inputId = "LoanType",
                label = "Select type of loans",
                choices = c("Single loan" = "SINGLE LOAN",
                            "Repeat loans" = "REPEAT LOANS"),
                selected = "SINGLE LOAN"),
  # Allow selection of variables from the chosen loan's dataset    
    uiOutput(outputId = "Variables"),
    uiOutput(outputId = "Correlations"),

    sliderInput(inputId = "yearSlider",
                label = "Year",
                min = 2017,
                max = 2019,
                value = 2017,
                step = 1,
                sep = "")),
  # Tabset panel with three options at the top of the main panel
  tabsetPanel(
    tabPanel("Explore"),
    tabPanel("Analysis"),
    tabPanel("Predict")
  ),
  # Mainpanel 
  mainPanel(
    # Upper plot
    plotOutput(outputId = "correlationplot"),
    
    # Lower plot
    plotOutput(outputId = "histogram")
  ))

# Define server logic required to allow reading of different dataset
server <- function(input, output) {
  data <- reactive({
    if (input$LoanType == "SINGLE LOAN") {
      return(newloan)
    } else if (input$LoanType == "REPEAT LOANS") {
      return(repeatloan)
    }
  })
  
  output$Variables <- renderUI({
    selectInput(inputId = "Variable",
                label = "Select variables from below for distribution analysis",
                choices = names(data()),
                multiple = TRUE)
  })
  output$Correlations <- renderUI({
    selectInput(inputId = "Correlations",
                label = "Select the pair of variables for correlation analysis",
                choices = names(data()),
                multiple = TRUE)
  })
  
  
  
  
  # [CORRECTION NEEDED. NOT VERY SURE HOW TO HANDLE THIS PART]
  # Define server logic required to draw histogram and correlation plot
  output$correlationplot <- renderPlot({
    # plot code to be input
  })
  output$histogram <- renderPlot({
    if(!is.null(input$Variable)) {
      hist_data_numeric <- as.numeric(data()[input$Variable]) 
      ggplot(hist_data,
           aes(x = hist_data_numeric)) +
        geom_histogram() +
        labs(title = "Distribution of Selected Variables")}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
