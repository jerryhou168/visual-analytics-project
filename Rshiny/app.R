library(shiny)
library(corrplot)
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
                        mainPanel(plotOutput(outputId = "barchart1"))
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
    selectInput(inputId = "Variables",
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
  # output$Variables <- renderUI({
  #   selectInput(inputId = "Variables",
  #               label = "Select variables from below",
  #               choices = names(data()),
  #               multiple = TRUE)
  # })
  
  
  
# Correlation plot  
  output$Correlationplot <- renderPlot({
    # To check if inputs are available before generation of plots
    req(input$LoanType)
    req(input$Variables)
    var_data <- data()[, input$Variables]
    corr_data <- cor(var_data)
    # Using corrplot from corrplot library
    corrplot(corr_data,
             method = "ellipse",
             diag = FALSE,
             type = "lower")

  })
  
# Below is not updated yet
  output$histogram <- renderPlot({
    if(!is.null(input$Variable)) {
      hist_data_numeric <- as.numeric(data()[input$Variable]) 
      ggplot(hist_data_numeric,
             aes(x = .data)) +
        geom_histogram() +
        labs(title = "Distribution of Selected Variables")}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

