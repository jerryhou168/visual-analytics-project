library(shiny)
library(corrplot)
library(ggplot2)
library(patchwork)
library(ggthemes)
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
                        mainPanel(plotOutput(outputId = "mainexploreplot"))
               ),
               tabPanel("Correlation Analysis",
                        # Input selections on the left panel
                        
                        # New or repeat loan
                        sidebarPanel(
                          selectInput(inputId = "Corr_LoanType",
                                      label = "Select type of loans",
                                      choices = c("Single loan" = "SINGLE LOAN",
                                                  "Repeat loans" = "REPEAT LOANS"),
                                      selected = "SINGLE LOAN"
                                      ),
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
# Disable cache to avoid errors due to cache  

  
  output$Corr_Variables <- renderUI({

    if (input$Corr_LoanType == "SINGLE LOAN"){
      corr_choices <- c("approval_duration",
                   "loanamount",
                   "totaldue",
                   "age_at_loan",
                   "longitude_gps",
                   "latitude_gps")
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
                        "max_interest_rate",
                        "mean_interst_rate",
                        "mean_referrals",
                        "max_chun_flag",
                        "mean_churn_flag",
                        "loannumber",
                        "totaldue",
                        "longitude_gps",
                        "latitude_gps")
    }
    selectInput(inputId = "Corr_Variables",
                label = "Select variables from below",
                choices = corr_choices,
                multiple = TRUE)
  })
  
  data <- reactive({
    if (input$LoanType == "SINGLE LOAN") {
      return(newloan)
    } else {
      return(repeatloan)
    }
  })  
  
  
  output$Bi_Variable_Y <- renderUI({
    selectInput(inputId = "Bi_Variable_Y",
                label = "Select variable Y from below",
                choices = names(data()),
                selected = "termdays",                
                multiple = FALSE)
  })
  output$Bi_Variable_X <- renderUI({
    selectInput(inputId = "Bi_Variable_X",
                label = "Select variable X from below",
                choices = names(data()),
                selected = "termdays",
                multiple = FALSE)
  })


  
  # Correlation plot  
  output$Correlationplot <- renderPlot({
    # To check if inputs are available before generation of plots
    req(input$Corr_LoanType)
    req(input$Corr_Variables)
    var_data <- data()[, input$Corr_Variables]
    # print(var_data)
    corr_data <- cor(var_data)
    # Using corrplot from corrplot library
    corrplot(corr_data,
             method = "ellipse",
             diag = FALSE,
             type = "lower")
    
  })
  

  output$mainexploreplot <- renderPlot({
    req(input$LoanType)
# Barchart for two categorical variables    
    if (input$Bi_Variable_Y %in% c("good_bad_flag",
                                  "bank_name_clients",
                                  "approval_duration_group",
                                  "age_at_loan_25th_pctile",
                                  "credit_rating",
                                  "termdays",
                                  "employment_status_risk",
                                  "level_of_education_risk",
                                  "referral",
                                  "bank_account_type_recode",
                                  "loanamount_group",
                                  "bank_account_type",
                                  "validation",
                                  "employment_status") &
      input$Bi_Variable_X %in% c("good_bad_flag",
                                   "bank_name_clients",
                                   "approval_duration_group",
                                   "age_at_loan_25th_pctile",
                                   "credit_rating",
                                   "termdays",
                                   "employment_status_risk",
                                   "level_of_education_risk",
                                   "referral",
                                   "bank_account_type_recode",
                                   "loanamount_group",
                                   "bank_account_type",
                                   "validation",
                                   "employment_status")) {
      p1 <- ggplot(data(), aes(x = factor(.data[[input$Bi_Variable_Y]]))) +
        geom_bar() + 
        theme_economist()

      p2 <- ggplot(data(), aes(x = factor(.data[[input$Bi_Variable_X]]))) +
        geom_bar() +
        theme_economist()
      p1 + p2 + plot_layout(ncol = 1)
        
# Boxplot for one categorical and one continuous variables      
    } else if (input$Bi_Variable_Y %in% c("good_bad_flag",
                                          "bank_name_clients",
                                          "approval_duration_group",
                                          "age_at_loan_25th_pctile",
                                          "credit_rating",
                                          "termdays",
                                          "employment_status_risk",
                                          "level_of_education_risk",
                                          "referral",
                                          "bank_account_type_recode",
                                          "loanamount_group",
                                          "bank_account_type",
                                          "validation",
                                          "employment_status") &
               !input$Bi_Variable_X %in% c("good_bad_flag",
                                           "bank_name_clients",
                                           "approval_duration_group",
                                           "age_at_loan_25th_pctile",
                                           "credit_rating",
                                           "termdays",
                                           "employment_status_risk",
                                           "level_of_education_risk",
                                           "referral",
                                           "bank_account_type_recode",
                                           "loanamount_group",
                                           "bank_account_type",
                                           "validation",
                                           "employment_status")) {
      p <- ggplot(data(), aes(x = factor(.data[[input$Bi_Variable_Y]]),
                               y = .data[[input$Bi_Variable_X]])) +
        geom_boxplot()
      p + theme_economist()

# Boxplot for one categorical and one continuous variables    
    } else if (!input$Bi_Variable_Y %in% c("good_bad_flag",
                                           "bank_name_clients",
                                           "approval_duration_group",
                                           "age_at_loan_25th_pctile",
                                           "credit_rating",
                                           "termdays",
                                           "employment_status_risk",
                                           "level_of_education_risk",
                                           "referral",
                                           "bank_account_type_recode",
                                           "loanamount_group",
                                           "bank_account_type",
                                           "validation",
                                           "employment_status") &
                input$Bi_Variable_X %in% c("good_bad_flag",
                                            "bank_name_clients",
                                            "approval_duration_group",
                                            "age_at_loan_25th_pctile",
                                            "credit_rating",
                                            "termdays",
                                            "employment_status_risk",
                                            "level_of_education_risk",
                                            "referral",
                                            "bank_account_type_recode",
                                            "loanamount_group",
                                            "bank_account_type",
                                            "validation",
                                            "employment_status")) {
      p <- ggplot(data(), aes(x = factor(.data[[input$Bi_Variable_X]]),
                              y = .data[[input$Bi_Variable_Y]])) +
        geom_boxplot()
      p + theme_economist()
    
# Scatter plot for two continuous variables    
    } else if (!input$Bi_Variable_Y %in% c("good_bad_flag",
                                            "bank_name_clients",
                                            "approval_duration_group",
                                            "age_at_loan_25th_pctile",
                                            "credit_rating",
                                            "termdays",
                                            "employment_status_risk",
                                            "level_of_education_risk",
                                            "referral",
                                            "bank_account_type_recode",
                                            "loanamount_group",
                                            "bank_account_type",
                                            "validation",
                                            "employment_status") &
                !input$Bi_Variable_X %in% c("good_bad_flag",
                                           "bank_name_clients",
                                           "approval_duration_group",
                                           "age_at_loan_25th_pctile",
                                           "credit_rating",
                                           "termdays",
                                           "employment_status_risk",
                                           "level_of_education_risk",
                                           "referral",
                                           "bank_account_type_recode",
                                           "loanamount_group",
                                           "bank_account_type",
                                           "validation",
                                           "employment_status")) {
      p <- ggplot(data(), aes(x = .data[[input$Bi_Variable_X]],
                              y = .data[[input$Bi_Variable_Y]])) +
        geom_point()
      p + theme_economist()
    } else {
      NULL
    }

    
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

