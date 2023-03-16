# Define the control panel on the left
t2_loanType <- selectInput(inputId = "t2_loanType", 
                           label = "Type of Loans",
                           choices = c("New Loan" = "S",
                                       "Repeat Loan" = "R"),
                           selected = "S")

# Define the variables to be studied in this tab
newloan_options <- setNames(newloan_factors, 
                            newloan_factors)

repeatloan_options <- setNames(repeatloan_factors, 
                               repeatloan_factors)



t2_variable_y <- uiOutput(outputId = "Bi_Variable_Y")
t2_variable_x <- uiOutput(outputId = "Bi_Variable_X")




# Define the navigation bar
bivariate_nav <- fluidRow(
  t2_loanType,
  t2_variable_y,
  t2_variable_x
)

bivariate_main <-fluidRow(
  plotOutput(outputId = "Bivarplot",
             width = "100%",
             height = 400)
  
)


# Server

bivar <- function(input, output) {
  
  bivar_opt <- reactive({
    if (input$t2_loanType == "S") {
      return(newloan_options)
    } else {
      return(repeatloan_options)
    }
  })
  
  output$Bi_Variable_Y <- renderUI({
    selectInput(inputId = "Bi_Variable_Y",
                label = "Select variable Y from below",
                choices = names(bivar_opt()),
                multiple = FALSE)
  })
  output$Bi_Variable_X <- renderUI({
    selectInput(inputId = "Bi_Variable_X",
                label = "Select variable X from below",
                choices = names(bivar_opt()),
                multiple = FALSE)
  })
  
  bivar_data <- reactive({
    if (input$t2_loanType == "S") {
      return(newloan)
    } else {
      return(repeatloan)
    }
  })
  
  output$Bivarplot <- renderPlot({
    # Mosaic plot for two categorical variables    
    if (input$Bi_Variable_Y %in% c("bank_name_clients",
                                   "approval_duration_group",
                                   "age_at_loan_25th_pctile",
                                   "credit_rating",
                                   "termdays",
                                   "employment_status_risk",
                                   "level_of_education_risk",
                                   "referral",
                                   "bank_account_type_recode",
                                   "loanamount_group",
                                   "loanamount",
                                   "Interestrate",
                                   "bank_account_type",
                                   "employment_status",
                                   "total_referrrals",
                                   "max_interest_rate",
                                   "mean_interest_rate",
                                   "mean_referrals",
                                   "max_churn_flag",
                                   "loanamount") &
        input$Bi_Variable_X %in% c("bank_name_clients",
                                   "approval_duration_group",
                                   "age_at_loan_25th_pctile",
                                   "credit_rating",
                                   "termdays",
                                   "employment_status_risk",
                                   "level_of_education_risk",
                                   "referral",
                                   "bank_account_type_recode",
                                   "loanamount_group",
                                   "loanamount",
                                   "Interestrate",
                                   "bank_account_type",
                                   "employment_status",
                                   "total_referrrals",
                                   "max_interest_rate",
                                   "mean_interest_rate",
                                   "mean_referrals",
                                   "max_churn_flag",
                                   "loanamount")) {
      # Barchart for two categorical variables          
      # p1 <- ggplot(bivar_data(), aes(x = factor(.data[[input$Bi_Variable_Y]]))) +
      #   geom_bar() +
      #   theme_economist() +
      #   theme(axis.title.y = element_text(vjust = 2.5),
      #         axis.text.x = element_text(angle = 60,
      #                                    vjust = 0.5))
      # 
      # p2 <- ggplot(bivar_data(), aes(x = factor(.data[[input$Bi_Variable_X]]))) +
      #   geom_bar() +
      #   theme_economist() +
      #   theme(axis.title.y = element_text(vjust = 2.5),
      #         axis.text.x = element_text(angle = 60,
      #                                    vjust = 0.5))
      # p1 + p2 + plot_layout(ncol = 1)
      
      mosaicplot(table(bivar_data()[[input$Bi_Variable_X]], bivar_data()[[input$Bi_Variable_Y]]),
                 color = TRUE,
                 main = paste("Mosaic plot of", input$Bi_Variable_Y, "vs", input$Bi_Variable_X),
                 xlab = input$Bi_Variable_Y,
                 ylab = input$Bi_Variable_X,
                 cex.axis = 0.8,
                 las = 2)

      
      
      
            
      # Boxplot for one categorical and one continuous variables      
    } else if (input$Bi_Variable_Y %in% c("bank_name_clients",
                                          "approval_duration_group",
                                          "age_at_loan_25th_pctile",
                                          "credit_rating",
                                          "termdays",
                                          "employment_status_risk",
                                          "level_of_education_risk",
                                          "referral",
                                          "bank_account_type_recode",
                                          "loanamount_group",
                                          "loanamount",
                                          "Interestrate",
                                          "bank_account_type",
                                          "employment_status",
                                          "total_referrrals",
                                          "max_interest_rate",
                                          "mean_interest_rate",
                                          "mean_referrals",
                                          "max_churn_flag",
                                          "loanamount") &
               !input$Bi_Variable_X %in% c("bank_name_clients",
                                           "approval_duration_group",
                                           "age_at_loan_25th_pctile",
                                           "credit_rating",
                                           "termdays",
                                           "employment_status_risk",
                                           "level_of_education_risk",
                                           "referral",
                                           "bank_account_type_recode",
                                           "loanamount_group",
                                           "loanamount",
                                           "Interestrate",
                                           "bank_account_type",
                                           "employment_status",
                                           "total_referrrals",
                                           "max_interest_rate",
                                           "mean_interest_rate",
                                           "mean_referrals",
                                           "max_churn_flag",
                                           "loanamount")) {
      p <- ggplot(bivar_data(), aes(x = factor(.data[[input$Bi_Variable_Y]]),
                                    y = .data[[input$Bi_Variable_X]])) +
        geom_boxplot()
      p + theme_economist() +
        labs(title = "Bivariate Analysis") +
        theme(axis.title.y = element_text(vjust = 2.5),
              axis.text.x = element_text(angle = 60,
                                         vjust = 0.5))
      
      # Boxplot for one categorical and one continuous variables    
    } else if (!input$Bi_Variable_Y %in% c("bank_name_clients",
                                           "approval_duration_group",
                                           "age_at_loan_25th_pctile",
                                           "credit_rating",
                                           "termdays",
                                           "employment_status_risk",
                                           "level_of_education_risk",
                                           "referral",
                                           "bank_account_type_recode",
                                           "loanamount_group",
                                           "loanamount",
                                           "Interestrate",
                                           "bank_account_type",
                                           "employment_status",
                                           "total_referrrals",
                                           "max_interest_rate",
                                           "mean_interest_rate",
                                           "mean_referrals",
                                           "max_churn_flag",
                                           "loanamount") &
               input$Bi_Variable_X %in% c("bank_name_clients",
                                          "approval_duration_group",
                                          "age_at_loan_25th_pctile",
                                          "credit_rating",
                                          "termdays",
                                          "employment_status_risk",
                                          "level_of_education_risk",
                                          "referral",
                                          "bank_account_type_recode",
                                          "loanamount_group",
                                          "loanamount",
                                          "Interestrate",
                                          "bank_account_type",
                                          "employment_status",
                                          "total_referrrals",
                                          "max_interest_rate",
                                          "mean_interest_rate",
                                          "mean_referrals",
                                          "max_churn_flag",
                                          "loanamount")) {
      p <- ggplot(bivar_data(), aes(x = factor(.data[[input$Bi_Variable_X]]),
                                    y = .data[[input$Bi_Variable_Y]])) +
        geom_boxplot()
      p + theme_economist() +
        labs(title = "Bivariate Analysis") +
        theme(axis.title.y = element_text(vjust = 2.5),
              axis.text.x = element_text(angle = 60,
                                         vjust = 0.5))
      
      # Scatter plot for two continuous variables    
    } else if (!input$Bi_Variable_Y %in% c("bank_name_clients",
                                           "approval_duration_group",
                                           "age_at_loan_25th_pctile",
                                           "credit_rating",
                                           "termdays",
                                           "employment_status_risk",
                                           "level_of_education_risk",
                                           "referral",
                                           "bank_account_type_recode",
                                           "loanamount_group",
                                           "loanamount",
                                           "Interestrate",
                                           "bank_account_type",
                                           "employment_status",
                                           "total_referrrals",
                                           "max_interest_rate",
                                           "mean_interest_rate",
                                           "mean_referrals",
                                           "max_churn_flag",
                                           "loanamount") &
               !input$Bi_Variable_X %in% c("bank_name_clients",
                                           "approval_duration_group",
                                           "age_at_loan_25th_pctile",
                                           "credit_rating",
                                           "termdays",
                                           "employment_status_risk",
                                           "level_of_education_risk",
                                           "referral",
                                           "bank_account_type_recode",
                                           "loanamount_group",
                                           "loanamount",
                                           "Interestrate",
                                           "bank_account_type",
                                           "employment_status",
                                           "total_referrrals",
                                           "max_interest_rate",
                                           "mean_interest_rate",
                                           "mean_referrals",
                                           "max_churn_flag",
                                           "loanamount")) {
      p <- ggplot(bivar_data(), aes(x = .data[[input$Bi_Variable_X]],
                                    y = .data[[input$Bi_Variable_Y]])) +
        geom_point()
      p + theme_economist() +
        labs(title = "Bivariate Analysis") +
        theme(axis.title.y = element_text(vjust = 2.5),
              axis.text.x = element_text(angle = 60,
                                         vjust = 0.5))
    } else {
      NULL
    }
    
    
  })
  
}
