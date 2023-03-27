library(shiny)
library(shinyjs)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(performance)
library(patchwork)
library(recipes)
library(graphics)
library(ggthemes)


## prediction packages, starts
library(see)
library(vip)
library(here)
library(nnet)
library(themis)
library(ranger)
library(skimr)
library(palmerpenguins)
library(kernlab)
library(janitor)
library(paletteer)
library(xgboost)
library(ggplot2)
library(yardstick)
library(performance)
library(patchwork)
library(parsnip)
library(graphics)
library(ggthemes)
library(randomForest)
library(randomForest)
library(tidymodels)
library(tidyverse)
## prediction packages, ended


## import data
newloan <- read_csv("data/latest_26Mar23/new_loans_cleaned.csv", show_col_types = FALSE)
repeatloan <- read_csv("data/latest_26Mar23/repeated_loans_cleaned.csv", show_col_types = FALSE)

repeatloan$pct_ontime[repeatloan$pct_ontime == 0] <- 0.001
repeatloan$total_ontime[repeatloan$total_ontime == 0] <- 0.001

newloan_prediction_ds <- newloan
repeatloan_prediction_ds <- repeatloan

## variables
newloan_factors <- c(
  "age_at_loan",
  "age_at_loan_25th_pctile",
  "approval_duration_group",
  "bank_account_type",
  "bank_account_type_recode",
  "bank_name_clients",
  "credit_rating",
  "level_of_education_risk",
  "employment_status_risk",
  "termdays",
  "referral"
)

newloan_factorNames <- c(
  "Age at Loan",
  "Age at Loan 25th Pctile",
  "Approval Duration Category",
  "Bank Account Type",
  "Bank Account Type Recode",
  "Bank Name",
  "Credit Rating",
  "Education Level Risk Category",
  "Employment Status Risk Category",
  "Term Days",
  "Referral"
)

repeatloan_factors <- c(
  "avg_age_at_loan",
  "bank_account_type",
  "bank_name_clients",
  "pct_ontime",
  "employment_status",
  "termdays",
  "total_ontime",
  "max_active_of_loans",
  "max_age_at_loan",
  "total_num_of_loans",
  "max_approval_duration"
)

repeatloan_factorNames <- c(
  "Avg Age at Loan",
  "Bank Account Type",
  "Bank Name",
  "Due Ontime Pctile",
  "Employment Status",
  "Term Days",
  "Total Due Ontime",
  "Max Active Loans",
  "Max Age at Loan",
  "Total no. of Loans",
  "Max approval Duration"
)

newloan_factor_options <- setNames(newloan_factors,
                                   newloan_factorNames)

repeatloan_factor_options <- setNames(repeatloan_factors,
                                      repeatloan_factorNames)

common_variable_name <- function(loanType, variable){
  if(loanType == 'S'){
    var_index <- which(newloan_factors == variable)
    return(newloan_factorNames[var_index])
  }else{
    var_index <- which(repeatloan_factors == variable)
    return(repeatloan_factorNames[var_index])
  }
}
