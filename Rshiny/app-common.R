library(shiny)
library(corrplot)
library(ggplot2)
library(patchwork)
library(graphics)
library(ggthemes)
library(tidyverse)

## import data
newloan <- read_csv("data/loan_data_v2/new_loans_cleaned.csv", 
                    show_col_types = FALSE)

repeatloan <- read_csv("data/loan_data_v2/repeated_loans_cleaned.csv", 
                       show_col_types = FALSE)

## variables

newloan_factors <- c("bank_name_clients",
                        "approval_duration_group",
                        "age_at_loan_25th_pctile",
                        "credit_rating",
                        "termdays",
                        "employment_status_risk",
                        "level_of_education_risk",
                        "referral",
                        "bank_account_type_recode",
                        "loanamount_group",
                        "approval_duration",
                        "loanamount",
                        "totaldue",
                        "Interestrate",
                        "age_at_loan",
                        "bank_account_type")


repeatloan_factors <- c( "pct_ontime",
                            "total_ontime",
                            "max_active_of_loans",
                            "max_approval_duration",
                            "bank_name_clients",
                            "max_age_at_loan",
                            "avg_age_at_loan",
                            "employment_status",
                            "bank_account_type",
                            "total_referrals",
                            "avg_num_of_loans",
                            "total_num_of_loans",
                            "total_approval_duration",
                            "mean_approval_duration",
                            "max_interest_rate",
                            "mean_interest_rate",
                            "mean_referrals",
                            "max_churn_flag",
                            "loanamount",
                            "totaldue",
                            "termdays")