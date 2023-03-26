library(shiny)
library(shinyjs)
library(corrplot)
library(ggplot2)
library(performance)
library(patchwork)
library(recipes)
library(graphics)
library(ggthemes)

## prediction packages, starts
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
library(tidymodels)
library(tidyverse)
## prediction packages, ended


## import data
newloan <- read_csv("data/latest_26Mar23/new_loans_cleaned.csv", 
                    show_col_types = FALSE)

repeatloan <- read_csv("data/latest_26Mar23/repeated_loans_cleaned.csv", 
                       show_col_types = FALSE)

repeatloan$pct_ontime[repeatloan$pct_ontime == 0] <- 0.01
repeatloan$total_ontime[repeatloan$total_ontime == 0] <- 0.01

## variables
newloan_factors <- c(
  "bank_name_clients",
  "approval_duration_group",
  "age_at_loan_25th_pctile",
  "credit_rating",
  "employment_status_risk",
  "level_of_education_risk",
  "referral",
  "bank_account_type_recode",
  "termdays",
  "age_at_loan",
  "bank_account_type"
)

repeatloan_factors <- c(
  "pct_ontime",
  "total_ontime",
  "max_active_of_loans",
  "bank_name_clients",
  "max_age_at_loan",
  "avg_age_at_loan",
  "employment_status",
  "bank_account_type",
  "total_num_of_loans",
  "max_approval_duration",
  "termdays"
)
