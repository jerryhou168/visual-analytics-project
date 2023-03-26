######### sampling parameters

t0_slt_loanType <- radioButtons(inputId = "t0_loanType", 
                               label = "Type of Loans",
                               choices = c("New Loan" = "S",
                                           "Repeat Loan" = "R"),
                               selected = "S", inline = TRUE)

t0_slt_method <- selectInput(inputId = "t0_method", 
                             label = "Sampling Method",
                             choices = c("No sampling" = "no_sample",
                                         "Up sampling" = "step_upsample",
                                         "SMOTE - Synthetic Minority Over-sampling" = "step_smote",
                                         "ROSE - Random Over-Sampling" = "step_rose"
                             ),
                             selected = "step_smote")


t0_slt_ratio <- sliderInput("t0_ratio", "Over Ratio", min = 0.1, max = 20, value = 2, step = 0.01)

t0_slt_normalize <- checkboxInput("t0_normalize", "Center and scale numeric data", value = TRUE)

t0_slt_clean_na <- checkboxInput("t0_remove_nan", "Remove NaN values", value = TRUE)

t0_slt_clean_zv <- checkboxInput("t0_remove_zv", "Remove zero variance Variable", value = TRUE)

t0_btn_sampling <- actionButton("sample", "Start Sampling")
t0_btn_resetSap <- actionButton("reset1", "Reset")

######### prediction parameters

t0_slt_algorithm <- checkboxGroupInput(inputId = "t0_algorithm", 
                                label = "Predicting Algorithm",
                                choices = c("Boosted Tree" = "BT",
                                            "Random Forest" = "RF",
                                            "Logistic Regression" = "LR"),
                                selected = "BT", inline = TRUE)


t0_slt_predictors <- uiOutput(outputId = "o0_predictors")

t0_slt_vfolds <- sliderInput("t0_vfolds", "V-fold cross-validation", min = 2, max = 10, value = 5, step = 1)

t0_btn_predict <- actionButton("predict", "Start Prediction")
t0_btn_resetPt <- actionButton("reset2", "Reset")

######### prediction extra setting parameters

t0_slt_corr <- sliderInput("t0_correlation", "Correlations", min = 0.1, max = 1, value = 0.85, step = 0.01)

t0_slt_split <- sliderInput("t0_split", "Training/Test Set Splitting", min = 0.5, max = 1, value = 0.75, step = 0.05)

t0_slt_treedepth <- sliderInput("t0_treedepth", "Tree Depth", min = 1, max = 50, value = 7, step = 1)

t0_slt_trees <- sliderInput("t0_trees", "Trees", min = 10, max = 500, value = 50, step = 5)

prediction_nav <- fluidRow(
  fluidRow(
    column(4, t0_slt_loanType),
    column(4, t0_slt_algorithm),
    column(4, t0_slt_split)
  ),
  fluidRow(
    column(4, t0_slt_method),
    column(4, t0_slt_predictors),
    column(4, t0_slt_corr)
    
  ),
  fluidRow(
    column(4, t0_slt_ratio),
    column(4, t0_slt_vfolds),
    column(4, t0_slt_treedepth)
    
  ),
  fluidRow(
    column(4, t0_slt_clean_na, t0_slt_clean_zv),
    column(4, t0_slt_normalize),
    column(4, t0_slt_trees)
  ),
  fluidRow(
    column(4, t0_btn_sampling, t0_btn_resetSap),
    column(4, t0_btn_predict, t0_btn_resetPt),
    column(4, )
  )
)

