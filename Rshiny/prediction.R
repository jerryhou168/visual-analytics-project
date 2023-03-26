source("prediction/r_sampling.R", local = TRUE)
source("prediction/r_prediction.R", local = TRUE)

source("prediction/prediction-ui-parameters.R", local = TRUE)

source("prediction/prediction-ui-sampling.R", local = TRUE)

source("prediction/prediction-ui-onealg.R", local = TRUE)

source("prediction/prediction-ui-twoalg.R", local = TRUE)

source("prediction/prediction-ui-threealg.R", local = TRUE)

source("prediction/prediction-ui.R", local = TRUE)

source("prediction/sampling-server.R", local = TRUE)

source("prediction/prediction-server.R", local = TRUE)


## Define a Global Prediction Working in Progress Variable
Global_Prediction_WIP_Flag <<- FALSE

Global_Prediction_TGT_Flag <<- c("good_bad_flag")

prediction <- function(input, output, session){
  
  prediction_parameter_loading(input, output, session)
  
}

