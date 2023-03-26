
use_step_upsample <- function(mtd){
  if(mtd == 'step_upsample'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

use_step_smote <- function(mtd){
  if(mtd == 'step_smote'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

use_step_rose <- function(mtd){
  if(mtd == 'step_rose'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

data_processing_recipe <- function(tgt, mtd, ratio, corr, nrm, rna, rzv){
  
  ds_recipe <- recipe(good_bad_flag ~ ., data = tgt) %>%
    ## log transform data on numeric variables
    step_log(all_numeric()) %>%
    ##  removes rows if they contain NA or NaN values
    step_naomit(everything(), skip = (!rna)) %>%
    ##step_naomit(all_predictors(), skip = (!rna)) %>%
    ## converts all nominal variables to factors related to categorical variables
    step_novel(all_nominal(), -all_outcomes()) %>%
    ##  converts all nominal variables into numeric variables.
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_nzv(all_predictors()) %>%
    ##  removes any numeric variables that have zero variance
    step_zv(all_numeric(), -all_outcomes(), skip = (!rzv)) %>%
    ## normalizes (center and scales) the numeric variables
    step_normalize(all_numeric(), -all_outcomes(), skip = (!nrm)) %>%
    ## remove predictor variables that have large correlations with other predictor variables
    step_corr(all_predictors(), threshold = corr, method = "spearman")
  
  if(use_step_upsample(mtd)){
    ds_recipe <- ds_recipe %>%
      step_upsample(good_bad_flag, over_ratio = ratio)
  }
  if(use_step_smote(mtd)){
    ds_recipe <- ds_recipe %>%
      step_smote(good_bad_flag, over_ratio = ratio)
  }
  if(use_step_rose(mtd)){
    ds_recipe <- ds_recipe %>%
      step_rose(good_bad_flag, over_ratio = ratio)
  }
  ## return recipe
  return (ds_recipe)
}

data_processing_juice <- function(ds_recipe){
  ds_juice <- ds_recipe %>%
    prep() %>%
    juice()
  return (ds_juice)
}

data_attribute_variables <- function(origin, sampled){
  ## create a dataframe that contains data details
  df <- data.frame(
    Name = c("Origin Data", "Sampled Data"),
    Count = c(ncol(origin), ncol(sampled))
  )
  return(df)
}

data_attribute_sizing <- function(origin, sampled){
  ## create a dataframe that contains data details
  df <- data.frame(
    Name = c("Origin Data", "Sampled Data"),
    Size = c(nrow(origin), nrow(sampled))
  )
  return(df)
}
