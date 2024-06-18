calcSplitRatio <- function(p = NA, df) {
  ## @p  = the number of parameters. by default, if none are provided, the number of columns (predictors) in the dataset are used
  ## @df = the dataframe that will be used for the analysis
  
  ## If the number of parameters isn't supplied, set it to the number of features minus 1 for the target
  if(is.na(p)) {
    p <- ncol(df) -1   ## COMMENT HERE
  }
  
  ## Calculate the ideal number of testing set
  test_N <- (1/sqrt(p))*nrow(df)
  ## Turn that into a testing proportion
  test_prop <- round((1/sqrt(p))*nrow(df)/nrow(df), 2)
  ## And find the training proportion
  train_prop <- 1-test_prop
  
  ## Tell us the results!
  print(paste0("The ideal split ratio is ", train_prop, ":", test_prop, " training:testing"))
  
  ## Return the size of the training set
  return(train_prop)
}


