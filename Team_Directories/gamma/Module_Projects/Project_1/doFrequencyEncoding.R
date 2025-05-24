#######################################
## Author: K.S. Geist
## Helper function to perform frequency
## encoding on datasets from Demo 1
## My answer to QUESTION 18-19
## TO RUN:
## source(file = "doFrequencyEncoding.R")
#######################################

## FREQUENCY ENCODING FUNCTION STARTS
## This works a bit differently from how we did it in the Demo because we 
## need to pass it both the training set and the testing set so that we can 
## apply the same frequency mapping (encoder), per column, to BOTH datasets.

doFrequencyEncoding <- function(train = train,
                                test = NA,
                                cols2encode = cols2encode,
                                quiet = F) {
    ## ARGUMENTS:
    ## train = the name of the training hospitals dataset you want to frequency encode
    ## test = the test set OR do not include to only encode a single dataset
    ## cols2encode = list of COLUMN NAMES to match for frequency encoding
    ## quiet = logical that will suppress output when set to TRUE   
  
    ## Move them to temporary data frames
    tempTrain <- train[, names(train) %in% cols2encode, drop = FALSE]
    tempTest  <- test[, names(test) %in% cols2encode, drop = FALSE]
    
    ## For each of the columns, generate a frequency map (encoder)
    for (col in cols2encode) {
      ## Create frequency map from the TRAINING data
      frequency_map <- table(tempTrain[[col]], useNA = "always")
      ## Show if quiet is FALSE
      if(quiet == FALSE) {
        print(paste0("Processing column ", col))
        print("Here's the encoder (store for your records):")
        print(frequency_map)
      }
      
      ## Apply frequency encoding to both train and test using the same map
      tempTrain[[col]] <- frequency_map[match(tempTrain[[col]], 
                                              names(frequency_map))]
      tempTest[[col]] <- frequency_map[match(tempTest[[col]], 
                                              names(frequency_map))]
      
      ## Replace NAs in training or test with 0 (especially important if 
      ## some classes were in train but not test and vice versa) 
      tempTrain[[col]][is.na(tempTrain[[col]])] <- 0
      tempTest[[col]][is.na(tempTest[[col]])] <- 0
      
      ## For each of the columns in each dataset, replaces in the original dataset
      train[[col]] <- tempTrain[[col]]
      test[[col]] <- tempTest[[col]]
    }

    ## Return both encoded data frames
    return(list(train = train, test = test))
  }
