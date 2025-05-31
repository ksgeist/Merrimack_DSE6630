#######################################
## Author: K.S. Geist
## Helper function to perform ordinal
## encoding on datasets from Demo 1
## My answer to QUESTION 15
## TO RUN:
## source(file = "doOrdinalEncoding.R")
#######################################

## ORDINAL ENCODING FUNCTION STARTS

doOrdinalEncoding <- function(df = df, 
                              encodeList = encodeList, 
                              quiet = TRUE) {
  ## ARGUMENTS:
  ## df = the name of whatever hospitals dataset you want to ordinal encode
  ## encodeList = list of PATTERNS to match in the dataset for ordinal encoding
  ## quiet = logical that will suppress output when set to TRUE   
  
  ## Move the categorical features to ordinal encode to a temporary dataframe
  temp <- df %>% 
    select(contains(encodeList))
  
  ## Also, store the column names to allow a swap-a-roo later:
  cols2encode <- colnames(temp)
  
  ## Loops through each of the columns stored in temp
  for (i in names(temp)) {
    ## Only print outcomes if quiet is set to FALSE
    if(quiet == FALSE) {
      print(paste("Column:", i, "before encoding:"))
      print(table(temp[[i]], useNA = "always"))
    }
    
    ## Make the column name lowercase for safety
    col <- tolower(temp[[i]])
    
    ## We need a check for whether the column has 3 levels or 5:
    if (any(grepl("very high|very low|medium", col), na.rm = TRUE)) {
      
      ## 5-level encoding; not part of Question 17, but we need to also need  
      ## 5 = "very high", 4 = "high", 3 = "medium", 2 = "low", 1 = "very low"
      temp[[i]] <- case_when(
        grepl("^very high", col) ~ 5,
        grepl("^high", col) ~ 4,
        grepl("^medium", col) ~ 3,
        grepl("^low", col) ~ 2,
        grepl("^very low", col) ~ 1,
        TRUE ~ NA_real_
      )
    } else {

      ## 3-level encoding
      ## 0 = "not different", 1 = "better", -1 = "worse"
      ## 0 = "no different than", 1 = "greater than", -1 = "less than" 
      ## "Fewer Days Than Average per 100 Discharges" == "better" = 1
      ## "More Days Than Average per 100 Discharges" == "worse" = -1
      ## "Average" = 0
      temp[[i]] <- case_when(
        grepl("^no|^average|^same", col) ~ 0,
        grepl("^better|^fewer days|^greater", col) ~ 1,
        grepl("^worse|^more days|^less", col) ~ -1,
        TRUE ~ NA_real_
      )
    }
    if(quiet == FALSE) {
      print("After encoding:")
      print(table(temp[[i]], useNA = "always"))
    }
  }
  
  ## Now swap out the original columns with the temporary columns:
  for (col in cols2encode) {
    df[[col]] <- temp[[col]]
  }
  return(df)
}
