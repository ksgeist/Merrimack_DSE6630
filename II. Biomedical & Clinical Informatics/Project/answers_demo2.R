## Load the packages needed
pacman::p_load(tidyverse,
               janitor,
               naniar,
               stringr
)

#########################
## Set the file path; you may need to update this on your computer.
filepath <- "~/Desktop/hospitals_current_data/"
## Grab just the hospital-level files:
files <- list.files(path = filepath, pattern = "Hospital.csv")

## Iterate over the hospital-level files and read each one, 
## naming dynamically & storing the object
for(f in 1:length(files)) {
  ## Clean the names and make into "Camel Case"
  dat <- clean_names(read_csv(paste0(filepath, files[f]), show_col_types = FALSE), 
                     case = "upper_camel")
  ## Strip off the -Hospital.csv from the end of each file name
  filename <- gsub(".Hospital\\.csv", "", files[f])
  ## Assign the filename to dat 
  assign(filename, dat)
}

## Make sure to tidy the MeasureName column:
FY_2024_Hospital_Readmissions_Reduction_Program <-  FY_2024_Hospital_Readmissions_Reduction_Program %>%
  mutate(MeasureName = gsub("READM-30-", "", MeasureName)) %>% 
  mutate(MeasureName = gsub("-HRRP", "", MeasureName)) 

#########################
######### My comments and thus answer to Questions 3-10:
## Let's make a function to clean up the readmissions:
cleanUpReadmissions <- function(df) {
  ## 1. Replace all the 'Too Few to Report' in NumberofReadmissions with a randomly sampled integer from 1 to 10
  y <- gregexpr("Too Few to Report", df$NumberOfReadmissions)
  ## Do this in an interative way using lapply()
  regmatches(df$NumberOfReadmissions, y) <- lapply(lengths(y), 
                                                   sample, 
                                                   x = seq(1, 10, by = 1))
  ## 2. replace all the N/A with NA
  df <-  df %>%
    replace_with_na_all(condition = ~.x %in% "N/A")
  
  ## 3. Convert to numeric with mutate_at()
  df <-  df %>% 
    mutate_at(c("PredictedReadmissionRate", 
                "ExcessReadmissionRatio", 
                "ExpectedReadmissionRate", 
                "NumberOfReadmissions"), as.numeric)

  return(df)
}

## Run the function
readmissionsClean <- cleanUpReadmissions(FY_2024_Hospital_Readmissions_Reduction_Program)
## Save file for students:
save(readmissionsClean, file = "readmissionsClean.Rdata")



############## This is where you will filter for whatever condition(s) you are choosing to focus on!
## Filter for just pneumonia:
readmissionsClean <- readmissionsClean %>% 
  filter(MeasureName == "PN")

#########################
## First, make sure to separate the Payment and Values tables, giving each facility IDs:
paymentOnly <- Payment_and_Value_of_Care %>% 
  select(FacilityId, PaymentMeasureName, PaymentCategory, Payment) %>% 
  mutate(Payment = gsub("\\$", "", Payment)) %>% # Remove the dollar sign
  mutate(Payment = gsub("\\,", "", Payment)) %>% # Remove the comma 
  rename(MeasureName = PaymentMeasureName)  # Make consistent with other tables

valueOnly <- Payment_and_Value_of_Care %>% 
  select(FacilityId, ValueOfCareDisplayName, ValueOfCareCategory) %>% 
  rename(MeasureName = ValueOfCareDisplayName)  # Make consistent with other tables

# HCAHPS is also a mess. Remove the columns that we will never use in any analysis, make a MeasureName column, and also drop the " - linear mean score" from some rows in the newly minted MeasureName
HCAHPS <- HCAHPS %>% 
  select(-HcahpsMeasureId, -PatientSurveyStarRatingFootnote, 
         -HcahpsAnswerPercentFootnote, -NumberOfCompletedSurveysFootnote,
         -SurveyResponseRatePercentFootnote, -HcahpsAnswerDescription,
         -PatientSurveyStarRating) %>% 
  rename(MeasureName = HcahpsQuestion) %>% 
  mutate(MeasureName = gsub(" - linear mean score", "", MeasureName))

## Choosing to pull the hospital information off of the Payment and Value table because it is complete there:
hospitalInfo <- Payment_and_Value_of_Care %>% 
  select(FacilityId, FacilityName, Address, CityTown, 
         State, ZipCode, CountyParish, TelephoneNumber)
## Put all the data together! Making pneumoniaFull:

## I chose 8 tables to join:
datList <- list(Healthcare_Associated_Infections,
                paymentOnly,
                Outpatient_Imaging_Efficiency,
                Complications_and_Deaths,
                Medicare_Hospital_Spending_Per_Patient, 
                Timely_and_Effective_Care, 
                Unplanned_Hospital_Visits,
                HCAHPS)

######### Answer to Question 12 begins here: ##########

## The criteria I will use to filter each table, if applicable
filterList <- list("MRSA Bacteremia", 
                   "Payment for pneumonia patients", 
                   "Abdomen CT Use of Contrast Material", 
                   
                   c("Death rate for pneumonia patients", 
                     "Perioperative pulmonary embolism or deep vein thrombosis rate",
                     "CMS Medicare PSI 90: Patient safety and adverse events composite", 
                     "Postoperative respiratory failure rate"),
                   
                   "Medicare spending per patient",
                   
                   c("Healthcare workers given influenza vaccination", 
                     "Percentage of healthcare personnel who completed COVID-19 primary vaccination series", 
                     "Average (median) time patients spent in the emergency department before leaving from the visit A lower number of minutes is better", 
                     "Left before being seen",
                     "Venous Thromboembolism Prophylaxis", 
                     "Intensive Care Unit Venous Thromboembolism Prophylaxis", 
                     "Emergency department volume"),
                   
                   "Hospital return days for pneumonia patients",
                   
                   c("Nurse communication",
                     "Doctor communication",
                     "Staff responsiveness",
                     "Communication about medicines",
                     "Discharge information",
                     "Care transition",
                     "Cleanliness",
                     "Quietness",
                     "Overall hospital rating",
                     "Recommend hospital")
)

## Set the function tidyNJoin:
tidyNjoin <- function(datList, filterList, hospitalInfo) {
  
  ## Initialize the new df with the hospitalInfo
  df <- hospitalInfo
  ## For each of the data frames in the datList:
  for(i in 1:length(datList)) {
    
    ## print(paste0("Dataset:  ", i))
    ## Coerce back to data frame
    dat <- as.data.frame(datList[i])   
    ## Drop start and end dates, plus drop hospital info, if they exist
    dat <- dat %>% 
      ## Remove any of those columns from hospitalInfo, etc. 
      select(-any_of(c("StartDate", "EndDate", "FacilityName", 
                       "Address", "CityTown", "State", "ZipCode", 
                       "CountyParish", "TelephoneNumber", "Footnote", "MeasureId", 
                       "Condition"))) %>% 
      ## Shorten the column name
      mutate(MeasureName = ifelse(MeasureName == "Medicare hospital spending per patient (Medicare Spending per Beneficiary)", "Medicare spending per patient", MeasureName))
    ### Filter to remove anything not in the filterList
    ### Note that I am resorting to indexing for this to work how I want.
    dat <- dat[which(dat$MeasureName %in% filterList[[i]]), ]
    ### Pivot wider
    wide <- pivot_wider(dat, names_from = "MeasureName", 
                        values_from = 2:ncol(dat))
    
    ## Join with previous (first iteration, it's the hospitalInfo)
    df <- full_join(df, wide, by = c("FacilityId"))
  }
  
  readmissionsClean <- readmissionsClean %>% 
    select(-Footnote, -FacilityName, -State, -StartDate, -EndDate)
  ### Join with the clean readmissions data
  df <- full_join(df, readmissionsClean, by = "FacilityId") 
  
  ## Final touches:
  ### Drop any columns that start with MeasureName
  df <- df %>% 
    select(-contains("MeasureName")) %>% 
    ### Replace all the N/A or "Not Available" with NA
    replace_with_na_all(condition = ~.x %in% c("Not Available", 
                                               "N/A",
                                               "Not Applicable",
                                               "Number of cases too small",
                                               "Number of Cases Too Small")) %>% 
    ### Drop any duplicated rows
    distinct() 
  
  
  return(df)
}

pneumoniaFull <- tidyNjoin(datList, filterList, hospitalInfo)
# table(Unplanned_Hospital_Visits$ComparedToNational)

## Save file for students:
save(pneumoniaFull, file = "pneumoniaFull.Rdata")
# load("pneumoniaFull.Rdata")

######################################
## ENCODING

## First drop a few more columns we don't need:
pneumoniaFull <- pneumoniaFull %>% 
  select(-contains(c("LowerEstimate", 
                     "HigherEstimate", 
                     "Denominator", 
                     "HcahpsAnswerPercent")))

## And these too
pneumoniaFull <- pneumoniaFull %>% 
  select(-TelephoneNumber, -Address, -FacilityName, -CityTown, -ZipCode)

################# MY ANSWER TO QUESTION 15:
## Move to a temporary df
temp <- pneumoniaFull %>% 
  select(contains(c("ComparedToNational_", 
                    "PaymentCategory", 
                    "ValueOfCareCategory")))

## Also, store the column names to allow a swap-a-roo later:
cols2encode <- colnames(temp)

## Loop through and encode: 0 = "not different", 1 = "better", -1 = "worse"
## Make sure that the ones Fewer Days Than Average per 100 Discharges == "better" whereas "More Days Than Average per 100 Discharges" == "worse"
for (i in names(temp)) {
  print("Before encoding:")
  print(table(temp[[i]], useNA = "always"))
  temp[[i]] <- ifelse(grepl("^No|^Average", temp[[i]]), 0, 
                      ifelse(grepl("^Better|^Fewer Days", temp[[i]]), 1,
                             ifelse(grepl("^Worse|^More Days", temp[[i]]), -1,
                                    NA)))
  print("After encoding:")
  print(table(temp[[i]], useNA = "always"))
}

## Now swap out the original columns with the temporary columns:
for (col in 1:length(cols2encode)) {
  pneumoniaFull[, cols2encode[col]] <- temp[, cols2encode[col]]
}

## This was not part of Question 15, but we do also need to encode one more column as 
## an ordinal:
# encode: 5 = "very high", 4 = "high", 3 = "medium", 2 = "low", 1 = "very low"
pneumoniaFullEncoded <- pneumoniaFull %>% 
  mutate(`Score_Emergency department volume` = case_when(`Score_Emergency department volume` == "very high" ~ "5",
                                                         `Score_Emergency department volume` == "high" ~ "4",
                                                         `Score_Emergency department volume` == "medium" ~ "3", 
                                                         `Score_Emergency department volume` == "low"~ "2", 
                                                         `Score_Emergency department volume` == "very low" ~ "1"))

## Save file for students:
save(pneumoniaFullEncoded, file = "pneumoniaFullEncoded.Rdata")
################# End my answer to Question 15

################# MY ANSWER TO QUESTION 16:

## Choose the columns I want to encode
cols2encode <- c("State", 
                 "CountyParish")

## Move them to a temporary data frame
temp <- pneumoniaFullEncoded[, names(pneumoniaFullEncoded) %in% cols2encode]

## Create a function that will do the frequency encoding
add_freq <- function(data, column_name) {
  ## Creates a map using the table() function for each category
  frequency_map <- table(data[[column_name]], useNA = "always")
  ## Then goes to the column and matches the category to its frequency from the map
  data[[column_name]] <- frequency_map[match(data[[column_name]],
                                             names(frequency_map))]
  return(data)
}

## Runs the add_freq() function on each column I have chosen
for (col in names(temp)) {
  temp <- add_freq(temp, col)
}

## For each of the columns in the dataset, replaces it in the original dataset
for (c in 1:length(cols2encode)) {
  pneumoniaFullEncoded[, cols2encode[c]] <- temp[, cols2encode[c]]
}
################# End my answer to Question 15

#########################################
## Now, let's make pneumoniaAnalyze:
pneumoniaAnalyze <- pneumoniaFullEncoded %>% 
  select(-CountyParish)

## Save file for students:
save(pneumoniaAnalyze, file = "pneumoniaAnalyze.Rdata")

pneumoniaAnalyze <- pneumoniaAnalyze %>% 
  column_to_rownames("FacilityId") %>% 
  mutate(across(where(is.character), as.numeric))

pneumoniaAnalyze <- pneumoniaAnalyze %>% 
  ## arbitrarily chose as the rep as they are identical
  mutate(NumberSurveysCompleted = NumberOfCompletedSurveys_Cleanliness,              
         SurveyResponseRate = SurveyResponseRatePercent_Cleanliness/100) %>%       ## Turned into an actual rate 
  select(-contains(c("NumberOfCompletedSurveys_", "SurveyResponseRatePercent_")))   ## drop the others

pneumoniaAnalyzeDemo <- pneumoniaAnalyze %>% 
  mutate(observed_readmission_rate = (NumberOfReadmissions / NumberOfDischarges)*100) %>% 
  select(-NumberOfDischarges, -NumberOfReadmissions)

## Save file for students:
save(pneumoniaAnalyzeDemo, file = "pneumoniaAnalyzefromDemo.Rdata")

