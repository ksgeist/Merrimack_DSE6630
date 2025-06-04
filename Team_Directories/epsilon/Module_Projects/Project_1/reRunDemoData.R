########################################
## Author: K.S. Geist
## Helper function to completely process
## hospitals datasets from Demo 1 but
## does not run any encoding.
## TO RUN:
## source(file = "reRunDemoData.R")
########################################

## Load the packages needed
pacman::p_load(tidyverse,
               janitor,
               naniar,
               stringr
)

#### CHANGE IF NEEDED #################
filepath <- "C:\\Users\\mike_\\OneDrive\\Documents\\School\\Healthcare Analytics\\hospitals_current_data\\"
########################################


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
  
  ## Note that the vroom warning in FY2025 is due to the footnote column; 
  ## this is not really an issue unless we are using that column.
}

## Make sure to tidy the MeasureName column:
FY_2025_Hospital_Readmissions_Reduction_Program <- FY_2025_Hospital_Readmissions_Reduction_Program %>%
  mutate(MeasureName = gsub("READM-30-", "", MeasureName)) %>% 
  mutate(MeasureName = gsub("-HRRP", "", MeasureName)) 

########################################
######### My comments and thus answer to Questions 3-10:
## Let's make a function to clean up the readmissions:
cleanUpReadmissions <- function(df) {
  ## 1. Replace all the 'Too Few to Report' in NumberofReadmissions with a 
  ## randomly sampled integer from 1 to 10
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
readmissionsClean <- cleanUpReadmissions(FY_2025_Hospital_Readmissions_Reduction_Program)
## Save file for students:
save(readmissionsClean, file = "FY2025_data_files/readmissionsClean2025.Rdata")
## Student load file:
load(file = "FY2025_data_files/readmissionsClean2025.Rdata")



#### CHANGE IF NEEDED #################
## Set a list of conditions:
conditionList <- c("HF", "CABG", "AMI")


########################################
## Separate the Payment and Values tables, giving each facility IDs:
paymentOnly <- Payment_and_Value_of_Care %>% 
  select(FacilityId, PaymentMeasureName, PaymentCategory, Payment) %>% 
  mutate(Payment = gsub("\\$", "", Payment)) %>% # Remove the dollar sign
  mutate(Payment = gsub("\\,", "", Payment)) %>% # Remove the comma 
  rename(MeasureName = PaymentMeasureName)  # Make consistent with other tables

valueOnly <- Payment_and_Value_of_Care %>% 
  select(FacilityId, ValueOfCareDisplayName, ValueOfCareCategory) %>% 
  rename(MeasureName = ValueOfCareDisplayName)  # Make consistent with other tables

## HCAHPS is also a mess. Remove the columns that we will never use in any 
## analysis, make a MeasureName column, and also drop the " - linear mean score"
## from some rows in the newly minted MeasureName
HCAHPS <- HCAHPS %>% 
  select(-HcahpsMeasureId, -PatientSurveyStarRatingFootnote, 
         -HcahpsAnswerPercentFootnote, -NumberOfCompletedSurveysFootnote,
         -SurveyResponseRatePercentFootnote, -HcahpsAnswerDescription,
         -PatientSurveyStarRating) %>% 
  rename(MeasureName = HcahpsQuestion) %>% 
  mutate(MeasureName = gsub(" - linear mean score", "", MeasureName))

## Choosing to pull the hospital information off of the Payment and Value table 
## because it is complete there:
hospitalInfo <- Payment_and_Value_of_Care %>% 
  select(FacilityId, FacilityName, Address, CityTown, 
         State, ZipCode, CountyParish, TelephoneNumber) %>% 
  distinct()   ## Get rid of duplicates if it's length(conditionList) > 1

########################################
## Answer to Question 12 here: #########
########################################

## I chose 8 tables to join:
datList <- list(Healthcare_Associated_Infections,
                paymentOnly,
                Outpatient_Imaging_Efficiency,
                Complications_and_Deaths,
                Medicare_Hospital_Spending_Per_Patient, 
                Timely_and_Effective_Care, 
                Unplanned_Hospital_Visits,
                HCAHPS)


## The criteria I will use to filter each table, if applicable
filterList <- list(
                  "MRSA Bacteremia", 
                   c(
                     "Payment for heart failure patients",
                     "Payment for heart attack patients"
                   ),
                   "Abdomen CT Use of Contrast Material", 
                   c(
                     "Death rate for heart attack patients", 
                     "Death rate for Coronary Artery Bypass Graft (CABG) surgery patients",
                     "Death rate for heart failure patients", 
                     "Death rate for stroke patients"
                   ),
                   c(
                    "Medicare spending per patient",
                    "Spending per Hospital Patient with Medicare (Medicare Spending per Beneficiary)"
                   ),
                   c(
                     "Healthcare workers given influenza vaccination", 
                     "Percentage of healthcare personnel who are up to date with COVID-19 vaccinations", 
                     "Average (median) time patients spent in the emergency department before leaving from the visit A lower number of minutes is better", 
                     "Left before being seen",
                     "Percentage of ED patients with a diagnosis of STEMI who received timely delivery of guideline-based reperfusion therapies appropriate for the care setting and delivered in the absence of contraindications",
                     "Percentage of ischemic stroke patients prescribed or continuing to take antithrombotic therapy at hospital discharge",
                     "Percentage of ischemic stroke patients with atrial fibrillation/flutter who are prescribed or continuing to take anticoagulation therapy at hospital discharge",
                     "Percentage of ischemic stroke patients who are prescribed or continuing to take statin medication at hospital discharge",
                     "Percentage of patients that received VTE prophylaxis after hospital admission or surgery", 
                     "Percentage of patients that received VTE prophylaxis after being admitted to the intensive care unit (ICU)", 
                     "Emergency department volume"
                   ),
                   
                   c(
                    "Hospital return days for heart attack patients",
                    "Hospital return days for heart failure patients",
                    "Rate of readmission for heart attack patients",
                    "Rate of readmission for CABG",
                    "Rate of readmission for heart failure patients"
                   ),
                   
                   c(
                     "Nurse communication",
                     "Doctor communication",
                     "Staff responsiveness",
                     "Communication about medicines",
                     "Discharge information",
                     "Care transition",
                     "Cleanliness",
                     "Quietness",
                     "Overall hospital rating",
                     "Recommend hospital"
                   )
)

########################################
## MY FUNCTION #########################
########################################

tidyNjoin <- function(datList = datList, 
                      filterList = filterList, 
                      hospitalInfo = hospitalInfo,
                      quiet = F,
                      condition = condition) {
  ## ARGUMENTS:
  ## datList = a list of data tables to tidy and join
  ## filterList = a list of MEASURE NAMES to filter from rows before pivoting
  ## hospitalInfo = a new df that contains the basic hospital information that
  ##                serves as my starting df
  ## condition is the condition being processed
  
  ## Initialize the new df with the hospitalInfo
  df <- hospitalInfo
  ## For each of the data frames in the datList:
  for(i in 1:length(datList)) {
    
    ## Only if quiet is set to FALSE, print update messages
    # if(quiet == FALSE) {
    #   print(paste0(datList[i], " processing..."))
    # }
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
  
  ## Prep for joining
  temp <- readmissionsClean %>%   ## Can pull from global environment
    ## Drop columns we are don't want in the way of the join
    select(-Footnote, -FacilityName, -State, -StartDate, -EndDate) %>% 
    ## Filter by the condition
    filter(MeasureName == condition)
  
  ### Join with the clean readmissions data
  df <- full_join(df, temp, by = "FacilityId") 
  
  ## Final touches:
  df <- df %>% 
    ### Drop any columns that start with MeasureName
    select(-contains("MeasureName")) %>% 
    ### Replace all the N/A or "Not Available" with NA
    replace_with_na_all(condition = ~.x %in% c("Not Available", 
                                               "N/A",
                                               "Not Applicable",
                                               "Number of cases too small",
                                               "Number of Cases Too Small")) %>% 
    ### Drop any duplicated rows
    distinct() 
  
  ## Return the finished product
  return(df)
}

## Initialize an empty dataframe for each condition
dataFull <- data.frame()
## To run the function FOR EACH CONDITION
for(l in 1:length(conditionList)) {
  if(l == 1) {
    dfFull <- tidyNjoin(datList, filterList, hospitalInfo, quiet = F, 
                      condition = conditionList[l]) %>%
    mutate(Condition = conditionList[l])
    print(conditionList[l])  
    dataFull <- dfFull
  } else {
    dfFull <- tidyNjoin(datList, filterList, hospitalInfo, quiet = F, 
                        condition = conditionList[l]) %>%
      mutate(Condition = conditionList[l])
    print(conditionList[l]) 
    ## Store these in dataFull
    dataFull <- bind_rows(dataFull, dfFull)
  }
}

dataFull <- dataFull %>% 
  arrange(FacilityId)

## Save file for students:
save(dataFull, file = "FY2025_data_files/dataFull2025.Rdata")
## Student load file:
load("FY2025_data_files/dataFull2025.Rdata")

####################################################
## NOTE THIS IS WHERE THE SHIFT HAPPENS FROM DEMO 1!
####################################################
## Now, let's make dataAnalyze that drops any identifying information, 
## except for state and all of the ordinal columns that contain 
## "ComparedToNational_",  "PaymentCategory", "ValueOfCareCategory", "Score_Emergency department volume"
dataAnalyze <- dataFull %>% 
  select(-contains(c("LowerEstimate", 
                     "HigherEstimate", 
                     "Denominator", 
                     "HcahpsAnswerPercent")),
         -TelephoneNumber, -Address, -FacilityName, 
         -CityTown, -ZipCode, -CountyParish)

## Move our columns to encode to their own dataframe:
columns2encode <- dataAnalyze %>% 
  select(State,            
        contains(c("ComparedToNational_", 
                    "PaymentCategory", 
                    "ValueOfCareCategory")),
        `Score_Emergency department volume`)

## Move FacilityId to its own spot:
facilityId <- dataAnalyze$FacilityId

# Now drop them from the main dataframe too:
dataAnalyzeNoEncoding <- dataAnalyze %>% 
  ## Exclude the columns we will eventually want to encode
  select(-all_of(colnames(columns2encode)), -FacilityId, -Condition) %>% 
  ## Turn everything that is currently a character into numeric
  mutate(across(where(is.character), as.numeric)) %>% 
  ## arbitrarily chose as the representative as they are identical
  mutate(NumberSurveysCompleted = NumberOfCompletedSurveys_Cleanliness, 
         ## Turned into a rate out of 100
         SurveyResponseRate = SurveyResponseRatePercent_Cleanliness/100) %>%   
  ## Dropped the variables used to make those variables
  select(-contains(c("NumberOfCompletedSurveys_", "SurveyResponseRatePercent_"))) %>% 
#### Create a brand new target! ##################
  mutate(observed_readmission_rate = (NumberOfReadmissions / NumberOfDischarges)*100) %>% 
## Dropped the variables used to make the new target 
  select(-NumberOfDischarges, -NumberOfReadmissions)

## Now put the columns2encode back onto the dataframe:
dataAnalyzeNoEncoding <- cbind(dataAnalyzeNoEncoding, columns2encode, facilityId) 

###############################################################
######## I didn't ask you to do this in the Demo. #############
###############################################################
## Also pre-drop `Score_Hospital return days for pneumonia patients` for collinearity purposes
## Have to use an if/else switch because of variable differences between 2025 and 2025
if ("Score_Percentage of healthcare personnel who completed COVID-19 primary vaccination series" %in% names(dataAnalyzeNoEncoding)) {
  dataAnalyzeNoEncoding <- dataAnalyzeNoEncoding %>% 
    select(-`Score_Hospital return days for heart attack patients`, 
           -`Score_Hospital return days for heart failure patients`, 
           -`Score_Percentage of healthcare personnel who completed COVID-19 primary vaccination series`)
} else {
  dataAnalyzeNoEncoding <- dataAnalyzeNoEncoding %>% 
    select(-`Score_Hospital return days for heart attack patients`, 
           -`Score_Hospital return days for heart failure patients`
    )
}



###############################################################
## Now, if planning to assess multiple conditions simultaneously,
## we need to aggregate across all fields:
dataAnalyzeNoEncoding <- dataAnalyzeNoEncoding %>%
    group_by(facilityId) %>%
    summarize(across(
        ## Summarize across all the columns
        .cols = everything(),
        ## If it's a numeric column, take the mean; otherwise, take the first()
        ## on the categorical columns; but retains the REAL NAs (vs conversion)
        ## to NaN
        .fns = ~ { if (is.numeric(.)) {
          if (all(is.na(.))) NA_real_ else mean(., na.rm = TRUE)
          } else {
            first(.)
          }},
        ## Retain column names
        .names = "{.col}"), 
        ## Ungroup
        .groups = "drop") %>% 
  ## Move our Facility IDs to rowname
  column_to_rownames("facilityId") 
    
## Save file for students:
save(dataAnalyzeNoEncoding, file = "FY2025_data_files/dataAnalyzeNoEncoding2025.Rdata")
## Student load file:
load(file = "FY2025_data_files/dataAnalyzeNoEncoding2025.Rdata")


## CLEAN UP OUR MESS! We can remove the datasets we are no longer using
rm(dataFull,
   Timely_and_Effective_Care,
   Unplanned_Hospital_Visits,
   Outpatient_Imaging_Efficiency,
   Payment_and_Value_of_Care,
   paymentOnly,
   readmissionsClean,
   valueOnly,
   hospitalInfo,
   Maternal_Health,
   Medicare_Hospital_Spending_Per_Patient,
   FY_2025_HAC_Reduction_Program,
   FY_2025_Hospital_Readmissions_Reduction_Program,
   files,
   HCAHPS,
   Healthcare_Associated_Infections,
   Complications_and_Deaths,
   dat,
   columns2encode,
   datList,
   filterList,
   dataAnalyze,
   dfFull,
   facilityId)
