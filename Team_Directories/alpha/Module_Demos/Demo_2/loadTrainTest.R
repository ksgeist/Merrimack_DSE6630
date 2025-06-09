########################################
## Author: K.S. Geist
## Helper code to load, aggregate, and
## join with spatial information the 
## training and testing sets from Proj 1 
## TO RUN:
## source(file = "loadTrainTest.R")
########################################

## Load the packages needed
pacman::p_load(tidyverse)

## Load the training and testing sets
if(exists("readyTrain")) {
  rm("readyTrain") } else { 
    load("../../../../I. Biomedical & Clinical Informatics/Project_1/FY2024_data_files/readyTrain.Rdata") 
}

if(exists("readyTest")) {
  rm("readyTest") } else { 
    load("../../../../I. Biomedical & Clinical Informatics/Project_1/FY2024_data_files/readyTest.Rdata") 
}

#### TRAIN
readyTrain <- readyTrain %>% 
## We have to move the rownames back to FacilityId temporarily
  mutate(FacilityId = rownames(readyTrain)) %>%
## Rename the column we want to keep
  rename(Hospital.return.days.for.pneumonia.patients = ComparedToNational_Hospital.return.days.for.pneumonia.patients) %>% 
## Also drop the columns from the VIF and other redundant one from Project 1
  select(-Nurse.communication, -Overall.hospital.rating, -Staff.responsiveness, -Care.transition, 
         -Recommend.hospital, -contains("Compared"), -contains("PaymentCategory"), -SurveyResponseRate,
## But also remove encoded state now
         -State)

#### TEST
## We first have to move the rownames back to FacilityId temporarily
readyTest <- readyTest %>% 
  ## We have to move the rownames back to FacilityId temporarily
  mutate(FacilityId = rownames(readyTest)) %>%
  ## Rename the column we want to keep
  rename(Hospital.return.days.for.pneumonia.patients = ComparedToNational_Hospital.return.days.for.pneumonia.patients) %>% 
  ## Also drop the columns from the VIF and other redundant one from Project 1
  select(-Nurse.communication, -Overall.hospital.rating, -Staff.responsiveness, -Care.transition, 
         -Recommend.hospital, -contains("Compared"), -contains("PaymentCategory"), -SurveyResponseRate,
         ## But also remove encoded state now
         -State)

#############
## Merge with pneumoniaFull just to get the state names again...
############
## Assumes that pneumoniaFull is already in your environment
if(!exists("pneumoniaFull")) {
  message("Do you need to run all the previous code again? pneumoniaFull is MISSING!")
} else {
  pneumoniaFull <- pneumoniaFull %>% 
    mutate(region = getStateName(State))      ## Naming it 'region' to make the name in the states dataframe
  
  #### TRAIN
  stateTrain <- readyTrain %>% 
    ## Merge with pneumoniaFull to get the state names
    left_join((pneumoniaFull %>%
              ## Don't want to merge with all features from pneumoniaFull
               select(FacilityId, 
                      region, 
                      PredictedReadmissionRate, 
                      `Score_Hospital return days for pneumonia patients`, 
                      `Score_Medicare spending per patient`,
                      `Score_Death rate for pneumonia patients`)), by = "FacilityId") %>% 
      ## Now move FacilityId back to rownames
      column_to_rownames("FacilityId") %>% 
      ## Rename the median raw values so we can distinguish from the transformed and scaled
      rename(Median_RawPredictedReadmissionRate = PredictedReadmissionRate, 
             Median_RawMedicareSpending = `Score_Medicare spending per patient`,
             Median_RawDeathRate = `Score_Death rate for pneumonia patients`,
             Median_RawHospitalReturnDays = `Score_Hospital return days for pneumonia patients`)
  
  #### TEST
  
## now merge with pneumoniaFull to get the state names
  stateTest <- readyTest %>% 
    ## Merge with pneumoniaFull to get the state names
    left_join((pneumoniaFull %>%
               ## Don't want to merge with all features from pneumoniaFull
               select(FacilityId, 
                      region, 
                      PredictedReadmissionRate, 
                      `Score_Hospital return days for pneumonia patients`,
                      `Score_Medicare spending per patient`,
                      `Score_Death rate for pneumonia patients`)), by = "FacilityId") %>% 
    ## Now move FacilityId back to rownames
    column_to_rownames("FacilityId") %>% 
    ## Rename the median raw values so we can distinguish from the transformed and scaled
    rename(Median_RawPredictedReadmissionRate = PredictedReadmissionRate, 
           Median_RawMedicareSpending = `Score_Medicare spending per patient`,
           Median_RawDeathRate = `Score_Death rate for pneumonia patients`,
           Median_RawHospitalReturnDays = `Score_Hospital return days for pneumonia patients`)

#############
## Merge with pneumoniaFull just to get the state names again...
############
## Now aggregate again - medians all around!
stateAggTrain <- stateTrain %>%
  group_by(region) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))

## Now aggregate again - medians all around!
stateAggTest <- stateTest %>%
  group_by(region) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
}

## Remove for cleanup and to save confusion
rm(readyTest, readyTrain)

