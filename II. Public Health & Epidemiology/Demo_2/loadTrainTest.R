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
load("../../I. Biomedical & Clinical Informatics/Project_1/FY2024_data_files/readyTrain.Rdata")
load("../../I. Biomedical & Clinical Informatics/Project_1/FY2024_data_files/readyTest.Rdata")

#### TRAIN
readyTrain <- readyTrain %>% 
  mutate(FacilityId = rownames(readyTrain)) %>%
  rename(
    Hospital.return.days.for.pneumonia.patients =
      ComparedToNational_Hospital.return.days.for.pneumonia.patients
  ) %>% 
  select(
    -Nurse.communication,
    -Overall.hospital.rating,
    -Staff.responsiveness,
    -Care.transition,
    -Recommend.hospital,
    -contains("Compared"),
    -contains("PaymentCategory"),
    -SurveyResponseRate,
    -State
  )

#### TEST
readyTest <- readyTest %>% 
  mutate(FacilityId = rownames(readyTest)) %>%
  rename(
    Hospital.return.days.for.pneumonia.patients =
      ComparedToNational_Hospital.return.days.for.pneumonia.patients
  ) %>% 
  select(
    -Nurse.communication,
    -Overall.hospital.rating,
    -Staff.responsiveness,
    -Care.transition,
    -Recommend.hospital,
    -contains("Compared"),
    -contains("PaymentCategory"),
    -SurveyResponseRate,
    -State
  )

#############
## Merge with pneumoniaFull just to get the state names again...
############

if(!exists("pneumoniaFull")) {
  
  message("Do you need to run all the previous code again? pneumoniaFull is MISSING!")
  
} else {
  
  pneumoniaFull <- pneumoniaFull %>%
    mutate(region = getStateName(State))
  
  #### TRAIN
  
  stateTrain <- readyTrain %>%
    left_join(
      pneumoniaFull %>%
        select(
          FacilityId,
          region,
          PredictedReadmissionRate,
          `Score_Hospital return days for pneumonia patients`,
          `Score_Medicare spending per patient`,
          `Score_Death rate for pneumonia patients`
        ),
      by = "FacilityId"
    ) %>%
    #column_to_rownames("FacilityId") %>%
    rename(
      Median_RawPredictedReadmissionRate = PredictedReadmissionRate,
      Median_RawMedicareSpending =
        `Score_Medicare spending per patient`,
      Median_RawDeathRate =
        `Score_Death rate for pneumonia patients`,
      Median_RawHospitalReturnDays =
        `Score_Hospital return days for pneumonia patients`
    )
  
  #### TEST
  
  stateTest <- readyTest %>%
    left_join(
      pneumoniaFull %>%
        select(
          FacilityId,
          region,
          PredictedReadmissionRate,
          `Score_Hospital return days for pneumonia patients`,
          `Score_Medicare spending per patient`,
          `Score_Death rate for pneumonia patients`
        ),
      by = "FacilityId"
    ) %>%
    #column_to_rownames("FacilityId") %>%
    rename(
      Median_RawPredictedReadmissionRate = PredictedReadmissionRate,
      Median_RawMedicareSpending =
        `Score_Medicare spending per patient`,
      Median_RawDeathRate =
        `Score_Death rate for pneumonia patients`,
      Median_RawHospitalReturnDays =
        `Score_Hospital return days for pneumonia patients`
    )
  
  #### Aggregate to state level
  
  stateAggTrain <- stateTrain %>%
    group_by(region) %>%
    summarise(
      across(where(is.numeric), median, na.rm = TRUE)
    )
  
  stateAggTest <- stateTest %>%
    group_by(region) %>%
    summarise(
      across(where(is.numeric), median, na.rm = TRUE)
    )
  
}

## Cleanup
rm(readyTrain, readyTest)

