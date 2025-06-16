############################################
## Author: K.S. Geist
## Example code to execute a temporal lagged
## random forest analysis to account for 
## temporal autocorrelation in Project_2
## TO RUN:
## source(file = "mortalityTemporalLagRF.R")
############################################

## Give a message to the user:
print("These are the results of a temporally lagged 10-CV random forest.")

## Load the additional packages needed
pacman::p_load(zoo)

########## Feature Engineering Lags and Rolling Means #############
## Random forest itself is not a time series model, but the great thing about
## RF and many other ML models is that we can engineer temporal features, just 
## as we engineered spatial features, to add as predictors to our model.
## We will calculate both LAGGED and ROLLING MEANS features to help us account
## for temporal autocorrelation. I am choosing just some common features; there
## ARE more robust ways for choosing the best features to engineer for a dataset
## Thus, be aware that I am not doing vigorous testing and selecting of the
## best ways to engineer temporal features for the sake of time, but rather
## doing some quickly to see if we can adjust for the temporal autocorrelation
## at all.

## LAGGED values capture temporal dependence. For example, if mortality is high 
## this month, it may have been rising in prior months.

## ROLLING MEANS smooth out short-term fluctuations and show trends over time.
## These are crucial for temporal forecasting, especially when using models that 
## are not inherently time-aware like Random Forest.

## Feature engineer a new mortality shapefile just so we don't overwrite the original
mortality_sf3 <- mortality_sf2 %>%
  ## Sort data by location and time
  arrange(COUNTY_NAM, Year, Month) %>%  
  ## Group data by county - because we know spatial autocorrelation exists!
  group_by(COUNTY_NAM) %>%  
  
  ## Create lag features: the percent mortality from 1, 2, and 3 months ago
  mutate(
    ## 1-month lag - is this month's mortality correlated to last month's?
    Lag1 = lag(monthlyPercMortality, 1),  
    ## 1-month lag - is this month's mortality correlated to 2 months ago?
    Lag2 = lag(monthlyPercMortality, 2),  
    
    ## 3-month lag - is this month's mortality correlated to 3 months ago? 
    ## (roughly a quarter - but not quite!!)
    Lag3 = lag(monthlyPercMortality, 3),  
    
    ## Create rolling mean features using a trailing (right-aligned) window
    ## 3-month rolling/moving average:
    rollMeans3 = rollmean(monthlyPercMortality, 3, fill = NA, align = "right"), 
    ## 6-month rolling/moving average:
    rollMeans6 = rollmean(monthlyPercMortality, 6, fill = NA, align = "right")   
  ) %>%
  ## Make sure to ungroup() at the end!
  ungroup()

########## Drop the rows with missing temporal data #############
## You might wonder why we'd do this as opposed to converting them to zero. 
## Doesn't this sacrifice data? Yes, but no. Here's why: since we're now including 
## information about temporal autocorrelation, an NA has true meaning. It means 
## that it was the first timepoint, so lagged or rolling means information wasn't
## available. But if we were to substitute it with zero, that would imply that 
## mortality was actually zero which it wasn't! That would introduce bias we don't
## want. Hence, we will drop the NAs from the dataset instead. 

## Drop rows where any lag or rolling feature is NA, which happens naturally 
## at the start of each time series
mortality_sf3 <- mortality_sf3 %>% drop_na()
## NOTE: We are able to do this with drop_na() quickly here because we have NO 
## MISSING DATA that we need to impute. Always drop NAs with caution!!

########## Data Partitioning in Training & Testing #############
## We will do our time-aware split as we did without the temporal features 
## (train on pre-2020 data and test on 2020) and perform the same 
## pre-processing we did without the temporal features

## Make the training data
mortalityTrain2 <- mortality_sf3 %>% 
  ## Drop the geometry to make processing downstream faster
  st_drop_geometry() %>% 
  ## Filter for just the year-month combos we want
  filter(Year != 2020) %>% 
  ## Drop the previously centered monthly percent mortality from when we made
  ## the spatial weights matrix
  select(-monthlyPercMortalityCentered, -COUNTY_NAM)

## Make the testing data
mortalityTest2 <- mortality_sf3 %>% 
  ## Drop the geometry to make processing downstream faster
  st_drop_geometry() %>% 
  ## Filter for just the year-month combos we want
  filter(Year == 2020) %>% 
  ## Drop the previously centered monthly percent mortality from when we made
  ## the spatial weights matrix
  select(-monthlyPercMortalityCentered, -COUNTY_NAM)

########## Pre-process Train & Test #############
## First, make a new object, preProcInstructions2, that stores the 
## pre-processing instructions from mortalityTrain2:
preProcInstructions2 <- preProcess(mortalityTrain2, 
                                   method = c("center", "scale"))

## Now apply the pre-processing instructions using predict()
mortalityTrain2 <- predict(preProcInstructions2,
                          newdata = mortalityTrain2)
mortalityTest2 <- predict(preProcInstructions2,
                          newdata = mortalityTest2)

########## 10-Fold Cross-Validated Random Forest #############
## This time we will skip doing the OOB, since it's just doing internal 
## validation. We already know that our performance goes up with external 
## validation, so we will just go straight to the 10-fold CV:

rfCV2 <- train(
  monthlyPercMortality ~ ., 
  data = mortalityTrain2,
  method = "rf",
  trControl = ctrlCV,
  importance = TRUE
)

## Take a look at the performance metrics; 
## Remember lower RMSE/MAE + higher R^2 == better model!
print("These are the training performance metrics:")
print(rfCV2$results)

########## Evaluate Test Performance #############
## Now, predict the training model's performance on the testing set to see if
## this model is performing better than just the spatial RF we did
mortalityTest2$predicted <- predict(rfCV2, newdata = mortalityTest2)

## Evaluate performance on holdout (2020) data. Remember that lower values
## means better performance!

### Compute RMSE (Root Mean Squared Error)
rfRMSE <- RMSE(mortalityTest2$predicted, mortalityTest2$monthlyPercMortality)
## Compute MAE (Mean Absolute Error) as well, optionally
rfMAE  <- MAE(mortalityTest2$predicted, mortalityTest2$monthlyPercMortality)
## Print results
print("These are the testing performance metrics using the 2020 holdout set:")
print(paste0("Random Forest Test RMSE:   ", round(rfRMSE, 4)))
print(paste0("Random Forest Test MAE:    ", round(rfMAE, 4)))

########## Extract Variable Importance #############
## Get variable importance
p <- varImp(rfCV2)[[1]] %>%
  ## Add row names and labels
  mutate(Variable = rownames(.),
         Label = case_when(
           Variable == "Month" ~ "Month",
           Variable == "lagged_monthlyPercMortality" ~ "Spatial Lag",
           Variable == "Year" ~ "Year",
           Variable == "Lag1" ~ "% Mortality lagged 1-month",
           Variable == "Lag2" ~ "% Mortality lagged 2-months",
           Variable == "Lag3" ~ "% Mortality lagged 3-months",
           Variable == "rollMeans3" ~ "3-month Moving Average of Mortality",
           Variable == "rollMeans6" ~ "6-month Moving Average of Mortality"
         )) %>%
  ## Make a column plot ordered from greatest to least
  ggplot(aes(y = fct_reorder(Label, Overall), x = Overall, fill = Label)) + 
  geom_col() +
  scale_fill_manual(values = skittles) +
  theme_minimal() +
  labs(title = "Variable Importance from 10-fold CV Random Forest",
       subtitle = "Monthly Percent Mortality (All Causes)",
       caption = "Source: California DPH Vital Records, 2017-2019",
       y = "Variable",
       x = "% Importance",
       fill = "Predictor")

print(p)