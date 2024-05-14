## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      cache = TRUE,
                      cache.comments = TRUE,
                      size = 13)


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'-----------------------------------------------------------------------------------
# Turn off scientific notation
options(scipen=999)

# Set seed; add CMRG for parallelization
set.seed(50009, "L'Ecuyer-CMRG")

# Clean, set up, and load
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
rm(list = ls(all = TRUE))

pacman::p_load(tidyverse, 
               ggplot2, 
               kableExtra,
               BiocManager,
               DESeq2,
               RColorBrewer,
               gridExtra,
               ggrepel,
               e1071,
               caret,
               randomForest,
               ranger,
               multtest,
               nestedcv,
               Rfast
)


## ----echo = FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------
load("../Demo/vst_all_timepoints.Rdata")
load("../Demo/DESeqResults.Rdata")              ## Load the results of the DESeq file
load("../Demo/OOB_RF_Importance.Rdata")         ## Load the OOB RF importance
load("../Demo/OOB_SVM_Importance.Rdata")        ## Load the OOB SVM importance


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------
doSplits <- function(vst, algorithm, splitRatio, filterCutoff) {
  ### @vst = vst dataset as extracted from DESeq2
  ### @algorithm = ML algorithm used; currently set up for rf and svm
  ### @splitRatio = the split ratio to employ (training size)
  ### @filterCutoff = the filter cutoff for median number of VST gene counts
  
  ## According to the Valabas et al. (2019) paper, make sure that we are filtering in TRAINING set only! 

  # Extract the VST data and transpose
  tVST <- t(assay(vst))
  
  # We do have gene names, e.g., TRNAV-UAC that are malformed for ranger and randomForest. We will fix that before proceeding:
  for (c in 1:ncol(tVST)) {
    colName <- colnames(tVST)[c]
    colName <- gsub("-", "_", colName)
    colName -> colnames(tVST)[c]
  }
  
  ## Add the metadata as columns & merge
  df1 <- cbind(colData(vst)[1], colData(vst)[3], colData(vst)[2])       ## We don't need the size factors
  tVST <- merge(tVST, df1, by = "row.names")

  ## The merge turns back into a dataframe and removes the sample names from the rows; let's put them back:
  rownames(tVST) <- tVST[,1]
  tVST <- tVST[,-1]
  
  if(algorithm == "svm") {
    ## Make the factors unordered
    tVST <- tVST %>% 
      mutate_if(is.ordered, factor, ordered = FALSE)
  }
  
  ## Create the data partitions
  ind <- createDataPartition(y = tVST[, c("Treatment")],     ## Treatment is evenly distributed
                             p = splitRatio,                    ## % into training
                             list = FALSE)                      ## don't return a list
  train <- tVST[ind, ]
  test <- tVST[-ind,]
  
  ## Now apply the filtering:
  # Calculate row medians of VST gene counts
  medians <- rowMedians(assay(vst))

  # Filter the features out of train:
  train <- train[, medians > filterCutoff]  
  print(paste0("After filtering, the number of genes remaining in the dataset are: ", ncol(train)))

  splits <- list(train, test)
  return(splits)
}


## ----echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
findOverlappingGenes <- function(lfc, important) {
  ### @lfc = the log-fold change cutoff you'd like to employ on the originall DESeq results
  ### @important = the list, df, or matrix that contains the importance values from the ML classifier; make sure it is already filtered if needed.

  res <- resultsDESeq %>% 
    as.data.frame() %>% 
    filter(abs(log2FoldChange) >= lfc)   # Make sure to filter by the ABSOLUTE VALUE :)
  
  # Move the rownames (genes) back to a column
  res$geneID <- rownames(res)
  # Coerce to a dataframe, if needed
  important <- important %>% 
    as.data.frame() %>% 
    filter()
  # Move the rownames (genes) back to a column, if needed
  if (!"geneID" %in% colnames(important)) {
      important$geneID <- rownames(important)
  }
  #Perform an inner join to find the overlap
  overlap <- inner_join(res, important, by = "geneID")
  
  return(overlap)
}


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------
compareConfusion <- function(confusionList) {
  ## instantiate
  finalDF <- data.frame()
  for(i in 1:length(confusionList)) {
    ## The first one
    if(i == 1) {
      confMat <- confusionList[[i]]   ## grab the first one
      df <- confMat$overall %>% as.data.frame() 
      finalDF <- rownames(df) %>% as.data.frame()
      colnames(finalDF)[1] <- "Metric"
      finalDF$`Confusion Matrix 1`  <- df[, 1]       ## grab the value
    }
    if(i > 1) {
      name <- paste0('Confusion Matrix ', i)
      confMat <- confusionList[[i]]
      df <- confMat$overall %>% as.data.frame()
      finalDF[, name] <- df[, 1]       ## grab the value
    }
  }
  return(finalDF)
}


## -----------------------------------------------------------------------------------------------------------------------------------------------
splits <- doSplits(vst = vsData, algorithm = "rf", splitRatio = 0.8, filterCutoff = 5)
train <- splits[[1]]
test <- splits[[2]]


## ----echo = TRUE, eval=T------------------------------------------------------------------------------------------------------------------------
rfOOB <- randomForest::randomForest(
  Treatment ~ ., 
  data = train)

pred.test.rf <- predict(rfOOB, test, type = "response")
confMat <- confusionMatrix(pred.test.rf, test$Treatment)


## ----eval = T-----------------------------------------------------------------------------------------------------------------------------------
# Set the CV arguments
kFoldCtrl <- trainControl(method = "cv",    # for k-fold CV
                          number = 10)      # k


## -----------------------------------------------------------------------------------------------------------------------------------------------
rfCV <- train(Treatment ~.,  
               data = train,
               method = "ranger",
               trControl = kFoldCtrl)    ## added in the 10-fold CV


## ----echo = FALSE, warning=FALSE, message=FALSE, eval=T-----------------------------------------------------------------------------------------
ggplot(rfCV, highlight = TRUE) +
  ggtitle("Random Forest Performance After 10-fold CV") + 
  theme_bw()


## ----echo = F-----------------------------------------------------------------------------------------------------------------------------------
rfCV$bestTune %>% 
kable(
    format = "html",
    caption = "Table 1. Results of the 10-fold CV Random Forest") %>%
    kable_styling(bootstrap_options = c("hover", full_width = F))


## ----echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
rfCV <- train(Treatment ~.,  
               data = train,
               method = "ranger",
               trControl = kFoldCtrl, 
               tuneGrid = rfCV$bestTune)   # Add in the results of the CV and auto tuning


pred.test.rf <- predict(rfCV, test, type = "raw")  ## type is now 'raw' 
# Store the confusion matrix
confMatCV <- confusionMatrix(pred.test.rf, test$Treatment)


## ----echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
compareConfusion(confusionList = list(confMat, confMatCV)) %>% 
  kable(
    format = "html",
    caption = "Table 2. Comparing Accuracy - Random Forest") %>%
    kable_styling(bootstrap_options = c("hover", full_width = F))


## ----echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
# Make a set of k = 10 seeds for reproducibility
seeds <- vector(mode = "list", length = 11)
for(i in 1:10) {
  seeds[[i]]<- sample.int(n=1000, 54)   # Increase 54 if you have a larger grid! 
}
# For the last model
seeds[[11]]<-sample.int(1000, 1)


## ----echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
# Set the CV arguments
kFoldCtrl <- trainControl(method = "cv",    # for k-fold CV
                          number = 10,      # k
                          seeds = seeds)    # sets the seeds, one for each split



## -----------------------------------------------------------------------------------------------------------------------------------------------
searchGrid <- expand.grid(
  mtry = floor(ncol(train) * c(.05, .15, .25, .35, .45)),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 3, 5, 10) 
)


## -----------------------------------------------------------------------------------------------------------------------------------------------
rfTuned <- train(Treatment ~.,  
               data = train,
               method = "ranger",
               trControl = kFoldCtrl, 
               tuneGrid = searchGrid # Add in the search grid
)


## ----echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
ggplot(rfTuned, highlight = TRUE) +
  ggtitle("Random Forest Performance Grid Search Tuning") + 
  theme_bw()


## -----------------------------------------------------------------------------------------------------------------------------------------------
rfTuned$bestTune %>% 
kable(
    format = "html",
    caption = "Table 3. Results of the Grid Search on Random Forest") %>%
    kable_styling(bootstrap_options = c("hover", full_width = F))


## -----------------------------------------------------------------------------------------------------------------------------------------------
rfTuned <- train(Treatment ~.,  
               data = train,
               method = "ranger",
               trControl = kFoldCtrl, 
               tuneGrid = rfTuned$bestTune)   # Add in the results of the CV and auto tuning


## -----------------------------------------------------------------------------------------------------------------------------------------------
pred.test.rf <- predict(rfTuned, test, type = "raw")  ## type is now 'raw' 
# Store the confusion matrix
confMatTuned <- confusionMatrix(pred.test.rf, test$Treatment)


## -----------------------------------------------------------------------------------------------------------------------------------------------
compareConfusion(confusionList = list(confMat, confMatCV, confMatTuned)) %>% 
    kable(
    format = "html",
    caption = "Table 4. Comparing Accuracy - Random Forest") %>%
    kable_styling(bootstrap_options = c("hover", full_width = F))


## -----------------------------------------------------------------------------------------------------------------------------------------------
# modelLookup("ranger")
# modelLookup("rf")


## ----echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
makeCustomMethod <- function(paramList, methodName) {
  
  custom <- list(type = "Classification", 
                 library = "ranger", 
                 loop = NULL)
  custom$parameters <- data.frame(parameter = paramList, 
                                  class = rep("numeric", length(paramList)), 
                                  label = paramList)
  custom$grid <- function(x, y, len = NULL, search = "grid") {}

  custom$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
        randomForest(x, y, 
               mtry = param$mtry,    ## need to update to make dynamic
               num.trees = param$num.trees)
  }
  custom$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata)

  custom$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata, type = "prob")

  custom$sort <- function(x) x[order(x[,1]),]

  custom$levels <- function(x) x$classes
return(custom)
}


## -----------------------------------------------------------------------------------------------------------------------------------------------
customRF <- makeCustomMethod(paramList = c("mtry", 
                                           "num.trees", 
                                           "min.node.size",
                                           "splitrule"),
                             methodName = "ranger")

searchGrid <- expand.grid(.mtry = floor(ncol(train) * c(.05, .15, .25)),
                        .num.trees = c(50, 200), 
                        .min.node.size = c(1, 5, 10, 20),
                        .splitrule = "gini"
                        )

customTuned <- train(y = train$Treatment, x = train, 
                method = customRF, 
                metric = "Accuracy", 
                tuneGrid = searchGrid, 
                trControl = kFoldCtrl)


## -----------------------------------------------------------------------------------------------------------------------------------------------
ggplot(customTuned, highlight = TRUE) +
  ggtitle("Customized RF Performance Grid Search Tuning") + 
  theme_bw()


## -----------------------------------------------------------------------------------------------------------------------------------------------
customTuned$bestTune %>% 
kable(
    format = "html",
    caption = "Table 4. Results of the Customized Grid Search on Random Forest") %>%
    kable_styling(bootstrap_options = c("hover", full_width = F))


## ----warning = FALSE, message=FALSE-------------------------------------------------------------------------------------------------------------
newTuned <- train(Treatment ~.,  
               data = train,
               method = customRF,
               trControl = kFoldCtrl, 
               tuneGrid = customTuned$bestTune)


## -----------------------------------------------------------------------------------------------------------------------------------------------
pred.test.rf <- predict(newTuned, test, type = "raw")  
# Store the confusion matrix
confMatTuned2 <- confusionMatrix(pred.test.rf, test$Treatment)


## ----echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
compareConfusion(confusionList = list(confMat, confMatCV, confMatTuned, confMatTuned2)) %>% 
    kable(
    format = "html",
    caption = "Table 4. Comparing Accuracy - Random Forest") %>%
    kable_styling(bootstrap_options = c("hover", full_width = F))


## -----------------------------------------------------------------------------------------------------------------------------------------------
searchGrid <- expand.grid(.mtry = floor(ncol(train) * c(0.01, 0.05, 0.10)),
                        .min.node.size = c(1, 5, 10),
                        .splitrule = "gini"                        
                        )

ncv <- nestcv.train(y = train$Treatment, x = train,
                    method = 'ranger',
                    tuneGrid = searchGrid, 
                    savePredictions = "final")


## ----echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
ggplot(ncv$outer_result[[1]]$fit) +
  scale_x_log10() +
  ggtitle("Results of Nested CV with hyperparameter tuning") +
  theme_bw()


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------
# Plot ROC and Precision-Recall curves
op <- par(mfrow = c(1, 2))

# Outer CV ROC
plot(ncv$roc, 
     main = "Outer-folds ROC", 
     col = 'blue')
legend("bottomright", 
       legend = paste0("AUC = ", signif(pROC::auc(ncv$roc), 3)), 
       bty = 'n')

# Inner CV ROC
inroc <- innercv_roc(ncv)
plot(inroc, 
     main = "Inner-folds ROC", 
     col = 'red')
legend("bottomright", 
       legend = paste0("AUC = ", signif(pROC::auc(inroc), 3)), 
       bty = 'n')


## -----------------------------------------------------------------------------------------------------------------------------------------------
preds <- predict(ncv, 
                 newdata = test)
confusionMatrix(preds, test$Treatment)


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------
nested_importantRF <- rf_filter(ncv, 
                               y = train$Treatment, 
                               x = train[, -ncol(train)], # drop Treatment
                               type ="full") %>% as.data.frame()
names(nested_importantRF) <- "MeanDecreaseGini"


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------------
nested_importantRF %>% 
  arrange(desc(abs(MeanDecreaseGini))) %>% 
  top_n(10) %>% 
kable(
    format = "html",
    caption = "Table 5. Important Features from the Nested CV Grid Search on Random Forest") %>%
    kable_styling(bootstrap_options = c("hover", full_width = F))


## ----echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
importantRF_filt <- nested_importantRF %>% 
  as.data.frame() %>% 
  filter(MeanDecreaseGini > 0)


## ----echo = FALSE-------------------------------------------------------------------------------------------------------------------------------
findOverlappingGenes(lfc = 1, importantRF_filt) %>% 
  arrange(desc(abs(MeanDecreaseGini))) %>% 
  kable(
    format = "html",
    caption = "Table 6. Overlapping Features identified from DESeq and  Nested CV Random Forest") %>%
    kable_styling(bootstrap_options = c("hover", full_width = F))


## ----ref.label = knitr::all_labels(), echo = FALSE, eval = FALSE--------------------------------------------------------------------------------
## # Make an R script

