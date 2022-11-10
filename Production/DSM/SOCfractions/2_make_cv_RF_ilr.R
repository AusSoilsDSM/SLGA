#### Functions for calibrating RF models on ilr transformed variables and perform 10-fold cross-validation.
#### Also, function for calculating the PICP for several prediction intervals

#### First function was writen by Alexandre Wadoux,(CV on one variable)
#### Functions for ilr-transformed variables and PICP written by Mercedes Roman

#### Date last edit: 5/11/2021


# 1. Cross-validation on RF model --------------------------------------------

# function to fit a model using ranger
# Arguments:  
# # data: spatialpointdataframe with columns where there is property and raster covariates values at points
# # property: name of the soil property
# # covars: names of the raster layers to be used for model fitting
# # raster: one raster to be used as background if spatial.cv is TRUE
# # spatial.cv: TRUE or FALSE. If TRUE, a spatial cross-validationis made using raster argument as background. 
# # # if otherwise (spatial.cv = F) a random split is made (each point is used only once for validation)
# # nfolds: number of folds for the data split
## tunning: if TRUE, a parameter tuning with grid search is made
## Author: Alexandre Wadoux

make_cv_RF <- function(data, property, covars, raster, spatial.cv, nfolds, tunning){
  
  #k <- nfolds
  
  # remove rows with NA
  library(spatialEco)
  data <- sp.na.omit(data, margin = 1)
  
  # create a data.frame to store the prediction of each fold (record)
  testTable <- data.frame(obs = as.data.frame(data)[property], pred = NA)
  
  # make the formula
  form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))
  
  if(spatial.cv == T){
    
    # make spatial blocks
    sb <- spatialBlock(speciesData = data,
                       species = property,
                       rasterLayer = raster,
                       theRange = 200000, # size of the blocks
                       k = nfolds,
                       selection = "random",
                       iteration = 10, # find evenly dispersed folds
                       biomod2Format = TRUE,
                       xOffset = 0, # shift the blocks horizontally
                       yOffset = 0, 
                       verbose = FALSE, 
                       progress = FALSE) 
    
    # extract the fold indices from buffering object 
    # created in the previous section
    # the folds (list) works for all three blocking strategies
    folds <- sb$folds
    
    for(i in 1:nfolds){
      
      # extracting the training and testing indices
      # this way works with folds list (but not foldID)
      trainSet <- unlist(folds[[i]][1]) # training set indices
      testSet <- unlist(folds[[i]][2]) # testing set indices
      
      dat.train <- as.data.frame(data[trainSet, ][c(property, covars)])
      #dat.train <- na.omit(dat.train)
      
      #################### 
      ## model parameter optimization
      library(caret)
      library(ranger)
      
      if(tunning == TRUE){
        grid <-  expand.grid(mtry = c(2, 4, 6), 
                             splitrule = 'variance', 
                             min.node.size = c(2 ,5, 10))
        
        fitControl <- trainControl(method = "CV",
                                   number = 5,
                                   verboseIter = TRUE)
        
        fit = train(
          x = dat.train[ , names(dat.train) != property],
          y = dat.train[ , names(dat.train) == property],
          method = 'ranger',
          tuneGrid = grid,
          num.trees = 500,
          trControl = fitControl, 
          quantreg = TRUE)
        
        # prediction using best parameters
        rf <- ranger(form, data = dat.train, 
                     mtry = fit$bestTune$mtry,
                     min.node.size = fit$bestTune$min.node.size,
                     splitrule = 'variance',
                     num.trees = 500, 
                     quantreg = TRUE) # model fitting on training set
        
        dat.test <- as.data.frame(data[testSet, ][covars])
        testTable$pred[testSet] <- predict(rf, data = dat.test)$predictions # predict the test set
        
      }else{
        
        # prediction using best parameters
        rf <- ranger(form, data = dat.train, 
                     num.trees = 500, 
                     quantreg = TRUE) #, mtry = 1) # model fitting on training set
        
        dat.test <- as.data.frame(data[testSet, ][covars])
        testTable$pred[testSet] <- predict(rf, data = dat.test)$predictions # predict the test set
      }
    }
    
    # best fit using all data
    rf_all_data <- ranger(form, data = as.data.frame(data), 
                          num.trees = 500, 
                          importance = 'impurity')
    
    return(list(rf_all_data, testTable))
  }
  else{
    
    
    # samples that will be used for the validation
    sp <- sample(1:nfolds, nrow(data), replace = T)
    sp <- sample(sp)
    data$sp <- sp
    
    
    for(i in 1:nfolds){
      # extracting the training and testing indices
      # this way works with folds list (but not foldID)
      
      dat.train <- as.data.frame(data[data$sp != i,][c(property, covars)])
      
      #################### 
      ## model parameter optimization
      library(caret)
      library(ranger)
      
      if(tunning == TRUE){
        grid <-  expand.grid(mtry = c(2, 4, 6),     
                             splitrule = 'variance', 
                             min.node.size = c(2 ,5, 10))
        
        fitControl <- trainControl(method = "CV",
                                   number = 5,
                                   verboseIter = TRUE)
        
        fit = caret::train(
          x = dat.train[ , names(dat.train) != property],
          y = dat.train[ , names(dat.train) == property],
          method = 'ranger',
          tuneGrid = grid,
          num.trees = 500,
          trControl = fitControl, 
          quantreg = TRUE)
        
        # prediction using best parameters
        rf <- ranger(form, data = dat.train, 
                     mtry = fit$bestTune$mtry,
                     min.node.size = fit$bestTune$min.node.size,
                     num.trees = 500, 
                     quantreg = TRUE) # model fitting on training set
        
        testSet <- as.data.frame(data[data$sp == i,][covars])
        testTable$pred[data$sp == i] <- predict(rf, data = testSet)$predictions # predict the test set
      }else{
        
        # prediction using best parameters
        rf <- ranger(form, data = dat.train, 
                     num.trees = 500, 
                     quantreg = TRUE) # model fitting on training set
        
        testSet <- as.data.frame(data[data$sp == i,][covars])
        testTable$pred[data$sp == i] <- predict(rf, data = testSet)$predictions # predict the test set
        
        
        
        
        
        
      }
    }
    
    # best fit using all data
    rf_all_data <- ranger(form, data = as.data.frame(data), 
                          num.trees = 5000, 
                          importance = 'impurity', 
                          quantreg = TRUE)
    
    return(list(rf_all_data, testTable))
  }
  
}


#########################################################################################################################

### author: Mercedes Roman

# 2. Cross-validation on RF model of three ilr-transformed variabl --------

make_cv_RF_ilr_order <- function(data,       ### Dataframe with covariates and SOC fractions
                                 properties, ### Vector with the names of SOC fractions
                                 orderProps, ### Vector with HOC, POC, ROC, in the same order as in "properties"
                                 covars,     ### Name of covariates
                                 nfolds,     ### Number of k-folds for cross-validation
                                 thisseed,   ### Number for set.seed, to have the same partition into train and test, in case we test a different set of covariates, etc.
                                 ntreefinal  ### Number of trees for the final quantile regression forest calibrated with all data (for mapping or checking variable importance with permutation)
                                 ){
  
  # Remove rows with NA for the depth of interest, but keeping the coordinates, etc.
  data.depth <-  data[complete.cases(data[,c(properties,covars)]), ]
  
  ### Perform the ilr transformation
  library(compositions)
  ilr.properties <- ilr(data.depth[,properties])
  ilr.properties <- as.data.frame(ilr.properties)
  data.depth$ilr1 <- ilr.properties$V1
  data.depth$ilr2 <- ilr.properties$V2
  
  # create a data.frame to store the prediction of each fold (record) - Five columns, three for the original fractions,
  # two for the ilr variables
  testTable  <- data.depth[,c(properties,"ilr1","ilr2")]
  predsTable <- data.frame(matrix(ncol = 5,nrow = nrow(testTable), data = NA))
  colnames(predsTable) <- c("pred.ilr1","pred.ilr2","pred.HOC","pred.POC","pred.ROC")
  testTable <- cbind(testTable,predsTable)
  
  # make the formulas for ilr1 and ilr2
  form1 <- as.formula(paste("ilr1", paste(covars, collapse=" + "), sep=" ~ "))
  form2 <- as.formula(paste("ilr2", paste(covars, collapse=" + "), sep=" ~ "))
  
  # samples that will be used for the validation
  set.seed(thisseed)
  sp <- sample(1:nfolds, nrow(data.depth), replace = T)
  sp <- sample(sp)
  data.depth$sp <- sp
  
  for(i in 1:nfolds){
    # extracting the training and testing indices
    # this way works with folds list (but not foldID)
    dat.train <- data.depth[data.depth$sp != i,][c(properties, "ilr1", "ilr2", covars)]
    testSet   <- data.depth[data.depth$sp == i,covars]
    
    #################### 
    ## model 
    library(ranger)
    # rf calibration and prediction
    rf.ilr1 <- ranger(form1, 
                      data = dat.train, 
                      num.trees = 500, 
                      quantreg = TRUE) # model fitting on training set
    testTable$pred.ilr1[data.depth$sp == i] <- predict(rf.ilr1, data = testSet)$predictions # predict the test set
    
    rf.ilr2 <- ranger(form2, 
                      data = dat.train, 
                      num.trees = 500, 
                      quantreg = TRUE) # model fitting on training set
    testTable$pred.ilr2[data.depth$sp == i] <- predict(rf.ilr2, data = testSet)$predictions # predict the test set
    
  }
  
  ### Back-transform to composition (in %)
  Backt <- compositions::ilrInv(testTable[,c("pred.ilr1","pred.ilr2")])
  Backt <- as.data.frame(Backt)
  colnames(Backt) <- orderProps # SOC fractions in the order used for the ilr-transformation, e.g., "HOC", "POC", "ROC"
  testTable$pred.HOC <- Backt$HOC * 100
  testTable$pred.POC <- Backt$POC * 100
  testTable$pred.ROC <- Backt$ROC * 100
  
  # best fit using all data
  set.seed(thisseed+1908)
  rf.ilr1_all_data <- ranger(form1, 
                             data = data.depth, 
                             num.trees = ntreefinal,
                             importance = 'permutation', 
                             scale.permutation.importance = TRUE, 
                             quantreg = TRUE)
  
  # best fit using all data
  set.seed(thisseed+2046)
  rf.ilr2_all_data <- ranger(form2, 
                             data = data.depth,
                             num.trees = ntreefinal, 
                             importance = 'permutation', 
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE)
  
  return(list(rf.ilr1_all_data, rf.ilr2_all_data, testTable))
}


# 3.And calculate several percentiles for Prediction interval limits --------------------------------------------------

### author: Mercedes Roman

make_cv_RF_ilr_perc <- function(data,       ### Dataframe with covariates and SOC fractions
                                properties, ### Vector with the names of SOC fractions
                                orderProps, ### Vector with HOC, POC, ROC, in the same order as in "properties"
                                covars,     ### Name of covariates
                                nfolds,     ### Number of k-folds for cross-validation
                                thisseed,   ### Number for set.seed, to have the same partition into train and test, in case we test a different set of covariates, etc.
                                ntreefinal, ### Number of trees for the final quantile regression forest calibrated with all data (for mapping or checking variable importance with permutation)
                                nsamp,      ### Number of samples to take from the distribution of ilr1 and ilr2, used later for back-transformation
                                ### Vector with confidence levels for validating the prediction intervals with PICP
                                Conf.Levels = c(5,10,20,30,40,50,60,70,75,80,85,90,95,97.5,99)){
  
  # Remove rows with NA for the depth of interest, but keeping the coordinates, etc.
  data.depth <-  data[complete.cases(data[,c(properties,covars)]), ]
  
  ### Perform the ilr transformation
  library(compositions)
  ilr.properties <- ilr(data.depth[,properties])
  ilr.properties <- as.data.frame(ilr.properties)
  data.depth$ilr1 <- ilr.properties$V1
  data.depth$ilr2 <- ilr.properties$V2
  
  # create a data.frame to store the prediction of each fold (record) - Five columns, three for the original fractions,
  # two for the ilr variables
  testTable  <- data.depth[,c(properties,"ilr1","ilr2")]
  predsTable <- data.frame(matrix(ncol = 5,nrow = nrow(testTable), data = NA))
  colnames(predsTable) <- c("pred.ilr1","pred.ilr2","pred.HOC","pred.POC","pred.ROC")
  testTable <- cbind(testTable,predsTable)
  
  ### Create an array to keep the samples for ilr1 and ilr2
  ### Dimnames for array
  column.names <- paste0("Samp", c(1:nsamp))
  row.names <- paste0("row", c(1:nrow(data.depth)))
  matrix.names <- c("ilr1","ilr2")
  ### Empty array
  z <- array(rep(NA,2*nsamp*nrow(data.depth)), 
             dim = c(nrow(data.depth), nsamp, 2),
             dimnames = list(row.names, column.names, matrix.names)) 
  
  # Write the formulas for ilr1 and ilr2
  form1 <- as.formula(paste("ilr1", paste(covars, collapse=" + "), sep=" ~ "))
  form2 <- as.formula(paste("ilr2", paste(covars, collapse=" + "), sep=" ~ "))
  
  # samples that will be used for the validation
  set.seed(thisseed)
  sp <- sample(1:nfolds, nrow(data.depth), replace = T)
  sp <- sample(sp)
  data.depth$sp <- sp
  
  for(j in 1:nfolds){
    # extracting the training and testing indices
    # this way works with folds list (but not foldID)
    dat.train <- data.depth[data.depth$sp != j,][c(properties, "ilr1", "ilr2", covars)]
    testSet   <- data.depth[data.depth$sp == j,covars]
    
    #################### 
    ## model 
    library(ranger)
    # rf calibration and prediction
    rf.ilr1 <- ranger(form1, 
                      data = dat.train, 
                      num.trees = 500, 
                      quantreg = TRUE) # model fitting on training set
    testTable$pred.ilr1[data.depth$sp == j] <- predict(rf.ilr1, data = testSet)$predictions # predict ilr1 on the test set
    
    rf.ilr2 <- ranger(form2, 
                      data = dat.train, 
                      num.trees = 500, 
                      quantreg = TRUE) # model fitting on training set
    testTable$pred.ilr2[data.depth$sp == j] <- predict(rf.ilr2, data = testSet)$predictions # predict ilr2 on the test set
    
    ### Sample 500 values of ilr1 and ilr2 from the quantile RF distribution to estimate the prediction interval limits
    pred.ilr1.sample <- predict(object=rf.ilr1,
                                data = testSet, 
                                type = "quantiles",
                                what = function(x) sample(x, nsamp, replace = TRUE))$predictions
    
    pred.ilr2.sample <- predict(object=rf.ilr2,
                                data = testSet, 
                                type = "quantiles",
                                what = function(x) sample(x, nsamp, replace = TRUE))$predictions
    
    ### Assign to the array
    index.rows <- which(data.depth$sp == j)
    z[index.rows,,1] <- pred.ilr1.sample
    z[index.rows,,2] <- pred.ilr2.sample
    
  }
  
  ### Back-transform to 3-D composition (in %)
  Backt <- compositions::ilrInv(testTable[,c("pred.ilr1","pred.ilr2")])
  Backt <- as.data.frame(Backt)
  colnames(Backt) <- orderProps # SOC fractions in the order used for the ilr-transformation, e.g., "HOC", "POC", "ROC"
  testTable$pred.HOC <- Backt$HOC * 100
  testTable$pred.POC <- Backt$POC * 100
  testTable$pred.ROC <- Backt$ROC * 100
  
  ### Data for checking the PICP
  ### Sample from ilr1 and ilr2 --> prediction interval limits
  ### Back-transform to HOC, POC, ROC (%) with apply over the array
  backtf <- function(x){Backt <- compositions::ilrInv(x); return(Backt)}
  Backt <- apply(X = z, MARGIN = c(1,2) , FUN = backtf)
  dimnames(Backt)[[1]] <- orderProps 
  
  ### Calculate percentiles for the following confidence levels
  conf.levels <-  Conf.Levels # c(5,10,20,30,40,50,60,70,75,80,85,90,95,97.5,99)
  
  ### Prepare 6 dataframes, for each SOC fraction, Upper and Lower prediction interval limits
  uMat.HOC <- matrix(NA, nrow = nrow(data.depth), ncol = length(conf.levels)); colnames(uMat.HOC) <- paste0("PI",conf.levels)
  uMat.POC <- matrix(NA, nrow = nrow(data.depth), ncol = length(conf.levels)); colnames(uMat.POC) <- paste0("PI",conf.levels)
  uMat.ROC <- matrix(NA, nrow = nrow(data.depth), ncol = length(conf.levels)); colnames(uMat.ROC) <- paste0("PI",conf.levels)
  
  lMat.HOC <- matrix(NA, nrow = nrow(data.depth), ncol = length(conf.levels)); colnames(lMat.HOC) <- paste0("PI",conf.levels)
  lMat.POC <- matrix(NA, nrow = nrow(data.depth), ncol = length(conf.levels)); colnames(lMat.POC) <- paste0("PI",conf.levels)
  lMat.ROC <- matrix(NA, nrow = nrow(data.depth), ncol = length(conf.levels)); colnames(lMat.POC) <- paste0("PI",conf.levels)
  
  ### Calculate percentiles on the array of samples by observation:
  ## UPL
  uq <- 1-(((100-conf.levels)/2)/100)
  # 0.5250 0.5500 0.6000 0.6500 0.7000 0.7500 0.8000 0.8500 0.8750 0.9000 0.9250 0.9500 0.9750 0.9875 0.9950
  
  ## LPL
  lq <- ((100-conf.levels)/2)/100
  # 0.4750 0.4500 0.4000 0.3500 0.3000 0.2500 0.2000 0.1500 0.1250 0.1000 0.0750 0.0500 0.0250 0.0125 0.0050
  
  for(q in 1:length(uq)) {
    
    # Calculate the percentile over the 500 samples
    UPL <- apply(X = Backt, MARGIN = c(1,2) , FUN = function(x) quantile(x, uq[[q]]))
    LPL <- apply(X = Backt, MARGIN = c(1,2) , FUN = function(x) quantile(x, lq[[q]]))
    
    ### Transpose and transform into %
    UPL <- as.data.frame(t(UPL)); UPL <- UPL*100
    LPL <- as.data.frame(t(LPL)); LPL <- LPL*100
    
    ## Assign to the matrices
    uMat.HOC[,q] <- UPL$HOC
    uMat.POC[,q] <- UPL$POC
    uMat.ROC[,q] <- UPL$ROC
    
    lMat.HOC[,q] <- LPL$HOC
    lMat.POC[,q] <- LPL$POC
    lMat.ROC[,q] <- LPL$ROC
  }
  
  # best fit using all data
  set.seed(thisseed+1908)
  rf.ilr1_all_data <- ranger(form1, 
                             data = data.depth, 
                             num.trees = ntreefinal,
                             importance = 'permutation', 
                             scale.permutation.importance = TRUE, 
                             quantreg = TRUE)
  
  # best fit using all data
  set.seed(thisseed+2046)
  rf.ilr2_all_data <- ranger(form2, 
                             data = data.depth,
                             num.trees = ntreefinal, 
                             importance = 'permutation', 
                             scale.permutation.importance = TRUE,
                             quantreg = TRUE)
  
  return(list(rf.ilr1_all_data, rf.ilr2_all_data, testTable, uMat.HOC,uMat.POC, uMat.ROC, lMat.HOC, lMat.POC, lMat.ROC))
}


# 4. PICP for ilr variables and Vp -----------------------------------------------

### author: Mercedes Roman

make_cv_RF_ilr_PICP <- function(data,       ### Dataframe with covariates and SOC fractions
                                 properties, ### Vector with the names of SOC fractions
                                 orderProps, ### Vector with HOC, POC, ROC, in the same order as in "properties"
                                 covars,     ### Name of covariates
                                 nfolds,     ### Number of k-folds for cross-validation
                                 thisseed   ### Number for set.seed, to have the same partition into train and test, in case we test a different set of covariates, etc.
){
  
  # Remove rows with NA for the depth of interest, but keeping the coordinates, etc.
  data.depth <-  data[complete.cases(data[,c(properties,covars)]), ]
  
  ### Perform the ilr transformation
  library(compositions)
  ilr.properties <- ilr(data.depth[,properties])
  ilr.properties <- as.data.frame(ilr.properties)
  data.depth$ilr1 <- ilr.properties$V1
  data.depth$ilr2 <- ilr.properties$V2
  
  # create a data.frame to store the prediction of each fold (record) - Six columns
  testTable  <- data.depth[,c(properties,"ilr1","ilr2")]
  predsTable <- data.frame(matrix(ncol = 6,nrow = nrow(testTable), data = NA))
  colnames(predsTable) <- c("pred.ilr1","pred.ilr2","pred.ilr1.UPL","pred.ilr1.LPL","pred.ilr2.UPL", "pred.ilr2.LPL")
  testTable <- cbind(testTable,predsTable)
  
  # make the formulas for ilr1 and ilr2
  form1 <- as.formula(paste("ilr1", paste(covars, collapse=" + "), sep=" ~ "))
  form2 <- as.formula(paste("ilr2", paste(covars, collapse=" + "), sep=" ~ "))
  
  # samples that will be used for the validation
  set.seed(thisseed)
  sp <- sample(1:nfolds, nrow(data.depth), replace = T)
  sp <- sample(sp)
  data.depth$sp <- sp
  
  for(i in 1:nfolds){
    # extracting the training and testing indices
    # this way works with folds list (but not foldID)
    dat.train <- data.depth[data.depth$sp != i,][c(properties, "ilr1", "ilr2", covars)]
    testSet   <- data.depth[data.depth$sp == i,covars]
    
    #################### 
    ## model 
    library(ranger)
    # rf calibration and prediction
    rf.ilr1 <- ranger(form1, 
                      data = dat.train, 
                      num.trees = 500, 
                      quantreg = TRUE) # model fitting on training set
    testTable$pred.ilr1[data.depth$sp == i] <- predict(rf.ilr1, data = testSet)$predictions # predict the test set
    testTable$pred.ilr1.UPL[data.depth$sp == i] <- predict(rf.ilr1, data = testSet, type = "quantiles", quantiles = c(0.95))$predictions # predict the test set
    testTable$pred.ilr1.LPL[data.depth$sp == i] <- predict(rf.ilr1, data = testSet, type = "quantiles", quantiles = c(0.05))$predictions # predict the test set
    
    rf.ilr2 <- ranger(form2, 
                      data = dat.train, 
                      num.trees = 500, 
                      quantreg = TRUE) # model fitting on training set
    testTable$pred.ilr2[data.depth$sp == i] <- predict(rf.ilr2, data = testSet)$predictions # predict the test set
    testTable$pred.ilr2.UPL[data.depth$sp == i] <- predict(rf.ilr2, data = testSet, type = "quantiles", quantiles = c(0.95))$predictions # predict the test set
    testTable$pred.ilr2.LPL[data.depth$sp == i] <- predict(rf.ilr2, data = testSet, type = "quantiles", quantiles = c(0.05))$predictions # predict the test set
    
  }
  
 
  return(testTable)
}

make_cv_RF_PICP <- function(data,       ### Dataframe with covariates and SOC fractions
                            property, ### Vector with the names of the soil property (e.g., Vp)
                            covars,     ### Name of covariates
                            nfolds,     ### Number of k-folds for cross-validation
                            thisseed   ### Number for set.seed, to have the same partition into train and test, in case we test a different set of covariates, etc.
){
  
  # Remove rows with NA for the depth of interest, but keeping the coordinates, etc.
  data.depth <-  data[complete.cases(data[,c(property,covars)]), ]
  
  # create a data.frame to store the prediction of each fold (record) - Six columns
  testTable <- data.frame(obs = data.depth[property], pred = NA)
  predsTable <- data.frame(matrix(ncol = 2,nrow = nrow(testTable), data = NA))
  colnames(predsTable) <- c("pred.UPL","pred.LPL")
  testTable <- cbind(testTable,predsTable)
  
  # make the formula
  form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))
  
  # samples that will be used for the validation
  set.seed(thisseed)
  sp <- sample(1:nfolds, nrow(data.depth), replace = T)
  sp <- sample(sp)
  data.depth$sp <- sp
  
  for(i in 1:nfolds){
    # extracting the training and testing indices
    # this way works with folds list (but not foldID)
    dat.train <- data.depth[data.depth$sp != i,][c(property, covars)]
    testSet   <- data.depth[data.depth$sp == i,covars]
    
    #################### 
    ## model 
    library(ranger)
    # rf calibration and prediction
    rf.model <- ranger(form, 
                      data = dat.train, 
                      num.trees = 500, 
                      quantreg = TRUE) # model fitting on training set
    testTable$pred[data.depth$sp == i] <- predict(rf.model, data = testSet)$predictions # predict the test set
    testTable$pred.UPL[data.depth$sp == i] <- predict(rf.model, data = testSet, type = "quantiles", quantiles = c(0.95))$predictions # predict the test set
    testTable$pred.LPL[data.depth$sp == i] <- predict(rf.model, data = testSet, type = "quantiles", quantiles = c(0.05))$predictions # predict the test set
    
  }
  
  
  return(testTable)
}


#########################################################################################################################

### Eval function from A. Wadoux
### https://github.com/AlexandreWadoux/soilspec/blob/master/R/eval.R

#' @title eval
#'
#' @description function to evaluate predictions from a spectroscopic model
#'
#' @param obs vector of observed values
#' @param pred vector of predicted values
#' @param obj either `cat` for categorical variables of `quant` for quantitative variables
#'
#' @return a set of accuracy measures
#'
#' @export

eval <- function(obs, pred, obj, ...){
  
  if(obj == 'quant'){
    
    # mean error
    ME <- round(mean(pred - obs, na.rm = TRUE), digits = 2)
    
    # root mean square error
    RMSE <-   round(sqrt(mean((pred - obs)^2, na.rm = TRUE)), digits = 2)
    
    # Pearson's correlation squared
    r2 <-  round((cor(pred, obs, method = 'pearson', use = 'pairwise.complete.obs')^2), digits = 2)
    
    # coefficient of determination
    SSE <- sum((pred - obs) ^ 2, na.rm = T)
    SST <- sum((obs - mean(obs, na.rm = T)) ^ 2, na.rm = T)
    R2 <- round((1 - SSE/SST), digits = 2)
    
    # concordance correlation coefficient
    n <- length(pred)
    sdPred <- sd(pred, na.rm = T)
    sdObs <- sd(obs, na.rm = T)
    r <- stats::cor(pred, obs, method = 'pearson', use = 'pairwise.complete.obs')
    # scale shift
    v <- sdPred / sdObs
    sPred2 <- var(pred, na.rm = T) * (n - 1) / n
    sObs2 <- var(obs, na.rm = T) * (n - 1) / n
    # location shift relative to scale
    u <- (mean(pred, na.rm = T) - mean(obs, na.rm = T)) / ((sPred2 * sObs2)^0.25)
    Cb <- ((v + 1 / v + u^2)/2)^-1
    rCb <- r * Cb
    rhoC <- round(rCb, digits = 2)
    
    # RPD
    sdObs <- sd(obs)
    RMSE_2 <- sqrt(mean((pred - obs)^2))
    RPD <- round((sdObs/RMSE_2), digits = 2)
    
    # RPIQ
    q25 <- as.numeric(quantile(obs)[2])
    q75 <- as.numeric(quantile(obs)[4])
    iqDist <- q75 - q25
    RMSE2 <- sqrt(mean((pred - obs)^2))
    rpiq <- round((iqDist/RMSE2), digits = 2)
    
    # return the results
    evalRes <- data.frame(ME = ME, RMSE = RMSE, r2 = r2, R2 = R2, rhoC = rhoC, RPD = RPD, RPIQ = rpiq)
  }
  if (obj =='cat'){
    
    # overall accuracy
    cm = as.matrix(table(obs = obs, pred = pred))
    n <- length(obs)
    diag = diag(cm)
    OA <- round((sum(diag) / n), digits = 2)
    
    # Cohens' kappa
    cm = as.matrix(table(obs = obs, pred = pred))
    rowsums = apply(cm, 1, sum)
    colsums = apply(cm, 2, sum)
    n <- length(obs)
    diag = diag(cm)
    accuracy <- sum(diag) / n
    p = rowsums / n
    q = colsums / n
    expAccuracy = sum(p*q)
    kappa = round(((accuracy - expAccuracy) / (1 - expAccuracy)), digits = 2)
    
    # return the results
    evalRes <- data.frame(OA = OA, kappa = kappa)
  }
  return(evalRes)
}

