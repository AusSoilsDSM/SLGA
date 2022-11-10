# function to fit a probability model using ranger
# Arguments:  
# # data: dataframe with soil property and environmental covariates
# # property: name of the soil property - Coarse Fragments class
# # covars: names of the raster layers to be used for model fitting
# # a random split is made (each point is used only once for validation)
# # nfolds: number of folds for the data split
## tunning: if TRUE, a parameter tuning with grid search is made - not used here

make_cv_RF_cf_prob <- function(data, property, covars, nfolds){
 
  
  # create a data.frame to store the prediction of each fold (record)
  testTable <- data.frame(obs = as.data.frame(data)[property],
                          #X0.p = NA,
                          X1.p = NA, X2.p = NA, X3.p = NA,
                          X4.p = NA, X5.p = NA, X6.p = NA, pred = NA)
  
  # make the formula
  form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))
  
    
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
      #library(caret)
      library(ranger)
   
        # prediction using best parameters
        rf <- ranger(form, 
                     data = dat.train,
                     num.trees = 500,
                     importance="permutation",
                     probability = TRUE) # model fitting on training set

        testSet <- as.data.frame(data[data$sp == i,][covars])
        testTable[data$sp == i, 2:7] <- predict(rf, data = testSet)$predictions # predict the test set
      }
    
    
    # best fit using all data
    rf_all_data <- ranger(form, data = as.data.frame(data), 
                          num.trees = 500,
                          #importance="permutation",
                          importance='impurity_corrected',
                          probability = TRUE)
    
    return(list(rf_all_data, testTable))
  }
  
### Now, for classification
make_cv_RF_cf_class <- function(data, property, covars, nfolds){
  
  
  # create a data.frame to store the prediction of each fold (record)
  testTable <- data.frame(obs = as.data.frame(data)[property],pred = NA)
  
  # make the formula
  form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))
  
  
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
    #library(caret)
    library(ranger)
    
    #   if(tunning == TRUE){
    #     grid <-  expand.grid(mtry = c(2, 4, 6),     
    #                          splitrule = 'variance', 
    #                          min.node.size = c(2 ,5, 10))
    #     
    #     fitControl <- trainControl(method = "CV",
    #                                number = 5,
    #                                verboseIter = TRUE)
    #     
    #     fit = caret::train(
    #       x = dat.train[ , names(dat.train) != property],
    #       y = dat.train[ , names(dat.train) == property],
    #       method = 'ranger',
    #       tuneGrid = grid,
    #       num.trees = 500,
    #       trControl = fitControl, 
    #       quantreg = TRUE)
    #     
    #     # prediction using best parameters
    #     rf <- ranger(form, data = dat.train, 
    #                  mtry = fit$bestTune$mtry,
    #                  min.node.size = fit$bestTune$min.node.size,
    #                  num.trees = 500, 
    #                  quantreg = TRUE) # model fitting on training set
    #     
    #     testSet <- as.data.frame(data[data$sp == i,][covars])
    #     testTable$pred[data$sp == i] <- predict(rf, data = testSet)$predictions # predict the test set
    #   }else{
    #     
    # prediction using best parameters
    rf <- ranger(form, 
                 data = dat.train,
                 num.trees = 500,
                 min.node.size = 5,
                 importance="permutation",
                 classification= TRUE) # model fitting on training set
    
    testSet <- as.data.frame(data[data$sp == i,][covars])
    testTable$pred[data$sp == i] <- predict(rf, data = testSet)$predictions # predict the test set
  }
  
  
  # best fit using all data
  rf_all_data <- ranger(form, 
                        data = as.data.frame(data), 
                        num.trees = 500,
                        importance="permutation",
                        min.node.size = 5,
                        classification= TRUE)
  
  return(list(rf_all_data, testTable))
}

### Now, for gravimetric coarse fragments
make_cv_RF_cf_grav <- function(data, property, covars, nfolds){
  
  
  # create a data.frame to store the prediction of each fold (record)
  testTable <- data.frame(obs = as.data.frame(data)[property],pred = NA)
  
  # make the formula
  form <- as.formula(paste(property, paste(covars, collapse=" + "), sep=" ~ "))
  
  
  # samples that will be used for the validation
  sp <- sample(1:nfolds, nrow(data), replace = T)
  sp <- sample(sp)
  data$sp <- sp
  
  
  for(i in 1:nfolds){
    # extracting the training and testing indices
    # this way works with folds list (but not foldID)
    
    dat.train <- as.data.frame(data[data$sp != i,][c(property, covars)])
    
    #################### 
    ## model calibration
    library(ranger)
    
    rf <- ranger(form, 
                 data = dat.train,
                 num.trees = 500,
                 quantreg = TRUE) # model fitting on training set
    
    testSet <- as.data.frame(data[data$sp == i,][covars])
    testTable$pred[data$sp == i] <- predict(rf, data = testSet)$predictions # predict the test set
  }
  
  
  # model using all data for mapping
  rf_all_data <- ranger(form, 
                        data = as.data.frame(data), 
                        num.trees = 500,
                        importance="permutation",
                        scale.permutation.importance = TRUE,
                        classification= TRUE)
  
  return(list(rf_all_data, testTable))
}

### Multinomial logistic regression
make_cv_multinom_cf <- function(data, property, covars, nfolds){
  
  # make the formula
  form <- as.formula(paste(property, paste(covars, collapse="+"), sep="~"))
  #form <- as.formula(paste0(property, "~ ." ))
  data <- data[,c(property,covars)]
  #data$CF.class.0_5 <- as.factor(data$CF.class.0_5)
  
  # Split into cv folds
  library(groupdata2)
  train_set <- groupdata2::fold(data, k = nfolds, cat_col = property)
  
  # create a data.frame to store the prediction of each fold (record)
  testTable <- data.frame(obs = as.data.frame(train_set)[property],
                          X0.p = NA, X1.p = NA, X2.p = NA, X3.p = NA,
                          X4.p = NA, X5.p = NA, X6.p = NA, pred = NA)
  ### attach fold levels
  testTable$folds <- train_set$.folds
  
  ### Perform cross-validation
  folds <- levels(train_set$.folds)
  
  for(i in 1:nfolds){
    # extracting the training and testing indices
    dat.train <- as.data.frame(train_set[train_set$.folds != folds[[i]],][c(property, covars)])
    library(nnet)
    cf.MNLR <- multinom(form, data = dat.train) # model fitting on training set
    testSet <- as.data.frame(train_set[train_set$.folds == folds[[i]],c(property, covars)])
    testTable[testTable$folds == folds[[i]], 2:8] <- predict(cf.MNLR, newdata= testSet, "probs") # predict probabilities on the test set
    testTable[testTable$folds == folds[[i]],"pred"] <- as.vector(predict(cf.MNLR, newdata=testSet)) # predict the class
  }
  
  # Model using all data
  cf.MNLR <- multinom(form, data = as.data.frame(data[,c(property, covars)]))
  return(list(cf.MNLR, testTable))
}

###End of script