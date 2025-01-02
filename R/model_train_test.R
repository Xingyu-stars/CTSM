#' @title Cum-Time Stacking Model Training and Prediction
#' @param data_feature A list containing feature data for different time windows. This can be obtained from feature extraction functions such as data_feature_func_KPCA() or data_feature_func_LSTM().
#' @param lookback_time A vector representing the length of the cumulative time window.
#' @param test_index The indices of the test set, specifying which data points belong to the test set.
#' @return A data frame containing the evaluation results on the test set.
#' @export
#'
stacking_func0 <- function(data_feature,lookback_time,test_index){
  library(caret)
  library(tidyverse)
  library(caTools)
  library(pROC)
  library(kernlab)
  library(rpart)
  library(caretEnsemble)

  for(i in 1:length(data_feature)){
    #Load data
    data <- data.frame(data_feature[[i]])
    #become factor
    data[,2]<- factor(data[,2])
    #Convert to valid R names
    levels(data$sepsis_diagnose) <- make.names(levels(data$sepsis_diagnose))

    # Split into training and test sets
    traindata <- data[-test_index,]
    testdata <- data[test_index,]

    #Setup base learner cross validation
    my_control <- trainControl(
      method="cv",
      number=5,
      savePredictions="final",
      classProbs=TRUE,
      summaryFunction=twoClassSummary,
      returnResamp="all"# Add this line to save the validation set predictions for each cross validation
    )

    ## Constructing dependent and independent variables
    form_cls <- as.formula(
      paste0(
        "sepsis_diagnose~",
        paste(colnames(traindata)[3:ncol(data)],collapse = "+")
      )
    )

    ##Training Base Learner
    model_list <- caretList(
      form_cls,
      data = traindata,
      trControl = my_control,
      methodList = c("rf", "ada",'gbm')
    )

    # Extract validation set predictions for each cross-validation of Random Forest, XGBoost and SVM
    rf_pred <- model_list$rf$pred
    ada_pred <- model_list$ada$pred
    gbm_pred <- model_list$gbm$pred
    # Put validation set results from different models in one table
    merg_model_pred <- left_join(rf_pred %>% select(rowIndex, X1),ada_pred %>% select(rowIndex, X1), by = "rowIndex") %>%left_join(gbm_pred %>% select(rowIndex, X1),by = "rowIndex") %>%
      select(rowIndex, X1.x, X1.y, X1) %>%  # Select the appropriate column
      rename(X1_rf = X1.x, X1_ada = X1.y, X1_gbm = X1)  #Renaming column names

    #filter traindata for hadm_id and sepsis_diagnose, add rowIndex
    first_data_selected <- traindata %>%
      mutate(rowIndex = row_number()) %>%
      select(hadm_id, sepsis_diagnose,rowIndex)

    # Join the above two tables by rowIndex
    merg_trainpred0 <- merge(first_data_selected, merg_model_pred, by = "rowIndex", all = TRUE)[,-1]
    #Column Name Definitions
    colnames(merg_trainpred0) <- c('hadm_id','sepsis_diagnose',paste0('X',lookback_time[i],'h_',c('rf','ada','gbm'),sep=''))

    if(i==1){merg_trainpred <- merg_trainpred0}else{
      merg_trainpred <- cbind(merg_trainpred,merg_trainpred0[,-(1:2)])
    }

    #Test set prediction results
    model_testpreds <- lapply(model_list, predict, newdata=testdata, type="prob")
    model_testpreds <- lapply(model_testpreds, function(x) x[,"X1"])
    model_testpreds <- data.frame(model_testpreds)
    #Column Name Definitions
    colnames(model_testpreds) <- paste0('X',lookback_time[i],'h_',c('rf','ada','gbm'),sep='')

    if(i==1){merg_testpred <- cbind(testdata[,c(1,2)],model_testpreds)}else{merg_testpred <- cbind(merg_testpred,model_testpreds)}


  }

  #The following steps calculate the evaluation indicators for different scenarios
  traindata <- merg_trainpred
  testdata <- merg_testpred

  out <- c()
  for(i in 1:(ncol(testdata)-2)){
    testpredprob <- testdata[,i+2]
    testroc <- roc(response = testdata$sepsis_diagnose,
                   predictor = testpredprob)
    out_roc <- testroc$auc
    conf_matrix <- confusionMatrix(data=as.factor(ifelse(testpredprob>=testroc$thresholds[which.max(testroc$sensitivities+testroc$specificities-1)],'X1','X0')),reference = testdata$sepsis_diagnose,positive="X1",mode ="everything")
    out_acc <- c(conf_matrix$byClass[c('Sensitivity','Specificity','F1','Recall')],conf_matrix$overall['Accuracy'])
    out <- rbind(out,c(out_acc,out_roc))
  }

  #Setting up meta-learner cross-validation
  ctrl <- trainControl(method = "cv",number = 5)
  #Get the time prefix of the column name
  str_lookback <- unique(unlist(lapply(names(testdata)[-c(1,2)],function(x){return(strsplit(x,'_')[[1]][1])})))
  #Generate all possible cumulative prefixes based on the temporal prefixes in the str_lookback vector
  str_lookback_cum <- unlist(lapply(2:length(str_lookback),function(i){return(paste0(str_lookback[1:i],collapse='-'))}))
  #All possible scenarios
  sub_str_vect <- c(str_lookback,str_lookback_cum,unique(unlist(lapply(names(testdata)[-c(1,2)],function(x){return(strsplit(x,'_')[[1]][2])}))),'all')
  #Stacking for different scenarios
  for(sub_str in sub_str_vect){
    if(sub_str=='all'){which_vect <- 3:ncol(testdata)}else if(grepl('-',sub_str)){
      which_vect <- unlist(lapply(strsplit(sub_str,'-')[[1]],function(sub_str0){which(grepl(sub_str0,names(testdata)))}))
    }else{
      which_vect <- which(grepl(sub_str,names(testdata)))
    }
    form_cls_2 <- as.formula(paste0("sepsis_diagnose ~",paste(colnames(traindata)[which_vect],collapse = "+")))
    model <- train(form_cls_2,data = traindata[,c(1,2,which_vect)],method = "glmnet",trControl = ctrl,metric = "Accuracy")
    testpredprob <- predict(model, newdata = testdata[,c(1,2,which_vect)],type = "prob")
    testroc <- roc(response = testdata$sepsis_diagnose,predictor = testpredprob[,2])
    out_roc <- testroc$auc
    conf_matrix <- confusionMatrix(data=as.factor(ifelse(testpredprob[,2]>=testroc$thresholds[which.max(testroc$sensitivities+testroc$specificities-1)],'X1','X0')),reference = testdata$sepsis_diagnose,positive="X1",mode ="everything")
    out_acc <- c(conf_matrix$byClass[c('Sensitivity','Specificity','F1','Recall')],conf_matrix$overall['Accuracy'])
    out <- rbind(out,c(out_acc,out_roc))
  }

  rownames(out) <- c(names(testdata)[-c(1,2)],paste(sub_str_vect,'stacking',sep='_'))
  colnames(out) <- c('Sensitivity','Specificity','F1','Recall','Accuracy','AUC')

  return(out)
}

#' @title Cross-validation Prediction Results Based on Model
#' @param data_feature A list containing feature data from different time windows, which can be returned by feature extraction functions such as data_feature_func_KPCA() or data_feature_func_LSTM().
#' @param lookback_time A vector representing the length of the cumulative time window.
#' @return A data frame containing the average evaluation results from cross-validation.
#' @export
#'
#' @example
#' data(eg_sep_data)
#' data(eg_nonsep_data)
#' pre_time <- 3
#' lookback_time <- c(2,4,6,8,10)
#' data_raw <- data_func(data,data0,pre_time,lookback_time)
#' data_feature <- data_feature_func_KPCA(data_raw,lookback_time)
#' data_stacking <- stacking_func(data_feature,lookback_time)
#'
stacking_func <- function(data_feature,lookback_time){
  library(caret)
  library(tidyverse)
  library(caTools)
  library(pROC)
  library(kernlab)
  library(rpart)
  library(caretEnsemble)
  # Divide the data into 10 folds to ensure a positive to negative ratio of 1:1
  folds <- createFolds(data_feature[[1]]$sepsis_diagnose, k = 10, list = TRUE, returnTrain = FALSE)#Returns the index of the test data
  out <- 0
  #Calculating and averaging test predictions for each fold
  for(fk in 1:10){
    out <- out + stacking_func0(data_feature,lookback_time,folds[[fk]])
  }
  out <- out/10
  return(out)
}
