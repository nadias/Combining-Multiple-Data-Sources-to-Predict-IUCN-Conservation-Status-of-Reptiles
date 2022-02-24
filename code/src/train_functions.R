
define_control <- function (type) {
  
  if (type == "none") {
    control <- trainControl(method="none",
                            search='grid',
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            savePredictions = TRUE)
  } else if (type == "LOOCV") {
    control <- trainControl(method="LOOCV",
                            search='grid',
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            savePredictions = TRUE)
  } else if (type == "2x5cv") {
    control <- trainControl(method="repeatedcv", 
                            number=5,
                            repeats=2,
                            search='grid',
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            savePredictions = TRUE)
  } else {
    control <- trainControl(method="repeatedcv", 
                            number=10,
                            repeats=10,
                            search='grid',
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            savePredictions = TRUE)
  }
  
  return(control)
}

get_results <- function (data, y_pred, y_obs, threshold, verbose=FALSE) {
  predictions_probs <- factor( ifelse(y_pred[, "threatened"] >= threshold, "threatened", "non_threatened") )  # for old models, used > only
  levels(predictions_probs) <- c(levels(predictions_probs), "threatened", "non_threatened")      # Force to have both levels
  predictions_probs <- relevel(predictions_probs, "threatened")

  if(verbose == TRUE) {
    print(confusionMatrix(predictions_probs, y_obs, positive="threatened"))
  }
  
  sens <- caret::sensitivity(predictions_probs, y_obs, relevant = "threatened")
  spec <- caret::specificity(predictions_probs, y_obs, relevant = "threatened")
  prec <- caret::precision(predictions_probs, y_obs, relevant = "threatened")
  
  f1 <- F1_Score(predictions_probs, y_obs, positive="threatened")
  fb05 <- FBeta_Score(predictions_probs, y_obs, positive="threatened", beta = 0.5)
  fb2 <- FBeta_Score(predictions_probs, y_obs, positive="threatened", beta = 2)
  
  tss <- sens + spec - 1
  
  train_no_species_bool <- (y_obs == "threatened")*1
  predictions_probs_bool <- (predictions_probs == "threatened")*1
  auc<- AUC(predictions_probs_bool, train_no_species_bool)
  
  acc <- Accuracy(predictions_probs, y_obs)
  bacc <- bal_accuracy(data, y_obs, predictions_probs)$.estimate
  
  result <- data.frame(sens, spec, prec, f1, fb05, fb2, tss, auc, acc, bacc)
  colnames(result) <- c("Sens", "Spec", "Precision", "F1", "Fb05", "Fb2", "TSS", "AUC", "Accuracy", "BalancedAcc")
  
  return(result)
}

generate_predictions <- function (model, testset_df, threshold){
  predictions_test_probs <- predict(model, testset_df, type = "prob")
  predictions_test <- factor( ifelse(predictions_test_probs[, "threatened"] >= threshold, "threatened", "non_threatened") ) # for old models, used > only
  predictions_test <- relevel(predictions_test, "threatened")
  
  table(predictions_test)
}

print_results <- function (train_results) {
  boxplot(train_results$Sens, train_results$Spec, train_results$Precision, train_results$F1,
          train_results$Fb05, train_results$Fb2, train_results$TSS, train_results$AUC, train_results$Accuracy,
          train_results$BalancedAcc,
          names=c("Sens","Spec","Prec", "F1", "Fb05", "Fb2", "TSS", "AUC", "Acc", "BAcc"))
  
  sens_mean <- mean(train_results$Sens, na.rm=TRUE)
  spec_mean <- mean(train_results$Spec, na.rm=TRUE)
  prec_mean <- mean(train_results$Precision, na.rm=TRUE)
  f1_mean <- mean(train_results$F1, na.rm=TRUE)
  fb05_mean <- mean(train_results$Fb05, na.rm=TRUE)
  fb2_mean <- mean(train_results$Fb2, na.rm=TRUE)
  tss_mean <- mean(train_results$TSS, na.rm=TRUE)
  auc_mean <- mean(train_results$AUC, na.rm=TRUE)
  acc_mean <- mean(train_results$Accuracy, na.rm=TRUE)
  bacc_mean <- mean(train_results$BalancedAcc, na.rm=TRUE)
  
  print(paste0("mean sens: ", sens_mean))
  print(paste0("mean spec: ", spec_mean))
  print(paste0("mean prec: ", prec_mean))
  print(paste0("mean f1: ", f1_mean))
  print(paste0("mean fb05: ", fb05_mean))
  print(paste0("mean fb2: ", fb2_mean))
  print(paste0("mean tss: ", tss_mean))
  print(paste0("mean auc: ", auc_mean))
  print(paste0("mean acc:", acc_mean))
  print(paste0("mean bacc: ", bacc_mean))
  
  print("")
  
  print(paste0("sd sens: ", sd(train_results$Sens, na.rm=TRUE)))
  print(paste0("sd spec: ", sd(train_results$Spec, na.rm=TRUE)))
  print(paste0("sd prec: ", sd(train_results$Precision, na.rm=TRUE)))
  print(paste0("sd f1: ", sd(train_results$F1, na.rm=TRUE)))
  print(paste0("sd fb05: ", sd(train_results$Fb05, na.rm=TRUE)))
  print(paste0("sd fb2: ", sd(train_results$Fb2, na.rm=TRUE)))
  print(paste0("sd tss: ", sd(train_results$TSS, na.rm=TRUE)))
  print(paste0("sd auc: ", sd(train_results$AUC, na.rm=TRUE)))
  print(paste0("sd acc:", sd(train_results$Accuracy, na.rm=TRUE)))
  print(paste0("sd bacc: ", sd(train_results$BalancedAcc, na.rm=TRUE)))
  
  result <- data.frame(sens_mean, spec_mean, prec_mean, f1_mean, fb05_mean, fb2_mean, tss_mean, auc_mean, acc_mean, bacc_mean)
  colnames(result) <- c("Sens", "Spec", "Precision", "F1", "Fb05", "Fb2", "TSS", "AUC", "Accuracy", "BalancedAcc")
  
  return(result)
  
}

# Random forest

train_rf <- function (train_data, control, ntree, mtry, nodesize=1) {
  model <- train(threatened ~ .,
                         data = train_data,
                         method = 'rf',
                         #preProc=c("center", "scale"),
                         metric = 'ROC',     # "ROC" , "Spec" , "Accuracy"
                         tuneGrid = expand.grid(.mtry = mtry),
                         trControl = control,
                         ntree=ntree,
                         nodesize=nodesize)
  return(model)
}

tune_ntree <- function (train_data, control, value_list=c(1,2,3,4,5,10,20,50)) { #c(1,2,3,4,5,10,20,50,75,100,200,500,1000
  modellist <- list()
  train_results <- data.frame(ntree=integer(), ROC=double(), Sens=double(), Spec=double())
  
  for (ntree in value_list){
    set.seed(123)
    fit <- train_rf(train_data, control, ntree, mtry=c(sqrt(ncol(train_data))))
    
    key <- toString(ntree)
    modellist[[key]] <- fit
    result <- data.frame(c(ntree), c(fit$results$ROC), c(fit$results$Sens), c(fit$results$Spec))
    colnames(result) <- c("ntree", "ROC", "Sens", "Spec")
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}

get_training_results_old <- function (train_data, control, ntree, mtry, threshold, nodesize=1) {
  
  train_results <- data.frame(Sens=double(), Spec=double(), Precision=double(), F1=double(),
                              Fb05=double(), Fb2=double(), TSS=double(), AUC=double(), Accuracy=double(),
                              BalancedAcc=double())
  set.seed(123)
  for (i in c(1:10)){
    fit <- train_rf(train_data, control, ntree, mtry, nodesize)
    
    probs_amphisbaenians <- predict(fit, train_data, type = "prob")
    
    result <- get_results(train_data, probs_amphisbaenians, train_data$threatened, threshold)
    
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}

tune_rf <- function (train_data, control, ntree_list=c(5,10,20,50, 100, 200, 500), mtry_list=c(2:7), nodesize=c(1,2,3,4,5)) { #c(1,2,3,4,5,10,20,50,75,100,200,500,1000
  modellist <- list()
  train_results <- data.frame(ntree=integer(), mtry=integer(), node=integer(), ROC=double(), Sens=double(), Spec=double())
  
  for (ntree in ntree_list){
    for (mtry in mtry_list){
      for (node in nodesize){
        set.seed(123)
        fit <- train_rf(train_data, control, ntree, mtry, node)
        
        result <- data.frame(ntree, mtry, node, fit$results$ROC, fit$results$Sens, fit$results$Spec)
        colnames(result) <- c("ntree", "mtry", "node", "ROC", "Sens", "Spec")
        train_results <- bind_rows(train_results, result)
      }
    }
  }
  return(train_results)
}

get_training_results <- function (train_data, control, ntree, mtry, threshold, nodesize=1) {
  
  train_results <- data.frame(Sens=double(), Spec=double(), Precision=double(), F1=double(),
                              Fb05=double(), Fb2=double(), TSS=double(), AUC=double(), Accuracy=double(),
                              BalancedAcc=double())
  set.seed(123)
  fit <- train_rf(train_data, control, ntree, mtry, nodesize)
  
  samples <- fit$pred %>% distinct(Resample)
  
  for (i in samples$Resample){
    data <- fit$pred %>% filter(fit$pred$Resample == i)
    predictions <- data %>% dplyr::select(c("threatened", "non_threatened"))
    original_data <- train_data %>% slice(data$rowIndex)
    label <- original_data$threatened
    result <- get_results(original_data, predictions, label, threshold)
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}

# XGBoost

train_xgboost <- function (train_data, control, tunegrid) {
  model <- train(threatened ~ .,
                 data = train_data,
                 method = 'xgbTree',
                 metric = 'ROC',
                 tuneGrid = tunegrid,
                 trControl = control)

  return(model)
}

get_training_results_xgboost_old <- function (train_data, control, tunegrid, threshold) {
  
  train_results <- data.frame(Sens=double(), Spec=double(), Precision=double(), F1=double(), AUC=double(), Accuracy=double(), BalancedAcc=double())
  set.seed(123)
  for (i in c(1:10)){
    fit <- train_xgboost(train_data, control, tunegrid)
    
    probs <- predict(fit, train_data, type = "prob")
    
    result <- get_results(train_data, probs, train_data$threatened, threshold)
    
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}

get_training_results_xgboost <- function (train_data, control, tunegrid, threshold) {
  
  train_results <- data.frame(Sens=double(), Spec=double(), Precision=double(), F1=double(),
                              Fb05=double(), Fb2=double(), TSS=double(), AUC=double(), Accuracy=double(),
                              BalancedAcc=double())
  set.seed(123)
  fit <- train_xgboost(train_data, control, tunegrid)
  
  samples <- fit$pred %>% distinct(Resample)
  
  for (i in samples$Resample){
    data <- fit$pred %>% filter(fit$pred$Resample == i)
    predictions <- data %>% dplyr::select(c("threatened", "non_threatened"))
    original_data <- train_data %>% slice(data$rowIndex)
    label <- original_data$threatened
    result <- get_results(original_data, predictions, label, threshold)
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}

# GLM

train_glm <- function (train_data, control) {
  model <- train(threatened ~ .,
                 data = train_data,
                 method = "glm",
                 family = "binomial",
                 metric = 'ROC',
                 trControl = control)
  
  return(model)
}

get_training_results_glm_old <- function (train_data, control, threshold) {
  
  train_results <- data.frame(Sens=double(), Spec=double(), Precision=double(), F1=double(),
                              Fb05=double(), Fb2=double(), TSS=double(), AUC=double(), Accuracy=double(),
                              BalancedAcc=double())
  set.seed(123)
  for (i in c(1:10)){
    fit <- train_glm(train_data, control)
    
    probs <- predict(fit, train_data, type = "prob")
    
    result <- get_results(train_data, probs, train_data$threatened, threshold)
    
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}

get_training_results_glm <- function (train_data, control, threshold) {
  
  train_results <- data.frame(Sens=double(), Spec=double(), Precision=double(), F1=double(),
                              Fb05=double(), Fb2=double(), TSS=double(), AUC=double(), Accuracy=double(),
                              BalancedAcc=double())
  set.seed(123)
  fit <- train_glm(train_data, control)
  samples <- fit$pred %>% distinct(Resample)
  
  for (i in samples$Resample){
    data <- fit$pred %>% filter(fit$pred$Resample == i)
    predictions <- data %>% dplyr::select(c("threatened", "non_threatened"))
    original_data <- train_data %>% slice(data$rowIndex)
    label <- original_data$threatened
    result <- get_results(original_data, predictions, label, threshold)
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}

# Rpart
train_rpart_old <- function (train_data, control, cp_) {
  model <- train(threatened ~ .,
                 data = train_data,
                 method = "rpart",
                 metric = 'ROC',
                 tuneGrid = expand.grid(cp = cp_),
                 trControl = control)
  
  return(model)
}

get_training_results_rpart_old <- function (train_data, control, threshold, cp_) {
  
  train_results <- data.frame(Sens=double(), Spec=double(), Precision=double(), F1=double(),
                              Fb05=double(), Fb2=double(), TSS=double(), AUC=double(), Accuracy=double(),
                              BalancedAcc=double())
  set.seed(123)
  
  for (i in c(1:10)){
    fit <- train_rpart(train_data, control, cp_)
    
    probs <- predict(fit, train_data, type = "prob")
    
    result <- get_results(train_data, probs, train_data$threatened, threshold)
    
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}

train_rpart <- function (train_data, control, max_depth_) {
  model <- train(threatened ~ .,
                 data = train_data,
                 method = "rpart2",
                 metric = 'ROC',
                 tuneGrid = expand.grid(maxdepth = max_depth_),
                 trControl = control)
  
  return(model)
}

get_training_results_rpart <- function (train_data, control, threshold, max_depth_) {
  
  train_results <- data.frame(Sens=double(), Spec=double(), Precision=double(), F1=double(),
                              Fb05=double(), Fb2=double(), TSS=double(), AUC=double(), Accuracy=double(),
                              BalancedAcc=double())
  set.seed(123)
  fit <- train_rpart(train_data, control, max_depth_)
  samples <- fit$pred %>% distinct(Resample)
    
  for (i in samples$Resample){
    data <- fit$pred %>% filter(fit$pred$Resample == i)
    predictions <- data %>% dplyr::select(c("threatened", "non_threatened"))
    original_data <- train_data %>% slice(data$rowIndex)
    label <- original_data$threatened
    result <- get_results(original_data, predictions, label, threshold)
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}

# C5.0
train_c50 <- function (train_data, control, max_depth_) {
  model <- train(threatened ~ .,
                 data = train_data,
                 method = "C5.0Tree",
                 metric = 'ROC',
                 trControl = control)
  
  return(model)
}

get_training_results_c50 <- function (train_data, control, threshold) {
  
  train_results <- data.frame(Sens=double(), Spec=double(), Precision=double(), F1=double(),
                              Fb05=double(), Fb2=double(), TSS=double(), AUC=double(), Accuracy=double(),
                              BalancedAcc=double())
  set.seed(123)
  fit <- train_c50(train_data, control)
  samples <- fit$pred %>% distinct(Resample)
  
  for (i in samples$Resample){
    data <- fit$pred %>% filter(fit$pred$Resample == i)
    predictions <- data %>% dplyr::select(c("threatened", "non_threatened"))
    original_data <- train_data %>% slice(data$rowIndex)
    label <- original_data$threatened
    result <- get_results(original_data, predictions, label, threshold)
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}

# knn
train_knn <- function (train_data, control, k_) {
  model <- train(threatened ~ .,
                 data = train_data,
                 method = "knn",
                 metric = 'ROC',
                 tuneGrid = expand.grid(k = k_),
                 trControl = control)
  
  return(model)
}

get_training_results_knn_old <- function (train_data, control, threshold, k_) {
  
  train_results <- data.frame(Sens=double(), Spec=double(), Precision=double(), F1=double(),
                              Fb05=double(), Fb2=double(), TSS=double(), AUC=double(), Accuracy=double(),
                              BalancedAcc=double())
  set.seed(123)
  for (i in c(1:10)){
    fit <- train_knn(train_data, control, k_)
    
    probs <- predict(fit, train_data, type = "prob")
    
    result <- get_results(train_data, probs, train_data$threatened, threshold)
    
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}

get_training_results_knn <- function (train_data, control, threshold, k_) {
  
  train_results <- data.frame(Sens=double(), Spec=double(), Precision=double(), F1=double(),
                              Fb05=double(), Fb2=double(), TSS=double(), AUC=double(), Accuracy=double(),
                              BalancedAcc=double())
  set.seed(123)
  fit <- train_knn(train_data, control, k_)
  samples <- fit$pred %>% distinct(Resample)
  
  for (i in samples$Resample){
    data <- fit$pred %>% filter(fit$pred$Resample == i)
    predictions <- data %>% dplyr::select(c("threatened", "non_threatened"))
    original_data <- train_data %>% slice(data$rowIndex)
    label <- original_data$threatened
    result <- get_results(original_data, predictions, label, threshold)
    train_results <- bind_rows(train_results, result)
  }
  return(train_results)
}
