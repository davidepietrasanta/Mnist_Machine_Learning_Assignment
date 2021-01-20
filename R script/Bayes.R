# NAIVE BAYES

naive_bayes <- function(train, test){
  
  # We have to factor the labels
  train$label = factor(train$label)
  test$label = factor(test$label)
  
  library(caret)
  
  # Define training control
  train_control_nb <- trainControl(method="cv", number=10)
  # Train the model
  #options(warn=1) 
  model_nb <- train(label~., data=train, trControl=train_control_nb, method="nb")
  cv_nb_accuracy <- model_nb$resample['Accuracy']$Accuracy #For the 10-fold cross validation
  model_accuracy_nb <- mean(cv_nb_accuracy)
  print(model_accuracy_nb)
  
  # Confusion Matrix for the train set
  pred_train_nb <- predict(model_nb, train)
  pred_Matrix.train <- table(pred_train_nb, train$label)
  confusion_Matrix.train <- confusionMatrix(pred_Matrix.train)
  confusion_Matrix.train
  
  # Plot Confusion Matrix for the train set
  data_conf_Matrix <- data.frame(confusion_Matrix.train$table)
  colnames(data_conf_Matrix)[1] <- "Prediction"
  colnames(data_conf_Matrix)[2] <- "Target"
  
  library(cvms)
  plot_confusion_matrix(data_conf_Matrix,
                                prediction_col = "Prediction",
                                target_col = "Target",
                                counts_col = "Freq",
                                add_normalized = FALSE
  ) + ggtitle(paste("Confusion Matrix Naive Bayes"))
  
  
  #Confusion Matrix for test set
  start_time_nb_test <- Sys.time() 
  pred_nb <- predict(model_nb, test)
  end_time_nb_test <- Sys.time()
  time_nb_test <- end_time_nb_test - start_time_nb_test
  predMatrix_nb <- table(pred_nb, test$label)
  print(predMatrix_nb)
  print(confusionMatrix(predMatrix_nb)$overall['Accuracy'])
  
  
  #Save
  save.image(file = "my_work_space_Naive_Bayes.RData")
  #load("my_work_space_Naive_Bayes.RData") # To restore the data in any time
  
  return(list(pred_nb, time_nb_test))
}
