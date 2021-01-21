# NAIVE BAYES

naive_bayes <- function(train, test){
  
  # We have to factor the labels
  train$label = factor(train$label)
  test$label = factor(test$label)
  
  library(caret)
  
  ###TRAIN###
  
  # Define training control
  train_control_nb <- trainControl(method="cv", number=10)
  # Train the model with a 10-fold cv #options(warn=1) 
  model_nb <- train(label~., data=train, trControl=train_control_nb, method="nb")
  
  #Accuracy
  accuracy.cv.nb <- model_nb$resample['Accuracy']$Accuracy 
  accuracy.cv.nb.avg <- mean(accuracy.cv.nb) #Average accuracy
  print(paste0("Accuracy for Naive Bayes in 10-fold cv: ",accuracy.cv.nb.avg))
  
  ###TEST###
  
  #Prediction for test set and time
  start_time_nb_test <- Sys.time() 
  pred_nb <- predict(model_nb, test)
  end_time_nb_test <- Sys.time()
  time_nb_test <- end_time_nb_test - start_time_nb_test
  
  ##Accuracy for test set
  confusionMatrix.test.nb <- confusionMatrix(table(pred_nb, test$label))
  accuracy.test.nb <- confusionMatrix.test.nb$overall['Accuracy']
  print(paste0("Accuracy for Naive Bayes in test set: ",accuracy.test.nb))
  
  start_time_nb_train <- Sys.time()
  predict.nb.train <- predict(model_nb)
  end_time_nb_train <- Sys.time()
  time_nb_train <- end_time_nb_train - start_time_nb_train
  
  
  #Save
  save.image(file = "my_work_space_Naive_Bayes.RData")
  #load("my_work_space_Naive_Bayes.RData") # To restore the data at any time
  
  return_list <- list(model_nb,
                      predict.nb.train, accuracy.cv.nb.avg, model_nb[["times"]][["everything"]][["user.self"]],
                      pred_nb, accuracy.test.nb, as.numeric(time_nb_test), as.numeric(time_nb_train))
  names(return_list) <- c("model", 
                          "Prediction.train", "Accuracy.train", "Time.train", 
                          "Prediction.test", "Accuracy.test", "Time.test", "Time.train.Prediction")
  
  return(return_list)
}
