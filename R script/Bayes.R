# NAIVE BAYES

naive_bayes <- function(train, test){
  
  # We have to factor the labels
  train$label = factor(train$label)
  test$label = factor(test$label)
  
  #install.packages("e1071)
  library(e1071)
  #install.packages("caret")
  library(caret)
  
  # Define training control
  train_control_nb <- trainControl(method="cv", number=10)
  # Train the model
  model_nb <- train(label~., data=train, trControl=train_control_nb, method="nb")
  cv_nb_accuracy <- model_nb$resample['Accuracy']$Accuracy #For the 10-fold cross validation
  model_accuracy_nb <- mean(cv_nb_accuracy)
  model_accuracy_nb
  
  
  #Test on Validation test
  pred_nb <- predict(model_nb, test)
  predMatrix_nb <- table(pred_nb, test$label)
  predMatrix_nb
  confusionMatrix(predMatrix_nb)$overall['Accuracy']
  
  #Save
  save.image(file = "my_work_space_Naive_Bayes.RData")
  #load("my_work_space_Naive_Bayes.RData") # To restore the data in any time
  
  
  return(pred_nb)
}
