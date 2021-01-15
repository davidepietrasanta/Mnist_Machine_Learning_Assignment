# Neural net model using caret

neural_network <- function(train, test)
{
  train_label <- as.factor(train$label)
  summary(train_label)

  library(caret)
  library(doParallel)
  
  cores <- detectCores()
  registerDoParallel(cores = cores)
  cluster <- makeCluster(cores)
  
  # Set up 10-cross validation
  cv <- trainControl(method = "cv",
                   number = 10
                   )
  
  set.seed(123)
  # Train classification model using mlpML method
  nn <- train(train,
              train_label,
              method = "mlpML",
              # Best layout with the lowest RMSE
              tuneGrid = expand.grid(layer1 = 300,
                                     layer2 = 20,
                                     layer3 = 0                                     ),
              type = "Classification",
              trControl = cv,
              verbose = FALSE,
              )
  
  library(NeuralNetTools)
  par(mar = c(3.5, 3.5, 3.5, 3.5))
  plotnet(nn$finalModel)
  
  # Stop using parallel computing
  stopCluster(cluster)
  
  # Measure the accuracy of the model
  cv_nn_accuracy <- nn$resample['Accuracy']$Accuracy #For the 10-fold cross validation
  nn_accuracy <- mean(cv_nn_accuracy)
  print(nn_accuracy) # ~61% not that bad using 20PCs
  
  # Predict using model
  nn_pred <- predict(nn, test)
  predMatrix <- table(nn_pred, test$label)
  print(predMatrix)
  print(confusionMatrix(predMatrix)$overall['Accuracy'])
  
  # Save workspace data
  save.image(file = "my_work_space_Neural_Network_Caret.RData")
  
  return(nn_pred)
}