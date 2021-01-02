# Neural net model using caret

neural_network2 <- function(train, test)
{
  train_label <- as.factor(train$label)
  summary(train_label)
  
  # install.packages(caret)
  library(caret)
  # install.packages("doParallel")
  library(doParallel)
  
  cores <- detectCores()
  registerDoParallel(cores = cores)
  cluster <- makeCluster(cores)
  
  # Set up 10-cross validation
  cv <- trainControl(method = "cv",
                   number = 10
                   )
  
  set.seed(123)
  
  # Train classification model using nnet method
  # Single hidden layer with 11 nodes (can be tuned)
  nn <- train(train,
               train_label,
               method = "nnet",
               linear.output = FALSE,
               tuneGrid = expand.grid(size = 11,
                                      decay = 0),
               trControl = cv,
               verbose = FALSE,
              )
  
  # Stop using parallel computing
  stopCluster(cluster)
  
  # Measure the accuracy of the model
  cv_nn_accuracy <- nn$resample['Accuracy']$Accuracy #For the 10-fold cross validation
  nn_accuracy <- mean(cv_nn_accuracy)
  nn_accuracy # ~75% not that bad using 20PCs and a single layer NN
  
  # Predict using model
  pred <- predict(nn, test)
  predMatrix <- table(pred, test$label)
  predMatrix
  
  confMatrix <- confusionMatrix(predMatrix)$overall['Accuracy']
  
  # Save workspace data
  save.image(file = "my_work_space_Neural_Network_Caret.RData")
  
  return(c(nn, pred, test$label))
}