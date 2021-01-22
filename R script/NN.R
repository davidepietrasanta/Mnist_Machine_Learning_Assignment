# Neural net model using caret

neural_network <- function(train, test)
{
  train_label <- factor(train$label)
  test$label <- factor(test$label)

  library(caret)
  library(doParallel)
  
  ###TRAIN###
  
  cores <- detectCores()
  registerDoParallel(cores = cores)
  cluster <- makeCluster(cores)
  
  # Define training control wiht 10-f cv
  train_control_nn <- trainControl(method = "cv",
                   number = 10
                   )
  
  # Set seed for repeatability
  set.seed(123)
  
  # Train classification model using mlpML method
  model_nn <- train(x = train[, 2 : ncol(train)],
              y= train_label,
              method = "mlpML",
              # Best layout with the lowest RMSE
              tuneGrid = expand.grid(layer1 = 300,
                                     layer2 = 20,
                                     layer3 = 0),
              type = "Classification",
              trControl = train_control_nn,
              verbose = FALSE
              )
  
  # Plot NN topology
  library(NeuralNetTools)
  par(mar = c(3.5, 3.5, 3.5, 3.5))
  plotnet(model_nn$finalModel)
  
  # Stop using parallel computing
  stopCluster(cluster)
  
  # Measure the accuracy of the model
  accuracy.cv.nn <- model_nn$resample['Accuracy']$Accuracy 
  accuracy.cv.nn.avg <- mean(accuracy.cv.nn) # Average accuracy
  print(paste0("Accuracy for Neural Network in 10-fold cv: ", accuracy.cv.nn.avg))
  
  ###TEST###
  
  # Predict using model
  start_time_nn_test <- Sys.time() 
  pred_nn <- predict(model_nn, test)
  end_time_nn_test <- Sys.time()
  time_nn_test <- end_time_nn_test - start_time_nn_test
  
  # Accuracy for test set
  confusionMatrix.test.nn <- confusionMatrix(table(pred_nn, test$label))
  accuracy.test.nn <- confusionMatrix.test.nn$overall['Accuracy']
  print(paste0("Accuracy for Neural Network in test set: ", accuracy.test.nn))
  
  start_time_nn_train <- Sys.time()
  predict.nn.train <- predict(model_nn)
  end_time_nn_train <- Sys.time()
  time_nn_train <- end_time_nn_train - start_time_nn_train
  
  # Save workspace data
  save.image(file = "my_work_space_Neural.RData")
  
  return_list <- list(model_nn,
                      predict.nn.train, accuracy.cv.nn.avg, model_nn[["times"]][["everything"]][["user.self"]],
                      pred_nn, accuracy.test.nn, as.numeric(time_nn_test, units="secs"), as.numeric(time_nn_train, units="secs"))
  names(return_list) <- c("model", 
                          "Prediction.train", "Accuracy.train", "Time.train", 
                          "Prediction.test", "Accuracy.test", "Time.test", "Time.train.Prediction")
  
  return(return_list)
}