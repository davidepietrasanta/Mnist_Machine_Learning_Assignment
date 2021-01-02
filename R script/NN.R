# Neural net model 

neural_network1 <- function(train, test)
{
  # Add 0-9 cols based on the digits on the dataset
  train$n0 = train$label == 0
  train$n1 = train$label == 1
  train$n2 = train$label == 2
  train$n3 = train$label == 3
  train$n4 = train$label == 4
  train$n5 = train$label == 5
  train$n6 = train$label == 6
  train$n7 = train$label == 7
  train$n8 = train$label == 8
  train$n9 = train$label == 9
  
  # Scale data
  scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
  train[, 2:21] <- data.frame(lapply(train[, 2 : 21], scl))
  head(train)
  
  # Setting the formula for neuralnet function
  names <- names(train[2 : 21])
  nums <- c("n0", "n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9")
  formula <- as.formula(paste(paste(nums, collapse = "+"), " ~ ", paste(names, collapse = "+")))
  
  start_time <- Sys.time()
  set.seed(123)
  
  library(neuralnet)
  
  # Create a NN classifier ?? takes too much time and it didn't converge
  nn <- neuralnet(formula,
                  train,
                  hidden = 12,
                  #algorithm = "backprop",
                  stepmax = 1e6,
                  act.fct = "logistic",
                  learningrate = 0.1,
                  linear.output = FALSE
                  )
  
  # Plot NN
  plot(nn)
  
  end_time <- Sys.time()
  exec_time <- end_time - start_time
  
  # Save workspace data
  save.image(file = "my_work_space_Neural_Network_Neuralnet.RData")
  
  # TODO: it doesn't work yet, asking why to the tutor
  # return(c(nn, pred, test$label))
}