# Neural net model 

# Read CSVs
train <- read.csv("CSV/mnist_train_pca.csv")
test <- read.csv("CSV/mnist_test_pca.csv")

train_label <- train[, 1]
train_label <- as.factor(train_label)
summary(train_label)

library(caret)
library(doParallel)
library(nnet)

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
model <- train(train,
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
cv_model_accuracy <- model$resample['Accuracy']$Accuracy #For the 10-fold cross validation
model_accuracy <- mean(cv_model_accuracy)
model_accuracy # ~75% not that bad using 20PCs and a single layer NN

# Predict using model
pred <- predict(model, test)
predMatrix <- table(pred, test$label)
predMatrix

confMatrix <- confusionMatrix(predMatrix)$overall['Accuracy']

# Save workspace data
# save.image(file = "my_work_space_Neural_Network.RData")