# NAIVE BAYES

# Starting Time
start_time <- Sys.time()

# Set working directory
setwd("E:/Program/R/Progetto ML/Git/Mnist_Machine_Learning_Assignment-main")

# Load the dataset after the PCA
train <- read.csv("CSV/mnist_train_pca.csv")
test <- read.csv("CSV/mnist_test_pca.csv")

# We have to factor the labels
train$label = factor(train$label)
test$label = factor(test$label)

# Ending Time for datatset processing
end_time_dataset <- Sys.time()
# Starting Time for Naive Bayes
start_time_bayes <- end_time_dataset

library(e1071)
library(caret)

# Define training control
train_control <- trainControl(method="cv", number=10) #, repeats=1)
# Train the model
model <- train(label~., data=train, trControl=train_control, method="nb")#, tuneGrid=grid)
#print(model)
cv_model_accuracy <- model$resample['Accuracy']$Accuracy #For the 10-fold cross validation
model_accuracy <- mean(cv_model_accuracy)
model_accuracy


#Test on Validation test
pred <- predict(model, test)
predMatrix <- table(pred, test$label)
predMatrix
confusionMatrix(predMatrix)$overall['Accuracy']

#Ending Time
end_time <- Sys.time()

#Time
time_all <- end_time - start_time
time_all
time_dataset <- end_time_dataset - start_time
time_dataset
time_naive_bayes <- end_time - start_time_bayes
time_naive_bayes


#Esecuzione di una 10-fold cross validation e stima delle seguenti misure di performance:
#  . Matrice di confusione complessiva
#  . Precision, recall, f-measure, ROC e AUC

save.image(file = "my_work_space_Naive_Bayes.RData")
#load("my_work_space_Naive_Bayes.RData") to restore the data in any time