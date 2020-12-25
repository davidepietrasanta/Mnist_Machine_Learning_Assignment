#Load the dataset after the PCA
#The first column is the "label"
train <- read.csv("CSV/mnist_train_pca.csv")
test <- read.csv("CSV/mnist_test_pca.csv")

train$label = factor(train$label)
test$label = factor(test$label)


library(e1071)
# Create a Naive Bayes classifier
model = naiveBayes(label ~ ., data  = train)
#print(model)
pred <- predict(model, test)
predMatrix <- table(pred, test$label)
predMatrix


library(caret) 
confusionMatrix(predMatrix)


