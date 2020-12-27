# Neural net model 

# Read CSVs
train <- read.csv("CSV/mnist_train_pca.csv")
test <- read.csv("CSV/mnist_test_pca.csv")

library(neuralnet)
library(caret)
library(doParallel)

cores <- detectCores() - 4
registerDoParallel(cores = cores) 
cluster <- makeCluster(cores)

# Set up 10-cross validation
cv <- trainControl(method = "cv", 
                   number = 10
                   )

set.seed(123)
nn2 <- train(label ~ .,
             data = train,
             method = "neuralnet",
             tuneGrid = data.frame(layer1 = 10:12,
                                   layer2 = 0,
                                   layer3 = 0),
             trControl = cv,
             stepmax = 1e+09
             )

# stop using parallel computing
stopCluster(cluster)