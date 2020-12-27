# Neural net model 

# Read CSVs
train <- read.csv("~/Documents/MachineLearning_R/progettoML/Mnist_Machine_Learning_Assignment/CSV/mnist_train_pca.csv")
test <- read.csv("~/Documents/MachineLearning_R/progettoML/Mnist_Machine_Learning_Assignment/CSV/mnist_test_pca.csv")

# train$label <- as.factor(train$label)

# train$n0 = train$label == 0
# train$n1 = train$label == 1
# train$n2 = train$label == 2
# train$n3 = train$label == 3
# train$n4 = train$label == 4
# train$n5 = train$label == 5
# train$n6 = train$label == 6
# train$n7 = train$label == 7
# train$n8 = train$label == 8
# train$n9 = train$label == 9
# 
# # Picking up every names of the co
# names <- names(train[2 : 21])
# nums <- c("n0", "n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9")
# formula <- as.formula(paste(paste(nums, collapse = "+"), " ~ ", paste(names, collapse = "+")))

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