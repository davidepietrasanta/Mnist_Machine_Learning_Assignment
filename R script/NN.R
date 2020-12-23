# Neural net model 
library(neuralnet)

# Read CSVs
train <- read.csv("CSV/mnist_train_pca.csv")
test <- read.csv("CSV/mnist_test_pca.csv")

# ReLU custom activation function
# reLU <- function(x) if(x <= 0) 0 else x  # unable to use it
# Approsimation of ReLU function
# softplus <- function(x) log(1 + exp(x))

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

# Picking up every names of the co
names <- names(train[2:21])
nums <- c("n0", "n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9")
formula <- as.formula(paste(paste(nums, collapse = "+"), " ~ ", paste(names, collapse = "+")))

# Create a NN classifier
nn <- neuralnet(formula,
                train,
                hidden = c(10, 5, 3),
                linear.output = FALSE,
                exclude = NULL,
                #threshold = 0.1,
                stepmax=1e6,
                act.fct = "logistic"
                )
plot(nn)