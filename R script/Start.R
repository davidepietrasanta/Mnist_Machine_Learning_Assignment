# Starting Program

#Library

# Set working directory
current_path <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(current_path)

# Data Analysis and PCA
source("R script/data_analysis.R")
data_analysis()

# Read dataset
train <- read.csv("CSV/mnist_train_pca.csv")
test <- read.csv("CSV/mnist_test_pca.csv")

# Naive Bayes
source("R script/Bayes.R")
nb_pred <- naive_bayes(train, test)

# Neural Network using neuralnet // TODO
# source("R scritp/NN.R")
# nn <- neural_network1(train, test)

# Neural Network using caret
source("R script/NN2.R")
nn <- neural_network2(train, test)

# Model Evaluation
source("R script/Model Evaluation.R")
test$label = factor(test$label)
mod_eva <- model_evaluation(nb_pred, test$label)
mod_eva
