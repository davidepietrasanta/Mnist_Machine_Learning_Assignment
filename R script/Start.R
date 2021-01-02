# Starting Program

#Library

# Set working directory
current_path <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(current_path)

# Data Analysis and PCA
source("R script/data_analysis.R")
data_analysis()

# Dataset
train <- read.csv("CSV/mnist_train_pca.csv")
test <- read.csv("CSV/mnist_test_pca.csv")

# Naive Bayes
source("R script/Bayes.R")
start_time_nb <- Sys.time() 
nb_pred <- naive_bayes(train, test) 
end_time_nb <- Sys.time() 
time_nb <- end_time_nb - start_time_nb #Time Naive Bayes

# Neural Network using neuralnet // TODO
# source("R scritp/NN.R")
# nn <- neural_network1(train, test)

# Neural Network using caret
source("R script/NN2.R")
nn2 <- neural_network2(train, test)

# Model Evaluation
source("R script/Model Evaluation.R")
test$label = factor(test$label)
mod_eval_nb <- model_evaluation(nb_pred, test$label)
mod_eval_nb
multi_roc_nb <- multi_roc_function(nb_pred, test)
auc_np <- multi_roc_nb[1]
auc_np 
plot_roc_nb <- multi_roc_nb[2] #plot
plot_roc_nb

