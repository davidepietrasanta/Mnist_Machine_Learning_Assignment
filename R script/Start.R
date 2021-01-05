# Starting Program

# Check for packages and install them
check_packages <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

check_packages(c("ggplot2",
                 "caret",
                 "cvms",
                 "pROC",
                 "e1071",
                 #"neuralnet",
                 "doParallel",
                 "NeuralNetTools",
                 "factoextra",
                 "plot.matrix"
))

# Set working directory
current_path <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(current_path)

# Data Analysis and PCA
source("R script/data_analysis.R")
data_analysis()

# Dataset
train <- read.csv("CSV/mnist_train_pca.csv")
test <-  read.csv("CSV/mnist_test_pca.csv")

# Naive Bayes
source("R script/Bayes.R")
start_time_nb <- Sys.time() 
nb_pred <- naive_bayes(train, test) 
end_time_nb <- Sys.time() 
time_nb <-  end_time_nb - start_time_nb #Time Naive Bayes
time_nb

# Neural Network using neuralnet // TODO
# source("R scritp/NN.R")
# nn1_pred <- neural_network1(train, test)

# Neural Network using caret
source("R script/NN2.R")
start_time_nn2 <- Sys.time()
nn2_pred <- neural_network2(train, test)
end_time_nn2 <- Sys.time()
time_nn2 <-  end_time_nn2 - start_time_nn2 #Time NN
time_nn2

# Model Evaluation
source("R script/Model Evaluation.R")
test$label = factor(test$label)

mod_eval_nb <- model_evaluation(nb_pred, test$label, "Naive Bayes")
val_nb <- mod_eval_nb[1] # Precision, Recall, F1
val_nb

plot_confmatrix_nb <- mod_eval_nb[2] #plot
plot_confmatrix_nb

multi_roc_nb <- multi_roc_function(nb_pred, test, "Naive Bayes")
auc_nb <- multi_roc_nb[1]
auc_nb 

plot_roc_nb <- multi_roc_nb[2] #plot
plot_roc_nb

# ROC AUC evaluation NN using caret
mod_eval_nn2 <- model_evaluation(nn2_pred, test$label, "NN using Caret")
val_nn2 <- mod_eval_nn2[1] # Precision, Recall, F1
val_nn2

plot_confmatrix_nn2 <- mod_eval_nn2[2] #plot
plot_confmatrix_nn2

multi_roc_nn2 <- multi_roc_function(nn2_pred, test, "NN using Caret")
auc_nn2 <- multi_roc_nn2[1]
auc_nn2 

plot_roc_nn2 <- multi_roc_nn2[2] #plot
plot_roc_nn2
