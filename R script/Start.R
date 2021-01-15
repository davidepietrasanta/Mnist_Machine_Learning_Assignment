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
                 "doParallel",
                 "factoextra",
                 "plot.matrix",
                 "plotly",
                 "dplyr"
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

# Neural Network using caret
source("R script/NN.R")
start_time_nn <- Sys.time()
nn_pred <- neural_network(train, test)
end_time_nn <- Sys.time()
time_nn <-  end_time_nn - start_time_nn #Time NN
time_nn

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
mod_eval_nn <- model_evaluation(nn_pred, test$label, "NN using Caret")
val_nn <- mod_eval_nn[1] # Precision, Recall, F1
val_nn

plot_confmatrix_nn <- mod_eval_nn[2] #plot
plot_confmatrix_nn

multi_roc_nn <- multi_roc_function(nn_pred, test, "NN using Caret")
auc_nn <- multi_roc_nn[1]
auc_nn 

plot_roc_nn <- multi_roc_nn[2] #plot
plot_roc_nn
