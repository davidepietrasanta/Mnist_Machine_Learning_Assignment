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
                 "doParallel",
                 "NeuralNetTools",
                 "factoextra",
                 "plot.matrix",
                 "fitdistrplus",
                 "multiROC",
                 "plotly"
))

# Set working directory
current_path <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(current_path)
set.seed(2)

# Data Analysis and PCA
source("R script/data_analysis.R")
data_analysis()

# Dataset
train <- read.csv("CSV/mnist_train_pca.csv")
test <-  read.csv("CSV/mnist_test_pca.csv")

# Naive Bayes
source("R script/Bayes.R")
Naive_Bayes <- naive_bayes(train, test) 
Naive_Bayes$Time.train
Naive_Bayes$Time.test
Naive_Bayes$Time.train.Prediction


# Neural Network using caret
source("R script/NN.R")
Neural_Network <- neural_network(train, test)
Neural_Network$Time.train
Neural_Network$Time.test
Neural_Network$Time.train.Prediction


# Model Evaluation
source("R script/Model Evaluation.R")
train$label = factor(train$label)

##NAIVE BAYES##
mod_eval_nb <- model_evaluation(Naive_Bayes$Prediction.train, train$label, "Naive Bayes")
mod_eval_nb$Precision # Precision
mod_eval_nb$Recall #Recall
mod_eval_nb$F1 #F1
confusionMatrix.plot.nb <- mod_eval_nb$confusionMatrix.plot
confusionMatrix.plot.nb #Plot Confusion Matrix

multiROC.nb <- multi_roc_function(Naive_Bayes$Prediction.train, train, "Naive Bayes")
multiROC.nb$AUC #AUC
multiROC.nb$avg.AUC #Average AUC
multiROC.nb.plot <- multiROC.nb$roc.plot
multiROC.nb.plot #Plot of ROC


##NEURAL NETWORK##
mod_eval_nn <- model_evaluation(Neural_Network$Prediction.train, train$label, "Neural Network")
mod_eval_nn$Precision # Precision
mod_eval_nn$Recall #Recall
mod_eval_nn$F1 #F1
confusionMatrix.plot.nn <- mod_eval_nn$confusionMatrix.plot
confusionMatrix.plot.nn #Plot Confusion Matrix

multiROC.nn <- multi_roc_function(Neural_Network$Prediction.train, train, "Neural Network")
multiROC.nn$AUC #AUC
multiROC.nn$avg.AUC #Average AUC
multiROC.nn.plot <- multiROC.nn$roc.plot 
multiROC.nn.plot #Plot of ROC

save.image(file = "my_work_space_Start.RData")
#load(file = "save.image(file = "my_work_space_Start.RData")
