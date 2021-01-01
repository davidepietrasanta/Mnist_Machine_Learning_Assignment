#Models Evaluation

#load("my_work_space_Naive_Bayes.RData")

#install.packages("caret")
library(caret)
#install.packages("multiROC")
library(multiROC)


# Precision, recall, f-measure, ROC e AUC
model_evaluation <- function(predictions, labels){
  
  # Confusion Matrix
  pred_Matrix <- table(predictions, labels)
  confusion_Matrix <- confusionMatrix(pred_Matrix)
  
  # Precision, recall, f-measure
  precision <- mean(confusion_Matrix$byClass[,5]) #["Precision"]
  recall <- mean(confusion_Matrix$byClass[,6]) #["Recall"]
  f_measure <- mean(confusion_Matrix$byClass[,7]) #["F1"]
  
  # ROC e AUC
  # TO DO, need both classifier
  
  
  val <- c(precision, recall, f_measure)
  return(val)
}

#For Naive Bayes
mod_eva <- model_evaluation(pred, test$label)
mod_eva


