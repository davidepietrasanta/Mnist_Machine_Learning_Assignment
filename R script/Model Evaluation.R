#Models Evaluation

#load("my_work_space_Naive_Bayes.RData")

# Precision, recall, f-measure, ROC e AUC
model_evaluation <- function(predictions, labels, model){
  library(caret)
  
  # Confusion Matrix
  pred_Matrix <- table(predictions, labels)
  confusion_Matrix <- confusionMatrix(pred_Matrix)
  confusion_Matrix
  
  # Plot Confusion Matrix
  data_conf_Matrix <- data.frame(confusion_Matrix$table)
  colnames(data_conf_Matrix)[1] <- "Prediction"
  colnames(data_conf_Matrix)[2] <- "Target"
  plot <- plot_confusion_matrix(data_conf_Matrix,
                        prediction_col = "Prediction",
                        target_col = "Target",
                        counts_col = "Freq",
                        add_normalized = FALSE
                        )
  
  # Precision, recall, f-measure
  precision <- mean(confusion_Matrix$byClass[,5]) #["Precision"]
  recall <- mean(confusion_Matrix$byClass[,6]) #["Recall"]
  f_measure <- mean(confusion_Matrix$byClass[,7]) #["F1"]
  
  val <- c(precision, recall, f_measure)
  return(list(val, plot))
}


# AUC and ROC Plots
multi_roc_function <- function(pred, test, model_name){
  library(pROC)
  
  pred.roc <- as.numeric(as.character(pred))
  roc.multi_test <- multiclass.roc(test$label, pred.roc, direction = "<")
  rs_test <- roc.multi_test[['rocs']]
  roc.list <- list("0"=rs_test[[1]],"1"=rs_test[[2]],
                   "2"=rs_test[[3]],"3"=rs_test[[4]],
                   "4"=rs_test[[5]],"5"=rs_test[[6]],
                   "6"=rs_test[[7]],"7"=rs_test[[8]],
                   "8"=rs_test[[9]],"9"=rs_test[[10]])

  plot <- ggroc(roc.list, legacy.axes = TRUE) + geom_abline() + ggtitle(paste("ROC curve", model_name)) + geom_line(size = 1)
  
  auc <- as.numeric(roc.multi_test$auc)
  auc
  
  return( list(auc, plot) )
}



