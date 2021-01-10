#Models Evaluation

#load("my_work_space_Naive_Bayes.RData")

# Precision, recall, f-measure, ROC e AUC
model_evaluation <- function(predictions, labels, model_name)
{
  library(caret)
  
  # Confusion Matrix
  pred_Matrix <- table(predictions, labels)
  confusion_Matrix <- confusionMatrix(pred_Matrix)
  confusion_Matrix
  
  # Plot Confusion Matrix
  data_conf_Matrix <- data.frame(confusion_Matrix$table)
  colnames(data_conf_Matrix)[1] <- "Prediction"
  colnames(data_conf_Matrix)[2] <- "Target"
  
  library(cvms)
  plot <- plot_confusion_matrix(data_conf_Matrix,
                        prediction_col = "Prediction",
                        target_col = "Target",
                        counts_col = "Freq",
                        add_normalized = FALSE
                        ) + ggtitle(paste("Confusion Matrix", model_name))
  
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
  roc.list <- list("(0-1)"=rs_test[[1]],"(0-2)"=rs_test[[2]],
                   "(0-3)"=rs_test[[3]],"(0-4)"=rs_test[[4]],
                   "(0-5)"=rs_test[[5]],"(0-6)"=rs_test[[6]],
                   "(0-7)"=rs_test[[7]],"(0-8)"=rs_test[[8]],
                   "(0-9)"=rs_test[[9]],"(1-2)"=rs_test[[10]],
                   "(1-3)"=rs_test[[11]],"(1-4)"=rs_test[[12]],
                   "(1-5)"=rs_test[[13]],"(1-6)"=rs_test[[14]],
                   "(1-7)"=rs_test[[15]],"(1-8)"=rs_test[[16]],
                   "(1-9)"=rs_test[[17]],"(2-3)"=rs_test[[18]],
                   "(2-4)"=rs_test[[19]],"(2-5)"=rs_test[[20]],
                   "(2-6)"=rs_test[[21]],"(2-7)"=rs_test[[22]],
                   "(2-8)"=rs_test[[23]],"(2-9)"=rs_test[[24]],
                   "(3-4)"=rs_test[[25]],"(3-5)"=rs_test[[26]],
                   "(3-6)"=rs_test[[27]],"(3-7)"=rs_test[[28]],
                   "(3-8)"=rs_test[[29]],"(3-9)"=rs_test[[30]],
                   "(4-5)"=rs_test[[31]],"(4-6)"=rs_test[[32]],
                   "(4-7)"=rs_test[[33]],"(4-8)"=rs_test[[34]],
                   "(4-9)"=rs_test[[35]],"(5-6)"=rs_test[[36]],
                   "(5-7)"=rs_test[[37]],"(5-8)"=rs_test[[38]],
                   "(5-9)"=rs_test[[39]],"(6-7)"=rs_test[[40]],
                   "(6-8)"=rs_test[[41]],"(6-9)"=rs_test[[42]],
                   "(7-8)"=rs_test[[43]],"(7-9)"=rs_test[[44]],
                   "(8-9)"=rs_test[[45]])
  plot <- ggroc(roc.list, legacy.axes = TRUE) + geom_abline() + ggtitle(paste("ROC curve", model_name)) + geom_line(size = 1)
  auc <- as.numeric(roc.multi_test$auc)
  auc
  return( list(auc, plot) )
}



