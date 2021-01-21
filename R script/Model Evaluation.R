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
  
  
  return_list <- list(precision, recall, f_measure, confusion_Matrix, plot)
  names(return_list) <- c("Precision", "Recall", "F1", "confusionMatrix", "confusionMatrix.plot")
  
  return(return_list)
}

# AUC and ROC Plots
multi_roc_function <- function(predictions, labels, model_name){
  
  library(multiROC)
  
  predictions<- Naive_Bayes$Prediction.train
  labels <- train
  model_name <- "Naive Bayes"
  
  labels$label = factor(labels$label)
  
  n0_true = as.numeric(labels$label == 0)
  n1_true = as.numeric(labels$label == 1)
  n2_true = as.numeric(labels$label == 2)
  n3_true = as.numeric(labels$label == 3)
  n4_true = as.numeric(labels$label == 4)
  n5_true = as.numeric(labels$label == 5)
  n6_true = as.numeric(labels$label == 6)
  n7_true = as.numeric(labels$label == 7)
  n8_true = as.numeric(labels$label == 8)
  n9_true = as.numeric(labels$label == 9)
  
  S1_pred_m1 <- as.numeric(as.character(predictions))
  
  n0_pred_m1 = as.numeric(S1_pred_m1 == 0)
  n1_pred_m1 = as.numeric(S1_pred_m1 == 1)
  n2_pred_m1 = as.numeric(S1_pred_m1 == 2)
  n3_pred_m1 = as.numeric(S1_pred_m1 == 3)
  n4_pred_m1 = as.numeric(S1_pred_m1 == 4)
  n5_pred_m1 = as.numeric(S1_pred_m1 == 5)
  n6_pred_m1 = as.numeric(S1_pred_m1 == 6)
  n7_pred_m1 = as.numeric(S1_pred_m1 == 7)
  n8_pred_m1 = as.numeric(S1_pred_m1 == 8)
  n9_pred_m1 = as.numeric(S1_pred_m1 == 9)
  
  labels_data <- data.frame(n0_true,n1_true,n2_true,n3_true,n4_true,n5_true,n6_true,n7_true,n8_true,n9_true,
                            n0_pred_m1,n1_pred_m1,n2_pred_m1,n3_pred_m1,n4_pred_m1,n5_pred_m1,n6_pred_m1,
                            n7_pred_m1,n8_pred_m1,n9_pred_m1)
  #head(labels_data)
  
  res <- multi_roc(labels_data, force_diag=T)
  auc <- unlist(res$AUC)
  avg.auc <- mean(unlist(auc, use.names=FALSE)[1:10])
  
  
  #PLOT
  n_method <- length(unique(res$Methods))
  n_group <- length(unique(res$Groups))
  res_df <- data.frame(Specificity= numeric(0), Sensitivity= numeric(0), Group = character(0), AUC = numeric(0), Method = character(0))
  for (i in 1:n_method) {
    for (j in 1:n_group) {
      temp_data_1 <- data.frame(Specificity=res$Specificity[[i]][j],
                                Sensitivity=res$Sensitivity[[i]][j],
                                Group=unique(res$Groups)[j],
                                AUC=res$AUC[[i]][j],
                                Method = unique(res$Methods)[i])
      colnames(temp_data_1) <- c("Specificity", "Sensitivity", "Group", "AUC", "Method")
      res_df <- rbind(res_df, temp_data_1)
      
    }
  }
  
  plot_roc <-ggplot2::ggplot(res_df, ggplot2::aes(x = 1-Specificity, y=Sensitivity)) + ggplot2::geom_path(ggplot2::aes(color = Group, linetype=Method)) + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1), colour='grey', linetype = 'dotdash') + ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.justification=c(1, 0), legend.position=c(.95, .05), legend.title=ggplot2::element_blank(), legend.background = ggplot2::element_rect(fill=NULL, size=0.5, linetype="solid", colour ="black"))
  
  return_list <- list(auc, avg.auc, plot_roc)
  names(return_list) <- c("AUC", "avg.AUC", "roc.plot")
  
  return(return_list)
}