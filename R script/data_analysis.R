# Data Analysis

# load dataset
dataset.train <- read.csv("CSV/mnist_train.csv", na.strings=c("NA", ""), stringsAsFactors = FALSE) 

# Plotting distribution of the label
label.distribution <- hist(dataset.train$X5, 
     main = "Distribution of the label", 
     xlab = "Labels",
     col = "red",
     breaks=seq(-1,9)
     )

# Apply counter on the coloumns of the hist
text(label.distribution$mids,
     label.distribution$counts,
     labels=label.distribution$counts, 
     adj=c(0.5, -0.5)
     )