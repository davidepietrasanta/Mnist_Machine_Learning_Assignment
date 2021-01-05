# Data Analysis

data_analysis <- function()
{
        # Load datasets
        mnist_train <- read.csv("CSV/mnist_train.csv", header = FALSE) 
        mnist_test <- read.csv("CSV/mnist_test.csv", header = FALSE)
        
        colnames(mnist_train)[1] <- "Number"
        colnames(mnist_test)[1] <- "Number"
        
        # Removing missing value
        mnist_train[is.na(mnist_train)] <- 0
        mnist_test[is.na(mnist_test)] <- 0
        
        # Check distribution of every number
        table_train = table(mnist_train$Number) / nrow(mnist_train) * 100 # uniform distribution
        table_test = table(mnist_test$Number) / nrow(mnist_test) * 100 # uniform distribution
        
        plot(table_train, main="Distribution of labels in trainset", xlab = "Labels", ylab = "Distribution %")
        chisq_uniform_train = chisq.test(table_train)
        chisq_uniform_train$expected
        chisq_uniform_train$p.value
        # We can conclude that the observed proportions are not significantly different
        # from an uniform distribution, with a p-value of 0.9999972.
        
        plot(table_test, main="Distribution of labels in testset", xlab = "Labels", ylab = "Distribution %")
        chisq_uniform_test = chisq.test(table_test)
        chisq_uniform_test$expected
        chisq_uniform_test$p.value
        # We can conclude that the observed proportions are not significantly different
        # from an uniform distribution, with a p-value of 0.9999935.
        
        # Correlation matrix from train set
        corr <- round(cor(mnist_train), 2)
        
        # Looking for distribution of color in every images
        # Stack every value of color
        col <- stack(mnist_train)[, "values", drop = FALSE]
        plot_color_distr <- ggplot(col, aes(values)) + geom_histogram() + ggtitle("Distribution of different color")
        plot(plot_color_distr)
        
        # Making target class as a factor 
        mnist_train$Number <- factor(mnist_train$Number)
        mnist_test$Number <- factor(mnist_test$Number)
        
        # Studying centroids to find similarities between digits
        par(mfrow = c(2, 5), mar = c(5, 5, 5, 5))
        for(i in 0 : 9)
        {
                sub_v <- subset(mnist_train, Number == i)
                mean <- colMeans(sub_v[, 2 : 784])
                digits_matrix <- matrix(mean, nrow = 28, ncol = 28)
                # Plot every centroid
                plot(t(digits_matrix), main = paste("Centroids", i))
        }
        
        # Normalizing dataset
        mnist_train_norm <- as.matrix(mnist_train[, -1]) / 255
        mnist_train_norm.conv <- cov(mnist_train_norm)
        
        # Applying PCA computation
        pca <- prcomp(mnist_train_norm.conv)
        
        # Calculate eigenvalue to check how many PCs we can pick up
        library(factoextra)
        eig <- get_eigenvalue(pca)
        print(eig)
        pca_plot <- fviz_eig(pca, addlabels = TRUE, ncp = 15)
        plot(pca_plot)
        
        # Applying PCA reduction
        mnist_train_red <- as.matrix(mnist_train[ , -1]) %*% pca$x[ , 1 : 20] # using only 20 PCs
        mnist_test_red <- as.matrix(mnist_test[ , -1]) %*% pca$x[ , 1 : 20] # using only 20 PCs
        
        # Preparing data frame for csv writing
        mnist_train_red <- data.frame(label = mnist_train$Number, mnist_train_red)
        mnist_test_red <- data.frame(label = mnist_test$Number, mnist_test_red)
        
        # Writing csv files
        write.csv(mnist_train_red, "CSV/mnist_train_pca.csv", row.names = FALSE)
        write.csv(mnist_test_red, "CSV/mnist_test_pca.csv", row.names = FALSE)
}


