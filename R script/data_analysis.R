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
        
        # Correlation matrix from train set\
        library(plotly)
        corr <- round(cor(mnist_train), 2)
        corr_matrix_plot <- plot_ly(z = corr, type = "heatmap")
        plot(corr_matrix_plot)
        
        # Looking for distribution of color in every images
        # Stack every value of color
        col <- stack(mnist_train)[, "values", drop = FALSE]
        plot_color_distr <- ggplot(col, aes(values)) + geom_histogram() + ggtitle("Distribution of different color")
        plot_color_distr
        
        # Making target class as a factor 
        mnist_train$Number <- factor(mnist_train$Number)
        mnist_test$Number <- factor(mnist_test$Number)
        
        # Studying centroids to find similarities between digits
        sub_v <- lapply(0:9, function(i) subset(mnist_train, Number == i))
        mean_vec <- lapply(1:10, function(i) colMeans(sub_v[[i]][, 2 : 784]))
        digits_matrix <- lapply(1:10, function(i) matrix(mean_vec[[i]], ncol = 28, nrow = 28))
        # Plot every centroids
        plots <- lapply(1:10, function(i)
                plot_ly(z = t(digits_matrix[[i]][, nrow(digits_matrix[[i]]) : 1]),
                type = "heatmap",
                showscale = FALSE
                ))
        subplot(plots, nrows = 2)
        # Compute distance from centroids
        euclidian_distance <- lapply(1:10, function(i) colMeans(t(sqrt((sub_v[[i]][,2 : 784] - mean_vec[[i]]) ^ 2))))

        # Boxplot of distance for digits
        par(mfrow = c(2, 5), mar = c(3.5, 3.5, 3.5, 3.5))
        boxplot(lapply(1:10, function(i) euclidian_distance[[i]]), main = "Distance from centroids", xlab = "Digit", ylab = "Distance")
        
        # Pick the index of the 5 most distant digits from the centroid
        euclidian_distance <- lapply(1:10, function(i) unlist(euclidian_distance[[i]]))
        row_index <- lapply(1:10, function(i) head(euclidian_distance[[i]][order(-euclidian_distance[[i]])], 5))
        row_index <- lapply(1:10, function(i) unlist(row_index[[i]]))
        diff_digits <- lapply(1:10, function(i) { 
                lapply(1:5, function(n){
                        matrix(t(mnist_train[attr(row_index[[i]], 'names')[n],]), ncol = 28, nrow = 28)
                })})
        # Plot the most different digits in the dataset
        diff_plots <- lapply(1:10, function(i){
                lapply(1:5, function(n){
                plot_ly(z = t(diff_digits[[i]][[n]][, 28 : 1]),
                        type = "heatmap", 
                        showscale = FALSE,
                        colors = c("white", "black")
                        )
                })})
        subplot(lapply(1:10, function(i) subplot(diff_plots[[i]], nrows = 1)), nrows = 10)
        
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


