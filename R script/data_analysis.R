# Data Analysis

# Load datasets
mnist_train <- read.csv("CSV/mnist_train.csv") 
mnist_test <- read.csv("CSV/mnist_test.csv")

colnames(mnist_test)[1] <- "Number"
colnames(mnist_train)[1] <- "Number"

# Removing missing value
mnist_train[is.na(mnist_train)] <- 0
mnist_test[is.na(mnist_test)] <- 0

# Check distribution of every number
table_train = table(mnist_train$Number) / nrow(mnist_train) * 100 # uniform distribution
table_test = table(mnist_test$Number) / nrow(mnist_test) * 100 # uniform distribution

plot(table_train)
chisq_uniform_train = chisq.test(table_train)
chisq_uniform_train$expected
chisq_uniform_train$p.value
# We can conclude that the observed proportions are not significantly different
# from an uniform distribution, with a p-value of 0.9999972.

plot(table_test)
chisq_uniform_test = chisq.test(table_test)
chisq_uniform_test$expected
chisq_uniform_test$p.value
# We can conclude that the observed proportions are not significantly different
# from an uniform distribution, with a p-value of 0.9999935.


# Making target class as a factor 
mnist_train$Number <- factor(mnist_train$Number)
mnist_test$Number <- factor(mnist_test$Number)

# Normalizing dataset
mnist_train_norm <- as.matrix(mnist_train[, -1]) / 255
mnist_train_norm.conv <- cov(mnist_train_norm)

# Applying PCA computation
pca <- prcomp(mnist_train_norm.conv)

# Checking relationships between PCs and variance
variance_explained <- as.data.frame(pca$sdev ^ 2 / sum(pca$sdev ^ 2))
variance_explained <- cbind(c(1 : 784),
                    variance_explained,
                    cumsum(variance_explained[ , 1])
                    )

# Set col names
colnames(variance_explained) <- c("Number_of_Principal_Components",
                                  "Individual_Variance_Explained",
                                  "Cumulative_Variance_Explained"
                                  )

#Plot between cumulative variance & PCs
plot(variance_explained$Number_of_Principal_Components,
     variance_explained$Cumulative_Variance_Explained, 
     xlim = c(0 , 150),
     xlab = "Principal Componets",
     ylab = "Cumulative Variance Explained",
     main = 'Principal Components vs Cumulative Variance Explained'
     )

#Table showing Cumulative Variance & Principal Components
variance_explained[seq(0, 784, 5),] # with 20 PCs we can explain 95% of the variance

# Applying PCA reduction
mnist_train_red <- as.matrix(mnist_train[ , -1]) %*% pca$x[ , 1 : 20] # using only 20 PCs
mnist_test_red <- as.matrix(mnist_test[ , -1]) %*% pca$x[ , 1 : 20] # using only 20 PCs

# Preparing data frame for csv writing
mnist_train_red <- data.frame(label = mnist_train$Number, mnist_train_red)
mnist_test_red <- data.frame(label = mnist_test$Number, mnist_test_red)

# Writing csv files
write.csv(mnist_train_red, "CSV/mnist_train_pca.csv", row.names = FALSE)
write.csv(mnist_test_red, "CSV/mnist_test_pca.csv", row.names = FALSE)
