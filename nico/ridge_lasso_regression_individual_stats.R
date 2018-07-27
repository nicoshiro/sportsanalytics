library(glmnet)
library(listdtr)

# Set working directory
setwd("/Users/nicoshiro/Downloads/sports_analytics/data")

# Read in the training/testing data
sorted_indiv_train <- read.csv("sdecision_data.csv")
sorted_indiv_test <- read.csv("decision_test_data.csv")

# Remove lineup column
sorted_indiv_train$LINEUP <- NULL
sorted_indiv_test$LINEUP <- NULL

# Extract and remove the plus minus column from the data
sorted_train_pm <- sorted_indiv_train$TOT_PT_DIFF
sorted_test_pm <- sorted_indiv_test$TOT_PT_DIFF
sorted_indiv_train$TOT_PT_DIFF <- NULL
sorted_indiv_test$TOT_PT_DIFF <- NULL

# Change the data frames to matrices
sorted_indiv_train <- as.matrix(sorted_indiv_train)
sorted_indiv_test <- as.matrix(sorted_indiv_test)

# OPTIONAL SCALED DATA - IGNORE
# sorted_indiv_train_scaled <- scale(sorted_indiv_train)
# sorted_indiv_test_scaled <- scale(sorted_indiv_test)
# 
# sorted_train_pm_scaled <- as.vector(scale(sorted_train_pm))
# sorted_test_pm_scaled <- as.vector(scale(sorted_test_pm))


# Create the ridge regression model
ridge <- glmnet(sorted_indiv_train, sorted_train_pm, alpha = 0, nlambda = 100, lambda.min.ratio = 0.0001)

# Use cv.glmnet to find the best lambda
set.seed(1)
cv.out <- cv.glmnet(sorted_indiv_train, sorted_train_pm, alpha=0, nlambda=100, lambda.min.ratio=0.0001)
plot(cv.out)
best.lambda <- cv.out$lambda.min
best.lambda


# Predict the coefficients
predict(ridge, s=best.lambda, type="coefficients")[1:8, ]


# Prediction on the training set
prediction.train <- predict(ridge, s=best.lambda, newx = sorted_indiv_train)

# SST and SSE
sst.train <- sum((sorted_train_pm - mean(sorted_train_pm))^2)
sse.train <- sum((prediction.train - sorted_train_pm)^2)

# R squared
rsq.train <- 1 - sse.train / sst.train
rsq.train

# Mean Squared Error of the training data
MSE.ridge.train <- mean((prediction.train-sorted_train_pm)^2)
MSE.ridge.train


# Prediction on the testing set
prediction.test <- predict(ridge, s=best.lambda, newx = sorted_indiv_test)

# SST and SSE
sst.test <- sum((sorted_test_pm - mean(sorted_test_pm))^2)
sse.test <- sum((prediction.test - sorted_test_pm)^2)

# R squared
rsq.test <- 1 - sse.test / sst.test
rsq.test

# Mean Squared Error on the testing data
MSE.ridge.test <- mean((prediction.test - sorted_test_pm)^2)
MSE.ridge.test 



# LASSO MODEL
lasso <- glmnet(sorted_indiv_train, sorted_train_pm, alpha = 1, nlambda = 100, lambda.min.ratio = 0.0001)

# Use cv.glmnet to find the best lambda
set.seed(1)
lasso.cv.out <- cv.glmnet(sorted_indiv_train, sorted_train_pm, alpha=1, nlambda=100, lambda.min.ratio=0.0001)
plot(lasso.cv.out)
lasso.best.lambda <- lasso.cv.out$lambda.min
lasso.best.lambda


# Prediction on the training set
lasso.prediction.train <- predict(lasso, s=lasso.best.lambda, newx = sorted_indiv_train)

# SST and SSE
lasso.sst.train <- sum((sorted_train_pm - mean(sorted_train_pm))^2)
lasso.sse.train <- sum((lasso.prediction.train - sorted_train_pm)^2)

# R squared
lasso.rsq.train <- 1 - lasso.sse.train / lasso.sst.train
lasso.rsq.train

# Mean Squared Error of the training data
MSE.lasso.train <- mean((lasso.prediction.train-sorted_train_pm)^2)
MSE.lasso.train


# Prediction on the training set
lasso.prediction.test <- predict(lasso, s=lasso.best.lambda, newx = sorted_indiv_test)

# SST and SSE
lasso.sst.test <- sum((sorted_test_pm - mean(sorted_test_pm))^2)
lasso.sse.test <- sum((lasso.prediction.test - sorted_test_pm)^2)

# R squared
lasso.rsq.test <- 1 - lasso.sse.test / lasso.sst.test
lasso.rsq.test

# Mean Squared Error of the testing data
MSE.lasso.test <- mean((lasso.prediction.test-sorted_test_pm)^2)
MSE.lasso.test



# Plot the lasso predicted values against the actual values to see if there is a linear relationship
plot(sorted_test_pm, lasso.prediction.test, xlim = c(-1, 1), ylim = c(-1,1))
abline(lm(lasso.prediction.test ~ sorted_test_pm))
linear <- lm(sorted_test_pm ~ lasso.prediction.test)
summary(linear)

# Plot the ridge predicted values against the actual values to see if there is a linear relationship
plot(sorted_train_pm, prediction.train, xlim = c(-1, 1), ylim = c(-1,1))
abline(lm(prediction.train ~ sorted_train_pm))
linear <- lm(prediction.train ~ sorted_train_pm)
summary(linear)
