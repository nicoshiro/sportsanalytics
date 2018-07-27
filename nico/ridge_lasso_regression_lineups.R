library(glmnet)
library(listdtr)

# Set working directory
setwd("/Users/nicoshiro/Downloads/sports_analytics/data")

# Read in the training/testing data set
lineups_trainer <- read.csv("train_for_lineups_full.csv")
lineups_tester <- read.csv("test_for_lineups_full.csv")

# Get rid of unnecessary rows
lineups_trainer$X1 <- NULL
lineups_trainer$X <- NULL
lineups_trainer$LINEUP <- NULL

lineups_tester$X1 <- NULL
lineups_tester$X <- NULL
lineups_tester$LINEUP <- NULL

# Extract the plus-minus column from the lineup data
lineup_trainer_pm <- lineups_trainer$TOT_PT_DIFF
lineup_tester_pm <- lineups_tester$TOT_PT_DIFF

# Remove the plus minus column from the rest of the data
lineups_trainer$TOT_PT_DIFF <- NULL
lineups_tester$TOT_PT_DIFF <- NULL

# Scale the lineup data
lineup_trainer_scaled <- scale(lineups_trainer)
lineup_tester_scaled <- scale(lineups_tester)

#Scale the plus minus vector
lineup_trainer_pm_scaled <- as.vector(scale(lineup_trainer_pm))
lineup_tester_pm_scaled <- as.vector(scale(lineup_tester_pm))


# Create the ridge regression model
ridge <- glmnet(lineup_trainer_scaled, lineup_trainer_pm_scaled, alpha = 0, nlambda = 1000, lambda.min.ratio = 0.0001)

# Use cv.glmnet to find the best lambda
set.seed(1)
cv.out <- cv.glmnet(lineup_trainer_scaled, lineup_trainer_pm_scaled, alpha=0, nlambda=1000, lambda.min.ratio=0.0001)
plot(cv.out)
best.lambda <- cv.out$lambda.min
best.lambda


# Predict the coefficients
predict(ridge, s=best.lambda, type="coefficients")[1:8, ]


# Prediction on the training set
prediction.train <- predict(ridge, s=best.lambda, newx = lineup_trainer_scaled)

# SST and SSE
sst.train <- sum((lineup_trainer_pm_scaled - mean(lineup_trainer_pm_scaled))^2)
sse.train <- sum((prediction.train - lineup_trainer_pm_scaled)^2)

# R squared
rsq.train <- 1 - sse.train / sst.train
rsq.train

# Mean Squared Error of the training set
MSE.ridge.train <- mean((prediction.train-lineup_trainer_pm_scaled)^2)
MSE.ridge.train


# Prediction on the testing set
prediction.test <- predict(ridge, s=best.lambda, newx = lineup_tester_scaled)

# SST and SSE
sst.test <- sum((lineup_tester_pm_scaled - mean(lineup_tester_pm_scaled))^2)
sse.test <- sum((prediction.test - lineup_tester_pm_scaled)^2)

# R squared
rsq.test <- 1 - sse.test / sst.test
rsq.test

# Mean Squared Error of the testing set
MSE.ridge.test <- mean((prediction.test-lineup_tester_pm_scaled)^2)
MSE.ridge.test 


# LASSO MODEL
lasso <- glmnet(lineup_trainer_scaled, lineup_trainer_pm_scaled, alpha = 1, nlambda = 1000, lambda.min.ratio = 0.0001)

# Use cv.glmnet to find the best lambda
set.seed(1)
lasso.cv.out <- cv.glmnet(lineup_trainer_scaled, lineup_trainer_pm_scaled, alpha=1, nlambda=1000, lambda.min.ratio=0.0001)
plot(lasso.cv.out)
lasso.best.lambda <- lasso.cv.out$lambda.min
lasso.best.lambda


# Prediction on the training set
lasso.prediction.train <- predict(lasso, s=lasso.best.lambda, newx = lineup_trainer_scaled)

# SST and SSE
lasso.sst.train <- sum((lineup_trainer_pm_scaled - mean(lineup_trainer_pm_scaled))^2)
lasso.sse.train <- sum((lasso.prediction.train - lineup_trainer_pm_scaled)^2)

# R squared
lasso.rsq.train <- 1 - lasso.sse.train / lasso.sst.train
lasso.rsq.train

# Mean Squared Error of the training set
MSE.lasso.train <- mean((lasso.prediction.train-lineup_trainer_pm_scaled)^2)
MSE.lasso.train


# Prediction on the testing set
lasso.prediction.test <- predict(lasso, s=lasso.best.lambda, newx = lineup_tester_scaled)

# SST and SSE
lasso.sst.test <- sum((lineup_tester_pm_scaled - mean(lineup_tester_pm_scaled))^2)
lasso.sse.test <- sum((lasso.prediction.test - lineup_tester_pm_scaled)^2)

# R squared
lasso.rsq.test <- 1 - lasso.sse.test / lasso.sst.test
lasso.rsq.test

# Mean Squared Error of the testing set
MSE.lasso.test <- mean((lasso.prediction.test-lineup_tester_pm_scaled)^2)
MSE.lasso.test
