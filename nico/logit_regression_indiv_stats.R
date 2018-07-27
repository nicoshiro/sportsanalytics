library(dplyr)
library(readr)
library(glmnet)
library(listdtr)
library(caret)
library(e1071)
library(logistf)
library(arm)
library(stringi)
library(ggplot2)

# Set working directory
setwd("/Users/nicoshiro/Downloads/sports_analytics/data")

# Read in the training/testing data
sorted_indiv_train <- read.csv("sdecision_data.csv")
sorted_indiv_test <- read.csv("decision_test_data.csv")

# Extract and remove the lineup names column from the data
lineup_names <- sorted_indiv_test$LINEUP
lineup_names_train <- sorted_indiv_train$LINEUP
sorted_indiv_train$LINEUP <- NULL
sorted_indiv_test$LINEUP <- NULL


# Create column that indicates whether a lineup is positive or negative
sorted_indiv_train$POS_NEG <- 0
sorted_indiv_test$POS_NEG <- 0

for (i in 1:302) {
  if (sorted_indiv_train$TOT_PT_DIFF[i] > 0) {
    sorted_indiv_train$POS_NEG[i] <- 1
  }
  else {
    sorted_indiv_train$POS_NEG[i] <- 0
  }
}

for (i in 1:72) {
  if (sorted_indiv_test$TOT_PT_DIFF[i] > 0) {
    sorted_indiv_test$POS_NEG[i] <- 1
  }
  else {
    sorted_indiv_test$POS_NEG[i] <- 0
  }
}


# Remove the actual plus minus column from the data
sorted_indiv_train$TOT_PT_DIFF <- NULL

# Make sure the POS_NEG column is numeric
sorted_indiv_train$POS_NEG <- as.numeric(sorted_indiv_train$POS_NEG)

# Create the logistic regression model using bayesglm
set.seed(3)
model <- bayesglm(POS_NEG ~ ., family = binomial(link = logit), data = sorted_indiv_train)
summary(model)

# Create a copy of the training data frame that does not include the POS_NEG column
sorted_indiv_train_no_PN <- sorted_indiv_train
sorted_indiv_train_no_PN$POS_NEG <- NULL

# Get the predicted results from the training set
fitted.results <- predict(model, newdata = sorted_indiv_train_no_PN ,type = 'response')

# Use a .95 threshold to characterize a lineup as positive. This will reduce our overall accuracy
# but it will give us better accuracy on those predicted positive.
fitted.results <- ifelse(fitted.results > 0.95,1,0)

# Get the MSE from the training data
misClasificError <- mean(fitted.results != sorted_indiv_train$POS_NEG)
print(paste('Accuracy', 1 - misClasificError))
MSE <- mean((fitted.results - sorted_indiv_train$POS_NEG)^2)
MSE

# Show tables for predicted POS_NEG and actual POS_NEG
table(sorted_indiv_train$POS_NEG)
table(fitted.results)

# Create a data frame that puts predicted POS_NEG next to actual POS_NEG for each row
results.v.actual <- data.frame(sorted_indiv_train$POS_NEG, fitted.results)
results.v.actual$fitted.results <- as.factor(results.v.actual$fitted.results)
results.v.actual$sorted_indiv_train.POS_NEG <- as.factor(results.v.actual$sorted_indiv_train.POS_NEG)

# Plot the predicted values against the actual values
plot(results.v.actual$sorted_indiv_train.POS_NEG, results.v.actual$fitted.results)

# Create a confusion matrix to see how many predictions were correct
matrix <- confusionMatrix(results.v.actual$fitted.results, results.v.actual$sorted_indiv_train.POS_NEG)
matrix

# Extract the number of minutes each training lineup has played
lineups_minutes_train <- read.csv("train_for_lineups_full.csv")
minutes <- lineups_minutes_train$TOTAL_MIN

# Bind all of the vectors into one data frame
total_train <- cbind(lineup_names_train, sorted_indiv_train, minutes, fitted.results)




# Run the model on the testing data set
sorted_indiv_test$POS_NEG <- as.numeric(sorted_indiv_test$POS_NEG)
sorted_indiv_test_no_PN <- sorted_indiv_test
sorted_indiv_test_no_PN$POS_NEG <- NULL
sorted_indiv_test_no_PN$TOT_PT_DIFF <- NULL

# Get the predicted results from the testing data
fitted.results.test <- predict(model, newdata = sorted_indiv_test_no_PN ,type = 'response')
fitted.results.test <- ifelse(fitted.results.test > 0.95,1,0)

# Get the accuracy and MSE from the testing data
misClasificError <- mean(fitted.results.test != sorted_indiv_test$POS_NEG)
print(paste('Accuracy',1-misClasificError))
MSE.test <- mean((fitted.results.test - sorted_indiv_test$POS_NEG)^2)
MSE.test

# Show tables for predicted POS_NEG and actual POS_NEG
table(sorted_indiv_test$POS_NEG)
table(fitted.results.test)

# Create a data frame that puts predicted POS_NEG next to actual POS_NEG for each row
results.v.actual.test <- data.frame(sorted_indiv_test$POS_NEG, fitted.results.test)
results.v.actual.test$fitted.results.test <- as.factor(results.v.actual.test$fitted.results.test)
results.v.actual.test$sorted_indiv_test.POS_NEG <- as.factor(results.v.actual.test$sorted_indiv_test.POS_NEG)

# Plot the predicted values against the actual values
plot(results.v.actual.test$sorted_indiv_test.POS_NEG, results.v.actual.test$fitted.results.test)

# Create a confusion matrix to see how many predictions were correct
matrix <- confusionMatrix(results.v.actual.test$fitted.results.test, results.v.actual.test$sorted_indiv_test.POS_NEG)
matrix

# Extract the number of minutes each training lineup has played
lineups_minutes <- read.csv("test_for_lineups_full.csv")
minutes <- lineups_minutes$TOTAL_MIN

# Bind all of the vectors into one data frame
total_test <- cbind(lineup_names, sorted_indiv_test, minutes, fitted.results.test)


# Create histogram of the actual vs predicted plus minus for all testing set
qplot(total_test$TOT_PT_DIFF, 
      fill = factor(total_test$fitted.results.test),
      main = "Histogram of Predicted PM",
      xlab = "Actual PM",
      binwidth = .1)

# Create histogram of the predicted negative plus minus for the testing set
total_test_pred_neg <- total_test[total_test$fitted.results.test == 0, ]
qplot(total_test_pred_neg$TOT_PT_DIFF,
      color = I("red"), 
      fill = I("red"), 
      main = "Histogram of Predicted Negative PM",
      xlab = "Actual PM",
      binwidth = .1)

# Create histogram of the predicted positive plus minus for the testing set
total_test_pred_pos <- total_test[total_test$fitted.results.test == 1, ]
qplot(total_test_pred_pos$TOT_PT_DIFF, 
      fill = I("blue"),
      main = "Histogram of Predicted Positive PM",
      xlab = "Actual PM",
      binwidth = .1)


# EDA of the predicted values of the testing set
total_test_pred_pos_act_pos <- subset(total_test, total_test$fitted.results.test == 1 & total_test$POS_NEG == 1)
total_test_pred_neg_act_neg <- subset(total_test, total_test$fitted.results.test == 0 & total_test$POS_NEG == 0)

total_test_pred_pos_act_neg <- subset(total_test, total_test$fitted.results.test == 1 & total_test$POS_NEG == 0)
total_test_pred_neg_act_pos <- subset(total_test, total_test$fitted.results.test == 0 & total_test$POS_NEG == 1)

hist(total_test_pred_pos_act_pos$TOT_PT_DIFF)
hist(total_test_pred_neg_act_neg$TOT_PT_DIFF)

hist(total_test_pred_pos_act_neg$TOT_PT_DIFF)
hist(total_test_pred_neg_act_pos$TOT_PT_DIFF)

hist(total_test_pred_neg$TOT_PT_DIFF)
hist(total_test_pred_pos$TOT_PT_DIFF)




# LAKERS DATA
# Read in lakers data
lakers <- read.csv("lakers_stats.csv")
lakers$X <- NULL
lakers_lineups <- as.character(lakers$lakers_combos_vector)
lakers$lakers_combos_vector <- NULL

# Predict the POS_NEG for the lakers data
fitted.results.lakers <- predict(model, newdata = lakers ,type = 'response')
fitted.results.lakers <- ifelse(fitted.results.lakers > 0.95,1,0)
table(fitted.results.lakers)

# Bind results, lineups, and lineup data together
lakers <- cbind(fitted.results.lakers, lakers_lineups, lakers)

# EDA of the lakers data
lakers_pred_pos <- subset(lakers, lakers$fitted.results.lakers == 1)
lakers_pred_neg <- subset(lakers, lakers$fitted.results.lakers == 0)

# Count frequency of the players in predicted positive lineups
NAMES <- c("Lonzo Ball", "Kentavious Caldwell-Pope", "Alex Caruso", "Josh Hart", "Brandon Ingram", "LeBron James", "Kyle Kuzma", "JaVale McGee", "Rajon Rondo", "Lance Stephenson", "Ivica Zubac", "Michael Beasley")
lakers_freq <- data.frame(NAMES)
lakers_freq$FREQ <- 0
for (i in 1:353){
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "Lonzo")){
    lakers_freq$FREQ[1] <- lakers_freq$FREQ[1] + 1
  }
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "Kentavious")){
    lakers_freq$FREQ[2] <- lakers_freq$FREQ[2] + 1
  }
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "Alex")){
    lakers_freq$FREQ[3] <- lakers_freq$FREQ[3] + 1
  }
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "Josh")){
    lakers_freq$FREQ[4] <- lakers_freq$FREQ[4] + 1
  }
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "Brandon")){
    lakers_freq$FREQ[5] <- lakers_freq$FREQ[5] + 1
  }
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "LeBron")){
    lakers_freq$FREQ[6] <- lakers_freq$FREQ[6] + 1
  }
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "Kyle")){
    lakers_freq$FREQ[7] <- lakers_freq$FREQ[7] + 1
  }
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "JaVale")){
    lakers_freq$FREQ[8] <- lakers_freq$FREQ[8] + 1
  }
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "Rondo")){
    lakers_freq$FREQ[9] <- lakers_freq$FREQ[9] + 1
  }
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "Lance")){
    lakers_freq$FREQ[10] <- lakers_freq$FREQ[10] + 1
  }
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "Ivica")){
    lakers_freq$FREQ[11] <- lakers_freq$FREQ[11] + 1
  }
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "Michael")){
    lakers_freq$FREQ[12] <- lakers_freq$FREQ[12] + 1
  }
  
}

# Count frequency of the players in predicted negative lineups
NAMES <- c("Lonzo Ball", "Kentavious Caldwell-Pope", "Alex Caruso", "Josh Hart", "Brandon Ingram", "LeBron James", "Kyle Kuzma", "JaVale McGee", "Rajon Rondo", "Lance Stephenson", "Ivica Zubac", "Michael Beasley")
lakers_freq_neg <- data.frame(NAMES)
lakers_freq_neg$FREQ <- 0
for (i in 1:439){
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "Lonzo")){
    lakers_freq_neg$FREQ[1] <- lakers_freq_neg$FREQ[1] + 1
  }
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "Kentavious")){
    lakers_freq_neg$FREQ[2] <- lakers_freq_neg$FREQ[2] + 1
  }
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "Alex")){
    lakers_freq_neg$FREQ[3] <- lakers_freq_neg$FREQ[3] + 1
  }
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "Josh")){
    lakers_freq_neg$FREQ[4] <- lakers_freq_neg$FREQ[4] + 1
  }
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "Brandon")){
    lakers_freq_neg$FREQ[5] <- lakers_freq_neg$FREQ[5] + 1
  }
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "LeBron")){
    lakers_freq_neg$FREQ[6] <- lakers_freq_neg$FREQ[6] + 1
  }
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "Kyle")){
    lakers_freq_neg$FREQ[7] <- lakers_freq_neg$FREQ[7] + 1
  }
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "JaVale")){
    lakers_freq_neg$FREQ[8] <- lakers_freq_neg$FREQ[8] + 1
  }
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "Rondo")){
    lakers_freq_neg$FREQ[9] <- lakers_freq_neg$FREQ[9] + 1
  }
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "Lance")){
    lakers_freq_neg$FREQ[10] <- lakers_freq_neg$FREQ[10] + 1
  }
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "Ivica")){
    lakers_freq_neg$FREQ[11] <- lakers_freq_neg$FREQ[11] + 1
  }
  if (stri_detect_fixed(lakers_pred_neg$lakers_lineups[i], "Michael")){
    lakers_freq_neg$FREQ[12] <- lakers_freq_neg$FREQ[12] + 1
  }
  
}



# WARRIORS DATA
# Read in warriors data
warriors <- read.csv("warriors_stats.csv")
warriors$X <- NULL
warriors_lineups <- as.character(warriors$warriors_combos_vector)
warriors$warriors_combos_vector <- NULL

# Predict the POS_NEG for the warriors data
fitted.results.warriors <- predict(model, newdata = warriors ,type = 'response')
fitted.results.warriors <- ifelse(fitted.results.warriors > 0.95,1,0)
table(fitted.results.warriors)

# Bind results, lineups, and lineup data together
warriors <- cbind(fitted.results.warriors, warriors_lineups, warriors)

# EDA of the warriors data
warriors_pred_pos <- subset(warriors, warriors$fitted.results.warriors == 1)
warriors_pred_neg <- subset(warriors, warriors$fitted.results.warriors == 0)

# Count frequency of the players in predicted positive lineups
NAMES <- c("Jordan Bell", "DeMarcus Cousins", "Stephen Curry", "Kevin Durant", "Draymond Green", "Andre Iguodala", "Shaun Livingston", "Kevon Looney", "Klay Thompson", "David West", "Nick Young")
warriors_freq <- data.frame(NAMES)
warriors_freq$FREQ <- 0
for (i in 1:254){
  if (stri_detect_fixed(warriors_pred_pos$warriors_lineups[i], "Jordan")){
    warriors_freq$FREQ[1] <- warriors_freq$FREQ[1] + 1
  }
  if (stri_detect_fixed(warriors_pred_pos$warriors_lineups[i], "DeMarcus")){
    warriors_freq$FREQ[2] <- warriors_freq$FREQ[2] + 1
  }
  if (stri_detect_fixed(warriors_pred_pos$warriors_lineups[i], "Stephen")){
    warriors_freq$FREQ[3] <- warriors_freq$FREQ[3] + 1
  }
  if (stri_detect_fixed(warriors_pred_pos$warriors_lineups[i], "Kevin")){
    warriors_freq$FREQ[4] <- warriors_freq$FREQ[4] + 1
  }
  if (stri_detect_fixed(warriors_pred_pos$warriors_lineups[i], "Draymond")){
    warriors_freq$FREQ[5] <- warriors_freq$FREQ[5] + 1
  }
  if (stri_detect_fixed(warriors_pred_pos$warriors_lineups[i], "Andre")){
    warriors_freq$FREQ[6] <- warriors_freq$FREQ[6] + 1
  }
  if (stri_detect_fixed(warriors_pred_pos$warriors_lineups[i], "Shaun")){
    warriors_freq$FREQ[7] <- warriors_freq$FREQ[7] + 1
  }
  if (stri_detect_fixed(warriors_pred_pos$warriors_lineups[i], "Kevon")){
    warriors_freq$FREQ[8] <- warriors_freq$FREQ[8] + 1
  }
  if (stri_detect_fixed(warriors_pred_pos$warriors_lineups[i], "Klay")){
    warriors_freq$FREQ[9] <- warriors_freq$FREQ[9] + 1
  }
  if (stri_detect_fixed(warriors_pred_pos$warriors_lineups[i], "David")){
    warriors_freq$FREQ[10] <- warriors_freq$FREQ[10] + 1
  }
  if (stri_detect_fixed(warriors_pred_pos$warriors_lineups[i], "Nick")){
    warriors_freq$FREQ[11] <- warriors_freq$FREQ[11] + 1
  }
  
}

# Count frequency of the players in predicted negative lineups
NAMES <- c("Jordan Bell", "DeMarcus Cousins", "Stephen Curry", "Kevin Durant", "Draymond Green", "Andre Iguodala", "Shaun Livingston", "Kevon Looney", "Klay Thompson", "David West", "Nick Young")
warriors_freq_neg <- data.frame(NAMES)
warriors_freq_neg$FREQ <- 0
for (i in 1:208){
  if (stri_detect_fixed(warriors_pred_neg$warriors_lineups[i], "Jordan")){
    warriors_freq_neg$FREQ[1] <- warriors_freq_neg$FREQ[1] + 1
  }
  if (stri_detect_fixed(warriors_pred_neg$warriors_lineups[i], "DeMarcus")){
    warriors_freq_neg$FREQ[2] <- warriors_freq_neg$FREQ[2] + 1
  }
  if (stri_detect_fixed(warriors_pred_neg$warriors_lineups[i], "Stephen")){
    warriors_freq_neg$FREQ[3] <- warriors_freq_neg$FREQ[3] + 1
  }
  if (stri_detect_fixed(warriors_pred_neg$warriors_lineups[i], "Kevin")){
    warriors_freq_neg$FREQ[4] <- warriors_freq_neg$FREQ[4] + 1
  }
  if (stri_detect_fixed(warriors_pred_neg$warriors_lineups[i], "Draymond")){
    warriors_freq_neg$FREQ[5] <- warriors_freq_neg$FREQ[5] + 1
  }
  if (stri_detect_fixed(warriors_pred_neg$warriors_lineups[i], "Andre")){
    warriors_freq_neg$FREQ[6] <- warriors_freq_neg$FREQ[6] + 1
  }
  if (stri_detect_fixed(warriors_pred_neg$warriors_lineups[i], "Shaun")){
    warriors_freq_neg$FREQ[7] <- warriors_freq_neg$FREQ[7] + 1
  }
  if (stri_detect_fixed(warriors_pred_neg$warriors_lineups[i], "Kevon")){
    warriors_freq_neg$FREQ[8] <- warriors_freq_neg$FREQ[8] + 1
  }
  if (stri_detect_fixed(warriors_pred_neg$warriors_lineups[i], "Klay")){
    warriors_freq_neg$FREQ[9] <- warriors_freq_neg$FREQ[9] + 1
  }
  if (stri_detect_fixed(warriors_pred_neg$warriors_lineups[i], "David")){
    warriors_freq_neg$FREQ[10] <- warriors_freq_neg$FREQ[10] + 1
  }
  if (stri_detect_fixed(warriors_pred_neg$warriors_lineups[i], "Nick")){
    warriors_freq_neg$FREQ[11] <- warriors_freq_neg$FREQ[11] + 1
  }
  
}