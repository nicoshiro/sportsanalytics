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

setwd("/Users/nicoshiro/Downloads/sports_analytics/data")

sorted_indiv_train <- read.csv("sdecision_data.csv")
sorted_indiv_test <- read.csv("decision_test_data.csv")

sorted_indiv_train$LINEUP <- NULL
sorted_indiv_test$LINEUP <- NULL

#sorted_indiv_train <- sorted_indiv_train[sorted_indiv_train$TOT_PT_DIFF != 0, ]
#sorted_indiv_test <- sorted_indiv_test[sorted_indiv_test$TOT_PT_DIFF != 0, ]

is.numeric(sorted_indiv_train$TOT_PT_DIFF)

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


sorted_indiv_train$TOT_PT_DIFF <- NULL
sorted_indiv_train$POS_NEG <- as.numeric(sorted_indiv_train$POS_NEG)

set.seed(3)
model <- bayesglm(POS_NEG ~ ., family = binomial(link = logit), data = sorted_indiv_train)

#model <- glm(POS_NEG ~ ., family = binomial(link = logit), data = sorted_indiv_train, maxit = 100)
summary(model)
#anova(model, test="Chisq")

sorted_indiv_train_no_PN <- sorted_indiv_train
sorted_indiv_train_no_PN$POS_NEG <- NULL


fitted.results <- predict(model, newdata = sorted_indiv_train_no_PN ,type = 'response')
fitted.results <- ifelse(fitted.results > 0.95,1,0)

misClasificError <- mean(fitted.results != sorted_indiv_train$POS_NEG)
print(paste('Accuracy', 1 - misClasificError))

MSE <- mean((fitted.results - sorted_indiv_train$POS_NEG)^2)
MSE

table(sorted_indiv_train$POS_NEG)
table(fitted.results)



results.v.actual <- data.frame(sorted_indiv_train$POS_NEG, fitted.results)
results.v.actual$fitted.results <- as.factor(results.v.actual$fitted.results)

results.v.actual$sorted_indiv_train.POS_NEG <- as.factor(results.v.actual$sorted_indiv_train.POS_NEG)


plot(results.v.actual$sorted_indiv_train.POS_NEG, results.v.actual$fitted.results)

matrix <- confusionMatrix(results.v.actual$fitted.results, results.v.actual$sorted_indiv_train.POS_NEG)
matrix

total_accuracy <- (24 + 36)/72
false_positive <- 9/33
false_negative <- 3/39
naive_classifier <- 39/72






# TEST
sorted_indiv_test$POS_NEG <- as.numeric(sorted_indiv_test$POS_NEG)
sorted_indiv_test_no_PN <- sorted_indiv_test
sorted_indiv_test_no_PN$POS_NEG <- NULL
sorted_indiv_test_no_PN$TOT_PT_DIFF <- NULL

fitted.results.test <- predict(model, newdata = sorted_indiv_test_no_PN ,type = 'response')
fitted.results.test <- ifelse(fitted.results.test > 0.95,1,0)

misClasificError <- mean(fitted.results.test != sorted_indiv_test$POS_NEG)
print(paste('Accuracy',1-misClasificError))

MSE.test <- mean((fitted.results.test - sorted_indiv_test$POS_NEG)^2)
MSE.test

table(sorted_indiv_test$POS_NEG)
table(fitted.results.test)



results.v.actual.test <- data.frame(sorted_indiv_test$POS_NEG, fitted.results.test)
results.v.actual.test$fitted.results.test <- as.factor(results.v.actual.test$fitted.results.test)
results.v.actual.test$sorted_indiv_test.POS_NEG <- as.factor(results.v.actual.test$sorted_indiv_test.POS_NEG)


plot(results.v.actual.test$sorted_indiv_test.POS_NEG, results.v.actual.test$fitted.results.test)

matrix <- confusionMatrix(results.v.actual.test$fitted.results.test, results.v.actual.test$sorted_indiv_test.POS_NEG)
matrix

# total_accuracy_test <- (21 + 34)/68
# total_accuracy_test
# false_positive_test <- 8/29
# false_positive_test
# false_negative_test <- 5/39
# false_negative_test
# naive_classifier_test <- 39/68
# naive_classifier_test


total_test <- cbind(sorted_indiv_test, fitted.results.test)
hist(total_test$TOT_PT_DIFF, col = factor(total_test$fitted.results.test))
qplot(total_test$TOT_PT_DIFF, color = factor(total_test$fitted.results.test))

#ggplot(total_test$TOT_PT_DIFF, aes(fill = factor(total_test$fitted.results.test))) + geom_bar()

total_test_pred_neg <- total_test[total_test$fitted.results.test == 0, ]
total_test_pred_pos <- total_test[total_test$fitted.results.test == 1, ]

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


#LAKERS
lakers <- read.csv("lakers_stats.csv")
lakers$X <- NULL

lakers_lineups <- as.character(lakers$lakers_combos_vector)
lakers$lakers_combos_vector <- NULL

fitted.results.lakers <- predict(model, newdata = lakers ,type = 'response')
fitted.results.lakers <- ifelse(fitted.results.lakers > 0.95,1,0)

table(fitted.results.lakers)

lakers <- cbind(fitted.results.lakers, lakers_lineups, lakers)

lakers_pred_pos <- subset(lakers, lakers$fitted.results.lakers == 1)
lakers_pred_neg <- subset(lakers, lakers$fitted.results.lakers == 0)
#lakers_pred_pos_w_bron <- subset(lakers_pred_pos, grepl(lakers_pred_pos$lakers_lineups, "LeBron", fixed = T))
lakers_pred_pos_w_bron <- data.frame()
lakers_pred_pos_wo_bron <- data.frame()


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


for (i in 1:353){
  if (stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "LeBron")){
    lakers_pred_pos_w_bron <- rbind(lakers_pred_pos_w_bron, lakers_pred_pos[i,])
  }
}

for (i in 1:353){
  if (!stri_detect_fixed(lakers_pred_pos$lakers_lineups[i], "LeBron")){
    lakers_pred_pos_wo_bron <- rbind(lakers_pred_pos_wo_bron, lakers_pred_pos[i,])
  }
}


#WARRIORS
warriors <- read.csv("warriors_stats.csv")
warriors$X <- NULL

warriors_lineups <- as.character(warriors$warriors_combos_vector)
warriors$warriors_combos_vector <- NULL

fitted.results.warriors <- predict(model, newdata = warriors ,type = 'response')
fitted.results.warriors <- ifelse(fitted.results.warriors > 0.95,1,0)

table(fitted.results.warriors)

warriors <- cbind(fitted.results.warriors, warriors_lineups, warriors)

warriors_pred_pos <- subset(warriors, warriors$fitted.results.warriors == 1)
warriors_pred_neg <- subset(warriors, warriors$fitted.results.warriors == 0)


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

