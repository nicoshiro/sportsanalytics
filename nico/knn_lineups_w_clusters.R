library(class)
library(readr)
library(caret)
library(dplyr)

# Set working directory
setwd("/Users/nicoshiro/Downloads/sports_analytics/data")

# Read in lineup data with column for clusters
lineups_new_train <- read.csv("lineups_new_train.csv")

# Clean the data
lineups_new_train$X <- NULL
lineups_new_train$CLUSTER <- NULL
lineups_new_train_wo_clusters <- lineups_new_train
MAIN_CLUSTER <- lineups_new_train$MAIN_CLUSTER
lineups_new_train$MAIN_CLUSTER <- NULL
scaled_lineups_new_train <- as.data.frame(scale(lineups_new_train))
scaled_lineups_new_train <- cbind(scaled_lineups_new_train, MAIN_CLUSTER)
scaled_lineups_new_train <- scaled_lineups_new_train[order(scaled_lineups_new_train$MAIN_CLUSTER),]
lineups_new_train <- lineups_new_train[order(lineups_new_train$MAIN_CLUSTER),]

# split into train and test set
set.seed(123)
train_indices <- createDataPartition(scaled_lineups_new_train$MAIN_CLUSTER, p=0.9) %>% unlist()
lineup_trainer <- scaled_lineups_new_train[train_indices,]
lineup_tester <- scaled_lineups_new_train[-train_indices,]

# KNN lineup prediction
lineup_pred <- class::knn(train = lineup_trainer, test = lineup_tester, cl = lineup_trainer$MAIN_CLUSTER, k = 8)
lineup_pred
lineup_tester$MAIN_CLUSTER


# Read in and clean the training data
lineups_new_train <- read.csv("lineups_new_train.csv")
lineups_new_train$X <- NULL
lineups_new_train$CLUSTER <- NULL
lineups_new_train <- lineups_new_train[order(lineups_new_train$MAIN_CLUSTER),]


# EDA on the clusters
cluster_1 <- subset(lineups_new_train, MAIN_CLUSTER == 1)
cluster_2 <- subset(lineups_new_train, MAIN_CLUSTER == 2)
cluster_3 <- subset(lineups_new_train, MAIN_CLUSTER == 3)
cluster_4 <- subset(lineups_new_train, MAIN_CLUSTER == 4)
cluster_5 <- subset(lineups_new_train, MAIN_CLUSTER == 5)
cluster_6 <- subset(lineups_new_train, MAIN_CLUSTER == 6)
cluster_7 <- subset(lineups_new_train, MAIN_CLUSTER == 7)
cluster_8 <- subset(lineups_new_train, MAIN_CLUSTER == 8)

cluster_1_median <- apply(cluster_1, MARGIN = 2, FUN = median)
cluster_2_median <- apply(cluster_2, MARGIN = 2, FUN = median)
cluster_3_median <- apply(cluster_3, MARGIN = 2, FUN = median)
cluster_4_median <- apply(cluster_4, MARGIN = 2, FUN = median)
cluster_5_median <- apply(cluster_5, MARGIN = 2, FUN = median)
cluster_6_median <- apply(cluster_6, MARGIN = 2, FUN = median)
cluster_7_median <- apply(cluster_7, MARGIN = 2, FUN = median)
cluster_8_median <- apply(cluster_8, MARGIN = 2, FUN = median)

all_medians_of_clusters <- as.data.frame(rbind(cluster_1_median,
                                 cluster_2_median,
                                 cluster_3_median,
                                 cluster_4_median,
                                 cluster_5_median,
                                 cluster_6_median,
                                 cluster_7_median,
                                 cluster_8_median))

PM_cluster_medians <- all_medians_of_clusters$TOT_PT_DIFF



lineups_new_train$PRED_PT_DIFF <- 0

for (i in 1:302){
  if (lineups_new_train[i, 10] == 1){
    lineups_new_train[i, 11] <- paste(PM_cluster_medians[1])
  }
  if (lineups_new_train[i, 10] == 2){
    lineups_new_train[i, 11] <- paste(PM_cluster_medians[2])
  }
  if (lineups_new_train[i, 10] == 3){
    lineups_new_train[i, 11] <- paste(PM_cluster_medians[3])
  }
  if (lineups_new_train[i, 10] == 4){
    lineups_new_train[i, 11] <- paste(PM_cluster_medians[4])
  }
  if (lineups_new_train[i, 10] == 5){
    lineups_new_train[i, 11] <- paste(PM_cluster_medians[5])
  }
  if (lineups_new_train[i, 10] == 6){
    lineups_new_train[i, 11] <- paste(PM_cluster_medians[6])
  }
  if (lineups_new_train[i, 10] == 7){
    lineups_new_train[i, 11] <- paste(PM_cluster_medians[7])
  }
  if (lineups_new_train[i, 10] == 8){
    lineups_new_train[i, 11] <- paste(PM_cluster_medians[8])
  }
  
}

lineups_new_train$TOT_PT_DIFF <- as.numeric(lineups_new_train$TOT_PT_DIFF)
lineups_new_train$PRED_PT_DIFF <- as.numeric(lineups_new_train$PRED_PT_DIFF)

# Calculate MSE
lineups_new_train <- lineups_new_train %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - PRED_PT_DIFF)^2)
lineups_new_train_MSE <- sum(lineups_new_train$PT_DIFF_SQUARE_ERROR)/302

