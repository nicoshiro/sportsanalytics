library(readr)
library(dplyr)
library(tree)
library(randomForest)
library(gbm)

#### LINEUP DATA SETUP ###
# load the full lineup stats (training)
train_for_lineups_full_stats <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/train_for_lineups_full.csv")
test_for_lineups_full_stats <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/test_for_lineups_full.csv")

#Exclude non-numeric data
train_lineups <- train_for_lineups_full_stats %>% dplyr::select(-LINEUP, -TOTAL_MIN, -X1, -X)

#Scale the numeric data
lineups_scaled <- scale(all_lineups)

#Training Data Cleaning
test_lineups <- test_for_lineups_full_stats %>% dplyr::select(-LINEUP, -TOTAL_MIN, -X1, -X)



# Decision Tree From Training Lineups
set.seed(4)
decision_tree <- tree(TOT_PT_DIFF ~ FG_PCT_DIFF + X3P_PCT_DIFF + REB + BLK + STL + AST + TOV,
                      data = train_lineups, method = 'anova')

plot(decision_tree)
text(decision_tree ,pretty=0)

# Prediction function that determines plus minus of train data using training decision tree

decision_tree_prediction_train <- predict(decision_tree, train_lineups)

train_lineups_error <- cbind(train_lineups, predicted_PLUS_MINUS = decision_tree_prediction_train)
train_lineups_error <- train_lineups_error %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - decision_tree_prediction_train)^2)
train_decision_MSE <- sum(train_lineups_error$PT_DIFF_SQUARE_ERROR)/302

# Determines how many nodes should be in pruned tree
cv_decision_tree =cv.tree(decision_tree)
plot(cv_decision_tree$size ,cv_decision_tree$dev ,type='b')


#Prunes tree to 2 nodes
prune_decision_tree_2 <- prune.tree(decision_tree ,best=3)
plot(prune_decision_tree_2)
text(prune_decision_tree_2 ,pretty=0)

#Another prediction with prune = 2 nodes
decision_tree_prediction_2 <- predict(prune_decision_tree_2, train_lineups)

train_lineups_error_2 <- cbind(train_lineups, decision_tree_prediction_2)
train_lineups_error_2 <- train_lineups_error_2 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - decision_tree_prediction_2)^2)
train_decision_MSE_2 <- sum(train_lineups_error_2$PT_DIFF_SQUARE_ERROR)/302


#Another prediction with prune = 2 nodes
test_decision_tree_prediction_2 <- predict(prune_decision_tree_2, test_lineups)

test_lineups_error_2 <- cbind(test_lineups, test_decision_tree_prediction_2)
test_lineups_error_2 <- test_lineups_error_2 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - test_decision_tree_prediction_2)^2)
test_decision_MSE_2 <- sum(test_lineups_error_2$PT_DIFF_SQUARE_ERROR)/72


#Prunes tree to 3 nodes
prune_decision_tree_3 <- prune.tree(decision_tree ,best=3)
plot(prune_decision_tree_3)
text(prune_decision_tree_3 ,pretty=0)

#Another prediction with prune = 3 nodes
decision_tree_prediction_3 <- predict(prune_decision_tree_3, train_lineups)

train_lineups_error_3 <- cbind(train_lineups, decision_tree_prediction_3)
train_lineups_error_3 <- train_lineups_error_3 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - decision_tree_prediction_3)^2)
train_decision_MSE_3 <- sum(train_lineups_error_3$PT_DIFF_SQUARE_ERROR)/302

#Another prediction with prune = 3 nodes
test_decision_tree_prediction_3 <- predict(prune_decision_tree_3, test_lineups)

test_lineups_error_3 <- cbind(test_lineups, test_decision_tree_prediction_3)
test_lineups_error_3 <- test_lineups_error_3 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - test_decision_tree_prediction_3)^2)
test_decision_MSE_3 <- sum(test_lineups_error_3$PT_DIFF_SQUARE_ERROR)/72


#Prunes tree to 4 nodes
prune_decision_tree_4 <- prune.tree(decision_tree ,best=4)
plot(prune_decision_tree_4)
text(prune_decision_tree_4 ,pretty=0)

#Another prediction with prune = 4 nodes
decision_tree_prediction_4 <- predict(prune_decision_tree_4, train_lineups)

train_lineups_error_4 <- cbind(train_lineups, decision_tree_prediction_4)
train_lineups_error_4 <- train_lineups_error_4 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - decision_tree_prediction_4)^2)
train_decision_MSE_4 <- sum(train_lineups_error_4$PT_DIFF_SQUARE_ERROR)/302


#Another prediction with prune = 4 nodes
test_decision_tree_prediction_4 <- predict(prune_decision_tree_4, test_lineups)

test_lineups_error_4 <- cbind(test_lineups, test_decision_tree_prediction_4)
test_lineups_error_4 <- test_lineups_error_4 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - test_decision_tree_prediction_4)^2)
test_decision_MSE_4 <- sum(test_lineups_error_4$PT_DIFF_SQUARE_ERROR)/72

#Prunes tree to 5 nodes
prune_decision_tree_5 <- prune.tree(decision_tree ,best=5)
plot(prune_decision_tree_5)
text(prune_decision_tree_5 ,pretty=0)

#Another prediction with prune = 5 nodes
decision_tree_prediction_5 <- predict(prune_decision_tree_5, train_lineups)

train_lineups_error_5 <- cbind(train_lineups, decision_tree_prediction_5)
train_lineups_error_5 <- train_lineups_error_5 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - decision_tree_prediction_5)^2)
train_decision_MSE_5 <- sum(train_lineups_error_5$PT_DIFF_SQUARE_ERROR)/302

#Another prediction with prune = 5 nodes
test_decision_tree_prediction_5 <- predict(prune_decision_tree_5, test_lineups)

test_lineups_error_5 <- cbind(test_lineups, test_decision_tree_prediction_5)
test_lineups_error_5 <- test_lineups_error_5 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - test_decision_tree_prediction_5)^2)
test_decision_MSE_5 <- sum(test_lineups_error_5$PT_DIFF_SQUARE_ERROR)/72


#Prunes tree to 6 nodes
prune_decision_tree_6 <- prune.tree(decision_tree ,best=6)
plot(prune_decision_tree_6)
text(prune_decision_tree_6 ,pretty=0)

decision_tree_prediction_6 <- predict(prune_decision_tree_6, train_lineups)

train_lineups_error_6 <- cbind(train_lineups, decision_tree_prediction_6)
train_lineups_error_6 <- train_lineups_error_6 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - decision_tree_prediction_6)^2)
train_decision_MSE_6 <- sum(train_lineups_error_6$PT_DIFF_SQUARE_ERROR)/302

#Another prediction with prune = 6 nodes
test_decision_tree_prediction_6 <- predict(prune_decision_tree_6, test_lineups)

test_lineups_error_6 <- cbind(test_lineups, test_decision_tree_prediction_6)
test_lineups_error_6 <- test_lineups_error_6 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - test_decision_tree_prediction_6)^2)
test_decision_MSE_6 <- sum(test_lineups_error_6$PT_DIFF_SQUARE_ERROR)/72



#Prunes tree to 7 nodes
prune_decision_tree_7 <- prune.tree(decision_tree ,best=7)
plot(prune_decision_tree_7)
text(prune_decision_tree_7 ,pretty=0)

decision_tree_prediction_7 <- predict(prune_decision_tree_7, train_lineups)

train_lineups_error_7 <- cbind(train_lineups, decision_tree_prediction_7)
train_lineups_error_7 <- train_lineups_error_7 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - decision_tree_prediction_7)^2)
train_decision_MSE_7 <- sum(train_lineups_error_7$PT_DIFF_SQUARE_ERROR)/302


#Another prediction with prune = 7 nodes
test_decision_tree_prediction_7 <- predict(prune_decision_tree_7, test_lineups)

test_lineups_error_7 <- cbind(test_lineups, test_decision_tree_prediction_7)
test_lineups_error_7 <- test_lineups_error_7 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - test_decision_tree_prediction_7)^2)
test_decision_MSE_7 <- sum(test_lineups_error_7$PT_DIFF_SQUARE_ERROR)/72



#Prunes tree to 8 nodes
prune_decision_tree_8 <- prune.tree(decision_tree ,best=8)
plot(prune_decision_tree_8)
text(prune_decision_tree_8 ,pretty=0)

#Another prediction with prune = 8 nodes
decision_tree_prediction_8 <- predict(prune_decision_tree_8, train_lineups)


train_lineups_error_8 <- cbind(train_lineups, decision_tree_prediction_8)
train_lineups_error_8 <- train_lineups_error_8 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - decision_tree_prediction_8)^2)
train_decision_MSE_8 <- sum(train_lineups_error_8$PT_DIFF_SQUARE_ERROR)/302


#Another prediction with prune = 8 nodes
test_decision_tree_prediction_8 <- predict(prune_decision_tree_8, test_lineups)

test_lineups_error_8 <- cbind(test_lineups, test_decision_tree_prediction_8)
test_lineups_error_8 <- test_lineups_error_8 %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - test_decision_tree_prediction_8)^2)
test_decision_MSE_8 <- sum(test_lineups_error_8$PT_DIFF_SQUARE_ERROR)/72



decision_data_frame <- data.frame("Number of NodesNodes" = 2:8, 
                               "MSE" = c(train_decision_MSE_2, train_decision_MSE_3, train_decision_MSE_4, train_decision_MSE_5, train_decision_MSE_6, train_decision_MSE_7, train_decision_MSE_8),
                               "test_MSE" = c(test_decision_MSE_2, test_decision_MSE_3, test_decision_MSE_4, test_decision_MSE_5, test_decision_MSE_6, test_decision_MSE_7,test_decision_MSE_8))


plot(decision_data_frame$Number.of.NodesNodes,decision_data_frame$MSE, type = 'b')
plot(decision_data_frame$Number.of.NodesNodes,decision_data_frame$test_MSE,type = 'b')



### Random Forest Section ###

# Generate a new refined tree using Random Forests
set.seed (1)
bag_lineup=randomForest(TOT_PT_DIFF ~ FG_PCT_DIFF + X3P_PCT_DIFF + REB + BLK + STL + AST + TOV,
                        data = train_lineups, mtry=8,importance =TRUE)

plot(bag_lineup)

# Prediction function that determines plus minus of train data using training decision tree
bag_lineup_prediction_train <- predict(bag_lineup, train_lineups)

#Computes mean squared error based on training data
bag_train_lineups_error <- cbind(train_lineups, predicted_PLUS_MINUS = bag_lineup_prediction_train)
bag_train_lineups_error <- train_lineups_error %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - bag_lineup_prediction_train)^2)
bag_train_decision_MSE <- sum(bag_train_lineups_error$PT_DIFF_SQUARE_ERROR)/302


### TEST DATA ###

# Prediction function that determines plus minus of test data using training decision tree
bag_lineup_prediction_test <- predict(bag_lineup, test_lineups)

#Computes mean squared error based on training data
bag_test_lineups_error <- cbind(test_lineups, predicted_PLUS_MINUS = bag_lineup_prediction_test)
bag_test_lineups_error <- test_lineups_error %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - bag_lineup_prediction_test)^2)
bag_test_decision_MSE <- sum(bag_test_lineups_error$PT_DIFF_SQUARE_ERROR)/72



### BOOSTING ###
boost_lineup <- gbm(TOT_PT_DIFF ~ FG_PCT_DIFF + X3P_PCT_DIFF + REB + BLK + STL + AST + TOV,
                 data = train_lineups, distribution="gaussian",n.trees=5000, interaction.depth=4)
plot(boost_lineup)

# Prediction function that determines plus minus of train data using training decision tree
boost_lineup_prediction_train <- predict(boost_lineup, train_lineups, n.trees=5000)

#Computes mean squared error based on training data
boost_train_lineups_error <- cbind(train_lineups, predicted_PLUS_MINUS = boost_lineup_prediction_train)
boost_train_lineups_error <- train_lineups_error %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - boost_lineup_prediction_train)^2)
boost_train_decision_MSE <- sum(boost_train_lineups_error$PT_DIFF_SQUARE_ERROR)/302


# Prediction function that determines plus minus of test data using training decision tree
boost_lineup_prediction_test <- predict(boost_lineup, test_lineups, n.trees=5000)

#Computes mean squared error based on training data
boost_test_lineups_error <- cbind(test_lineups, predicted_PLUS_MINUS = boost_lineup_prediction_test)
boost_test_lineups_error <- test_lineups_error %>% mutate(PT_DIFF_SQUARE_ERROR = (TOT_PT_DIFF - boost_lineup_prediction_test)^2)
boost_test_decision_MSE <- sum(boost_test_lineups_error$PT_DIFF_SQUARE_ERROR)/72
