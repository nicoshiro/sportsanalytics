library(expss)
library(readr)
library(rpart)
library(gbm)
library(randomForest)
library(adabag)
library(dplyr)
library(e1071)
library(stringr)
### INDIVIDUAL PLAYER SETUP ###
# modifications to load all players
player_train_filtered <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/player ID/player_train_filtered_for_lineups.csv")

# remove players who haven't played more than 50 minutes
train_fifty <- player_train_filtered %>% filter(MIN > 50) 

#### LINEUP DATA SETUP ###
# load the full lineup stats (training)
train_for_lineups_full_stats <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/train_for_lineups_full.csv")

#Exclude non-numeric data
all_lineups <- train_for_lineups_full_stats %>% dplyr::select(-LINEUP, -TOTAL_MIN, -X1, -X, -TOT_PT_DIFF)

# TESTING SET
test_for_lineups_full_stats <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/test_for_lineups_full.csv")
test_lineups <- test_for_lineups_full_stats %>% dplyr::select(-LINEUP, -TOTAL_MIN, -X1, -X, -TOT_PT_DIFF)

#Make sure that the 'arm' and 'MASS' packages are not loaded before proceeding with the next line of code. 
# Code will not work unless 'arm' and 'MASS' are detatched 
source('prep_for_tree_revised.R')

# Creates a data frame for the training lineup data which is composed of individual player stats from members of the lineup sorted from lowest to highest for a particular stat
decision_data <- cbind(train_for_lineups_full_stats, linep_1_test)
decision_data <-decision_data %>% dplyr::select(-(X1:TOTAL_MIN), -(FG_PCT_DIFF:TOV))

# Assigns classification to each lineup in training set: either "elite" or "terrible"
# elite if plus minus > 0, terrible if plus minus < 0 
elite_lineup <- decision_data %>% filter(TOT_PT_DIFF >= 0) %>% mutate(level = as.factor("elite"))
terrible_lineup <- decision_data %>% filter(TOT_PT_DIFF < 0) %>% mutate(level = as.factor("terrible"))
total_data <- rbind(elite_lineup, terrible_lineup)[,c(142,1:141)]
total_data[is.na(total_data)] <- 0


#Same procedure as before, now for testing data
test_data <- cbind(test_for_lineups_full_stats, linep_1_testing)
test_data <-test_data %>% dplyr::select(-(X1:TOTAL_MIN), -(FG_PCT_DIFF:TOV))

test_elite_lineup <- test_data %>% filter(TOT_PT_DIFF >=0) %>% mutate(level = as.factor("elite"))
test_terrible_lineup <- test_data %>% filter(TOT_PT_DIFF < 0) %>% mutate(level = as.factor("terrible"))
test_total_data <- rbind(test_elite_lineup, test_terrible_lineup)[,c(142,1:141)]
test_total_data[is.na(test_total_data)] <- 0

# Classification tree generate from training data
set.seed(4)
decision_tree <- rpart(level ~ FGM_1+ FGM_2+ FGM_3+ FGM_4+ FGM_5+
                         FGA_1+ FGA_2+ FGA_3+ FGA_4+ FGA_5+
                         FG_PCT_1+ FG_PCT_2+ FG_PCT_3+ FG_PCT_4+ FG_PCT_5+
                         FG3M_1+ FG3M_2+ FG3M_3+ FG3M_4+ FG3M_5+
                         FG3A_1+ FG3A_2+ FG3A_3+ FG3A_4+ FG3A_5+
                         FG3_PCT_1+ FG3_PCT_2+ FG3_PCT_3+ FG3_PCT_4+ FG3_PCT_5+
                         FTM_1+FTM_2+ FTM_3+ FTM_4+ FTM_5+
                         FTA_1+ FTA_2+ FTA_3+ FTA_4+ FTA_5+
                         FT_PCT_1+ FT_PCT_2+ FT_PCT_3+ FT_PCT_4+ FT_PCT_5+
                         OREB_1+ OREB_2+ OREB_3+ OREB_4+ OREB_5+
                         DREB_1+ DREB_2+ DREB_3+ DREB_4+ DREB_5+
                         AST_1+AST_2+ AST_3+ AST_4+ AST_5+
                         TOV_1+ TOV_2+ TOV_3+ TOV_4+ TOV_5+
                         STL_1+ STL_2+ STL_3+ STL_4+ STL_5+
                         BLK_1+ BLK_2+ BLK_3+ BLK_4+ BLK_5+
                         BLKA_1+ BLKA_2+ BLKA_3+ BLKA_4+ BLKA_5+
                         PF_1+ PF_2+ PF_3+ PF_4+ PF_5+
                         PFD_1+ PFD_2+ PFD_3+ PFD_4+ PFD_5+
                         PTS_1+ PTS_2+ PTS_3+ PTS_4+ PTS_5+
                         PLUS_MINUS_1+ PLUS_MINUS_2+ PLUS_MINUS_3+ PLUS_MINUS_4+ PLUS_MINUS_5 +
                         CONTESTED_SHOTS_1+ CONTESTED_SHOTS_2+ CONTESTED_SHOTS_3+ CONTESTED_SHOTS_4+ CONTESTED_SHOTS_5+
                         CONTESTED_SHOTS_2PT_1+ CONTESTED_SHOTS_2PT_2+ CONTESTED_SHOTS_2PT_3+ CONTESTED_SHOTS_2PT_4+ CONTESTED_SHOTS_2PT_5+
                         CONTESTED_SHOTS_3PT_1+ CONTESTED_SHOTS_3PT_2+ CONTESTED_SHOTS_3PT_3+ CONTESTED_SHOTS_3PT_4+ CONTESTED_SHOTS_3PT_5+
                         DEFLECTIONS_1+ DEFLECTIONS_2+ DEFLECTIONS_3+ DEFLECTIONS_4+ DEFLECTIONS_5+
                         LOOSE_BALLS_RECOVERED_1+ LOOSE_BALLS_RECOVERED_2+ LOOSE_BALLS_RECOVERED_3+ LOOSE_BALLS_RECOVERED_4+ LOOSE_BALLS_RECOVERED_5+
                         SCREEN_ASSISTS_1+ SCREEN_ASSISTS_2+ SCREEN_ASSISTS_3+ SCREEN_ASSISTS_4+ SCREEN_ASSISTS_5+
                         BOX_OUTS_1+ BOX_OUTS_2+ BOX_OUTS_3+ BOX_OUTS_4+ BOX_OUTS_5,data = total_data, method = "class")

plot(decision_tree)
text(decision_tree)
summary(decision_tree)

# Prediction on training data
decision_tree_prediction_train <- predict(decision_tree, total_data, type = "class")

#Prediction column binded to training data
decision_check <- cbind(total_data, predicted = decision_tree_prediction_train)[,c(143, 1:142)]

#Shows model errors
dim(decision_check %>% filter(decision_check$level != decision_check$predicted))

# Prediction on testing data
decision_tree_prediction_test <- predict(decision_tree, test_total_data, type = "class")

#Prediction column binded to testing data
test_decision_check <- cbind(test_total_data, predicted = decision_tree_prediction_test)[,c(143, 1:142)]

#Shows model errors
test_decision_check %>% filter(test_decision_check$level != test_decision_check$predicted)

#Apply Labels to data frame
test_decision_check = apply_labels(test_decision_check,
                                   predicted = "predicted level",
                                   level = "actual level")

#Confusion Matrix
cro(test_decision_check$predicted, test_decision_check$level)

# Overall accuracy for classification
accuracy_decision <- dim(test_decision_check %>% filter(test_decision_check$level == test_decision_check$predicted))[1] / dim(test_total_data)[1]

# The following code prunes the decision tree but performs the same kind of analysis regarding the training/testing data
printcp(decision_tree)
pfit<- prune(decision_tree, cp= 0.025000 ) # from cptable  
plot(pfit)
text(pfit)

pfit_tree_prediction_train <- predict(pfit, total_data, type = "class")
pfit_check <- cbind(total_data, predicted = pfit_tree_prediction_train)[,c(143, 1:142)]
pfit_check %>% filter(pfit_check$level != pfit_check$predicted)

pfit_tree_prediction_test <- predict(pfit, test_total_data, type = "class")
test_pfit_check <- cbind(test_total_data, predicted = pfit_tree_prediction_test)[,c(143, 1:142)]
test_pfit_check %>% filter(test_pfit_check$level != test_pfit_check$predicted)

test_pfit_check = apply_labels(test_pfit_check,
                               predicted = "predicted level",
                               level = "actual level")

cro(test_pfit_check$predicted, test_pfit_check$level)

accuracy_pfit <- dim(test_pfit_check %>% filter(test_pfit_check$level == test_pfit_check$predicted))[1] / dim(test_total_data)[1]



### THE NEXT CHUNKS OF CODE FOLLOW THE SAME FLOW AS DEMONSTRATED FRO CLASSIFICATION TREES, REFER BACK FOR ASSISTANCE
set.seed(1)
forest <- randomForest(level ~ FGM_1+ FGM_2+ FGM_3+ FGM_4+ FGM_5+
                         FGA_1+ FGA_2+ FGA_3+ FGA_4+ FGA_5+
                         FG_PCT_1+ FG_PCT_2+ FG_PCT_3+ FG_PCT_4+ FG_PCT_5+
                         FG3M_1+ FG3M_2+ FG3M_3+ FG3M_4+ FG3M_5+
                         FG3A_1+ FG3A_2+ FG3A_3+ FG3A_4+ FG3A_5+
                         FG3_PCT_1+ FG3_PCT_2+ FG3_PCT_3+ FG3_PCT_4+ FG3_PCT_5+
                         FTM_1+FTM_2+ FTM_3+ FTM_4+ FTM_5+
                         FTA_1+ FTA_2+ FTA_3+ FTA_4+ FTA_5+
                         FT_PCT_1+ FT_PCT_2+ FT_PCT_3+ FT_PCT_4+ FT_PCT_5+
                         OREB_1+ OREB_2+ OREB_3+ OREB_4+ OREB_5+
                         DREB_1+ DREB_2+ DREB_3+ DREB_4+ DREB_5+
                         AST_1+AST_2+ AST_3+ AST_4+ AST_5+
                         TOV_1+ TOV_2+ TOV_3+ TOV_4+ TOV_5+
                         STL_1+ STL_2+ STL_3+ STL_4+ STL_5+
                         BLK_1+ BLK_2+ BLK_3+ BLK_4+ BLK_5+
                         BLKA_1+ BLKA_2+ BLKA_3+ BLKA_4+ BLKA_5+
                         PF_1+ PF_2+ PF_3+ PF_4+ PF_5+
                         PFD_1+ PFD_2+ PFD_3+ PFD_4+ PFD_5+
                         PTS_1+ PTS_2+ PTS_3+ PTS_4+ PTS_5+
                         PLUS_MINUS_1+ PLUS_MINUS_2+ PLUS_MINUS_3+ PLUS_MINUS_4+ PLUS_MINUS_5 +
                         CONTESTED_SHOTS_1+ CONTESTED_SHOTS_2+ CONTESTED_SHOTS_3+ CONTESTED_SHOTS_4+ CONTESTED_SHOTS_5+
                         CONTESTED_SHOTS_2PT_1+ CONTESTED_SHOTS_2PT_2+ CONTESTED_SHOTS_2PT_3+ CONTESTED_SHOTS_2PT_4+ CONTESTED_SHOTS_2PT_5+
                         CONTESTED_SHOTS_3PT_1+ CONTESTED_SHOTS_3PT_2+ CONTESTED_SHOTS_3PT_3+ CONTESTED_SHOTS_3PT_4+ CONTESTED_SHOTS_3PT_5+
                         DEFLECTIONS_1+ DEFLECTIONS_2+ DEFLECTIONS_3+ DEFLECTIONS_4+ DEFLECTIONS_5+
                         LOOSE_BALLS_RECOVERED_1+ LOOSE_BALLS_RECOVERED_2+ LOOSE_BALLS_RECOVERED_3+ LOOSE_BALLS_RECOVERED_4+ LOOSE_BALLS_RECOVERED_5+
                         SCREEN_ASSISTS_1+ SCREEN_ASSISTS_2+ SCREEN_ASSISTS_3+ SCREEN_ASSISTS_4+ SCREEN_ASSISTS_5+
                         BOX_OUTS_1+ BOX_OUTS_2+ BOX_OUTS_3+ BOX_OUTS_4+ BOX_OUTS_5,data = total_data, n.tree = 500)

forest_train <- predict(forest, total_data, type = "class", n.tree = 50)
forest_check <- cbind(total_data, predicted = forest_train)[,c(143, 1:142)]
forest_check %>% filter(forest_check$level != forest_check$predicted)

test_forest_test <- predict(forest, test_total_data, type = "class", n.tree = 500)
test_forest_check <- cbind(test_total_data, predicted = test_forest_test)[,c(143, 1:142)]
test_forest_check %>% filter(test_forest_check$level != test_forest_check$predicted)


test_forest_check = apply_labels(test_forest_check,
                                 predicted = "predicted level",
                                 level = "actual level")

cro(test_forest_check$predicted, test_forest_check$level)

accuracy_forest <- dim(test_forest_check %>% filter(test_forest_check$level == test_forest_check$predicted))[1] / dim(test_total_data)[1]

set.seed(250)
boost <- boosting(level ~ FGM_1+ FGM_2+ FGM_3+ FGM_4+ FGM_5+
                    FGA_1+ FGA_2+ FGA_3+ FGA_4+ FGA_5+
                    FG_PCT_1+ FG_PCT_2+ FG_PCT_3+ FG_PCT_4+ FG_PCT_5+
                    FG3M_1+ FG3M_2+ FG3M_3+ FG3M_4+ FG3M_5+
                    FG3A_1+ FG3A_2+ FG3A_3+ FG3A_4+ FG3A_5+
                    FG3_PCT_1+ FG3_PCT_2+ FG3_PCT_3+ FG3_PCT_4+ FG3_PCT_5+
                    FTM_1+FTM_2+ FTM_3+ FTM_4+ FTM_5+
                    FTA_1+ FTA_2+ FTA_3+ FTA_4+ FTA_5+
                    FT_PCT_1+ FT_PCT_2+ FT_PCT_3+ FT_PCT_4+ FT_PCT_5+
                    OREB_1+ OREB_2+ OREB_3+ OREB_4+ OREB_5+
                    DREB_1+ DREB_2+ DREB_3+ DREB_4+ DREB_5+
                    AST_1+AST_2+ AST_3+ AST_4+ AST_5+
                    TOV_1+ TOV_2+ TOV_3+ TOV_4+ TOV_5+
                    STL_1+ STL_2+ STL_3+ STL_4+ STL_5+
                    BLK_1+ BLK_2+ BLK_3+ BLK_4+ BLK_5+
                    BLKA_1+ BLKA_2+ BLKA_3+ BLKA_4+ BLKA_5+
                    PF_1+ PF_2+ PF_3+ PF_4+ PF_5+
                    PFD_1+ PFD_2+ PFD_3+ PFD_4+ PFD_5+
                    PTS_1+ PTS_2+ PTS_3+ PTS_4+ PTS_5+
                    PLUS_MINUS_1+ PLUS_MINUS_2+ PLUS_MINUS_3+ PLUS_MINUS_4+ PLUS_MINUS_5 +
                    CONTESTED_SHOTS_1+ CONTESTED_SHOTS_2+ CONTESTED_SHOTS_3+ CONTESTED_SHOTS_4+ CONTESTED_SHOTS_5+
                    CONTESTED_SHOTS_2PT_1+ CONTESTED_SHOTS_2PT_2+ CONTESTED_SHOTS_2PT_3+ CONTESTED_SHOTS_2PT_4+ CONTESTED_SHOTS_2PT_5+
                    CONTESTED_SHOTS_3PT_1+ CONTESTED_SHOTS_3PT_2+ CONTESTED_SHOTS_3PT_3+ CONTESTED_SHOTS_3PT_4+ CONTESTED_SHOTS_3PT_5+
                    DEFLECTIONS_1+ DEFLECTIONS_2+ DEFLECTIONS_3+ DEFLECTIONS_4+ DEFLECTIONS_5+
                    LOOSE_BALLS_RECOVERED_1+ LOOSE_BALLS_RECOVERED_2+ LOOSE_BALLS_RECOVERED_3+ LOOSE_BALLS_RECOVERED_4+ LOOSE_BALLS_RECOVERED_5+
                    SCREEN_ASSISTS_1+ SCREEN_ASSISTS_2+ SCREEN_ASSISTS_3+ SCREEN_ASSISTS_4+ SCREEN_ASSISTS_5+
                    BOX_OUTS_1+ BOX_OUTS_2+ BOX_OUTS_3+ BOX_OUTS_4+ BOX_OUTS_5,data = total_data, n.tree = 5000, distribution = "gaussian")

boost_train <- predict(boost, total_data)
boost_check <- cbind(total_data, predicted = boost_train$class)[,c(143, 1:142)]
boost_check %>% filter(boost_check$level != boost_check$predicted)

test_boost_test <- predict(boost, test_total_data[,c(-1,-2)])
test_boost_check <- cbind(test_total_data, predicted = test_boost_test$class)[,c(143, 1:142)]
test_boost_check %>% filter(test_boost_check$level != test_boost_check$predicted)


test_boost_check = apply_labels(test_boost_check,
                                predicted = "predicted level",
                                level = "actual level")

cro(test_boost_check$predicted, test_boost_check$level)

accuracy_boost <- dim(test_boost_check %>% filter(test_boost_check$level == test_boost_check$predicted))[1] / dim(test_total_data)[1]

set.seed(2)
support <- svm(level ~ FGM_1+ FGM_2+ FGM_3+ FGM_4+ FGM_5+
                 FGA_1+ FGA_2+ FGA_3+ FGA_4+ FGA_5+
                 FG_PCT_1+ FG_PCT_2+ FG_PCT_3+ FG_PCT_4+ FG_PCT_5+
                 FG3M_1+ FG3M_2+ FG3M_3+ FG3M_4+ FG3M_5+
                 FG3A_1+ FG3A_2+ FG3A_3+ FG3A_4+ FG3A_5+
                 FG3_PCT_1+ FG3_PCT_2+ FG3_PCT_3+ FG3_PCT_4+ FG3_PCT_5+
                 FTM_1+FTM_2+ FTM_3+ FTM_4+ FTM_5+
                 FTA_1+ FTA_2+ FTA_3+ FTA_4+ FTA_5+
                 FT_PCT_1+ FT_PCT_2+ FT_PCT_3+ FT_PCT_4+ FT_PCT_5+
                 OREB_1+ OREB_2+ OREB_3+ OREB_4+ OREB_5+
                 DREB_1+ DREB_2+ DREB_3+ DREB_4+ DREB_5+
                 AST_1+AST_2+ AST_3+ AST_4+ AST_5+
                 TOV_1+ TOV_2+ TOV_3+ TOV_4+ TOV_5+
                 STL_1+ STL_2+ STL_3+ STL_4+ STL_5+
                 BLK_1+ BLK_2+ BLK_3+ BLK_4+ BLK_5+
                 BLKA_1+ BLKA_2+ BLKA_3+ BLKA_4+ BLKA_5+
                 PF_1+ PF_2+ PF_3+ PF_4+ PF_5+
                 PFD_1+ PFD_2+ PFD_3+ PFD_4+ PFD_5+
                 PTS_1+ PTS_2+ PTS_3+ PTS_4+ PTS_5+
                 PLUS_MINUS_1+ PLUS_MINUS_2+ PLUS_MINUS_3+ PLUS_MINUS_4+ PLUS_MINUS_5 +
                 CONTESTED_SHOTS_1+ CONTESTED_SHOTS_2+ CONTESTED_SHOTS_3+ CONTESTED_SHOTS_4+ CONTESTED_SHOTS_5+
                 CONTESTED_SHOTS_2PT_1+ CONTESTED_SHOTS_2PT_2+ CONTESTED_SHOTS_2PT_3+ CONTESTED_SHOTS_2PT_4+ CONTESTED_SHOTS_2PT_5+
                 CONTESTED_SHOTS_3PT_1+ CONTESTED_SHOTS_3PT_2+ CONTESTED_SHOTS_3PT_3+ CONTESTED_SHOTS_3PT_4+ CONTESTED_SHOTS_3PT_5+
                 DEFLECTIONS_1+ DEFLECTIONS_2+ DEFLECTIONS_3+ DEFLECTIONS_4+ DEFLECTIONS_5+
                 LOOSE_BALLS_RECOVERED_1+ LOOSE_BALLS_RECOVERED_2+ LOOSE_BALLS_RECOVERED_3+ LOOSE_BALLS_RECOVERED_4+ LOOSE_BALLS_RECOVERED_5+
                 SCREEN_ASSISTS_1+ SCREEN_ASSISTS_2+ SCREEN_ASSISTS_3+ SCREEN_ASSISTS_4+ SCREEN_ASSISTS_5+
                 BOX_OUTS_1+ BOX_OUTS_2+ BOX_OUTS_3+ BOX_OUTS_4+ BOX_OUTS_5,data = total_data, scale=FALSE, kernel = 'linear')

support_train <-predict(support, total_data)
support_check <- cbind(total_data, predicted = support_train)[,c(143, 1:142)]
support_check %>% filter(support_check$level != support_check$predicted)

test_support_test <- predict(support, test_total_data)
test_support_check <- cbind(test_total_data, predicted = test_support_test)[,c(143, 1:142)]
test_support_check %>% filter(test_support_check$level != test_support_check$predicted)

test_support_check = apply_labels(test_support_check,
                                predicted = "predicted level",
                                level = "actual level")

cro(test_support_check$predicted, test_support_check$level)

accuracy_support <- dim(test_support_check %>% filter(test_support_check$level == test_support_check$predicted))[1] / dim(test_total_data)[1]

library(class)
set.seed(5)
neighbors <- knn(total_data[-1:-2], test_total_data[-1:-2], cl = total_data$level, k = 6)
table(neighbors)

#FOR K-NEAREST NEIGHBORS ONLY TESTING PREDICTIONS ARE AVAILABLE
test_neighbors_check <- cbind(test_total_data, neighbors)[,c(143, 1:142)]

test_neighbors_check = apply_labels(test_neighbors_check,
                                    neighbors = "predicted level",
                                    level = "actual level")

cro(test_neighbors_check$neighbors, test_neighbors_check$level)

accuracy_neighbors <- dim(test_neighbors_check %>% filter(test_neighbors_check$level == test_neighbors_check$neighbors))[1] / dim(test_total_data)[1]


library(arm)
logistic <- bayesglm(level ~ FGM_1+ FGM_2+ FGM_3+ FGM_4+ FGM_5+
    FGA_1+ FGA_2+ FGA_3+ FGA_4+ FGA_5+
    FG_PCT_1+ FG_PCT_2+ FG_PCT_3+ FG_PCT_4+ FG_PCT_5+
    FG3M_1+ FG3M_2+ FG3M_3+ FG3M_4+ FG3M_5+
    FG3A_1+ FG3A_2+ FG3A_3+ FG3A_4+ FG3A_5+
    FG3_PCT_1+ FG3_PCT_2+ FG3_PCT_3+ FG3_PCT_4+ FG3_PCT_5+
    FTM_1+FTM_2+ FTM_3+ FTM_4+ FTM_5+
    FTA_1+ FTA_2+ FTA_3+ FTA_4+ FTA_5+
    FT_PCT_1+ FT_PCT_2+ FT_PCT_3+ FT_PCT_4+ FT_PCT_5+
    OREB_1+ OREB_2+ OREB_3+ OREB_4+ OREB_5+
    DREB_1+ DREB_2+ DREB_3+ DREB_4+ DREB_5+
    AST_1+AST_2+ AST_3+ AST_4+ AST_5+
    TOV_1+ TOV_2+ TOV_3+ TOV_4+ TOV_5+
    STL_1+ STL_2+ STL_3+ STL_4+ STL_5+
    BLK_1+ BLK_2+ BLK_3+ BLK_4+ BLK_5+
    BLKA_1+ BLKA_2+ BLKA_3+ BLKA_4+ BLKA_5+
    PF_1+ PF_2+ PF_3+ PF_4+ PF_5+
    PFD_1+ PFD_2+ PFD_3+ PFD_4+ PFD_5+
    PTS_1+ PTS_2+ PTS_3+ PTS_4+ PTS_5+
    PLUS_MINUS_1+ PLUS_MINUS_2+ PLUS_MINUS_3+ PLUS_MINUS_4+ PLUS_MINUS_5 +
    CONTESTED_SHOTS_1+ CONTESTED_SHOTS_2+ CONTESTED_SHOTS_3+ CONTESTED_SHOTS_4+ CONTESTED_SHOTS_5+
    CONTESTED_SHOTS_2PT_1+ CONTESTED_SHOTS_2PT_2+ CONTESTED_SHOTS_2PT_3+ CONTESTED_SHOTS_2PT_4+ CONTESTED_SHOTS_2PT_5+
    CONTESTED_SHOTS_3PT_1+ CONTESTED_SHOTS_3PT_2+ CONTESTED_SHOTS_3PT_3+ CONTESTED_SHOTS_3PT_4+ CONTESTED_SHOTS_3PT_5+
    DEFLECTIONS_1+ DEFLECTIONS_2+ DEFLECTIONS_3+ DEFLECTIONS_4+ DEFLECTIONS_5+
    LOOSE_BALLS_RECOVERED_1+ LOOSE_BALLS_RECOVERED_2+ LOOSE_BALLS_RECOVERED_3+ LOOSE_BALLS_RECOVERED_4+ LOOSE_BALLS_RECOVERED_5+
    SCREEN_ASSISTS_1+ SCREEN_ASSISTS_2+ SCREEN_ASSISTS_3+ SCREEN_ASSISTS_4+ SCREEN_ASSISTS_5+
    BOX_OUTS_1+ BOX_OUTS_2+ BOX_OUTS_3+ BOX_OUTS_4+ BOX_OUTS_5,data = total_data, family = binomial)

logistic_pred <- predict(logistic, test_total_data, type = "response")
glm_pred <- rep("elite",72)
glm_pred[logistic_pred >.05]="terrible"

test_logistic_check <- cbind(test_total_data, predicted = glm_pred)[,c(143, 1:142)]

test_logistic_check = apply_labels(test_logistic_check,
                                    predicted = "predicted level",
                                    level = "actual level")

cro(test_logistic_check$level, test_logistic_check$predicted)

accuracy_neighbors <- dim(test_logistic_check %>% filter(test_logistic_check$level == test_logistic_check$predicted))[1] / dim(test_total_data)[1]


library(MASS)

set.seed(250)
lda_check <-lda(level ~ FGM_1+ FGM_2+ FGM_3+ FGM_4+ FGM_5+
                  FGA_1+ FGA_2+ FGA_3+ FGA_4+ FGA_5+
                  FG_PCT_1+ FG_PCT_2+ FG_PCT_3+ FG_PCT_4+ FG_PCT_5+
                  FG3M_1+ FG3M_2+ FG3M_3+ FG3M_4+ FG3M_5+
                  FG3A_1+ FG3A_2+ FG3A_3+ FG3A_4+ FG3A_5+
                  FG3_PCT_1+ FG3_PCT_2+ FG3_PCT_3+ FG3_PCT_4+ FG3_PCT_5+
                  FTM_1+FTM_2+ FTM_3+ FTM_4+ FTM_5+
                  FTA_1+ FTA_2+ FTA_3+ FTA_4+ FTA_5+
                  FT_PCT_1+ FT_PCT_2+ FT_PCT_3+ FT_PCT_4+ FT_PCT_5+
                  OREB_1+ OREB_2+ OREB_3+ OREB_4+ OREB_5+
                  DREB_1+ DREB_2+ DREB_3+ DREB_4+ DREB_5+
                  AST_1+AST_2+ AST_3+ AST_4+ AST_5+
                  TOV_1+ TOV_2+ TOV_3+ TOV_4+ TOV_5+
                  STL_1+ STL_2+ STL_3+ STL_4+ STL_5+
                  BLK_1+ BLK_2+ BLK_3+ BLK_4+ BLK_5+
                  BLKA_1+ BLKA_2+ BLKA_3+ BLKA_4+ BLKA_5+
                  PF_1+ PF_2+ PF_3+ PF_4+ PF_5+
                  PFD_1+ PFD_2+ PFD_3+ PFD_4+ PFD_5+
                  PTS_1+ PTS_2+ PTS_3+ PTS_4+ PTS_5+
                  PLUS_MINUS_1+ PLUS_MINUS_2+ PLUS_MINUS_3+ PLUS_MINUS_4+ PLUS_MINUS_5 +
                  CONTESTED_SHOTS_1+ CONTESTED_SHOTS_2+ CONTESTED_SHOTS_3+ CONTESTED_SHOTS_4+ CONTESTED_SHOTS_5+
                  CONTESTED_SHOTS_2PT_1+ CONTESTED_SHOTS_2PT_2+ CONTESTED_SHOTS_2PT_3+ CONTESTED_SHOTS_2PT_4+ CONTESTED_SHOTS_2PT_5+
                  CONTESTED_SHOTS_3PT_1+ CONTESTED_SHOTS_3PT_2+ CONTESTED_SHOTS_3PT_3+ CONTESTED_SHOTS_3PT_4+ CONTESTED_SHOTS_3PT_5+
                  DEFLECTIONS_1+ DEFLECTIONS_2+ DEFLECTIONS_3+ DEFLECTIONS_4+ DEFLECTIONS_5+
                  LOOSE_BALLS_RECOVERED_1+ LOOSE_BALLS_RECOVERED_2+ LOOSE_BALLS_RECOVERED_3+ LOOSE_BALLS_RECOVERED_4+ LOOSE_BALLS_RECOVERED_5+
                  SCREEN_ASSISTS_1+ SCREEN_ASSISTS_2+ SCREEN_ASSISTS_3+ SCREEN_ASSISTS_4+ SCREEN_ASSISTS_5+
                  BOX_OUTS_1+ BOX_OUTS_2+ BOX_OUTS_3+ BOX_OUTS_4+ BOX_OUTS_5,data = total_data)
summary(lda_check)

lda_predict <- predict(lda_check, total_data)
lda_predict_check <- cbind(total_data, predicted = lda_predict$class)#[,c(143,144,2:142)]
lda_predict_check %>% filter(lda_predict_check$level != lda_predict_check$predicted)

test_lda_predict <- predict(lda_check, test_total_data)
test_lda_predict_check <- cbind(test_total_data, predicted = test_lda_predict$class)#[,c(143,144,2:142)]
test_lda_predict_check %>% filter(test_lda_predict_check$level != test_lda_predict_check$predicted)



test_lda_predict_check = apply_labels(test_lda_predict_check,
                                      predicted = "predicted level",
                                      level = "actual level")

cro(test_lda_predict_check$predicted, test_lda_predict_check$level)

accuracy_lda <- dim(test_lda_predict_check %>% filter(test_lda_predict_check$level == test_lda_predict_check$predicted))[1] / dim(test_total_data)[1]

#running_vote cbinds every algorithms prediction and assembles it onto a single data frame
running_vote <- test_total_data[1]
running_vote <- cbind(running_vote,  predicted_decision = pfit_tree_prediction_test)
running_vote <- cbind(running_vote,  predicted_forest = test_forest_test)
running_vote <- cbind(running_vote,  predicted_boost = test_boost_test$class)
running_vote <- cbind(running_vote,  predicted_svm = test_support_test)
running_vote <- cbind(running_vote,  predicted_lda = test_lda_predict$class)
running_vote <- cbind(running_vote, predicted_neighbors = test_neighbors_check$neighbors)
running_vote <- cbind(running_vote, predicted_logistic = test_logistic_check$predicted)

# for loop counts the number of votes per lineup
run <- data.frame(run = 1)
for (i in 1:72) {
  if (sum(str_count(as.character(as.matrix(running_vote[2:8][i,])),"elite")) >= 7) {
    run <- rbind(run, "elite")
  } else {
    run <- rbind(run, "terrible")
  }
}

#attaches decision to running_vote
running_vote <- cbind(running_vote, final_vote = run[-1,])

#overall accuracy (low in this case)
overall_accuracy <- dim(running_vote %>% filter(running_vote$level == running_vote$final_vote))[1] / dim(test_total_data)[1]

running_check = apply_labels(running_vote,
                                      final_vote = "predicted level",
                                      level = "actual level")
# Find the precision for elite (prob that an observation is elite given it was predicted to be elite = 10/12)
cro(running_check$level,running_check$final_vote)

