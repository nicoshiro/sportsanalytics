# Required Libraries
library(expss)
library(readr)
library(rpart)
library(gbm)
library(randomForest)
library(adabag)
library(dplyr)
library(e1071)
library(stringr)
library(stringi)

### INDIVIDUAL PLAYER SETUP ###
# modifications to load all players
player_train_filtered <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/player ID/player_train_filtered_for_lineups.csv")
player_test_filtered <- read_csv("~/Desktop/sportsanalytics-master/data/player_test_for_lineups.csv")

# remove players who haven't played more than 50 minutes
train_fifty <- rbind(player_train_filtered,player_test_filtered )
train_fifty[is.na(train_fifty)] <- 0
train_fifty <- train_fifty[!duplicated(train_fifty), ]

#### LINEUP DATA SETUP ###
# load the full lineup stats (training)
train_for_lineups_full_stats <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/train_for_lineups_full.csv")

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


#Data frame from which desired team will be tested on.
#Data frame usually obtained from 'template_team.R'
lakers_stats <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/lakers_stats.csv")
lakers_stats <-lakers_stats %>% dplyr::select(-X1)


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

#INFO on the classification
plot(decision_tree)
text(decision_tree)
summary(decision_tree)

#Predicts which lineups are elite/terrible 
lakers_decision_test <- predict(decision_tree, lakers_stats, type = "class") 

#Adds lineup predictions to original lineup data frame
lakers_decision_check <- cbind(lakers_stats, predicted = lakers_decision_test)

#Random Forest generated from training data
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

#Predicts which lineups are elite/terrible 
#Adds lineup predictions to original lineup data frame
lakers_forest_test <- predict(forest, lakers_stats, type = "class") 
lakers_forest_check <- cbind(lakers_stats, predicted = lakers_forest_test)

#Boosting generated by training data
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

#Predicts which lineups are elite/terrible 
#Adds lineup predictions to original lineup data frame
lakers_boost_test <- predict(boost, as.data.frame(lakers_stats))
lakers_boost_check <- cbind(lakers_stats, predicted = lakers_boost_test$class)

#Support Vector machine generated by training data
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

#Predicts which lineups are elite/terrible 
#Adds lineup predictions to original lineup data frame
lakers_support_test <- predict(support, lakers_stats, type = "class") 
lakers_support_check <- cbind(lakers_stats, predicted = lakers_support_test)

# k- nearest neighbors algorithms with k = 6, you can change k to experiment with overall results
library(class)
set.seed(5)
neighbors <- knn(total_data[-1:-2], lakers_stats[-1], cl = total_data$level, k = 6)

#Adds lineup predictions to original lineup data frame
lakers_neighbors_check <- cbind(lakers_stats, predicted = neighbors)

# Logisitic Regression Model fitted to training data
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

#Predicts which lineups are elite/terrible 
#Adds lineup predictions to original lineup data frame
#'logistic_pred >.05' : if probability is lowered, model is more lienent on what is considered elite
logistic_pred <- predict(logistic, test_total_data, type = "response")
glm_pred <- rep("elite",792)
glm_pred[logistic_pred >.05]="terrible"


library(MASS)

#lda fitted to training data
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

#Predicts which lineups are elite/terrible 
#Adds lineup predictions to original lineup data frame
lakers_lda_test <- predict(lda_check, lakers_stats, type = "class") 
lakers_lda_check <- cbind(lakers_stats, predicted = lakers_lda_test$class)

# running_vote is a data frame that combines each model's predicted classification for the lineups in question
# running_vote will be used to count 'votes' for each lineup 
running_vote <- data.frame("predicted_decision" = lakers_decision_check$predicted)
running_vote <- cbind(running_vote,  predicted_forest = lakers_forest_check$predicted)
running_vote <- cbind(running_vote,  predicted_boost = lakers_boost_test$class)
running_vote <- cbind(running_vote,  predicted_svm = lakers_support_check$predicted)
running_vote <- cbind(running_vote,  predicted_lda = lakers_lda_test$class)
running_vote <- cbind(running_vote, predicted_neighbors = lakers_neighbors_check$predicted)
running_vote <- cbind(running_vote, predicted_logistic = glm_pred)

#A for loop that determines whether a lineup is considered elite/terrible
#To maximize the accuracy of lineups predicted elite that are actually elite, all 7 models must predict elite
run <- data.frame(run = 1)
for (i in 1:792) {
  if (sum(str_count(as.character(as.matrix(running_vote[1:7][i,])),"elite")) >= 7) {
    run <- rbind(run, "elite")}
  else {
    run <- rbind(run, "decent")
  }
}


#adds column to lineup stats that shows model's predicted lineup classification
predicted <- cbind(final_vote = run[-1,], lakers_stats )
elite_lakers <- (predicted %>% filter(final_vote == "elite"))
dim(elite_lakers)


#Code gives the frequency from which each player appears in elite lineups
#Below is a sample from the lakers 2018-2019 data, replace names with desired team
#I reccomend copying and pasting names from 'train_fifty'
NAMES <- c("Lonzo Ball", "Kentavious Caldwell-Pope", "Alex Caruso", "Josh Hart", "Brandon Ingram", "LeBron James", "Kyle Kuzma", "JaVale McGee", "Rajon Rondo", "Lance Stephenson", "Ivica Zubac", "Michael Beasley")
lakers_freq <- data.frame(NAMES)
lakers_freq$FREQ <- 0

for (i in 1:41){
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "Lonzo")== TRUE){
    lakers_freq$FREQ[1] <- lakers_freq$FREQ[1] + 1
  }
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "Kentavious")== TRUE){
    lakers_freq$FREQ[2] <- lakers_freq$FREQ[2] + 1
  }
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "Alex")== TRUE){
    lakers_freq$FREQ[3] <- lakers_freq$FREQ[3] + 1
  }
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "Josh")== TRUE){
    lakers_freq$FREQ[4] <- lakers_freq$FREQ[4] + 1
  }
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "Brandon")== TRUE){
    lakers_freq$FREQ[5] <- lakers_freq$FREQ[5] + 1
  }
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "LeBron")== TRUE){
    lakers_freq$FREQ[6] <- lakers_freq$FREQ[6] + 1
  }
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "Kyle")== TRUE){
    lakers_freq$FREQ[7] <- lakers_freq$FREQ[7] + 1
  }
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "JaVale")== TRUE){
    lakers_freq$FREQ[8] <- lakers_freq$FREQ[8] + 1
  }
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "Rondo")== TRUE){
    lakers_freq$FREQ[9] <- lakers_freq$FREQ[9] + 1
  }
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "Lance")== TRUE){
    lakers_freq$FREQ[10] <- lakers_freq$FREQ[10] + 1
  }
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "Ivica")== TRUE){
    lakers_freq$FREQ[11] <- lakers_freq$FREQ[11] + 1
  }
  if (stri_detect_fixed(elite_lakers$lakers_combos_vector[i], "Michael")== TRUE){
    lakers_freq$FREQ[12] <- lakers_freq$FREQ[12] + 1
  }}





