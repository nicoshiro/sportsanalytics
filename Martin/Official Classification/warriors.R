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

# remove players who haven't played more than 50 minutes
train_fifty <- player_train_filtered %>% filter(MIN > 50) 

# get numeric data (excluding result, GS, MP, and GmSc)
train_numeric <- train_fifty %>% dplyr::select(FGM:BOX_OUTS, -POS)

# set NA percentages to 0
train_numeric[is.na(train_numeric)] <- 0

# scale numeric data before clustering
train_scaled <- as.data.frame(scale(train_numeric))

#### LINEUP DATA SETUP ###
# load the full lineup stats (training)
train_for_lineups_full_stats <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/train_for_lineups_full.csv")

#Exclude non-numeric data
all_lineups <- train_for_lineups_full_stats %>% dplyr::select(-LINEUP, -TOTAL_MIN, -X1, -X, -TOT_PT_DIFF)

#Scale the numeric data
lineups_scaled <- scale(all_lineups)

# TESTING SET
test_for_lineups_full_stats <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/test_for_lineups_full.csv")
test_lineups <- test_for_lineups_full_stats %>% dplyr::select(-LINEUP, -TOTAL_MIN, -X1, -X, -TOT_PT_DIFF)

source('prep_for_tree_revised.R')

decision_data <- cbind(train_for_lineups_full_stats, linep_1_test)
decision_data <-decision_data %>% dplyr::select(-(X1:TOTAL_MIN), -(FG_PCT_DIFF:TOV))

elite_lineup <- decision_data %>% filter(TOT_PT_DIFF >= 0) %>% mutate(level = as.factor("elite"))
terrible_lineup <- decision_data %>% filter(TOT_PT_DIFF < 0) %>% mutate(level = as.factor("terrible"))
total_data <- rbind(elite_lineup, terrible_lineup)[,c(142,1:141)]
total_data[is.na(total_data)] <- 0

warriors_stats <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/warriors_stats.csv")
warriors_stats <-warriors_stats %>% dplyr::select(-X1)

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

warriors_decision_test <- predict(decision_tree, warriors_stats, type = "class") 
warriors_decision_check <- cbind(warriors_stats, predicted = warriors_decision_test)


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

warriors_forest_test <- predict(forest, warriors_stats, type = "class") 
warriors_forest_check <- cbind(warriors_stats, predicted = warriors_forest_test)

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

warriors_boost_test <- predict(boost, as.data.frame(warriors_stats))

warriors_boost_check <- cbind(warriors_stats, predicted = warriors_boost_test$class)

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

warriors_support_test <- predict(support, warriors_stats, type = "class") 
warriors_support_check <- cbind(warriors_stats, predicted = warriors_support_test)


library(class)
set.seed(5)
neighbors <- knn(total_data[-1:-2], warriors_stats[-1], cl = total_data$level, k = 6)
table(neighbors)

warriors_neighbors_check <- cbind(warriors_stats, predicted = neighbors)


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

logistic_pred <- predict(logistic, warriors_stats, type = "response")
glm_pred <- rep("elite",462)
glm_pred[logistic_pred >.05]="terrible"


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

warriors_lda_test <- predict(lda_check, warriors_stats, type = "class") 
warriors_lda_check <- cbind(warriors_stats, predicted = warriors_lda_test$class)

cro(lakers_lda_check$predicted, lakers_lda_check$level)

accuracy_lda <- dim(test_lda_predict_check %>% filter(test_lda_predict_check$level == test_lda_predict_check$predicted))[1] / dim(test_total_data)[1]


running_vote <- data.frame("predicted_decision" = warriors_decision_check$predicted)
running_vote <- cbind(running_vote,  predicted_forest = warriors_forest_check$predicted)
running_vote <- cbind(running_vote,  predicted_boost = warriors_boost_test$class)
running_vote <- cbind(running_vote,  predicted_svm = warriors_support_check$predicted)
running_vote <- cbind(running_vote,  predicted_lda = warriors_lda_test$class)
running_vote <- cbind(running_vote, predicted_neighbors = warriors_neighbors_check$predicted)
running_vote <- cbind(running_vote, predicted_logistic = glm_pred)


run <- data.frame(run = 1)
for (i in 1:462) {
  if (sum(str_count(as.character(as.matrix(running_vote[1:7][i,])),"elite")) >= 7) {
    run <- rbind(run, "elite")}
  else {
    run <- rbind(run, "decent")
  }
}


# run_2 <- data.frame(run = 1)
# for (i in 1:462) {
#   if (sum(str_count(as.character(as.matrix(running_vote[1:7][i,])),"terrible")) >= 6) {
#     run_2 <- rbind(run_2, "terrible")
#   } else {
#     run_2 <- rbind(run_2, "decent")
#   }
# }

# predicted_2 <- cbind(lakers_stats, final_vote = run_2[-1,])[,c(141,1:140)]

predicted <- cbind(final_vote = run[-1,], warriors_stats )
elite_warriors <- (predicted %>% filter(final_vote == "elite"))
dim(elite_warriors)

NAMES <- c("Jordan Bell", "DeMarcus Cousins", "Stephen Curry", "Kevin Durant", "Draymond Green", "Andre Iguodala", "Shaun Livingston", "Kevon Looney", "Klay Thompson", "David West", "Nick Young")
warriors_freq <- data.frame(NAMES)
warriors_freq$FREQ <- 0

for (i in 1:238){
  if (stri_detect_fixed(elite_warriors$warriors_combos_vector[i], "Jordan")){
    warriors_freq$FREQ[1] <- warriors_freq$FREQ[1] + 1
  }
  if (stri_detect_fixed(elite_warriors$warriors_combos_vector[i], "DeMarcus")){
    warriors_freq$FREQ[2] <- warriors_freq$FREQ[2] + 1
  }
  if (stri_detect_fixed(elite_warriors$warriors_combos_vector[i], "Stephen")){
    warriors_freq$FREQ[3] <- warriors_freq$FREQ[3] + 1
  }
  if (stri_detect_fixed(elite_warriors$warriors_combos_vector[i], "Kevin")){
    warriors_freq$FREQ[4] <- warriors_freq$FREQ[4] + 1
  }
  if (stri_detect_fixed(elite_warriors$warriors_combos_vector[i], "Draymond")){
    warriors_freq$FREQ[5] <- warriors_freq$FREQ[5] + 1
  }
  if (stri_detect_fixed(elite_warriors$warriors_combos_vector[i], "Andre")){
    warriors_freq$FREQ[6] <- warriors_freq$FREQ[6] + 1
  }
  if (stri_detect_fixed(elite_warriors$warriors_combos_vector[i], "Shaun")){
    warriors_freq$FREQ[7] <- warriors_freq$FREQ[7] + 1
  }
  if (stri_detect_fixed(elite_warriors$warriors_combos_vector[i], "Kevon")){
    warriors_freq$FREQ[8] <- warriors_freq$FREQ[8] + 1
  }
  if (stri_detect_fixed(elite_warriors$warriors_combos_vector[i], "Klay")){
    warriors_freq$FREQ[9] <- warriors_freq$FREQ[9] + 1
  }
  if (stri_detect_fixed(elite_warriors$warriors_combos_vector[i], "David")){
    warriors_freq$FREQ[10] <- warriors_freq$FREQ[10] + 1
  }
  if (stri_detect_fixed(elite_warriors$warriors_combos_vector[i], "Nick")){
    warriors_freq$FREQ[11] <- warriors_freq$FREQ[11] + 1
  }
  
}





