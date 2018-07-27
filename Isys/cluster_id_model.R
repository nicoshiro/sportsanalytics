library(cluster)
library(factoextra)
library(mclust)
library(descr)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(Matching)
library(gmodels)
library(class)
library(listdtr)

setwd("/Users/isysjo/Documents/Summer REU 2018/SportsAnalytics/R")

# get lineups
all_lineups <- read.csv('../Data/2017_2018/train_for_lineups_full.csv')


# get clusters for individual players
# new data
clean_individual_player_data <- function(player_table) {
  player_table <- player_table %>%
    filter(MIN > 0)
  return(player_table)
}

ap_training <- read.csv('../Data/train_test/player_train_filtered_for_lineups.csv')
aggregate_training <- ap_training %>% clean_individual_player_data() %>% filter(MIN > 50)

filtered_training <- aggregate_training %>% dplyr::select(FGM:BOX_OUTS, -POS)
filtered_training[is.na(filtered_training)] <- 0
filtered_scaled <- scale(filtered_training)

get_lineup_type <- function(lineup, clusters_added) {
  fixed_lineup <- strsplit(lineup, ',')[[1]]
  # make a list that has the cluster for each players and 0 if that player was not in the clusters list
  people_edit <- sapply(fixed_lineup, 
                        function(x) if(x %in% clusters_added[grep(x, clusters_added$PLAYER_NAME),"PLAYER_NAME"]) 
                        {clusters_added[grep(x,clusters_added$PLAYER_NAME),]$cluster} else {0} )
  people_edit <- sort(people_edit)
  type <- paste(people_edit, collapse="")
  return(type)
}


### testing
# given a new lineup of players, see the probabilities that they are in each cluster?
# and then for each case, find the predicted plus-minus for whatever clusters it's likely in
# compare
# refine by comparing stats for indiduals verses their lineup

# bring in testing data
testing_data <- read.csv("test_for_lineups_full.csv")
numeric_test <- testing_data %>% dplyr::select(TOTAL_MIN:TOV, -TOTAL_MIN, -LINEUP)
numeric_test_scaled <- scale(numeric_test)

# get the probabilities of being in each cluster for each type
get_cluster_probabilities <- function(lineup_id, clusters_added_types) {
  cluster_probs <- c()
  for(i in 1:length(unique(clusters_added_types$cluster))) {
    cluster_probs[i] <- length(which(
      clusters_added_types[which(clusters_added_types$cluster == i),]$LINEUP_ID %>% as.character == lineup_id))/length(clusters_added_types$LINEUP_ID[which(clusters_added_types$LINEUP_ID %>% as.character== lineup_id)])
  }
  cluster_probs[is.nan(cluster_probs)] <- 0
  return(cluster_probs)
}

# from the mean plus minus of the lineup id in a cluster, predict the new plus/minus
predict_ps <- function(lineup, clusters_added_types) {
  cluster_probs <- get_cluster_probabilities(lineup, clusters_added_types)
  predicted_ps <-c()
  for(j in 1:length(cluster_probs)) {
    if(cluster_probs[j] == 0) {
        predicted_ps[j] <- mean(clusters_added_types[which(clusters_added_types$cluster == j),]$TOT_PT_DIFF)
    }
    else {
      cluster <- clusters_added_types[which(clusters_added_types$cluster == j),]
      predicted_ps[j] <- mean(cluster[which(cluster$LINEUP_ID %>% as.character == lineup),]$TOT_PT_DIFF)
    }
  }
  return(predicted_ps)
}


# get the best guess from each prediction
get_optimal_error <-function(prediction_matrix,testing_clusters_added) {
errors <- c()
for(k in 1:72) {
  errors[k] <- min((testing_clusters_added$TOT_PT_DIFF[k]-prediction_matrix[,k])^2)
}

 return(mean(errors))
}


#linear_mod <- lm(TOT_PT_DIFF ~ FG_PCT_DIFF + REB + AST + TOV, data=numeric_lin)
#predict_reg <- predict(linear_mod, testing_data)

# use regression to predict lineup stats  of the TESTING data
get_individual_stats <- function(lineup, stat_name, df) {
  names <- strsplit(lineup %>% as.character, ',') %>% unlist
  stats <- sapply(names, function(x) df[which(df$PLAYER_NAME == x),stat_name]) %>% sort
  return(stats)
}


#get predict lineup stats (not including points)
#                   ASSUMPTIONS:
# assumes that aggregate_training and all_lineups are already in your workspace
get_predicted_stats <-function() {
# rebound predict
# get training data to do regression on individual stats vs lineup stat
ors <- sapply(all_lineups$LINEUP, function(x) get_individual_stats(x, "OREB", aggregate_training) %>% as.double)
drs <- sapply(all_lineups$LINEUP, function(x) get_individual_stats(x, "DREB", aggregate_training) %>% as.double)
trs <- t(ors+drs)
colnames(trs) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

rebounds_lp <- cbind(trs, REB = all_lineups$REB) %>% as.data.frame
lm_reb <- lm(REB ~ Player_1 + Player_2 + Player_3 + Player_4 + Player_5, data=rebounds_lp)
# prepare testing data
ors_testing <- sapply(testing_data$LINEUP, function(x) get_individual_stats(x, "OREB", aggregate_training) %>% as.double)
drs_testing <- sapply(testing_data$LINEUP, function(x) get_individual_stats(x, "DREB", aggregate_training) %>% as.double)
trs_testing <- t(ors_testing+drs_testing) %>% as.data.frame
colnames(trs_testing) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

predict_rb <- predict(lm_reb, trs_testing)

# block predictions
# get training data to do regression on individual stats vs lineup stat
blks <- sapply(all_lineups$LINEUP, function(x) get_individual_stats(x, "BLK", aggregate_training) %>% as.double) %>% t
colnames(blks) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

blocks_lp <- cbind(blks, BLK = all_lineups$BLK) %>% as.data.frame
lm_blk <- lm(BLK ~ Player_1 + Player_2 + Player_3 + Player_4 + Player_5, data=blocks_lp)
# prepare testing data
blks_testing <- sapply(testing_data$LINEUP, function(x) get_individual_stats(x, "BLK", aggregate_training) %>% as.double) %>% t %>% as.data.frame
colnames(blks_testing) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

predict_blk <- predict(lm_blk, blks_testing)

#steal predictions
# get training data to do regression on individual stats vs lineup stat
stls <- sapply(all_lineups$LINEUP, function(x) get_individual_stats(x, "STL", aggregate_training) %>% as.double) %>% t
colnames(stls) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

steals_lp <- cbind(stls, STL = all_lineups$STL) %>% as.data.frame
lm_stl <- lm(STL ~ Player_1 + Player_2 + Player_3 + Player_4 + Player_5, data=steals_lp)
# prepare testing data
stls_testing <- sapply(testing_data$LINEUP, function(x) get_individual_stats(x, "STL", aggregate_training) %>% as.double) %>% t %>% as.data.frame
colnames(stls_testing) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

predict_stl <- predict(lm_stl, stls_testing)

# assist predictions
# get training data to do regression on individual stats vs lineup stat
asts <- sapply(all_lineups$LINEUP, function(x) get_individual_stats(x, "AST", aggregate_training) %>% as.double) %>% t
colnames(asts) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

assists_lp <- cbind(asts, AST = all_lineups$AST) %>% as.data.frame
lm_ast <- lm(AST ~ Player_1 + Player_2 + Player_3 + Player_4 + Player_5, data=assists_lp)
# prepare testing data
asts_testing <- sapply(testing_data$LINEUP, function(x) get_individual_stats(x, "AST", aggregate_training) %>% as.double) %>% t %>% as.data.frame
colnames(asts_testing) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

predict_ast <- predict(lm_ast, asts_testing)

#turnover predictions
# get training data to do regression on individual stats vs lineup stat
tovs <- sapply(all_lineups$LINEUP, function(x) get_individual_stats(x, "TOV", aggregate_training) %>% as.double) %>% t
colnames(tovs) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

turnovers_lp <- cbind(tovs, TOV = all_lineups$TOV) %>% as.data.frame
lm_tov <- lm(TOV ~ Player_1 + Player_2 + Player_3 + Player_4 + Player_5, data=turnovers_lp)
# prepare testing data
tovs_testing <- sapply(testing_data$LINEUP, function(x) get_individual_stats(x, "TOV", aggregate_training) %>% as.double) %>% t %>% as.data.frame
colnames(tovs_testing) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

predict_tov <- predict(lm_tov, tovs_testing)

# x3p predictions
# get training data to do regression on individual stats vs lineup stat
x3ps <- sapply(all_lineups$LINEUP,
               function(x) get_individual_stats(x, "FG3_PCT", aggregate_training) %>% as.double) %>% t
colnames(x3ps) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

x3ps_lp <- cbind(x3ps, X3P_PCT_DIFF = all_lineups$X3P_PCT_DIFF) %>% as.data.frame
lm_x3p <- lm(X3P_PCT_DIFF ~ Player_1 + Player_2 + Player_3 + Player_4 + Player_5, data=x3ps_lp)
# prepare testing data
x3ps_testing <- sapply(testing_data$LINEUP,
                       function(x) get_individual_stats(x, "FG3_PCT", aggregate_training) %>% as.double) %>% t %>% as.data.frame
colnames(x3ps_testing) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

predict_x3p <- predict(lm_x3p, x3ps_testing)

# fg_pct predictinos
# get training data to do regression on individual stats vs lineup stat
fgs <- sapply(all_lineups$LINEUP,
              function(x) get_individual_stats(x, "FG_PCT", aggregate_training) %>% as.double) %>% t
colnames(fgs) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

fgs_lp <- cbind(fgs, FG_PCT_DIFF = all_lineups$FG_PCT_DIFF) %>% as.data.frame
lm_fg <- lm(FG_PCT_DIFF ~ Player_1 + Player_2 + Player_3 + Player_4 + Player_5, data=fgs_lp)
# prepare testing data
fgs_testing <- sapply(testing_data$LINEUP,
                      function(x) get_individual_stats(x, "FG_PCT", aggregate_training) %>% as.double) %>% t %>% as.data.frame
colnames(fgs_testing) <- c("Player_1", "Player_2", "Player_3", "Player_4", "Player_5")

predict_fg <- predict(lm_fg, fgs_testing)


# use predicted lineup stats to do knn
predicted_stats <- data.frame(LINEUP=testing_data$LINEUP,
                              FG_PCT_DIFF=predict_fg,X3P_PCT_DIFF=predict_x3p,REB=predict_rb,STL=predict_stl,
                              AST=predict_ast,BLK=predict_blk,TOV=predict_tov)
return(predicted_stats)

}

#predicted_points <- predict(linear_mod, predicted_stats)



#find errors from testing data using KNN, Mlogit, and LDA
#                           ASSUMPTIONS
# assume that lineup_clusters_added is a dataframe with a clusters column
# p_stats is assumed to be unscaled predicted testing stats without predicted points
get_errors_and_predictions <- function(prediction_matrix,p_stats,lineup_clusters_added) {
  #KNN
  numeric_predict_scaled <- p_stats %>% dplyr::select(-LINEUP) %>% scale
  nn_test <- class::knn(numeric_lin_scaled,numeric_predict_scaled, lineup_clusters_added$cluster)

  # attempt to find clusters using mlogit
  multin <- multinom(lineup_clusters_added$cluster ~ X3P_PCT_DIFF + REB + BLK + STL + AST + TOV, data=,lineup_clusters_added)

  predict_mn <- predict(multin, p_stats)
  # attempt to find clusters using LDA
  lda_test <- lda(lineup_clusters_added$cluster ~ X3P_PCT_DIFF + REB + BLK + STL + AST + TOV, data=lineup_clusters_added)

  predict_lda <- predict(lda_test,p_stats)

  # find predictions and errors based on cluster assignments
  type_predictions <- matrix(nrow=ncol(prediction_matrix),ncol=3)
  errors <- matrix(nrow=1, ncol=3)
  
  bb <- predict_mn %>% as.integer
  cc <- predict_lda$class %>% as.integer
  tests <- matrix(c(nn_test,bb,cc), nrow=72, ncol=3)
  for(e in 1:3) {
    for(n in 1:72) {
      type_predictions[n,e] = t(predictions)[n,tests[n,e]]
    }
    errors[e] <- mean((testing_data$TOT_PT_DIFF-type_predictions[,e])^2)
  }
  colnames(errors) <- c("KNN", "MLogit", "LDA")
  colnames(type_predictions) <- colnames(errors)
  df.results <- list(predictions=type_predictions, error=errors)
  
  return(df.results) 
}

                      
# demonstration of how to use the functions from this file
# seen in testing section of hclustering_lineup_analysis.Rmd
