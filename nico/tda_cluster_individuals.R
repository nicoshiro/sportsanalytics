library(gmodels)
library(descr)
library(dplyr)
library(tidyr)
library(mclust)
library(FactoMineR)
library(TDAmapper) 
library(igraph)
library(Matching)

setwd("/Users/nicoshiro/Downloads/sports_analytics/R")

source("preprocess_individuals.R")

clean_individual_player_data <- function(player_table) 
  { player_table <- player_table %>% filter(MIN > 0) 
    return(player_table) }


# Read in and clean data
ap_training <- read.csv('/Users/nicoshiro/Downloads/sports_analytics/data/player_train.csv')

#clean_load <- all_players %>% clean_individual_player_data()

#aggregate_players <- clean_load
aggregate_training <- ap_training %>% clean_individual_player_data()

#aggregate_players <- aggregate_players %>% filter(MIN > 50)
aggregate_training <- aggregate_training %>% filter(MIN > 50)

#numeric <- aggregate_players %>% select(FGM:BOX_OUTS, -POS)
numeric_training <- aggregate_training %>% dplyr::select(FGM:BOX_OUTS, -POS)

#numeric[is.na(numeric)] <- 0
numeric_training[is.na(numeric_training)] <- 0

#numeric_scaled <- scale(numeric)
ns_training <- scale(numeric_training)

zscores_for_cluster <- function(players_in_cluster, data)
{ players <- unlist(players_in_cluster) 
  values <- data[players,] 
  zscores <- c() 
  for(i in 1:length(data[1,]))
  { if(length(players) == 1)
  { zscores[i] <- mean(values[i]) }
    else zscores[i] <- mean(values[,i]) } 
  return(zscores) }

filtered_training <- aggregate_training %>% dplyr::select(FGM:BOX_OUTS, -POS, -PTS, -PLUS_MINUS) 
filtered_training[is.na(filtered_training)] <- 0 
filtered_scaled <- scale(filtered_training)


# Run the mapper function
set.seed("123") 
nba.mapper.training <- mapper( dist_object = dist(filtered_scaled), filter_values = list(ns_training[,19],ns_training[,20]), num_intervals = c(3,2), percent_overlap = 50, num_bins_when_clustering = 10)
nba.graph.training <- graph.adjacency(nba.mapper.training$adjacency, mode="undirected") 
plot(nba.graph.training, layout = layout.auto(nba.graph.training), main ="TDAmapper Individual Players" )


# Group the clusters
cluster_1 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(1)]))),] 
cluster_2 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(2)]))),] 
cluster_3 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(3)]))),] 
cluster_4 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(4)]))),] 
cluster_5 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(5)]))),] 
cluster_6 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(6)]))),] 
cluster_7 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(7)]))),] 
cluster_8 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(8)]))),] 

# Take the median values of the columns in the clusters
cluster_1_median <- apply(cluster_1, MARGIN = 2, FUN = median)
cluster_2_median <- apply(cluster_2, MARGIN = 2, FUN = median)
cluster_3_median <- apply(cluster_3, MARGIN = 2, FUN = median)
cluster_4_median <- apply(cluster_4, MARGIN = 2, FUN = median)
cluster_5_median <- apply(cluster_5, MARGIN = 2, FUN = median)
cluster_6_median <- apply(cluster_6, MARGIN = 2, FUN = median)
cluster_7_median <- apply(cluster_7, MARGIN = 2, FUN = median)
cluster_8_median <- apply(cluster_8, MARGIN = 2, FUN = median)

all_means_of_clusters <- rbind(cluster_1_median,
                               cluster_2_median,
                               cluster_3_median,
                               cluster_4_median,
                               cluster_5_median,
                               cluster_6_median,
                               cluster_7_median,
                               cluster_8_median)

# Compare the groups with ks test
group_comparison <- function(group_1) 
  { pvals <- c() 
  for(i in 1:ncol(filtered_scaled)) 
  { test <- ks.boot(group_1[,i], filtered_training[,i], alternative = "two.sided") 
    pvals[i] <- test$ks.boot.pvalue } 
  return(pvals) }

g1 <- group_comparison(cluster_1)
g2 <- group_comparison(cluster_2)
g3 <- group_comparison(cluster_3)
g4 <- group_comparison(cluster_4)
g5 <- group_comparison(cluster_5)
g6 <- group_comparison(cluster_6)
g7 <- group_comparison(cluster_7)
g8 <- group_comparison(cluster_8)


# Begin extracting features from the clusters
ids_of_cluster_1 <- sort(nba.mapper.training[["points_in_vertex"]][[1]])
ids_of_cluster_2 <- sort(nba.mapper.training[["points_in_vertex"]][[2]])
ids_of_cluster_3 <- sort(nba.mapper.training[["points_in_vertex"]][[3]])
ids_of_cluster_4 <- sort(nba.mapper.training[["points_in_vertex"]][[4]])
ids_of_cluster_5 <- sort(nba.mapper.training[["points_in_vertex"]][[5]])
ids_of_cluster_6 <- sort(nba.mapper.training[["points_in_vertex"]][[6]])
ids_of_cluster_7 <- sort(nba.mapper.training[["points_in_vertex"]][[7]])
ids_of_cluster_8 <- sort(nba.mapper.training[["points_in_vertex"]][[8]])


# Assign clusters to the rows in the data frame
aggregate_training$CLUSTER <- 0
for (i in 1:387){
  if (i %in% ids_of_cluster_3){
    aggregate_training$CLUSTER[i] <- paste(aggregate_training$CLUSTER[i], 3)
  }
  if (i %in% ids_of_cluster_8){
    aggregate_training$CLUSTER[i] <- paste(aggregate_training$CLUSTER[i], 8)
  }
  if (i %in% ids_of_cluster_2){
    aggregate_training$CLUSTER[i] <- paste(aggregate_training$CLUSTER[i], 2)
  }
  if (i %in% ids_of_cluster_1){
    aggregate_training$CLUSTER[i] <- paste(aggregate_training$CLUSTER[i], 1)
  }
  if (i %in% ids_of_cluster_5){
    aggregate_training$CLUSTER[i] <- paste(aggregate_training$CLUSTER[i], 5)
  }
  if (i %in% ids_of_cluster_6){
    aggregate_training$CLUSTER[i] <- paste(aggregate_training$CLUSTER[i], 6)
  }
  if (i %in% ids_of_cluster_4){
    aggregate_training$CLUSTER[i] <- paste(aggregate_training$CLUSTER[i], 4)
  }
  if (i %in% ids_of_cluster_7){
    aggregate_training$CLUSTER[i] <- paste(aggregate_training$CLUSTER[i], 7)
  }
}

aggregate_training$CLUSTER <- substr(aggregate_training$CLUSTER, 3, 10)
aggregate_training$MAIN_CLUSTER <- substr(aggregate_training$CLUSTER, 1, 2)
aggregate_training$MAIN_CLUSTER <- as.numeric(aggregate_training$MAIN_CLUSTER)
aggregate_training$MAIN_CLUSTER <- as.character(aggregate_training$MAIN_CLUSTER)

write.csv(aggregate_training, "individual_player_data_with_TDA_clusters.csv")

# Plots to extract features from clusters
clPairs(filtered_scaled, aggregate_training$MAIN_CLUSTER)

df_filtered_scaled <- as.data.frame(filtered_scaled)
ggplot(df_filtered_scaled, aes(x = FGM, y = FGA, color = aggregate_training$MAIN_CLUSTER)) + geom_point()
ggplot(df_filtered_scaled, aes(x = FGM, y = AST, color = aggregate_training$MAIN_CLUSTER)) + geom_point()
ggplot(df_filtered_scaled, aes(x = FGM, y = FGA, color = aggregate_training$CLUSTER)) + geom_point()

cor_matrix <- cor(df_filtered_scaled)


# IGNORE THIS - EXTRA WORK
# set.seed("123") 
# nba.mapper.training.2 <- mapper( dist_object = dist(filtered_scaled), filter_values = list(ns_training[,19],ns_training[,20]), num_intervals = c(4,4), percent_overlap = 50, num_bins_when_clustering = 10)
# nba.graph.training.2 <- graph.adjacency(nba.mapper.training.2$adjacency, mode="undirected") 
# plot(nba.graph.training.2, layout = layout.auto(nba.graph.training), main ="TDAmapper w/ c(4,4) Intervals" )
# 
# interesting_group_1 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(1,4,5)]))),] 
# interesting_group_2 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(9,10,11,16,12)]))),] 
# interesting_group_3 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(20,18,25)]))),] 
# interesting_group_4 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(21,26,27)]))),] 
# interesting_group_5 <- filtered_training[unique( sort(unlist(nba.mapper.training$points_in_vertex[c(6,7,3,14)]))),]
# 
# group_comparison <- function(group_1) 
# { pvals <- c() 
#   for(i in 1:length(filtered_scaled[1,])) 
#   { test <- ks.boot(group_1[,i], filtered_training[,i], alternative = "two.sided") 
#     pvals[i] <- test$p.value } 
#   return(pvals) }
# 
# test <- ks.boot(interesting_group_1[,1], filtered_training[,1], alternative = "two.sided")
# 
# 
# g1 <- group_comparison(interesting_group_1)
# g2 <- group_comparison(interesting_group_2)
# g3 <- group_comparison(interesting_group_3)
# g4 <- group_comparison(interesting_group_4)
# g5 <- group_comparison(interesting_group_5)
# 

