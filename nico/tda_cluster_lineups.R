library(mclust)
library(cluster)
library(FactoMineR)
library(descr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(TDAmapper)
library(plyr)

setwd("/Users/nicoshiro/Downloads/sports_analytics/data")

# Read/clean lineup data and individual data
lineups <- read.csv("train_for_lineups_full.csv")
individual_stats <- read.csv("individual_player_data_with_TDA_clusters.csv")

lineups$X1 <- NULL
lineups$X <- NULL
lineups$LINEUP <- as.character(lineups$LINEUP)


#pca_lineups <- PCA(lineups)
#autoplot(pca_lineups)

# Lineup type function
get_lineup_type <- function(lineup, clusters_added) {
  fixed_lineup <- strsplit(lineup, ",")[[1]]
  #   fixed_lineup <- as.character(sapply(strsplit(lineup, ','), function(x) sub("[.]", " ",x)))
  
  # make a list that has the cluster for each players and 0 if that player was not in the clusters list
  people_edit <- sapply(fixed_lineup, 
                        function(x) if(x %in% clusters_added[grep(x, clusters_added$PLAYER_NAME),"PLAYER_NAME"]) 
                        {clusters_added[grep(x,clusters_added$PLAYER_NAME),]$MAIN_CLUSTER} else {0} )
  people_edit <- sort(people_edit)
  type <- paste(people_edit, collapse="")
  return(type)
}


# Add the lineup type to the data frame
lineups$LINEUP_TYPE <- 0
for (i in 1:302) {
  lineups[i, 11] <- get_lineup_type(lineups[i, 5], individual_stats)
}





# Read/clean lineup data to extract features
lineups_new <- read.csv("train_for_lineups_full.csv")
lineups_new$X1 <- NULL
lineups_new$X <- NULL
lineups_new$LINEUP <- NULL
lineups_scaled <- scale(lineups_new)

lineups_scaled <- as.data.frame(lineups_scaled)
scaled_total_pt_diff <- lineups_scaled$TOT_PT_DIFF


# TDA on clusters
set.seed("123") 
lineups.mapper.training <- mapper( dist_object = dist(lineups_scaled), filter_values = list(scaled_total_pt_diff), num_intervals = 8, percent_overlap = 50, num_bins_when_clustering = 10)
lineups.graph.training <- graph.adjacency(lineups.mapper.training$adjacency, mode="undirected") 
plot(lineups.graph.training, layout = layout.auto(lineups.graph.training), main ="TDAmapper Lineups" )

# Extract the clusters
level_set_1 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_level_set[c(1)]))),] 
level_set_2 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_level_set[c(2)]))),] 
level_set_3 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_level_set[c(3)]))),] 
level_set_4 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_level_set[c(4)]))),] 
level_set_5 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_level_set[c(5)]))),] 
level_set_6 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_level_set[c(6)]))),] 
level_set_7 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_level_set[c(7)]))),] 
level_set_8 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_level_set[c(8)]))),]


# Do analysis of the medians
level_set_1_median <- apply(level_set_1, MARGIN = 2, FUN = median)
level_set_2_median <- apply(level_set_2, MARGIN = 2, FUN = median)
level_set_3_median <- apply(level_set_3, MARGIN = 2, FUN = median)
level_set_4_median <- apply(level_set_4, MARGIN = 2, FUN = median)
level_set_5_median <- apply(level_set_5, MARGIN = 2, FUN = median)
level_set_6_median <- apply(level_set_6, MARGIN = 2, FUN = median)
level_set_7_median <- apply(level_set_7, MARGIN = 2, FUN = median)
level_set_8_median <- apply(level_set_8, MARGIN = 2, FUN = median)


all_medians_of_level_set <- rbind(level_set_1_median,
                                 level_set_2_median,
                                 level_set_3_median,
                                 level_set_4_median,
                                 level_set_5_median,
                                 level_set_6_median,
                                 level_set_7_median,
                                 level_set_8_median)

all_medians_of_level_set <- as.data.frame(all_medians_of_level_set)

all_medians_of_level_set$CLUSTER <- 0
all_medians_of_level_set[1, 10] <- 1
all_medians_of_level_set[2, 10] <- 2
all_medians_of_level_set[3, 10] <- 3
all_medians_of_level_set[4, 10] <- 4
all_medians_of_level_set[5, 10] <- 5
all_medians_of_level_set[6, 10] <- 6
all_medians_of_level_set[7, 10] <- 7
all_medians_of_level_set[8, 10] <- 8

barplot(all_medians_of_level_set$TOTAL_MIN, names.arg = all_medians_of_level_set$CLUSTER )
barplot(all_medians_of_level_set$TOT_PT_DIFF, names.arg = all_medians_of_level_set$CLUSTER )
barplot(all_medians_of_level_set$FG_PCT_DIFF, names.arg = all_medians_of_level_set$CLUSTER )
barplot(all_medians_of_level_set$X3P_PCT_DIFF, names.arg = all_medians_of_level_set$CLUSTER )
barplot(all_medians_of_level_set$REB, names.arg = all_medians_of_level_set$CLUSTER )
barplot(all_medians_of_level_set$BLK, names.arg = all_medians_of_level_set$CLUSTER )
barplot(all_medians_of_level_set$STL, names.arg = all_medians_of_level_set$CLUSTER )
barplot(all_medians_of_level_set$AST, names.arg = all_medians_of_level_set$CLUSTER )
barplot(all_medians_of_level_set$TOV, names.arg = all_medians_of_level_set$CLUSTER )



# Extract the row IDs of each cluster
ids_of_cluster_1 <- sort(lineups.mapper.training[["points_in_level_set"]][[1]])
ids_of_cluster_2 <- sort(lineups.mapper.training[["points_in_level_set"]][[2]])
ids_of_cluster_3 <- sort(lineups.mapper.training[["points_in_level_set"]][[3]])
ids_of_cluster_4 <- sort(lineups.mapper.training[["points_in_level_set"]][[4]])
ids_of_cluster_5 <- sort(lineups.mapper.training[["points_in_level_set"]][[5]])
ids_of_cluster_6 <- sort(lineups.mapper.training[["points_in_level_set"]][[6]])
ids_of_cluster_7 <- sort(lineups.mapper.training[["points_in_level_set"]][[7]])
ids_of_cluster_8 <- sort(lineups.mapper.training[["points_in_level_set"]][[8]])


# Assign each row in the data frame a cluster
lineups_scaled$CLUSTER <- 0
for (i in 1:302){
  if (i %in% ids_of_cluster_1){
    lineups_scaled$CLUSTER[i] <- paste(lineups_new$CLUSTER[i], 1)
  }
  if (i %in% ids_of_cluster_8){
    lineups_scaled$CLUSTER[i] <- paste(lineups_new$CLUSTER[i], 8)
  }
  if (i %in% ids_of_cluster_2){
    lineups_scaled$CLUSTER[i] <- paste(lineups_new$CLUSTER[i], 2)
  }
  if (i %in% ids_of_cluster_7){
    lineups_scaled$CLUSTER[i] <- paste(lineups_new$CLUSTER[i], 7)
  }
  if (i %in% ids_of_cluster_3){
    lineups_scaled$CLUSTER[i] <- paste(lineups_new$CLUSTER[i], 3)
  }
  if (i %in% ids_of_cluster_6){
    lineups_scaled$CLUSTER[i] <- paste(lineups_new$CLUSTER[i], 6)
  }
  if (i %in% ids_of_cluster_4){
    lineups_scaled$CLUSTER[i] <- paste(lineups_new$CLUSTER[i], 4)
  }
  if (i %in% ids_of_cluster_5){
    lineups_scaled$CLUSTER[i] <- paste(lineups_new$CLUSTER[i], 5)
  }
}
lineups_scaled$CLUSTER <- substr(lineups_new$CLUSTER, 3, 10)
lineups_scaled$MAIN_CLUSTER <- substr(lineups_new$CLUSTER, 1, 2)
lineups_scaled$MAIN_CLUSTER <- as.numeric(lineups_new$MAIN_CLUSTER)
lineups_scaled$MAIN_CLUSTER <- as.character(lineups_new$MAIN_CLUSTER)
lineups_scaled <- lineups_scaled[order(lineups_new$MAIN_CLUSTER),]
write.csv(lineups_scaled, "lineups_scaled_train.csv")


# Barplots to extract features from the clusters
barplot(lineups_scaled$TOT_PT_DIFF,
        main = "Point Differential Per Cluster",
        col = lineups_new$MAIN_CLUSTER,
        xlab = "Lineup Cluster",
        ylab = "Total Point Differential")

barplot(lineups_scaled$FG_PCT_DIFF,
        main = "Field Goal Differential Per Cluster",
        col = lineups_new$MAIN_CLUSTER,
        xlab = "Lineup Cluster",
        ylab = "Total Field Goal Differential")

barplot(lineups_scaled$X3P_PCT_DIFF,
        main = "Point Differential Per Cluster",
        col = lineups_new$MAIN_CLUSTER,
        xlab = "Lineup Cluster",
        ylab = "Total Point Differential")

barplot(lineups_scaled$REB,
        main = "Point Differential Per Cluster",
        col = lineups_new$MAIN_CLUSTER,
        xlab = "Lineup Cluster",
        ylab = "Total Point Differential")

barplot(lineups_scaled$BLK,
        main = "Point Differential Per Cluster",
        col = lineups_new$MAIN_CLUSTER,
        xlab = "Lineup Cluster",
        ylab = "Total Point Differential")

barplot(lineups_scaled$STL,
        main = "Point Differential Per Cluster",
        col = lineups_new$MAIN_CLUSTER,
        xlab = "Lineup Cluster",
        ylab = "Total Point Differential")

barplot(lineups_scaled$AST,
        main = "Point Differential Per Cluster",
        col = lineups_new$MAIN_CLUSTER,
        xlab = "Lineup Cluster",
        ylab = "Total Point Differential")

barplot(lineups_scaled$TOV,
        main = "Point Differential Per Cluster",
        col = lineups_new$MAIN_CLUSTER,
        xlab = "Lineup Cluster",
        ylab = "Total Point Differential")



# IGNORE THIS - EXTRA WORK
# cluster_1 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_vertex[c(1)]))),] 
# cluster_2 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_vertex[c(2)]))),] 
# cluster_3 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_vertex[c(3)]))),] 
# cluster_4 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_vertex[c(4)]))),] 
# cluster_5 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_vertex[c(5)]))),] 
# cluster_6 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_vertex[c(6)]))),] 
# cluster_7 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_vertex[c(7)]))),] 
# cluster_8 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_vertex[c(8)]))),] 
# cluster_9 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_vertex[c(9)]))),] 
# cluster_10 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_vertex[c(10)]))),] 
# cluster_11 <- lineups_scaled[unique( sort(unlist(lineups.mapper.training$points_in_vertex[c(11)]))),] 
# 
# cluster_1_median <- apply(cluster_1, MARGIN = 2, FUN = median)
# cluster_2_median <- apply(cluster_2, MARGIN = 2, FUN = median)
# cluster_3_median <- apply(cluster_3, MARGIN = 2, FUN = median)
# cluster_4_median <- apply(cluster_4, MARGIN = 2, FUN = median)
# cluster_5_median <- apply(cluster_5, MARGIN = 2, FUN = median)
# cluster_6_median <- apply(cluster_6, MARGIN = 2, FUN = median)
# cluster_7_median <- apply(cluster_7, MARGIN = 2, FUN = median)
# cluster_8_median <- apply(cluster_8, MARGIN = 2, FUN = median)
# cluster_9_median <- apply(cluster_9, MARGIN = 2, FUN = median)
# cluster_10_median <- apply(cluster_10, MARGIN = 2, FUN = median)
# cluster_11_median <- apply(cluster_11, MARGIN = 2, FUN = median)
# 
# all_medians_of_clusters <- rbind(cluster_1_median,
#                                  cluster_2_median,
#                                  cluster_3_median,
#                                  cluster_4_median,
#                                  cluster_5_median,
#                                  cluster_6_median,
#                                  cluster_7_median,
#                                  cluster_8_median,
#                                  cluster_9_median,
#                                  cluster_10_median,
#                                  cluster_11_median)
# 
# all_medians_of_clusters <- as.data.frame(all_medians_of_clusters)
# 
# 
# all_medians_of_clusters$CLUSTER <- 0
# all_medians_of_clusters[1, 10] <- 1
# all_medians_of_clusters[2, 10] <- 2
# all_medians_of_clusters[3, 10] <- 3
# all_medians_of_clusters[4, 10] <- 4
# all_medians_of_clusters[5, 10] <- 5
# all_medians_of_clusters[6, 10] <- 6
# all_medians_of_clusters[7, 10] <- 7
# all_medians_of_clusters[8, 10] <- 8
# all_medians_of_clusters[9, 10] <- 9
# all_medians_of_clusters[10, 10] <- 10
# all_medians_of_clusters[11, 10] <- 11
# 
# barplot(all_medians_of_clusters$TOTAL_MIN, names.arg = all_medians_of_clusters$CLUSTER )
# barplot(all_medians_of_clusters$TOT_PT_DIFF, names.arg = all_medians_of_clusters$CLUSTER )
# barplot(all_medians_of_clusters$FG_PCT_DIFF, names.arg = all_medians_of_clusters$CLUSTER )
# barplot(all_medians_of_clusters$X3P_PCT_DIFF, names.arg = all_medians_of_clusters$CLUSTER )
# barplot(all_medians_of_clusters$REB, names.arg = all_medians_of_clusters$CLUSTER )
# barplot(all_medians_of_clusters$BLK, names.arg = all_medians_of_clusters$CLUSTER )
# barplot(all_medians_of_clusters$STL, names.arg = all_medians_of_clusters$CLUSTER )
# barplot(all_medians_of_clusters$AST, names.arg = all_medians_of_clusters$CLUSTER )
# barplot(all_medians_of_clusters$TOV, names.arg = all_medians_of_clusters$CLUSTER )
