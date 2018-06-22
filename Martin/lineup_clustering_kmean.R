library(readr)
library(dplyr)


train_aggregate_bf_lineups_2017_2018 <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/train_aggregate_bf_lineups_2017_2018.csv")
all_lineups <- train_aggregate_bf_lineups_2017_2018 %>% dplyr::select(-LINEUP, -TOTAL_MIN, -X1)
lineups_scaled <- scale(all_lineups)


library(cluster)
library(FactoMineR)
pca_lineup <- PCA(lineups_scaled)
plot(pca_lineup, type = 'l') # Four Principle Components

pca_lineup_coord <- pca_lineup$ind$coord
pca_lineup_coord[is.na(pca_lineup_coord)] <- 0
pca_lineup_scaled <- scale(pca_lineup_coord)

k_lineup <- kmeans(lineups_scaled, 4, nstart=25, iter.max=1000)
k_lineup
plot(pca_lineup_scaled, col=k_lineup$clust, pch=16)

library(factoextra)
fviz_cluster(
  k_lineup,
  data = lineups_scaled,
  main = ''
)


k_lineup <- kmeans(lineups_scaled, 6, nstart=25, iter.max=1000)
k_lineup
plot(pca_lineup_scaled, col=k_lineup$clust, pch=16)

library(factoextra)
fviz_cluster(
  k_lineup,
  data = lineups_scaled,
  main = ''
)

library(rgl)
plot3d(pca_lineup_scaled, col=k_lineup$clust) #Clustering Without PCA

library(broom)
clust_lineup <-augment(k_lineup, train_aggregate_bf_lineups_2017_2018)
lineup_cluster_1 <- clust_lineup %>% filter(.cluster == 1)
lineup_cluster_2 <- clust_lineup %>% filter(.cluster == 2)
lineup_cluster_3 <- clust_lineup %>% filter(.cluster == 3)
lineup_cluster_4 <- clust_lineup %>% filter(.cluster == 4)



k_pca_lineup <- kmeans(pca_lineup_scaled, 4, nstart=25, iter.max=1000)
k_pca_lineup
plot(pca_lineup_scaled, col=k_pca_lineup$clust, pch=16)

fviz_cluster(
  k_pca_lineup,
  data = pca_lineup_scaled,
  main = ''
)

plot3d(pca_lineup_scaled, col=k_pca_lineup$clust) #Clustering Without PCA



get_lineup_type <- function(lineup, clusters_added) {
  fixed_lineup <- strsplit(lineup, ',')[[1]]
  #   fixed_lineup <- as.character(sapply(strsplit(lineup, ','), function(x) sub("[.]", " ",x)))
  
  # make a list that has the cluster for each players and 0 if that player was not in the clusters list
  people_edit <- sapply(fixed_lineup, 
                        function(x) if(x %in% clusters_added[grep(x, clusters_added$PLAYER_NAME),"PLAYER_NAME"]) 
                        {clusters_added[grep(x,clusters_added$PLAYER_NAME),]$cluster} else {0} )
  people_edit <- sort(people_edit)
  type <- paste(people_edit, collapse="")
  return(type)
}

# get lineup types for all lineups
all_lineup_types <- sapply(train_aggregate_bf_lineups_2017_2018$LINEUP %>% as.character, function(x) get_lineup_type(x, k_player_clust))
#bind the lineup types with the clusters_added
agnes_lineup_types <- cbind(clusters_agg_line, LINEUP_ID = all_lineup_types)



