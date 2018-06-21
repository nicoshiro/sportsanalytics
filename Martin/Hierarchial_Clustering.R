library(cluster)
library(factoextra)
library(gmodels)
library(descr)
library(dplyr)
library(tidyr)
library(stargazer)
library(FactoMineR)
library(Matching)
library(readr)

source('preprocess_individuals.R')

#View(player_train_filtered)

# new data
clean_individual_player_data <- function(player_table) 
{ player_table <- player_table %>% filter(MIN > 0)
return(player_table) }

# modifications to load all players

player_train_filtered <- read_csv("~/Desktop/sportsanalytics-master/data/player_train_filtered.csv")

# # get aggregate player data
# aggregate_players <- all_players %>% clean_individual_player_data()

# remove players who haven't played more than 50 minutes
#Can be used to find players' corresponding to the results of mapper
train_fifty <- player_train_filtered %>% filter(MIN > 50) 

# get numeric data (excluding result, GS, MP, and GmSc)
train_numeric <- train_fifty %>% select(FGM:BOX_OUTS, -POS)

# set NA percentages to 0
train_numeric[is.na(train_numeric)] <- 0

# scale numeric data before clustering
train_scaled <- as.data.frame(scale(train_numeric))

dist_train <- select(as.data.frame(train_scaled), -PTS, -PLUS_MINUS)

library(cluster)
library(FactoMineR)
pca <- PCA(dist_train)
pca_coord <- pca$ind$coord
pca_coord[is.na(pca_coord)] <- 0
pca_coord_scaled <- scale(pca_coord)


#agglomerative clustering method
set.seed(123)
agg_average <- agnes(dist_train, metric = "euclidean", method = "average")
plot(agg_average)

set.seed(123)
agg_single <- agnes(dist_train, metric = "euclidean", method = "single")
plot(agg_single)

set.seed(123)
agg_complete <- agnes(dist_train, metric = "euclidean", method = "complete")
plot(agg_complete)

set.seed(123) #WORKS THE BEST
agg_ward <- agnes(dist_train, metric = "euclidean", method = "ward")
plot(agg_ward)


#divisive clustering method
set.seed(123)
dian_t <- diana(dist_train)
plot(dian_t)


fviz_nbclust(dist_train, hcut, method = "wss", main = "Within Cluster Sum of Squared Errors versus Number of Clusters")
fviz_nbclust(dist_train, hcut, method = "silhouette")
fviz_nbclust(dist_train, hcut, method = "gap_stat")

clusters_agg <- cbind(train_fifty, cluster = cutree(agg_ward,6))
clusters_diana <- cbind(train_fifty, cluster = cutree(dian_t,4))



library(rgl)
# Agglomerative (Ward's Method) with 4 clusters
plot(pca, choix = "ind",col.ind=ifelse(clusters_agg$cluster ==1, "red", ifelse(clusters_agg$cluster ==2, "blue", ifelse(clusters_agg$cluster ==4, "green", "black"))), label = "none")

# Agglomerative (Ward's Method) with 6 clusters
plot(pca, choix = "ind",col.ind=ifelse(clusters_agg$cluster ==1, "red", ifelse(clusters_agg$cluster ==2, "blue", ifelse(clusters_agg$cluster ==3, "green", ifelse(clusters_agg$cluster ==4, "orange", ifelse(clusters_agg$cluster ==6, "pink", "black"))))), label = "none")
plot3d(pca_coord_scaled, col=clusters_agg$cluster)

plot(pca, choix = "ind",col.ind=ifelse(clusters_diana$cluster ==1, "red", ifelse(clusters_diana$cluster ==2, "blue", ifelse(clusters_diana$cluster ==4, "green", "black"))), label = "none")



diana_pcat <- diana(pca_coord_scaled)

# WITH PCA
set.seed(123)
agg_pcat_ward <- agnes(pca_coord_scaled, metric = 'euclidean', method = "ward")
plot(agg_pcat_ward)

#   get optimal number of clusters
fviz_nbclust(pca_coord_scaled, hcut, method = "wss")
fviz_nbclust(pca_coord_scaled, hcut, method = "silhouette")
fviz_nbclust(pca_coord_scaled, hcut, method = "gap_stat")

table_diana <- table(cutree(diana_pcat,6))

clusters_agg_ward <- cbind(train_fifty, cluster = cutree(agg_pcat_ward,6))

#clusters_PCA_D <- as.data.frame(cbind(pcat_coord_scaled, cluster = cutree(diana_pcat,6)))
clusters_PCA_D <- cbind(clusters_PCA_D, Name = aggregate_training$PLAYER_NAME)

plot(pca, choix = "ind",col.ind=ifelse(clusters_agg_ward$cluster ==1, "red",
                                        ifelse(clusters_agg_ward$cluster ==2, "blue", 
                                               ifelse(clusters_agg_ward$cluster ==3, "green", 
                                                      ifelse(clusters_agg_ward$cluster ==4, "purple", 
                                                             ifelse(clusters_agg_ward$cluster ==5,"black", "orange"))))), label = "none")

plot3d(pca_coord_scaled, col=clusters_agg_ward$cluster)

### WITH ALL DATA (INCLUDING PTS PLUS/MINUS)
pca_all_data <- PCA(train_scaled)
all_pca_coord <- pca_all_data$ind$coord
all_pca_coord[is.na(all_pca_coord)] <- 0
all_pca_coord_scaled <- scale(all_pca_coord)

set.seed(123)
all_agg_pcat_ward <- agnes(all_pca_coord_scaled, metric = 'euclidean', method = "ward")
plot(all_agg_pcat_ward)

all_clusters_agg_ward <- cbind(train_fifty, cluster = cutree(all_agg_pcat_ward,6))

plot(pca_all_data, choix = "ind",col.ind=ifelse(all_agg_pcat_ward$cluster ==1, "red",
  ifelse(all_clusters_agg_ward$cluster ==2, "blue", 
                                               ifelse(all_clusters_agg_ward$cluster ==3, "green", 
                                                      ifelse(all_clusters_agg_ward$cluster ==4, "purple", 
                                                             ifelse(all_clusters_agg_ward$cluster ==5,"black", "orange"))))), label = "none")

plot3d(all_pca_coord_scaled, col=all_clusters_agg_ward$cluster)
