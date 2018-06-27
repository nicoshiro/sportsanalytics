library(readr)
library(dplyr)
library(cluster)
library(FactoMineR)
library(factoextra)
library(broom)
library(rgl)

# load the full lineup stats (training)
train_for_lineups_full_stats <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/train_for_lineups_full.csv")

#Exclude non-numeric data
all_lineups <- train_for_lineups_full_stats %>% dplyr::select(-LINEUP, -TOTAL_MIN, -X1, -X)

#Scale the numeric data
lineups_scaled <- scale(all_lineups)

#Run PCA on the scaled numeric lineup data
set.seed(235)
pca_lineup <- PCA(lineups_scaled)
plot(pca_lineup, type = 'l')

#Isolate the coordinates from PCA and Scale them
pca_lineup_coord <- pca_lineup$ind$coord
pca_lineup_coord[is.na(pca_lineup_coord)] <- 0
pca_lineup_scaled <- scale(pca_lineup_coord)

#The Following Plot gives an indication for the number of clusters, using the elbow method
wss <- (nrow(lineups_scaled)-1)*sum(apply(lineups_scaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(lineups_scaled,
                                     centers=i, nstart = 24, iter.max = 1000)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") # Elbow at about 3~4 clusters


#Runs K-means on the scaled lineup data
set.seed(235)
k_lineup <- kmeans(lineups_scaled, 4, nstart=25, iter.max=1000)


#Another visual representation using factoextra package
fviz_cluster(
  k_lineup,
  data = lineups_scaled,
  main = ''
)


#Nice looking plot with points colored coded based on cluster, 
#feel free to change the parametrs to reflect different number of clusters
lineup_clust_train <- augment(k_lineup,lineups_scaled)
plot(pca_lineup_scaled,
     main = "Lineup Clustering: K=4",
     xlab = 'PCA Dim 1 (24.38%)', xlim = c(-4, 4),
     ylab = 'PCA Dim 2 (15.07%)', ylim = c(-4,4),
     pch = c(1,2,3,4,5)[unclass(lineup_clust_train$.cluster)],
     col = c("red","blue","green","orange", "purple")[unclass(lineup_clust_train$.cluster)])

     legend(-4.15,1,
            legend=c("Cluster 1", "Cluster 2","Cluster 3","Cluster 4","Cluster 5"),
            col=c("red", "blue", "green", "orange"),
            pch= c(1,2,3,4), cex=0.6)
     
#3D visualization wth rgl package (make sure XQuartz is installed), great tool to 
# distinguish between clusters in 3D
plot3d(pca_lineup_scaled, 
       main = "Lineup K-Means Clustering: K=4",
       xlab = 'PCA Dim 1 (24.28%)', xlim = c(-4, 4),
       ylab = 'PCA Dim 2 (15.76%)', ylim = c(-4, 4),
       zlab = 'PCA Dim 3 (13.87%)', zlim = c(-4, 4),
       pch = c(1,2,3,4,5)[unclass(lineup_clust_train$.cluster)],
       col = c("red","blue","green","orange","purple")[unclass(lineup_clust_train$.cluster)]) 

legend3d("topright", paste('Cluster', c("1", "2", "3","4")), 
         pch = 16, col=c("red", "blue", "green", "orange"), 
         cex=1, inset=c(0.02,0.2))


#Adds an additional column to the scaled numeric lineup data indicating the cluster it belongs to
lineup_clust_train <- augment(k_lineup,lineups_scaled)
lineup_clust_train <- lineup_clust_train %>% arrange(.cluster)
barplot(lineup_clust_train$TOT_PT_DIFF,
        main = "Point Differential Per Cluster",
        col = c("red","blue","green","orange")[unclass(lineup_clust_train$.cluster)],
        xlab = "Lineup Cluster",
        ylab = "Total Point Differential")

legend("bottomright", 
       legend = c("Cluster 1", "Cluster 2","Cluster 3","Cluster 4"), 
       fill = c("red", "blue", "green", "orange"))

barplot(lineup_clust_train$FG_PCT_DIFF,
        main = "Field Goal Percentage Differential Per Cluster",
        col = c("red","blue","green","orange")[unclass(lineup_clust_train$.cluster)],
        xlab = "Lineup Cluster",
        ylab = "Field Goal Percentage")

barplot(lineup_clust_train$X3P_PCT_DIFF,
        main = "Three Point Percentage Differential Per Cluster",
        col = c("red","blue","green","orange")[unclass(lineup_clust_train$.cluster)],
        xlab = "Lineup Cluster",
        ylab = "Three Point Percentage")
        
barplot(lineup_clust_train$REB,
        main = "Rebounds Per Cluster",
        col = c("red","blue","green","orange")[unclass(lineup_clust_train$.cluster)],
        xlab = "Lineup Cluster",
        ylab = "Rebounds")

barplot(lineup_clust_train$AST,
        main = "Assists Per Cluster",
        col = c("red","blue","green","orange")[unclass(lineup_clust_train$.cluster)],
        xlab = "Lineup Cluster",
        ylab = "Assists")

barplot(lineup_clust_train$BLK,
        main = "Blocks Per Cluster",
        col = c("red","blue","green","orange")[unclass(lineup_clust_train$.cluster)],
        xlab = "Lineup Cluster",
        ylab = "Blocks")

barplot(lineup_clust_train$STL,
        main = "Steals Per Cluster",
        col = c("red","blue","green","orange")[unclass(lineup_clust_train$.cluster)],
        xlab = "Lineup Cluster",
        ylab = "Steals")

barplot(lineup_clust_train$TOV,
        main = "Turnovers Per Cluster",
        col = c("red","blue","green","orange")[unclass(lineup_clust_train$.cluster)],
        xlab = "Lineup Cluster",
        ylab = "Turnovers")


#Code (from Isys) used to determine lineup type based on the clustering of players completed previously.
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

source('kmeans_preprocess.R')
# get lineup types for all lineups
all_lineup_types <- sapply(train_for_lineups_full_stats$LINEUP %>% as.character, function(x) get_lineup_type(x, k_player_clust))

lineup_player_types <- cbind(lineup_clust_train, LINEUP_ID = all_lineup_types)
lineup_player_types <- as.data.frame(lineup_player_types) %>% arrange(LINEUP_ID) %>% select(-.cluster)
View(lineup_player_types)

barplot(lineup_player_types$TOT_PT_DIFF,
        main = "Point Differential Per Cluster Player Type",
        xlab = "Lineup Cluster",
        ylab = "Total Point Differential")

barplot((lineup_player_types %>% arrange(desc(TOT_PT_DIFF)))$TOT_PT_DIFF,
        main = "Point Differential Per Cluster Player Type",
        xlab = "Lineup Cluster",
        ylab = "Total Point Differential",
        names.arg = (lineup_player_types %>% arrange(desc(TOT_PT_DIFF)))$LINEUP_ID)


barplot(lineup_player_types$FG_PCT_DIFF,
        main = "Field Goal Percentage Differential Per Cluster Player Type",
        xlab = "Lineup Cluster",
        ylab = "Field Goal Percentage")

barplot((lineup_player_types %>% arrange(desc(FG_PCT_DIFF)))$FG_PCT_DIFF,
        main = "Point Differential Per Cluster Player Type",
        xlab = "Lineup Cluster",
        ylab = "Total Point Differential",
        names.arg = (lineup_player_types %>% arrange(desc(TOT_PT_DIFF)))$LINEUP_ID)

barplot(lineup_player_types$X3P_PCT_DIFF,
        main = "Three Point Percentage Differential Per Cluster Player Type",
        xlab = "Lineup Cluster",
        ylab = "Three Point Percentage")

barplot(lineup_player_types$REB,
        main = "Rebounds Per Cluster Player Type",
        xlab = "Lineup Cluster",
        ylab = "Rebounds")

barplot(lineup_player_types$AST,
        main = "Assists Per Cluster Player Type",
        xlab = "Lineup Cluster",
        ylab = "Assists")

barplot(lineup_player_types$BLK,
        main = "Blocks Per Cluster Player Type",
        xlab = "Lineup Cluster",
        ylab = "Blocks")

barplot(lineup_player_types$STL,
        main = "Steals Per Cluster Player Type",
        xlab = "Lineup Cluster",
        ylab = "Steals")

barplot(lineup_player_types$TOV,
        main = "Turnovers Per Cluster Player Type",
        xlab = "Lineup Cluster",
        ylab = "Turnovers")




# The following code are other examples of lineup clustering that has yet to be expanded upon, but feel free
# to use previous code for heatmaps and lineup type to continue exploration


#k-means on lineup pca data
k_pca_lineup <- kmeans(pca_lineup_scaled, 4, nstart=25, iter.max=1000)
k_pca_lineup
plot(pca_lineup_scaled, col=k_pca_lineup$clust, pch=16)

fviz_cluster(
  k_pca_lineup,
  data = pca_lineup_scaled,
  main = ''
)

plot3d(pca_lineup_scaled, col=k_pca_lineup$clust) #Clustering Without PCA



library(FactoMineR)
library(ppclust)
set.seed(125)
c_lineup <- fcm(lineups_scaled, centers = 3,m=1.5, nstart = 5, fixmemb= TRUE) # fuzzy c-means testing
plot(pca_lineup_scaled,
     main = "Lineup Clustering: K=3",
     xlab = 'PCA Dim 1 (24.3%)', xlim = c(-4, 4),
     ylab = 'PCA Dim 2 (15.8%)', ylim = c(-4,4),
     pch = c(1,2,3)[unclass(c_lineup$cluster)],
     col = c("red","blue","green")[unclass(c_lineup$cluster)])
legend(-4.15,1,
       legend=c("Cluster 1", "Cluster 2","Cluster 3"),
       col=c("red", "blue", "green"),
       pch= c(1,2,3), cex=0.6)


c1_lineup <- ppclust2(c_lineup, "kmeans") # Makes it possible to see scatter plot

fviz_cluster(c1_lineup, data = lineups_scaled, 
             ellipse.type = "convex",
             palette = "jco"
)



x<- as.data.frame(unique(lineup_player_types$LINEUP_ID))
View(x)

for (i in 1:68){
print(unlist(unique(lineup_player_types$LINEUP_ID))[i])
print(mean(lineup_player_types$LINEUP_ID == unlist(unique(lineup_player_types$LINEUP_ID))[i]))
}

