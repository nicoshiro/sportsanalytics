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
plot(pca_lineup, type = 'p')

#Isolate the coordinates from PCA and Scale them
pca_lineup_coord <- pca_lineup$ind$coord
pca_lineup_coord[is.na(pca_lineup_coord)] <- 0
pca_lineup_scaled <- scale(pca_lineup_coord)

#
# extract some parts for plotting
PC1 <- pca_lineup$ind$coord[,1]
PC2 <- pca_lineup$ind$coord[,2]
labs <- rownames(pca_lineup$ind$coord)
PCs <- data.frame(cbind(PC1,PC2))
rownames(PCs) <- labs

set.seed(235)
k_lineup <- kmeans(lineups_scaled, 4, nstart=25, iter.max=1000)

#The following code is to create heatmaps based on the just constructed clusters,
#be sure to change the following code to reflect the number of clusters used above

library(ggplot2)
library(reshape2)
library(plyr)
library(scales)

#Adds an additional column to the full training lineup data indicating the cluster it belongs to
clust_lineup <-augment(k_lineup, train_for_lineups_full_stats)

heat_data_clust <- clust_lineup %>% arrange(.cluster) %>% select(-X,-X1,-.cluster)
heat.m <- melt(as.data.frame(heat_data_clust[c(5,1,2,3,4,6,7,8,9,10)]))
heat.m <- ddply(heat.m, .(variable), transform,rescale = rescale(value))
p <- ggplot(heat.m, aes(variable, LINEUP)) + geom_tile(aes(fill = rescale),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue", ggtitle("Cluster 2 Heatmap"))
p

#arranges full data based on cluster, filters for a specific cluster, and removes unimportant variables
heat_data_clust_1 <- clust_lineup %>% arrange(.cluster) %>% filter(.cluster==1) %>% select(-X,-X1,-.cluster)

#melts descriptor variables into single column named 'Variable' and sets LINEUP as id
heat.m.1 <- melt(as.data.frame(heat_data_clust_1[c(5,1,2,3,4,6,7,8,9,10)]))

#Scales numeric data between 0 and 1
heat.m.1 <- ddply(heat.m.1, .(variable), transform,rescale = rescale(value))

#Creates the Plot
p1 <- ggplot(heat.m.1, aes(variable, LINEUP)) + geom_tile(aes(fill = rescale),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue", ggtitle("Cluster 1 Heatmap"))
p1

#Same for the rest (each block of code represents a different cluster)
heat_data_clust_2 <- clust_lineup %>% arrange(.cluster) %>% filter(.cluster==2) %>% select(-X,-X1,-.cluster)
heat.m.2 <- melt(as.data.frame(heat_data_clust_2[c(5,1,2,3,4,6,7,8,9,10)]))
heat.m.2 <- ddply(heat.m.2, .(variable), transform,rescale = rescale(value))
p2 <- ggplot(heat.m.2, aes(variable, LINEUP)) + geom_tile(aes(fill = rescale),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue", ggtitle("Cluster 2 Heatmap"))
p2

heat_data_clust_3 <- clust_lineup %>% arrange(.cluster) %>% filter(.cluster==3) %>% select(-X,-X1,-.cluster)
heat.m.3 <- melt(as.data.frame(heat_data_clust_3[c(5,1,2,3,4,6,7,8,9,10)]))
heat.m.3 <- ddply(heat.m.3, .(variable), transform,rescale = rescale(value))
p3 <- ggplot(heat.m.3, aes(variable, LINEUP)) + geom_tile(aes(fill = rescale),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue", ggtitle("Cluster 3 Heatmap"))
p3

heat_data_clust_4 <- clust_lineup %>% arrange(.cluster) %>% filter(.cluster==4) %>% select(-X,-X1,-.cluster)
heat.m.4 <- melt(as.data.frame(heat_data_clust_4[c(5,1,2,3,4,6,7,8,9,10)]))
heat.m.4 <- ddply(heat.m.4, .(variable), transform,rescale = rescale(value))
p4 <- ggplot(heat.m.4, aes(variable, LINEUP)) + geom_tile(aes(fill = rescale),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue", ggtitle("Cluster 4 Heatmap"))
p4


library(ggfortify)
lineup_clust_train <- augment(k_lineup,lineups_scaled)
pr_lineup <- PCA(lineups_scaled)



ggplot(clust_lineup, aes(PC1,PC2, label=rownames(PCs))) +
  geom_point(aes(colour = TOT_PT_DIFF, shape = .cluster)) +
  scale_colour_gradientn(colours = terrain.colors(10))

ggplot(clust_lineup, aes(PC1,PC2, label=rownames(PCs))) +
  geom_point(aes(colour = FG_PCT_DIFF, shape = .cluster)) +
  scale_colour_gradientn(colours = terrain.colors(10))

ggplot(clust_lineup, aes(PC1,PC2, label=rownames(PCs))) +
  geom_point(aes(colour = X3P_PCT_DIFF, shape = .cluster)) +
  scale_colour_gradientn(colours = terrain.colors(10))

ggplot(clust_lineup, aes(PC1,PC2, label=rownames(PCs))) +
  geom_point(aes(colour = REB, shape = .cluster)) +
  scale_colour_gradientn(colours = terrain.colors(10))

ggplot(clust_lineup, aes(PC1,PC2, label=rownames(PCs))) +
  geom_point(aes(colour = BLK, shape = .cluster)) +
  scale_colour_gradientn(colours = terrain.colors(10))

ggplot(clust_lineup, aes(PC1,PC2, label=rownames(PCs))) +
  geom_point(aes(colour = STL, shape = .cluster)) +
  scale_colour_gradientn(colours = terrain.colors(10))

ggplot(clust_lineup, aes(PC1,PC2, label=rownames(PCs))) +
  geom_point(aes(colour = AST, shape = .cluster)) +
  scale_colour_gradientn(colours = terrain.colors(10))

ggplot(clust_lineup, aes(PC1,PC2, label=rownames(PCs))) +
  geom_point(aes(colour = TOV, shape = .cluster)) +
  scale_colour_gradientn(colours = terrain.colors(10))




autoplot(prcomp(lineups_scaled), data = lineup_clust_train) +
  geom_point(aes(colour = TOV, shape = .cluster)) +
  scale_colour_gradientn(colours = terrain.colors(10))


  
  
#


