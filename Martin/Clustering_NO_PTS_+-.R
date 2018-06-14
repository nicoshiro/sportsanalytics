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
train_fifty <- player_train_filtered %>% filter(MIN > 50)

# get numeric data (excluding result, GS, MP, and GmSc)
train_numeric <- train_fifty %>% select(FGM:BOX_OUTS, -POS)

# set NA percentages to 0
filtered_train_numeric[is.na(train_numeric)] <- 0

# scale numeric data before clustering
train_scaled <- scale(filtered_train_numeric)

### Without Plus/Minus or PTS (HEIRARCHAL CLUSTERING)
dist_train <- as.data.frame(train_scaled) %>% filter(-PTS, -PLUS_MINUS)

### K MEANS Clustering
wss <- (nrow(dist_train)-1)*sum(apply(dist_train,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dist_train,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") # Elbow at about 3~4 clusters


k <- kmeans(dist_train, 4, nstart=25, iter.max=1000)
k
library(RColorBrewer)
library(scales)
library(factoextra)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(pca_coord_scaled, col=k$clust, pch=16)

fviz_cluster(
  k,
  data = dist_train,
  main = ''
)


## WITH PCA
library(cluster)
library(FactoMineR)
pca <- PCA(dist_train)
plot(pc, type = 'l') # Four Principle Components

pca_coord <- pca$ind$coord 
pca_coord[is.na(pca_coord)] <- 0 
pca_coord_scaled <- scale(pca_coord)


k_pca <- kmeans(pca_coord_scaled, 4, nstart=25, iter.max=1000)
k_pca
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(pca_coord_scaled, col=k_pca$clust, pch=16)

fviz_cluster(
  k_pca,
  data = pca_coord_scaled,
  main = ''
)


### Fuzzy C Means
library(FactoMineR)
library(ppclust)
set.seed(145)
c <- fcm(dist_train, centers = 3,m=1.5, nstart = 5, fixmemb= TRUE) # fuzzy c-means testing
#plotcluster(fuzzy_test, cp=1, trans=TRUE)

c1 <- ppclust2(c, "kmeans") # Makes it possible to see scatter plot

fviz_cluster(c1, data = dist_train, 
             ellipse.type = "convex",
             palette = "jco"
)



### With PCA
set.seed(125)
c_pca <- fcm(pca_coord_scaled, centers = 3,m=1.5, nstart = 5, fixmemb= TRUE) # fuzzy c-means testing
#plotcluster(fuzzy_test, cp=1, trans=TRUE)

c1_pca <- ppclust2(c_pca, "kmeans") # Makes it possible to see scatter plot

fviz_cluster(c1_pca, data = dist_train, 
             ellipse.type = "convex",
             palette = "jco"
)
