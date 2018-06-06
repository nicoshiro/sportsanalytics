
### Using FactoMineR ###

library(FactoMineR)
source('preprocess_individuals.R')

# get aggregate player data
aggregate_players <- get_aggregate_players()

# make a copy of detailed positions
aggregate_players <- aggregate_players %>% mutate(PosDetail = Pos)

# group regular positions
levels(aggregate_players$Pos) <- c('Center',
                                   'Center',
                                   'Forward',
                                   'Forward',
                                   'Forward',
                                   'Guard',
                                   'Guard')

# remove players who haven't played more than 50 minutes
aggregate_players <- aggregate_players %>% filter(MP > 50)


# get numeric data (excluding result, GS, MP, and GmSc)
numeric <- aggregate_players %>% select(FG:PTS)

# set NA percentages to 0
numeric[is.na(numeric)] <- 0

# scale numeric data before clustering
numeric_scaled <- scale(numeric)

### PCA Function ###
pc <- prcomp(numeric_scaled,scale = TRUE, center = TRUE)
pc

plot(pc)
plot(pc, type='l') # Plots Variances per Principle Component
summary(pc) 

# First three principal components
comp <- data.frame(pc$x[,1:3])
comp
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

### Within Groups Sum ofSquares ###
wss <- (nrow(numeric_scaled)-1)*sum(apply(numeric_scaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(numeric_scaled,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") # Elbow at about 3~4 clusters

k <- kmeans(comp, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

fviz_cluster(
  k,
  data = numeric_scaled,
  main = ''
)