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



### Fuzzy C Means ###
c <- fcm(comp, centers = 4,m=1.5, nstart = 5, fixmemb= TRUE) # fuzzy c-means testing
#plotcluster(fuzzy_test, cp=1, trans=TRUE)

c1 <- ppclust2(c, "kmeans") # Makes it possible to see scatter plot

fviz_cluster(c1, data = numeric_scaled, 
             ellipse.type = "convex",
             palette = "jco"
)

