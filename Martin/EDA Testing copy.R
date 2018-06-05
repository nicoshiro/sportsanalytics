#fuzzy_test <- cmeans(numeric_scaled, 3, iter.max = 100, dist = "euclidean", m = 2)
fuzzy_vis <- ppclust2()
library(factoextra)
library(ppclust)
library(factoextra)
library(cluster)
library(fclust)

### Fuzzy C Means ###
fuzzy_test <- fcm(numeric_scaled, centers = 3,m=1.5, nstart = 5, fixmemb= TRUE) # fuzzy c-means testing
#plotcluster(fuzzy_test, cp=1, trans=TRUE)

fuzzy_test2 <- ppclust2(fuzzy_test, "kmeans") # Makes it possible to see scatter plot

fviz_cluster(fuzzy_test2, data = numeric_scaled, 
                         ellipse.type = "convex",
                         palette = "jco"
                                        )

### Divisive Hierarchal Clustering ###
plot(diana(numeric_scaled,metric = "euclidean"))



### Agglomerative ###
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

hclust(dist(numeric_scaled), method = "complete")
plot(hclust(dist(numeric_scaled), method = "complete"))


### dbscan ###
library(dbscan)
dbscan_test <- dbscan(numeric_scaled, eps = 3.20, minPts = 3)
kNNdistplot(numeric_scaled, k = 3)
abline(h=3.20, col = "red", lty=2)
plot(numeric_scaled, col=dbscan_test$cluster)
points(numeric_scaled[dbscan_test$cluster==0,], pch = 3, col = "grey")
hullplot(numeric_scaled, dbscan_test)


### OPTICS ###
optics_test <- optics(numeric_scaled,eps = 10, minPts = 10)
plot(optics_test)
polygon(numeric_scaled[optics_test$order,], )
hullplot(numeric_scaled, optics_test)


### EM Clustering ###
emcluster(numeric_scaled, emobj = NULL, assign.class = TRUE)

### Regression Tree ###
tree_test <- rpart(PTS ~ FG + FGA + FG. + X2P + X2PA + X2P. + X3P + X3PA + X3P. + FT + FTA + FT. + ORB + DRB + TRB + AST + STL + BLK + TOV + PF, data = as.data.frame(numeric_scaled), method = "anova") 
plot(tree_test)
plotcp(tree_test)



### PCA ###
prcomp(numeric, scale = TRUE, center = TRUE)
plot(prcomp(numeric, scale = TRUE, center = TRUE))
summary(prcomp(numeric, scale = TRUE, center = TRUE))
biplot(prcomp(numeric, scale = TRUE, center = TRUE))


### Kernal PCA ###
kpca(numeric_scaled,kernel="polydot", sigma=0.2)


# # define training control
# train_control <- trainControl(method="cv", number=10)
# # fix the parameters of the algorithm
# grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# # train the model
# model <- train( ,data=numeric_scaled, trControl=train_control, method="nb", tuneGrid=grid)
# # summarize results
# print(model)