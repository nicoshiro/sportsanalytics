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
filtered_train_numeric[is.na(train_numeric)] <- 0

# scale numeric data before clustering
train_scaled <- scale(filtered_train_numeric)

dist_train <- select(as.data.frame(train_scaled), -PTS, -PLUS_MINUS)

### PCA Without PTS and +/-

library(FactoMineR)
#PCA function implemented over scaled training data excluding points and plus/minus

pca <- PCA(dist_train)
plot(pc, type = 'l') # Four Principle Components

pca_coord <- pca$ind$coord 
pca_coord[is.na(pca_coord)] <- 0 
pca_coord_scaled <- scale(pca_coord) # Data frame with scaled values for PCs

library(devtools)
devtools::install_github("paultpearson/TDAmapper")
library(TDAmapper)
library(igraph)

set.seed(123) # To make results reproducible

#TDA Mapper Graph on pca scale coordinates and first principle component as filter
pca.mapper <- mapper1D(
  distance_matrix = dist(pca_coord_scaled),
  filter_values = pca_coord_scaled[,1],
  num_intervals = 10,
  percent_overlap = 30,
  num_bins_when_clustering = 10)
pca.graph <- graph.adjacency(pca.mapper$adjacency, mode="undirected")
plot(pca.graph, layout = layout.auto(pca.graph) )

pca.y.mean.vertex <- rep(0, pca.mapper$num_vertices)
for (i in 1:pca.mapper$num_vertices){
  points.in.vertex <- pca.mapper$points_in_vertex[[i]]
  pca.y.mean.vertex[i] <-mean((pca_coord_scaled[,2][points.in.vertex]))
}

pca.vertex.size <- rep(0, pca.mapper$num_vertices)
for (i in 1:pca.mapper$num_vertices){
  points.in.vertex <- pca.mapper$points_in_vertex[[i]]
  pca.vertex.size[i] <- length((pca.mapper$points_in_vertex[[i]]))
}

pca.y.mean.vertex.grey <- grey(1-(pca.y.mean.vertex - min(pca.y.mean.vertex))/(max(pca.y.mean.vertex) - min(pca.y.mean.vertex) ))
V(pca.graph)$color <- pca.y.mean.vertex.grey
V(pca.graph)$size <- pca.vertex.size
plot(pca.graph,main ="Mapper Graph")
legend(x=-2, y=-1, c("y small","y medium","large y"),pch=21,
       col="#777777", pt.bg=grey(c(1,0.5,0)), pt.cex=2, cex=.8, bty="n", ncol=1)

pca.mapper



### Mapper Witout PCA (ALSO EXCLUDING PTS and +/-)
### Plus Minus Filter Function
set.seed(123)
reg.mapper <- mapper1D(
  distance_matrix = dist_train,
  filter_values = train_scaled[,20],
  num_intervals = 10,
  percent_overlap = 30,
  num_bins_when_clustering = 10)
reg.graph <- graph.adjacency(reg.mapper$adjacency, mode="undirected")
plot(reg.graph, layout = layout.auto(reg.graph) )

reg.y.mean.vertex <- rep(0, reg.mapper$num_vertices)
for (i in 1:reg.mapper$num_vertices){
  points.in.vertex <- reg.mapper$points_in_vertex[[i]]
  reg.y.mean.vertex[i] <-mean(train_scaled[points.in.vertex])
}

reg.vertex.size <- rep(0, reg.mapper$num_vertices)
for (i in 1:reg.mapper$num_vertices){
  points.in.vertex <- reg.mapper$points_in_vertex[[i]]
  reg.vertex.size[i] <- length((reg.mapper$points_in_vertex[[i]]))
}

reg.y.mean.vertex.grey <- grey(1-(reg.y.mean.vertex - min(reg.y.mean.vertex))/(max(reg.y.mean.vertex) - min(reg.y.mean.vertex) ))
V(reg.graph)$color <- reg.y.mean.vertex.grey
V(reg.graph)$size <- reg.vertex.size
plot(reg.graph,main ="Mapper Graph")
legend(x=-2, y=-1, c("y small","y medium","large y"),pch=21,
       col="#777777", pt.bg=grey(c(1,0.5,0)), pt.cex=2, cex=.8, bty="n", ncol=1)

### Points Per Minute Filter Function
set.seed(123)
reg2.mapper <- mapper1D(
  distance_matrix = dist_train,
  filter_values = train_scaled[,19],
  num_intervals = 10,
  percent_overlap = 30,
  num_bins_when_clustering = 10)
reg2.graph <- graph.adjacency(reg2.mapper$adjacency, mode="undirected")
plot(reg2.graph, layout = layout.auto(reg2.graph) )

reg2.y.mean.vertex <- rep(0, reg2.mapper$num_vertices)
for (i in 1:reg2.mapper$num_vertices){
  points.in.vertex <- reg2.mapper$points_in_vertex[[i]]
  reg2.y.mean.vertex[i] <-mean(train_scaled[points.in.vertex])
}

reg2.vertex.size <- rep(0, reg2.mapper$num_vertices)
for (i in 1:reg.mapper$num_vertices){
  points.in.vertex <- reg2.mapper$points_in_vertex[[i]]
  reg2.vertex.size[i] <- length((reg2.mapper$points_in_vertex[[i]]))
}

reg2.y.mean.vertex.grey <- grey(1-(reg2.y.mean.vertex - min(reg.y.mean.vertex))/(max(reg2.y.mean.vertex) - min(reg2.y.mean.vertex) ))
V(reg2.graph)$color <- reg2.y.mean.vertex.grey
V(reg2.graph)$size <- reg2.vertex.size
plot(reg2.graph,main ="Mapper Graph")
legend(x=-2, y=-1, c("y small","y medium","large y"),pch=21,
       col="#777777", pt.bg=grey(c(1,0.5,0)), pt.cex=2, cex=.8, bty="n", ncol=1)


# (GENERAL MAPPER FUNCTION)
set.seed("123")
nba.mapper <- mapper(
  dist_object = dist(dist_train),
  filter_values = list(train_scaled[,19],train_scaled[,20]),
  num_intervals = c(4,4),
  percent_overlap = 20,
  num_bins_when_clustering = 10)
nba.graph <- graph.adjacency(nba.mapper$adjacency, mode="undirected")
plot(nba.graph,
     layout = layout.auto(nba.graph),
     main ="TDA" )


#Following code for KS TESTING DO NOT COPY AS IS< CHANGE NAMES TO FIT THE MAPPER GRAPH YOU ARE USING

#Data frame that excludes scaled points and plus/minus
#dist_train <- as.data.frame(train_scaled) %>% filter(-PTS, -PLUS_MINUS)
library(dplyr)
dist_train <- select(as.data.frame(train_scaled), -PTS, -PLUS_MINUS)

#Temporary Function giving data frame of those within a cluster of the mapper graph (Change index to explore other clusters)
#Change name of test to explore different mapper graphs
mapper_vertex <- as.data.frame(slice(as.data.frame(train_scaled), nba.mapper[["points_in_vertex"]][[8]]))

#Excludes members of the cluster
pop <- as.data.frame(slice(as.data.frame(train_scaled), -nba.mapper[["points_in_vertex"]][[8]]))

#KS TEST COMPARING CLUSTER AGAINST ALL TRAINING DATA
library(Matching)
for (i in 1:dim(train_scaled)[2]){
  print(i)
  print(ks.boot(as.data.frame(mapper_vertex[,i]), train_scaled[,i]))  }


#KS TEST COMPARING CLUSTER WITH AGANIST EVRYONE ELSE
for (i in 1:dim(train_scaled)[2]){
  print(i)
  print(ks.boot(as.data.frame(mapper_vertex[,i]), pop[,i]))
  }



#To View Players in Particular cluster 
View(slice(train_fifty, reg2.mapper[["points_in_vertex"]][[8]])) #(change indeices as needed) )



View(train_scaled)

