library(devtools)
devtools::install_github("paultpearson/TDAmapper")
library(TDAmapper)


# The fastcluster package is not necessary.  By loading the
# fastcluster package, the fastcluster::hclust() function 
# automatically replaces the slower stats::hclust() function
# whenever hclust() is called.
install.packages("fastcluster") 
require(fastcluster) 

distance_matrix = dist(data.frame( x=2*cos(0.5*(1:100)), y=sin(1:100) ))
filter_values = 2*cos(0.5*(1:100))
m1 <- mapper1D(
  distance_matrix = dist(data.frame( x=2*cos(0.5*(1:100)), y=sin(1:100) )),
  filter_values = 2*cos(0.5*(1:100)),
  num_intervals = 10,
  percent_overlap = 50,
  num_bins_when_clustering = 10)

# The igraph package is necessary to view simplicial complexes
# (undirected graph) resulting from mapper1D().
install.packages("igraph") 
library(igraph)

g1 <- graph.adjacency(m1$adjacency, mode="undirected")
plot(g1, layout = layout.auto(g1) )


### Using mapper2D
m2 <- mapper2D(
  distance_matrix = dist(data.frame( x=2*cos(1:100), y=sin(1:100) )),
  filter_values = list( 2*cos(1:100), sin(1:100) ),
  num_intervals = c(5,5),
  percent_overlap = 50,
  num_bins_when_clustering = 10)

g2 <- graph.adjacency(m2$adjacency, mode="undirected")
plot(g2, layout = layout.auto(g2) )


### using mapper1D to identify two independent spirals as two line segments
# sample points from two intertwined spirals
set.seed("1")
t <- runif(100, min=1, max=6.3) # theta
X <- data.frame( x = c( t*cos(t), -t*cos(t) ), y = c( t*sin(t), -t*sin(t) ) )
d <- dist(X)
plot(X[,1], X[,2])

filter <- X[,2] # height projection
num_intervals <- 10
percent_overlap <- 50
num_bins_when_clustering <- 10

m3 <- mapper1D(
  distance_matrix = d, 
  filter_values = filter,	
  # num_intervals = 10, # use default
  # percent_overlap = 50, # use default
  # num_bins_when_clustering = 10 # use default
)

g3 <- graph.adjacency(m3$adjacency, mode="undirected")
plot(g3, layout = layout.auto(g3) )


### Interactive graphs using networkD3
# use the github version so that vertices stay on the canvas
library(devtools)
devtools::install_github("christophergandrud/networkD3")
library(networkD3)

# parametrize a trefoil knot
n <- 100
t <- 2*pi*(1:n)/n
X <- data.frame(x = sin(t)+2*sin(2*t),
                y = cos(t)-2*cos(2*t),
                z = -sin(3*t))
f <- X

m5 <- mapper(dist(X), f, c(3,3,3), c(30,30,30), 5)
g5 <- graph.adjacency(m5$adjacency, mode="undirected")
plot(g5, layout = layout.auto(g5) )
tkplot(g5)

# create data frames for vertices and edges with the right variable names 
MapperNodes <- mapperVertices(m5, 1:dim(f)[1] )
MapperLinks <- mapperEdges(m5)

# interactive plot
forceNetwork(Nodes = MapperNodes, Links = MapperLinks, 
             Source = "Linksource", Target = "Linktarget",
             Value = "Linkvalue", NodeID = "Nodename",
             Group = "Nodegroup", opacity = 0.8, 
             linkDistance = 10, charge = -400)






### NBA Data


library(mclust)
library(cluster)
library(FactoMineR)
library(descr)
library(dplyr)
library(tidyr)

setwd("/Users/nicoshiro/Downloads/lbynum-lbynum-2018-thesis-e746e5f989ce/R")

source('preprocess_individuals.R')
# new data
clean_individual_player_data <- function(player_table) 
{ player_table <- player_table %>% filter(MIN > 0)
  return(player_table) }

# modifications to load all players
path_to_files <- '/Users/nicoshiro/Downloads/lbynum-lbynum-2018-thesis-e746e5f989ce/' 
filelist <- paste0(path_to_files, list.files(path_to_files, 'all_per_minute_2017_2018.csv'))
all_players <- read.csv(filelist)

# get aggregate player data
aggregate_players <- all_players %>% clean_individual_player_data()

# remove players who haven't played more than 50 minutes
aggregate_players <- aggregate_players %>% filter(MIN > 50)

# get numeric data (excluding result, GS, MP, and GmSc)
numeric <- aggregate_players %>% select(FGM:BOX_OUTS, -POS)

# set NA percentages to 0
numeric[is.na(numeric)] <- 0

# scale numeric data before clustering
numeric_scaled <- scale(numeric)


distance_matrix = dist(numeric_scaled)
set.seed("1")
m1 <- mapper1D(
  distance_matrix = dist(numeric_scaled),
  filter_values = numeric_scaled[,19],
  num_intervals = 10,
  percent_overlap = 30,
  num_bins_when_clustering = 10)
g1 <- graph.adjacency(m1$adjacency, mode="undirected")
plot(g1, layout = layout.auto(g1) )

y.mean.vertex <- rep(0, m1$num_vertices)
for (i in 1:m1$num_vertices){
  points.in.vertex <- m1$points_in_vertex[[i]]
  y.mean.vertex[i] <-mean((numeric_scaled[,20][points.in.vertex]))
}

vertex.size <- rep(0, m1$num_vertices)
for (i in 1:m1$num_vertices){
  points.in.vertex <- m1$points_in_vertex[[i]]
  vertex.size[i] <- length((m1$points_in_vertex[[i]]))
}

y.mean.vertex.grey <- grey(1-(y.mean.vertex - min(y.mean.vertex))/(max(y.mean.vertex) - min(y.mean.vertex) ))
V(g1)$color <- y.mean.vertex.grey
V(g1)$size <- vertex.size
plot(g1,main ="Mapper Graph")
legend(x=-2, y=-1, c("y small","y medium","large y"),pch=21,
       col="#777777", pt.bg=grey(c(1,0.5,0)), pt.cex=2, cex=.8, bty="n", ncol=1)

View(m1)



set.seed("123")
nba.mapper <- mapper(
  dist_object = dist(numeric_scaled),
  filter_values = list(numeric_scaled[,19],numeric_scaled[,20]),
  num_intervals = c(4,4),
  percent_overlap = 30,
  num_bins_when_clustering = 10)
nba.graph <- graph.adjacency(nba.mapper$adjacency, mode="undirected")
plot(nba.graph,
     layout = layout.auto(nba.graph),
     main ="TDA" )


## tda with the PCA coordinates
pca <- PCA(numeric_scaled) 
pca_coord <- pca$ind$coord 
pca_coord[is.na(pca_coord)] <- 0 
pca_coord_scaled <- scale(pca_coord)

set.seed("1")
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

View(m1)
