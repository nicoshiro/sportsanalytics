library(factoextra)
library(descr)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(igraph)

setwd("/Users/isysjo/Documents/Summer REU 2018/SportsAnalytics/R")
# make graph of lineups for each team
# do regression on correlations and predicted performance?
source("../Isys/cluster_id_model.R")

# get lineups
all_lineups <- read.csv('../Data/2017_2018/train_for_lineups_full.csv')
all_lineups<- rbind(all_lineups,read.csv("test_for_lineups_full.csv"))

numeric_lin <- all_lineups %>% dplyr::select(TOTAL_MIN:TOV, -TOTAL_MIN, -LINEUP, -TOT_PT_DIFF)
numeric_lin_scaled <- scale(numeric_lin)

# get individual player data and scale it
clean_individual_player_data <- function(player_table) {
  player_table <- player_table %>%
    filter(MIN > 0)
  return(player_table)
}

ap_training <- read.csv('../Data/train_test/player_train_filtered_for_lineups.csv')
aggregate_training <- ap_training %>% clean_individual_player_data() %>% filter(MIN > 50)

filtered_training <- aggregate_training %>% dplyr::select(FGM:BOX_OUTS, -POS)
filtered_training[is.na(filtered_training)] <- 0
filtered_scaled <- scale(filtered_training)

## function to get Lineup types
get_lineup_type <- function(lineup, clusters_added) {
  fixed_lineup <- strsplit(lineup, ',')[[1]]
  # make a list that has the cluster for each players and 0 if that player was not in the clusters list
  people_edit <- sapply(fixed_lineup, 
                        function(x) if(x %in% clusters_added[grep(x, clusters_added$PLAYER_NAME),"PLAYER_NAME"]) 
                        {clusters_added[grep(x,clusters_added$PLAYER_NAME),]$cluster} else {0} )
  people_edit <- sort(people_edit)
  type <- paste(people_edit, collapse="")
  return(type)
}

ap_training <- read.csv('../Data/train_test/player_train_filtered_for_lineups.csv')
aggregate_training <- ap_training %>% clean_individual_player_data() %>% filter(MIN > 50)

filtered_training <- aggregate_training %>% dplyr::select(FGM:BOX_OUTS, -POS)
filtered_training[is.na(filtered_training)] <- 0
filtered_scaled <- scale(filtered_training)

# given a list of lineups, find the ones that differ by one player
# modified connections, instead of one player difference, see id theres a way to relate ones with the same lineup types
# maybe the correlations are more indicative?
# another idea, find a way to see if two lineup types are similar? extend concept to similar lineup types 
#(in the case of singleton lineup types) there should be a "closest lineup ID" in terms of statistics, and then we can
#just fit the lineup type to said closest ID
model_clust <- Mclust(filtered_scaled, G=1:9, modelNames = mclust.options("emModelNames"))
clusters_added_EM <- cbind(aggregate_training, cluster = model_clust$classification)

# get lineup types for all lineups
all_mlineup_types <- sapply(all_lineups$LINEUP %>% as.character, function(x) get_lineup_type(x, clusters_added_EM))


# get player comination 


# getting the connected vertices for player differentials
get_connected_vertices <-function(lineup) {
  differences <- c()
  lineup_fixed <- strsplit(lineup %>% as.character, ',') %>% unlist
  for(i in 1:nrow(all_lineups)) {
    current_lineup <- strsplit(all_lineups$LINEUP[i] %>% as.character, ',') %>% unlist
    differences[i] <- length(which(lineup_fixed %in% current_lineup == FALSE))
  }
  differences[which(differences != 1)] <- 0
  return(differences)
}

# get square adjacency matrix
all_connected_vertices<- sapply(all_lineups$LINEUP, get_connected_vertices)
set.seed("123")
j.graph.training <- graph.adjacency(all_connected_vertices, mode="undirected")
plot(j.graph.training,
     layout = layout.auto(j.graph.training),
     main ="Player Point Differentials", vertex.size=5, vertex.label.cex=.5)


# set value of each vertex the point differential, and give each lineup an efficienct rating
V(j.graph.training)$PT_DIFF <- all_lineups$TOT_PT_DIFF
V(j.graph.training)$name <- all_lineups$LINEUP %>% as.character
V(j.graph.training)[1:302]$testing <- FALSE
V(j.graph.training)[303:372]$testing <- TRUE
V(j.graph.training)$LINEUP_ID <- all_mlineup_types



for(w in 1:374) {
  V(j.graph.training)[w]$EST_EFF <- sum(numeric_lin_scaled[w,1:6]) - numeric_lin_scaled[w,7]
}

#using scaled data, make the weights of the edges the correlation between the missing players

  for(v in 1:length(V(j.graph.training))) {
   
    vertex_lineup <- strsplit(V(j.graph.training)[v]$name %>% as.character, ',') %>% unlist
    #print(vertex_lineup)
    if(length(neighbors(j.graph.training, V(j.graph.training)[v])) > 0) {
    for(n in 1:length(neighbors(j.graph.training, V(j.graph.training)[v]))) {
      
      adjacent_lineup <- strsplit(V(j.graph.training)[neighbors(j.graph.training,V(j.graph.training)[v])[n]]$name 
                                  %>% as.character, ',') %>% unlist
      
      d_player1 <- vertex_lineup[which(vertex_lineup %in% adjacent_lineup == FALSE)]
      d_player2 <- adjacent_lineup[which(adjacent_lineup %in% vertex_lineup == FALSE)]
      correlation <- cor(filtered_scaled[which(aggregate_training$PLAYER_NAME == d_player1),],
                         filtered_scaled[which(aggregate_training$PLAYER_NAME == d_player2),])
      #print(correlation)
      E(j.graph.training)[which(E(j.graph.training) == incident(j.graph.training, V(j.graph.training)[v], 
                                mode="all")[n])]$COR <- correlation
    }
    }
  }

matrix_j <- matrix(nrow=length(E(j.graph.training)), ncol=7)
for(j in 1:nrow(matrix_j)) {
  matrix_j[j,1] <- ends(j.graph.training,E(j.graph.training)[j])[1]
  matrix_j[j,2] <- V(j.graph.training)[ends(j.graph.training,E(j.graph.training)[j])[1]]$EST_EFF %>% as.numeric
  matrix_j[j,3] <- V(j.graph.training)[ends(j.graph.training,E(j.graph.training)[j])[1]]$PT_DIFF %>% as.numeric
  matrix_j[j,4] <- ends(j.graph.training,E(j.graph.training)[j])[2]
  matrix_j[j,5] <- V(j.graph.training)[ends(j.graph.training,E(j.graph.training)[j])[2]]$EST_EFF %>% as.numeric
  matrix_j[j,6] <- V(j.graph.training)[ends(j.graph.training,E(j.graph.training)[j])[2]]$PT_DIFF %>% as.numeric
  matrix_j[j,7] <- E(j.graph.training)[j]$COR
  
  
}

#show that there is a positive correlation between efficiency and pt diff
plot(V(j.graph.training)[1:302]$EST_EFF,V(j.graph.training)[1:302]$PT_DIFF)
# correlation:  0.6477128

# the correlation between the player correlations and the differences in performance
# seems to be.... not a correlation
# correlation: -0.03835503
#see difference between difference in efficiencies and correlations
differences <- abs((matrix_j[,3] %>% as.numeric)-(matrix_j[,6] %>% as.numeric))[which(!is.na(matrix_j[,5]))]
plot(E(j.graph.training)$COR,differences)



