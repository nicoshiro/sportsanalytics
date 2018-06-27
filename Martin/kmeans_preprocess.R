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
train_numeric <- train_fifty %>% dplyr::select(FGM:BOX_OUTS, -POS)

# set NA percentages to 0
train_numeric[is.na(train_numeric)] <- 0

# scale numeric data before clustering
train_scaled <- as.data.frame(scale(train_numeric))

dist_train <- dplyr::select(as.data.frame(train_scaled), -PTS, -PLUS_MINUS)


set.seed(256)
k <- kmeans(dist_train, 4, nstart=25, iter.max=1000)
k

library(dplyr)
library(broom)
k_player_clust <- cbind(train_fifty, cluster = k$clust) #For lineup
clust_train_player <-augment(k, train_fifty)




