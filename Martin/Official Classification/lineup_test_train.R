library(readr)
library(caret)
library(dplyr)

# read in stints for all games in the season
print('Loading data...')
aggregated_bf_lineups_more_stats_2017_2018 <- read_csv("~/Desktop/sportsanalytics-master/data/2017-2018 Data/lineups/aggregated_bf_lineups_more_stats_2017_2018.csv")
#Parsed with column specification: ### Line subject to change depedning on where this file is saved

# split into train and test set
# TODO: make this split such that all players show up in both sets
set.seed(234)
train_indices_for_lineups <- createDataPartition(aggregated_bf_lineups_more_stats_2017_2018$TOT_PT_DIFF, p=0.8) %>% unlist()
train_for_lineups <- aggregated_bf_lineups_more_stats_2017_2018[train_indices_for_lineups,]
test_for_lineups <- aggregated_bf_lineups_more_stats_2017_2018[-train_indices_for_lineups,]

write_csv(train_for_lineups, '../data/train_for_lineups_full.csv')
write_csv(test_for_lineups, '../data/test_for_lineups_full.csv')




# train_for_lineups <- train_for_lineups %>%
#   filter(MIN > 50)
# test_for_lineups <- test_for_lineups %>%
#   filter(MIN > 50)
# 
# write_csv(train_for_lineups, '../data/train_for_lineups.csv')
# write_csv(test_for_lineups, '../data/test_for_lineups.csv')
# 
