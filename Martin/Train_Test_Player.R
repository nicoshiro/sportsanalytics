library(readr)
library(caret)
library(dplyr)

# read in stints for all games in the season
print('Loading data...')
all_stints <- read_csv('../data/NEW_all_per_minute_2017_2018.csv') ### Line subject to change depedning on where this file is saved

# split into train and test set
# TODO: make this split such that all players show up in both sets
set.seed(123)
train_indices <- createDataPartition(NEW_all_per_minute_2017_2018_cs$FGM, p=0.8) %>% unlist()
player_train <- NEW_all_per_minute_2017_2018_cs[train_indices,]
player_test <- NEW_all_per_minute_2017_2018_cs[-train_indices,]

write_csv(player_train, '../data/player_train.csv')
write_csv(player_test, '../data/player_test.csv')




player_train <- player_train %>%
  filter(MIN > 50)
player_test <- player_test %>%
  filter(MIN > 50)

write_csv(player_train, '../data/player_train_filtered.csv')
write_csv(player_test, '../data/player_test_filtered.csv')

