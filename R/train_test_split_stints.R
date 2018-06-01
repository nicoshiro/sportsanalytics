library(readr)
library(caret)
library(dplyr)

# read in stints for all games in the season
print('Loading data...')
all_stints <- read_csv('../data/new_all_stints_2015_2016.csv')

# split into train and test set
# TODO: make this split such that all players show up in both sets
set.seed(123)
train_indices <- createDataPartition(all_stints$pt.diff.per.min, p=0.8) %>% unlist()
stints_train <- all_stints[train_indices,]
stints_test <- all_stints[-train_indices,]

write_csv(stints_train, '../data/stints_train.csv')
write_csv(stints_test, '../data/stints_test.csv')


stints_train <- stints_train %>%
  filter(minutes_played > 2)
stints_test <- stints_test %>%
  filter(minutes_played > 2)

write_csv(stints_train, '../data/stints_train_filtered.csv')
write_csv(stints_test, '../data/stints_test_filtered.csv')

