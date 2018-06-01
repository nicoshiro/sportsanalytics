library(GGally)source('preprocess_individuals.R')

# get aggregate player data
aggregate_players <- get_aggregate_players()

# remove players who haven't played more than 50 minutes
# TOGGLE to show before/after
# aggregate_players <- aggregate_players %>% filter(MP > 50)

# get numeric data (excluding result, GS, MP, and GmSc)
numeric <- aggregate_players %>% select(Age, Result:GmSc)

# plot ggpairs for select variables
ggpairs(numeric %>% select(MP, X3P, FT, ORB, BLK, PTS))
