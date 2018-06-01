library(factoextra)
source('preprocess_individuals.R')

# get aggregate player data
aggregate_players <- get_aggregate_players()

# make a copy of detailed positions
aggregate_players <- aggregate_players %>% mutate(PosDetail = Pos)

# group regular positions
# levels(aggregate_players$Pos) <- c('C', 'C', 'F', 'F', 'F', 'G', 'G')

# remove players who haven't played more than 50 minutes
aggregate_players <- aggregate_players %>% filter(MP > 50)

# get numeric data (excluding result, GS, MP, and GmSc)
numeric <- aggregate_players %>% select(FG:PTS)

# fill in missing percentages with 0
numeric[is.na(numeric)] <- 0

fviz_pca_ind(
  prcomp(numeric, scale = TRUE, center = TRUE),
  title = '',
  labels = FALSE,
  habillage = aggregate_players$Pos)


