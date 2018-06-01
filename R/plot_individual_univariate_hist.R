source('preprocess_individuals.R')

# get aggregate player data and filter out players with less than 50 MP
all_players <- get_aggregate_players() %>% filter(MP > 50)

# make periods -> percentages for better axis labels
names(all_players) <- gsub('\\.', '%', names(all_players))

# make 5 x 5 density plot
par(mfrow = c(5,5),
    oma = c(5,4,0,0) + 0.1,
    mar = c(1,0,1,2) + 0.1)
for (colname in names(select_if(all_players, is.numeric))[2:26]) {
  data_to_plot <- all_players[,toString(colname)][[1]]
  plot(density(data_to_plot, na.rm = TRUE), xlab = '', main = '', ylab = '')
  title(xlab = colname, line=-5, adj = .98)
}

# make 5 x 5 histogram plot
par(mfrow = c(5,5),
    oma = c(5,4,0,0) + 0.1,
    mar = c(1,0,1,2) + 0.1)
for (colname in names(select_if(all_players, is.numeric))[2:26]) {
  data_to_plot <- all_players[,toString(colname)][[1]]
  hist(data_to_plot, xlab = '', main = '', ylab = '')
  title(xlab = colname, line=-5, adj = .98)
}

# highlight those with multimodal distributions
par(mfrow = c(3,3))
for (colname in c('Result', 'GS', 'X3P', 'X3PA', 'X3P%', 'ORB', 'TRB', 'BLK', 'TOV')) {
  data_to_plot <- all_players[,toString(colname)][[1]]
  plot(density(data_to_plot, na.rm = TRUE), xlab = '', main = '', ylab = '')
  title(xlab = colname, line=-9, adj = .98)
}
