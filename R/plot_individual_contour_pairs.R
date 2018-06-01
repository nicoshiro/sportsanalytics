library(GGally)
library(ggplot2)

source('preprocess_individuals.R')

# get aggregate player data
aggregate_players <- get_aggregate_players()

# remove players who haven't played more than 50 minutes
aggregate_players <- aggregate_players %>% filter(MP > 50)

# get numeric data (excluding result, GS, MP, and GmSc)
numeric <- aggregate_players %>% select(FG:PTS)

# fill in missing percentages with 0
numeric[is.na(numeric)] <- 0

# compute principal component analysis and plot first 4 pc dimensions
#  with contours
pc <- prcomp(numeric, scale = TRUE, center = TRUE)

pairs_contour_plot <- function(data_to_plot) {
  # make a ggpairs plot with a kernel density contour overlay for continuous
  # variables
  ggpairs(
    data_to_plot,
    lower = list(continuous = add_contour),
    upper = list(continuous = add_contour)
  )
}

add_contour <- function(data, mapping, ...){
  # create scatterplot with kernel density overlay
  plot_object <- ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.5) +
    geom_density2d()
  return(plot_object)
}

pairs_contour_plot(data.frame(pc$x[, 1:4]))

# check how much variance in first 4 PCs
summary(pc)


# plot a few example contours for some variables
levels(aggregate_players$Pos) <- c('C', 'C', 'F', 'F', 'F', 'G', 'G')
add_contour <- function(data, mapping, ...){
  # create scatterplot with kernel density overlay
  plot_object <- ggplot(data = data, mapping = mapping) +
    geom_point(aes(color = aggregate_players$Pos), alpha = 0.5) +
    geom_density2d(color = 'black', alpha = 0.5)
  return(plot_object)
}
pairs_contour_plot(numeric %>% select(BLK, X3P, TRB, PF))

