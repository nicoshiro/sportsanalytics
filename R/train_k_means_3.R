library(cluster)
library(factoextra)
library(gmodels)
library(descr)
library(stargazer)
library(dplyr)

source('preprocess_individuals.R')

# get aggregate player data
aggregate_players <- get_aggregate_players()

# make a copy of detailed positions
aggregate_players <- aggregate_players %>% mutate(PosDetail = Pos)

# group regular positions
levels(aggregate_players$Pos) <- c('Center',
                                   'Center',
                                   'Forward',
                                   'Forward',
                                   'Forward',
                                   'Guard',
                                   'Guard')

# remove players who haven't played more than 50 minutes
aggregate_players <- aggregate_players %>% filter(MP > 50)

# get numeric data (excluding result, GS, MP, and GmSc)
numeric <- aggregate_players %>% select(FG:PTS)

# set NA percentages to 0
numeric[is.na(numeric)] <- 0

# scale numeric data before clustering
numeric_scaled <- scale(numeric)

# show gap statistic plot
# set.seed(123)
# fviz_nbclust(
#   numeric_scaled,
#   kmeans,
#   nstart = 25,
#   method = "gap_stat",
#   k.max = 15,
#   nboot = 500,
#   verbose = TRUE,
#   main = ''
# )

# k means with 3 clusters
set.seed(123)
num_clusters <- 3
km.res <- kmeans(numeric_scaled, num_clusters, nstart = 25)
fviz_cluster(
  km.res,
  data = numeric_scaled,
  main = ''
)


# add column cluster labels to our player data
clusters_added <- cbind(aggregate_players, cluster = km.res$cluster)

# make table for position breakdown in each cluster
ct <- gmodels::CrossTable(clusters_added$Pos, clusters_added$cluster,
                 prop.r = TRUE, prop.c = TRUE, prop.t=FALSE, prop.chisq = FALSE)
ctdetail <- CrossTable(clusters_added$PosDetail, clusters_added$cluster,
                       prop.r = TRUE, prop.c = FALSE, prop.t=FALSE, prop.chisq = FALSE)

# make mosaic plots for position breakdown in each cluster
# emulate ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
colors = gg_color_hue(3)
crosstab(clusters_added$Pos, clusters_added$cluster,
         prop.r = TRUE, prop.c = TRUE, prop.t=FALSE, prop.chisq = FALSE,
         ylab = 'Position', xlab = '', color=colors)
par(mar = c(1,0,0,0))
# from "C"   "C-F" "F"   "F-C" "F-G" "G"   "G-F"
# to 'Center', 'Center-Forward', 'Forward', 'Forward Center', 'Forward-Guard',
#    'Guard', 'Guard-Forward'
# set names for mosaic plot
positions = clusters_added$PosDetail
levels(positions) = c('Center', 'Center-Forward', 'Forward',
                      'Forward Center', 'Forward-Guard\n\n', 'Guard\n',
                      'Guard-Forward')
clusters = clusters_added$cluster
clusters = paste('Cluster', clusters)
# make axis labels closer
# par(mgp=c(0,0,0))
crosstab(positions, clusters,
         prop.r = TRUE, prop.c = TRUE, prop.t=FALSE, prop.chisq = FALSE,
         ylab = 'Position', xlab = '', color=c(gg_color_hue(7)[c(1,2,3,4,6,5,7)]))#colors[1], colors[1],
                                               #colors[2], colors[2], colors[2],
                                               #colors[3], colors[3]))
         # color = rainbow(7, s = 0.5))

# turn simple crosstable row proportions into latex
row.props <- data.frame(ct$prop.row) %>%
  spread(key = y, value = Freq)
names(row.props) <- c('Position', 'Cluster 1', 'Cluster 2', 'Cluster 3')
row.props <- as.matrix(row.props)
for (colnum in 2:(num_clusters+1)) {
  row.props[,colnum] <- round(as.numeric(row.props[,colnum]), 3)
}
# print as latex
stargazer(row.props, type = 'latex')

# turn detailed crosstable row proportions into latex
row.props <- data.frame(ctdetail$prop.row) %>%
  spread(key = y, value = Freq)
names(row.props) <- c('Position', 'Cluster 1', 'Cluster 2', 'Cluster 3')
row.props <- as.matrix(row.props)
for (colnum in 2:(num_clusters+1)) {
  row.props[,colnum] <- round(as.numeric(row.props[,colnum]), 3)
}
# print as latex
stargazer(row.props, type = 'latex')
