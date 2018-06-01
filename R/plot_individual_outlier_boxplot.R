source('preprocess_individuals.R')

par(mfrow = c(1,2))
par(oma = c(3, 3, 0, 0)) # make room for single x and y axis labels
par(mar = c(2, 2, 1, 1)) # make plots closer together

# get aggregate player data
aggregate_players <- get_aggregate_players()
# get numeric data (excluding result, GS, MP, and GmSc)
numeric <- aggregate_players %>% select(Age, Result:GmSc)
# scale numeric data for plotting
numeric_data_scaled <- scale(numeric) %>% data.frame()
# make periods -> percentages for better axis labels
names(numeric_data_scaled) <- gsub('\\.', '%', names(numeric_data_scaled))
# make boxplot of all variables
boxplot(numeric_data_scaled,
        vertical=TRUE, las=2, xaxt='n', ann=FALSE,
        ylim=range(-6:19))

aggregate_players <- get_aggregate_players()
# remove players who haven't played more than 50 minutes
aggregate_players <- aggregate_players %>% filter(MP > 50)
numeric <- aggregate_players %>% select(Age, Result:GmSc)
numeric_data_scaled <- scale(numeric) %>% data.frame()
names(numeric_data_scaled) <- gsub('\\.', '%', names(numeric_data_scaled))
boxplot(numeric_data_scaled,
        vertical=TRUE, las=2, xaxt='n', ann=FALSE,
        ylim=range(-6:19))


title(xlab = 'Standardized Player Statistics', outer = TRUE, line = -0.25)

filename="indiviudal_outlier_boxplot.svg"
dev.copy(svg, filename)
dev.off()
