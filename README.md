# Modeling Subset Behavior: Prescriptive Analytics for Professional Basketball Data

## /python
- `nearest_neighbor_lineups.py`: Implements graphical model for predicting lineup plus-minus from individual player stats.
- `stint.py`: Implements the stint class used in `nearest_neighbor_lineups.py`.

## /R
- `create_stints.R`: Parses play-by-play data into stint tables. Generates files needed to run other R scripts. *Run this before other R files*
- `plot_*`: Generates figures used in thesis report.
- `scrapeIndividualStats.R`: Code used for scraping [basketball-reference.com](basketball-reference.com). Should be used for reference only -- does not run as is.
- `train_k_means_3.R`: Runs k-means clustering on individual player data. Generates figures used in thesis report.
- `create_interaction_RAPM_design_matrix.R`: Adds interaction columns to stint table.
- `train_test_split_stints.R`: Makes train/test split for stint table.
- `standardize_individuals_2015_2016.R`: Generates `standardized_aggregate_individuals_2015_2016.csv` from `aggregate_individuals_2015_2016.csv`.
- `shiny/*`: Shiny app for visualizing individual player data.

### How to Run RAPM Model
- If not already run, run `create_stints.R` then `train_test_split_stints.R`.
- Run `regular_RAPM.R`

### How to Run Interaction RAPM Model
- If not already run, run `create_stints.R` then `train_test_split_stints.R` then `create_interaction_RAPM_design_matrix.R`.
- Run `interaction_RAPM.R`.

### NBA Analytics Summer 2018
- `create_stints2017_2018_mod.R`: Parses play-by-play data into stint tables with counts for lineup statistics. Generates files needed to run other R scripts. *Run this before other R files*
- `stint_parser`: Parses and aggregates stint data into lineup data. Also parses and aggregates data from [basketball-reference.com](basketball-reference.com) *Run this right after create_stints2017_2018_mod.R*
- `johnson_graph.R`: Generates adjacency graph of lineups by player differential.
- `cluster_id_model.R`: Gives the functions needed to do the Cluster+Lineup ID model descriped in paper.
- `individual_lineup_regression.R`: Uses the basic lm function and explores the relationships between individual players statistics, clusters, and teams.
- `tda_cluster_lineups.R`: Uses topological data analysis to cluster the lineups into lineup types.
- `tda_cluster_individuals.R`: Uses topological data analysis to cluster the individual players into new positions.
- `scraper_player_stats.R`: Scrapes individual player stats from the 2017-2018 season from basketballreference.com.
- `ridge_lasso_regression_lineups.R`: Runs the ridge regression and lasso regression model on the lineup data to try and predict plus minus.
- `ridge_lasso_regression_individual_stats.R`: Runs the ridge regression and lasso regression model on the binded individual player data to try and predict plus minus.
- `logit_regression_indiv_stats.R`: Runs logistic regression on the binded individual player data to try and predict whether a lineup will be positive or negative.
- `knn_lineups_w_clusters.R`: Implements KNN on the lineup data to verify the cluster that it was placed into.
- `create_warriors_indiv_stats_df.R`: Creates the binded individual player data frame from all combinations of the Golden State Warriors 2018-2019 roster.
- `create_lakers_indiv_stats_df.R`: Creates the binded individual player data frame from all combinations of the LA Lakers 2018-2019 roster.
- `create_all_star_df.R`: Creates the binded individual player data frame from all combinations of an all star team.
