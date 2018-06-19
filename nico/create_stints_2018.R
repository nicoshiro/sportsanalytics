library(readr)
library(dplyr)

setwd("/Users/nicoshiro/Downloads/lbynum-lbynum-2018-thesis-e746e5f989ce/data/pbp")

# read in combined pbp stats for all games in the season
# pbp_combined <- read.csv('../data/sample-pbp-log/sample-combined-pbp-stats.csv')
print('Loading data...')
pbp_combined <- read_csv('[10-17-2017]-[06-08-2018]-combined-stats.csv') # modified this and the wd to load data
# fix game_id column

# initialize stint table with 1 column for each player as well as well
# as game_id, a.score, h.score, play_time, a.points, h.points,
# pt.differential
all_player_names <- sapply(dplyr::select(pbp_combined, a1:h5), unique) %>%
  unlist() %>%
  unique()

# column_names <- c('game_id', 'pt.diff.per.min', 'h.points',
# 'a.points','h.score', 'a.score', 'play_time',
# all_player_names)
# column_names <- c('game_id', 'pt.diff.per.min', all_player_names)
column_names <- c('game_id', 'pt.diff.per.min', 'pt.differential',
                  'minutes_played', all_player_names)
# stints <- as.data.frame(matrix(,0,length(column_names)))
stints <- as.data.frame(matrix(,length(pbp_combined),length(column_names)))
names(stints) <- column_names

# iterate through one game at a time and add results for each game
# to overall stint table
game_ids <- unique(pbp_combined$game_id)

#make the play_length colun uniform
for (i in 1:599389){
  if(nchar(pbp_combined$play_length[i]) == 7){
    pbp_combined$play_length[i] <- paste("0", pbp_combined$play_length[i], sep = "")
  }
}


# store column names as we go to make life easier later
player_columns <- c()
current_row <- 1
for (current_game_id in game_ids){
  print(paste('Collecting stints for game', current_game_id))
  # compute point differential
  pbp_table <- pbp_combined %>%
    # focus on one game
    filter(game_id==current_game_id) %>%
    # keep only columns of interest
    dplyr::select(game_id, a1:h5, h.score=home_score,
                  a.score=away_score, play_time=play_length)

  # make quarter/overtime starters' playing time 0 instead of -12 or -5
  pbp_table <- mutate(pbp_table, play_time = ifelse(play_time %in% c('00:-12:00', '00:-5:00'),
                                                    '00:00:00',
                                                    play_time))

  # convert play_time to minutes played (a double)
  pbp_table <- pbp_table %>%
    # minutes played is 60*hours played + minutes played + seconds played/60
    mutate(minutes_played = 60 * as.numeric(substr(pbp_table$play_time,1,2)) + as.numeric(substr(pbp_table$play_time,4,5)) + 1/60*as.numeric(substr(pbp_table$play_time,7,8))) %>%
    dplyr::select(-play_time)

  # join consecutive observations for each stint into one observation
  previous_players <- pbp_table[1,] %>% dplyr::select(a1:h5) %>% unlist() %>%
    as.vector() %>% sort()
  for (row in 2:nrow(pbp_table)){
    current_players <- pbp_table[row,] %>% dplyr::select(a1:h5) %>% unlist() %>%
      as.vector() %>% sort()
    if (setequal(current_players, previous_players)){
      # add minutes played to current row since stints are joined
      pbp_table[row,'minutes_played'] <- pbp_table[row-1,'minutes_played'] + pbp_table[row,'minutes_played']
      # ignore previous row
      pbp_table[row-1,] <- NA
    }
    previous_players <- current_players
  }

  # remove NA rows
  pbp_table <- na.omit(pbp_table)

  pbp_table <- pbp_table %>%
    # make points columns
    mutate(# points scored by stint i is (score_i - score_{i-1})
      h.points = (c(h.score,0)-c(0,h.score))[-(length(h.score)+1)],
      a.points = (c(a.score,0)-c(0,a.score))[-(length(a.score)+1)],
      # point differential is (home points for - home points against)
      pt.differential = h.points - a.points)
  
  # remove stints that still have 0 minutes played
  # this means any points from free throws made by stints in for 0 minutes
  # will not show up at all -- the pt.differential was calculated before
  # these stints were removed
  pbp_table <- pbp_table %>% filter(minutes_played != 0)

  # get home and away players for current game
  away_players <- sapply(dplyr::select(pbp_table, a1:a5), unique) %>%
    unlist() %>% unique()
  home_players <- sapply(dplyr::select(pbp_table, h1:h5), unique) %>%
    unlist() %>% unique()

  # add current players to running player column list
  player_columns <- c(away_players, home_players)

  # build stint data starting with pt.differential and minutes_played
  current_game_data <- data.frame(pbp_table %>%
                                    dplyr::select(
                                      game_id,
                                      pt.differential,
                                      minutes_played))

  # create indicator columns with 1 for home and -1 for away
  for (player in home_players){
    player_indicator <- apply(dplyr::select(pbp_table, h1:h5) == player, 1, any) %>%
      as.integer()
    current_game_data[player] <- player_indicator
  }
  for (player in away_players){
    player_indicator <- apply(dplyr::select(pbp_table, a1:a5) == player, 1, any) %>%
      as.integer() %>%
      # away players get -1
      (function(x) -1 * x)
    current_game_data[player] <- player_indicator
  }

  # get point differential per minute played
  current_game_data <- current_game_data %>%
    mutate(pt.diff.per.min = pt.differential / minutes_played) #%>%
  # dplyr::select(-c(pt.differential, minutes_played))

  # add current data to running stint table, setting indicator to 0 for any player not on the court
  print('Binding to stints')

  # add data row by row
  stints[current_row:(current_row+nrow(current_game_data)-1),names(current_game_data)] <- current_game_data
  stints[current_row:(current_row+nrow(current_game_data)-1),!(names(stints) %in% names(current_game_data))] <- 0
  current_row <- current_row + nrow(current_game_data)
}

# write out result
print('Writing out raw stints...')
write_csv(stints, path='../data/new_all_stints_2015_2016.csv')

# show that all missing values are entire rows that were removed due to
# 0 minutes played -- this justifies dropping rows via na.omit
# the number of NAs in a row throughout the df is always
# either 0 or 482 (and 482 is ncol(stints))
# apply(stints, 1, function(x) sum(is.na(x))) %>% unique()

# write out result without NA columns
print('Writing out stints...')
# remove 1315 NA rows
# stints <- na.omit(stints)
# write_csv(stints, path='../data/stints_joined.csv')

