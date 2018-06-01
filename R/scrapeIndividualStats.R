# libraries ####################################################################
library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(lubridate)

# constants ####################################################################
# all_800_team_ids <- c(
#   'CHA', 'BOS', 'MIA', 'ORL', 'NJN', 'MIN', 'HOU', 'LAL',
#   'CHI', 'DET', 'NYK', 'IND', 'POR', 'OKC', 'CLE', 'NOH',
#   'PHI', 'ATL', 'MIL', 'SAC', 'MEM', 'PHO', 'UTA', 'GSW'
# )
# all_900_team_ids <- c('SAS', 'GSW', 'DAL', 'NOH', 'WAS', 'LAC', 'DEN')

all_team_ids <- c(
  'CHA', 'BOS', 'MIA', 'ORL', 'NJN', 'MIN', 'HOU', 'LAL',
  'CHI', 'DET', 'NYK', 'IND', 'POR', 'OKC', 'CLE', 'NOH',
  'PHI', 'ATL', 'MIL', 'SAC', 'MEM', 'PHO', 'UTA', 'GSW',
  'SAS', 'GSW', 'DAL', 'NOH', 'WAS', 'LAC', 'DEN'
)

season_end_year <- 2016
team_id <- 'TOR'
order_table_by <- 'date_game'

filename <- paste0('../data/individual_2016/Individual_', team_id, '_2015_2016.tsv')

# functions ####################################################################
get_individual_season_URL <- function(season_end_year,
                                      team_id,
                                      order_table_by,
                                      offset) {
  url <- paste0(
    'https://www.basketball-reference.com/play-index/pgl_finder.cgi?request=1&',
    'player_id=&match=game&year_min=',
    season_end_year,
    '&year_max=',
    season_end_year,
    '&age_min=0&age_max=99&team_id=',
    team_id,
    '&opp_id=&season_start=1&season_end=-1&is_playoffs=N&round_id=&game_num_type=&',
    'game_num_min=&game_num_max=&game_month=&game_day=&game_location=&game_result=&',
    'is_starter=&is_active=&is_hof=&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&',
    'pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&c1stat=&c1comp=&c1val=&c2stat=&c2comp=&',
    'c2val=&c3stat=&c3comp=&c3val=&c4stat=&c4comp=&c4val=&is_dbl_dbl=&is_trp_dbl=&',
    'order_by=',
    order_table_by,
    '&order_by_asc=&offset=',
    offset
  )
  return(url)
}

get_individual_table <- function(lineup_table_URL) {
  # scrape table from basketball-reference
  lineup_table <- read_html(lineup_table_URL) %>%
    html_node("table") %>%
    html_table(fill = TRUE, header = TRUE)
  return(lineup_table)
}

clean_individual_table <- function(lineup_table) {
  # fill in missing names
  names(lineup_table)[c(7,9)] <- c("At", "Result")
  lineup_table <- lineup_table %>%
    # remove extra header rows
    filter(!(Player == 'Player')) %>%
    # turn date string into Date
    mutate(Date = ymd(Date)) %>%
    # remove unneeded columns
    select(-c(Rk, At))
  return(lineup_table)
}

# loop #########################################################################
# for (team_id in all_800_team_ids) {
#   filename <- paste0('Individual_', team_id, '_2014_2015.tsv')
#   complete_individual_table <- data.frame()
#   # offset up to 800
#   for (offset in seq(0, 800, 100)) {
#     print(paste('offset =', offset))
#     tryCatch({
#       new_individual_table <- get_individual_season_URL(
#         season_end_year = season_end_year,
#         team_id = team_id,
#         order_table_by = order_table_by,
#         offset = offset) %>%
#         get_individual_table() %>%
#         clean_individual_table()
#     }, error = function(e) {
#       Sys.sleep(2)
#       e
#     }
#     )
#     complete_individual_table <- rbind(complete_individual_table, 
#                                        new_individual_table)
#   }
#   write.table(complete_individual_table, file = filename, sep = '\t')
# }

# main #########################################################################
complete_individual_table <- data.frame()
# offset up to 800
for (offset in seq(0, 900, 100)) {
  print(paste('offset =', offset))
  tryCatch({
    new_individual_table <- get_individual_season_URL(
        season_end_year = season_end_year,
        team_id = team_id,
        order_table_by = order_table_by,
        offset = offset) %>%
      get_individual_table() %>%
      clean_individual_table()
    }, error = function(e) {
      Sys.sleep(2)
      e
    }
  )
  complete_individual_table <- rbind(complete_individual_table,
                                     new_individual_table)
}

View(complete_individual_table)

write.table(complete_individual_table, file = filename, sep = '\t')
################################################################################
