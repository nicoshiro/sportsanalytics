library(dplyr)

load_all_players <- function() {
  # load in data
  path_to_files <- '../data/individual/'
  filelist = paste0(path_to_files, list.files(path_to_files, '*.tsv'))
  all_players <- lapply(filelist, function(x) read.table(x, header = TRUE, sep = '\t'))
  all_players <- do.call(rbind, all_players)
  return(all_players)
}

clean_individual_player_data <- function(player_table) {
  player_table <- player_table %>%
    filter(MP > 0)
  return(player_table)
}

aggregate_individual_player_data <- function(player_table) {
  player_table <- player_table %>%
    dplyr::group_by(Player) %>%
    dplyr::summarize(
      Age = as.numeric(substr(Age[1], 1, 2)), # age is invariant
      Pos = Pos[1], # position is invariant
      # Date = NA, # date no longer applies
      Tm = Tm[1], # team is invariant
      # Opp = NA, # opponent no longer applies
      Result = mean(ifelse(Result == 'W', 1, 0)), # percentage of wins
      GS = mean(GS, na.rm = TRUE),
      MP = sum(MP), # net minutes played
      FG = sum(FG) / sum(MP),
      FGA = sum(FGA) / sum(MP),
      FG. = sum(FG) / sum(FGA), # total fraction
      X2P = sum(X2P) / sum(MP),
      X2PA = sum(X2PA) / sum(MP),
      X2P. = sum(X2P) / sum(X2PA), # total fraction
      X3P = sum(X3P) / sum(MP),
      X3PA = sum(X3PA) / sum(MP),
      X3P. = sum(X3P) / sum(X3PA), # total fraction
      FT = sum(FT) / sum(MP),
      FTA = sum(FTA) / sum(MP),
      FT. = sum(FT) / sum(FTA), # total fraction
      ORB = sum(ORB) / sum(MP),
      DRB = sum(DRB) / sum(MP),
      TRB = sum(TRB) / sum(MP),
      AST = sum(AST) / sum(MP),
      STL = sum(STL) / sum(MP),
      BLK = sum(BLK) / sum(MP),
      TOV = sum(TOV) / sum(MP),
      PF = sum(PF) / sum(MP),
      PTS = sum(PTS) / sum(MP),
      GmSc = PTS + 0.4 * FG - 0.7 * FGA - 0.4 * (FTA - FT) + 0.7 * ORB + 0.3 * DRB + STL + 0.7 * AST + 0.7 * BLK - 0.4 * PF - TOV
    )
  return(player_table)
}

get_aggregate_players <- function() {
  all_players <- load_all_players() %>%
    clean_individual_player_data() %>%
    aggregate_individual_player_data()
  return(all_players)
}