library(dplyr)
library(readr)

setwd("/Users/nicoshiro/Downloads/sports_analytics/data")

lakers <- c("Lonzo Ball", "Kentavious Caldwell-Pope", "Alex Caruso", "Josh Hart", "Brandon Ingram", "LeBron James", "Kyle Kuzma", "JaVale McGee", "Rajon Rondo", "Lance Stephenson", "Ivica Zubac", "Michael Beasley")
lakers_combos <- as.data.frame(t(combn(lakers, 5)))
lakers_combos_vector <- paste(lakers_combos$V1, lakers_combos$V2, lakers_combos$V3, lakers_combos$V4, lakers_combos$V5, sep = ",")

#### LINEUP DATA SETUP ###
# load the full lineup stats (training)
# train_for_lineups_full_stats <- read_csv("train_for_lineups_full.csv")
# test_for_lineups_full_stats <- read_csv("test_for_lineups_full.csv")
# 
# #Exclude non-numeric data
# train_lineups <- train_for_lineups_full_stats %>% dplyr::select(-LINEUP, -TOTAL_MIN, -X1, -X)

### INDIVIDUAL PLAYER SETUP ###
# modifications to load all players
player_train_filtered <- read_csv("NEW_all_per_minute_2017_2018.csv")

# remove players who haven't played more than 50 minutes
train_fifty <- player_train_filtered %>% filter(MIN > 50) 

train_fifty[is.na(train_fifty)] <- 0

player_from_lineup <- strsplit(lakers_combos_vector, ",")
linep_1 <- rbind(train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][1]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][2]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][3]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][4]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][5]))

linep_1 <- linep_1 %>% dplyr::select(-X1, -PLAYER_NAME, -TEAM_ABBREVIATION, -MIN, -POS) %>% arrange(FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, AST, TOV, STL, BLK, BLKA, PF, PFD, PTS, PLUS_MINUS, CONTESTED_SHOTS, CONTESTED_SHOTS_2PT, CHARGES_DRAWN, DEFLECTIONS, LOOSE_BALLS_RECOVERED, SCREEN_ASSISTS, BOX_OUTS, FG_PCT, FG3_PCT, FT_PCT)


linep_1_test <- cbind(linep_1[1] %>% arrange(FGM) %>% t,
                      linep_1[2] %>% arrange(FGA) %>% t,
                      linep_1[3] %>% arrange(FG_PCT) %>% t,
                      linep_1[4] %>% arrange(FG3M) %>% t,
                      linep_1[5] %>% arrange(FG3A) %>% t,
                      linep_1[6] %>% arrange(FG3_PCT) %>% t,
                      linep_1[7] %>% arrange(FTM) %>% t,
                      linep_1[8] %>% arrange(FTA) %>% t,
                      linep_1[9] %>% arrange(FT_PCT) %>% t,
                      linep_1[10] %>% arrange(OREB) %>% t,
                      linep_1[11] %>% arrange(DREB) %>% t,
                      linep_1[12] %>% arrange(AST) %>% t,
                      linep_1[13] %>% arrange(TOV) %>% t,
                      linep_1[14] %>% arrange(STL) %>% t,
                      linep_1[15] %>% arrange(BLK) %>% t,
                      linep_1[16] %>% arrange(BLKA) %>% t,
                      linep_1[17] %>% arrange(PF) %>% t,
                      linep_1[18] %>% arrange(PFD) %>% t,
                      linep_1[19] %>% arrange(PTS) %>% t,
                      linep_1[20] %>% arrange(PLUS_MINUS) %>% t,
                      linep_1[21] %>% arrange(CONTESTED_SHOTS) %>% t,
                      linep_1[22] %>% arrange(CONTESTED_SHOTS_2PT) %>% t,
                      linep_1[23] %>% arrange(CONTESTED_SHOTS_3PT) %>% t,
                      linep_1[24] %>% arrange(CHARGES_DRAWN) %>% t,
                      linep_1[25] %>% arrange(DEFLECTIONS) %>% t,
                      linep_1[26] %>% arrange(LOOSE_BALLS_RECOVERED) %>% t,
                      linep_1[27] %>% arrange(SCREEN_ASSISTS) %>% t,
                      linep_1[28] %>% arrange(BOX_OUTS) %>% t)

colnames(linep_1_test) <- c("FGM_1", "FGM_2", "FGM_3", "FGM_4", "FGM_5",
                            "FGA_1", "FGA_2", "FGA_3", "FGA_4", "FGA_5",
                            "FG_PCT_1", "FG_PCT_2", "FG_PCT_3", "FG_PCT_4", "FG_PCT_5",
                            "FG3M_1", "FG3M_2", "FG3M_3", "FG3M_4", "FG3M_5",
                            "FG3A_1", "FG3A_2", "FG3A_3", "FG3A_4", "FG3A_5",
                            "FG3_PCT_1", "FG3_PCT_2", "FG3_PCT_3", "FG3_PCT_4", "FG3_PCT_5",
                            "FTM_1", "FTM_2", "FTM_3", "FTM_4", "FTM_5",
                            "FTA_1", "FTA_2", "FTA_3", "FTA_4", "FTA_5",
                            "FT_PCT_1", "FT_PCT_2", "FT_PCT_3", "FT_PCT_4", "FT_PCT_5",
                            "OREB_1", "OREB_2", "OREB_3", "OREB_4", "OREB_5",
                            "DREB_1", "DREB_2", "DREB_3", "DREB_4", "DREB_5",
                            "AST_1", "AST_2", "AST_3", "AST_4", "AST_5",
                            "TOV_1", "TOV_2", "TOV_3", "TOV_4", "TOV_5",
                            "STL_1", "STL_2", "STL_3", "STL_4", "STL_5",
                            "BLK_1", "BLK_2", "BLK_3", "BLK_4", "BLK_5",
                            "BLKA_1", "BLKA_2", "BLKA_3", "BLKA_4", "BLKA_5",
                            "PF_1", "PF_2", "PF_3", "PF_4", "PF_5",
                            "PFD_1", "PFD_2", "PFD_3", "PFD_4", "PFD_5",
                            "PTS_1", "PTS_2", "PTS_3", "PTS_4", "PTS_5",
                            "PLUS_MINUS_1", "PLUS_MINUS_2", "PLUS_MINUS_3", "PLUS_MINUS_4", "PLUS_MINUS_5",
                            "CONTESTED_SHOTS_1", "CONTESTED_SHOTS_2", "CONTESTED_SHOTS_3", "CONTESTED_SHOTS_4", "CONTESTED_SHOTS_5",
                            "CONTESTED_SHOTS_2PT_1", "CONTESTED_SHOTS_2PT_2", "CONTESTED_SHOTS_2PT_3", "CONTESTED_SHOTS_2PT_4", "CONTESTED_SHOTS_2PT_5",
                            "CONTESTED_SHOTS_3PT_1", "CONTESTED_SHOTS_3PT_2", "CONTESTED_SHOTS_3PT_3", "CONTESTED_SHOTS_3PT_4", "CONTESTED_SHOTS_3PT_5",
                            "CHARGES_DRAWN_1", "CHARGES_DRAWN_2", "CHARGES_DRAWN_3", "CHARGES_DRAWN_4", "CHARGES_DRAWN_5",
                            "DEFLECTIONS_1", "DEFLECTIONS_2", "DEFLECTIONS_3", "DEFLECTIONS_4", "DEFLECTIONS_5",
                            "LOOSE_BALLS_RECOVERED_1", "LOOSE_BALLS_RECOVERED_2", "LOOSE_BALLS_RECOVERED_3", "LOOSE_BALLS_RECOVERED_4", "LOOSE_BALLS_RECOVERED_5",
                            "SCREEN_ASSISTS_1", "SCREEN_ASSISTS_2", "SCREEN_ASSISTS_3", "SCREEN_ASSISTS_4", "SCREEN_ASSISTS_5",
                            "BOX_OUTS_1", "BOX_OUTS_2", "BOX_OUTS_3", "BOX_OUTS_4", "BOX_OUTS_5")


for (i in 2:792) {
  linep_run  <- rbind(train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][1]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][2]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][3]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][4]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][5]))
  linep_run <- linep_run %>% dplyr::select(-X1, -PLAYER_NAME, -TEAM_ABBREVIATION, -MIN, -POS)
  linep_run_test <- cbind(linep_run[1] %>% arrange(FGM) %>% t,
                          linep_run[2] %>% arrange(FGA) %>% t,
                          linep_run[3] %>% arrange(FG_PCT) %>% t,
                          linep_run[4] %>% arrange(FG3M) %>% t,
                          linep_run[5] %>% arrange(FG3A) %>% t,
                          linep_run[6] %>% arrange(FG3_PCT) %>% t,
                          linep_run[7] %>% arrange(FTM) %>% t,
                          linep_run[8] %>% arrange(FTA) %>% t,
                          linep_run[9] %>% arrange(FT_PCT) %>% t,
                          linep_run[10] %>% arrange(OREB) %>% t,
                          linep_run[11] %>% arrange(DREB) %>% t,
                          linep_run[12] %>% arrange(AST) %>% t,
                          linep_run[13] %>% arrange(TOV) %>% t,
                          linep_run[14] %>% arrange(STL) %>% t,
                          linep_run[15] %>% arrange(BLK) %>% t,
                          linep_run[16] %>% arrange(BLKA) %>% t,
                          linep_run[17] %>% arrange(PF) %>% t,
                          linep_run[18] %>% arrange(PFD) %>% t,
                          linep_run[19] %>% arrange(PTS) %>% t,
                          linep_run[20] %>% arrange(PLUS_MINUS) %>% t,
                          linep_run[21] %>% arrange(CONTESTED_SHOTS) %>% t,
                          linep_run[22] %>% arrange(CONTESTED_SHOTS_2PT) %>% t,
                          linep_run[23] %>% arrange(CONTESTED_SHOTS_3PT) %>% t,
                          linep_run[24] %>% arrange(CHARGES_DRAWN) %>% t,
                          linep_run[25] %>% arrange(DEFLECTIONS) %>% t,
                          linep_run[26] %>% arrange(LOOSE_BALLS_RECOVERED) %>% t,
                          linep_run[27] %>% arrange(SCREEN_ASSISTS) %>% t,
                          linep_run[28] %>% arrange(BOX_OUTS) %>% t)
  
  
  linep_1_test <- rbind(linep_1_test, linep_run_test)
}

linep_1_test <- as.data.frame(linep_1_test)

rownames(linep_1_test) <- c()

linep_1_test <- cbind(lakers_combos_vector, linep_1_test)

write.csv(linep_1_test, "lakers_stats.csv")





warriors <- c("Jordan Bell", "DeMarcus Cousins", "Stephen Curry", "Kevin Durant", "Draymond Green", "Andre Iguodala", "Shaun Livingston", "Kevon Looney", "Klay Thompson", "David West", "Nick Young")
warriors_combos <- as.data.frame(t(combn(martin, 5)))
warriors_combos_vector <- paste(warriors_combos$V1, warriors_combos$V2, warriors_combos$V3, warriors_combos$V4, warriors_combos$V5, sep = ",")


### INDIVIDUAL PLAYER SETUP ###
# modifications to load all players
player_train_filtered <- read_csv("NEW_all_per_minute_2017_2018.csv")

# remove players who haven't played more than 50 minutes
train_fifty <- player_train_filtered %>% filter(MIN > 50) 

train_fifty[is.na(train_fifty)] <- 0

player_from_lineup <- strsplit(warriors_combos_vector, ",")
linep_1 <- rbind(train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][1]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][2]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][3]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][4]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][5]))

linep_1 <- linep_1 %>% dplyr::select(-X1, -PLAYER_NAME, -TEAM_ABBREVIATION, -MIN, -POS) %>% arrange(FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, AST, TOV, STL, BLK, BLKA, PF, PFD, PTS, PLUS_MINUS, CONTESTED_SHOTS, CONTESTED_SHOTS_2PT, CHARGES_DRAWN, DEFLECTIONS, LOOSE_BALLS_RECOVERED, SCREEN_ASSISTS, BOX_OUTS, FG_PCT, FG3_PCT, FT_PCT)


linep_1_test <- cbind(linep_1[1] %>% arrange(FGM) %>% t,
                      linep_1[2] %>% arrange(FGA) %>% t,
                      linep_1[3] %>% arrange(FG_PCT) %>% t,
                      linep_1[4] %>% arrange(FG3M) %>% t,
                      linep_1[5] %>% arrange(FG3A) %>% t,
                      linep_1[6] %>% arrange(FG3_PCT) %>% t,
                      linep_1[7] %>% arrange(FTM) %>% t,
                      linep_1[8] %>% arrange(FTA) %>% t,
                      linep_1[9] %>% arrange(FT_PCT) %>% t,
                      linep_1[10] %>% arrange(OREB) %>% t,
                      linep_1[11] %>% arrange(DREB) %>% t,
                      linep_1[12] %>% arrange(AST) %>% t,
                      linep_1[13] %>% arrange(TOV) %>% t,
                      linep_1[14] %>% arrange(STL) %>% t,
                      linep_1[15] %>% arrange(BLK) %>% t,
                      linep_1[16] %>% arrange(BLKA) %>% t,
                      linep_1[17] %>% arrange(PF) %>% t,
                      linep_1[18] %>% arrange(PFD) %>% t,
                      linep_1[19] %>% arrange(PTS) %>% t,
                      linep_1[20] %>% arrange(PLUS_MINUS) %>% t,
                      linep_1[21] %>% arrange(CONTESTED_SHOTS) %>% t,
                      linep_1[22] %>% arrange(CONTESTED_SHOTS_2PT) %>% t,
                      linep_1[23] %>% arrange(CONTESTED_SHOTS_3PT) %>% t,
                      linep_1[24] %>% arrange(CHARGES_DRAWN) %>% t,
                      linep_1[25] %>% arrange(DEFLECTIONS) %>% t,
                      linep_1[26] %>% arrange(LOOSE_BALLS_RECOVERED) %>% t,
                      linep_1[27] %>% arrange(SCREEN_ASSISTS) %>% t,
                      linep_1[28] %>% arrange(BOX_OUTS) %>% t)

colnames(linep_1_test) <- c("FGM_1", "FGM_2", "FGM_3", "FGM_4", "FGM_5",
                            "FGA_1", "FGA_2", "FGA_3", "FGA_4", "FGA_5",
                            "FG_PCT_1", "FG_PCT_2", "FG_PCT_3", "FG_PCT_4", "FG_PCT_5",
                            "FG3M_1", "FG3M_2", "FG3M_3", "FG3M_4", "FG3M_5",
                            "FG3A_1", "FG3A_2", "FG3A_3", "FG3A_4", "FG3A_5",
                            "FG3_PCT_1", "FG3_PCT_2", "FG3_PCT_3", "FG3_PCT_4", "FG3_PCT_5",
                            "FTM_1", "FTM_2", "FTM_3", "FTM_4", "FTM_5",
                            "FTA_1", "FTA_2", "FTA_3", "FTA_4", "FTA_5",
                            "FT_PCT_1", "FT_PCT_2", "FT_PCT_3", "FT_PCT_4", "FT_PCT_5",
                            "OREB_1", "OREB_2", "OREB_3", "OREB_4", "OREB_5",
                            "DREB_1", "DREB_2", "DREB_3", "DREB_4", "DREB_5",
                            "AST_1", "AST_2", "AST_3", "AST_4", "AST_5",
                            "TOV_1", "TOV_2", "TOV_3", "TOV_4", "TOV_5",
                            "STL_1", "STL_2", "STL_3", "STL_4", "STL_5",
                            "BLK_1", "BLK_2", "BLK_3", "BLK_4", "BLK_5",
                            "BLKA_1", "BLKA_2", "BLKA_3", "BLKA_4", "BLKA_5",
                            "PF_1", "PF_2", "PF_3", "PF_4", "PF_5",
                            "PFD_1", "PFD_2", "PFD_3", "PFD_4", "PFD_5",
                            "PTS_1", "PTS_2", "PTS_3", "PTS_4", "PTS_5",
                            "PLUS_MINUS_1", "PLUS_MINUS_2", "PLUS_MINUS_3", "PLUS_MINUS_4", "PLUS_MINUS_5",
                            "CONTESTED_SHOTS_1", "CONTESTED_SHOTS_2", "CONTESTED_SHOTS_3", "CONTESTED_SHOTS_4", "CONTESTED_SHOTS_5",
                            "CONTESTED_SHOTS_2PT_1", "CONTESTED_SHOTS_2PT_2", "CONTESTED_SHOTS_2PT_3", "CONTESTED_SHOTS_2PT_4", "CONTESTED_SHOTS_2PT_5",
                            "CONTESTED_SHOTS_3PT_1", "CONTESTED_SHOTS_3PT_2", "CONTESTED_SHOTS_3PT_3", "CONTESTED_SHOTS_3PT_4", "CONTESTED_SHOTS_3PT_5",
                            "CHARGES_DRAWN_1", "CHARGES_DRAWN_2", "CHARGES_DRAWN_3", "CHARGES_DRAWN_4", "CHARGES_DRAWN_5",
                            "DEFLECTIONS_1", "DEFLECTIONS_2", "DEFLECTIONS_3", "DEFLECTIONS_4", "DEFLECTIONS_5",
                            "LOOSE_BALLS_RECOVERED_1", "LOOSE_BALLS_RECOVERED_2", "LOOSE_BALLS_RECOVERED_3", "LOOSE_BALLS_RECOVERED_4", "LOOSE_BALLS_RECOVERED_5",
                            "SCREEN_ASSISTS_1", "SCREEN_ASSISTS_2", "SCREEN_ASSISTS_3", "SCREEN_ASSISTS_4", "SCREEN_ASSISTS_5",
                            "BOX_OUTS_1", "BOX_OUTS_2", "BOX_OUTS_3", "BOX_OUTS_4", "BOX_OUTS_5")


for (i in 2:462) {
  linep_run  <- rbind(train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][1]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][2]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][3]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][4]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][5]))
  linep_run <- linep_run %>% dplyr::select(-X1, -PLAYER_NAME, -TEAM_ABBREVIATION, -MIN, -POS)
  linep_run_test <- cbind(linep_run[1] %>% arrange(FGM) %>% t,
                          linep_run[2] %>% arrange(FGA) %>% t,
                          linep_run[3] %>% arrange(FG_PCT) %>% t,
                          linep_run[4] %>% arrange(FG3M) %>% t,
                          linep_run[5] %>% arrange(FG3A) %>% t,
                          linep_run[6] %>% arrange(FG3_PCT) %>% t,
                          linep_run[7] %>% arrange(FTM) %>% t,
                          linep_run[8] %>% arrange(FTA) %>% t,
                          linep_run[9] %>% arrange(FT_PCT) %>% t,
                          linep_run[10] %>% arrange(OREB) %>% t,
                          linep_run[11] %>% arrange(DREB) %>% t,
                          linep_run[12] %>% arrange(AST) %>% t,
                          linep_run[13] %>% arrange(TOV) %>% t,
                          linep_run[14] %>% arrange(STL) %>% t,
                          linep_run[15] %>% arrange(BLK) %>% t,
                          linep_run[16] %>% arrange(BLKA) %>% t,
                          linep_run[17] %>% arrange(PF) %>% t,
                          linep_run[18] %>% arrange(PFD) %>% t,
                          linep_run[19] %>% arrange(PTS) %>% t,
                          linep_run[20] %>% arrange(PLUS_MINUS) %>% t,
                          linep_run[21] %>% arrange(CONTESTED_SHOTS) %>% t,
                          linep_run[22] %>% arrange(CONTESTED_SHOTS_2PT) %>% t,
                          linep_run[23] %>% arrange(CONTESTED_SHOTS_3PT) %>% t,
                          linep_run[24] %>% arrange(CHARGES_DRAWN) %>% t,
                          linep_run[25] %>% arrange(DEFLECTIONS) %>% t,
                          linep_run[26] %>% arrange(LOOSE_BALLS_RECOVERED) %>% t,
                          linep_run[27] %>% arrange(SCREEN_ASSISTS) %>% t,
                          linep_run[28] %>% arrange(BOX_OUTS) %>% t)
  
  
  linep_1_test <- rbind(linep_1_test, linep_run_test)
}

linep_1_test <- as.data.frame(linep_1_test)

rownames(linep_1_test) <- c()

linep_1_test <- cbind(warriors_combos_vector, linep_1_test)

write.csv(linep_1_test, "warriors_stats.csv")






martin <- c("Jordan Bell", "DeMarcus Cousins", "Stephen Curry", "Kevin Durant", "Draymond Green", "Andre Iguodala", "Shaun Livingston", "Kevon Looney", "Klay Thompson", "David West", "Nick Young")
martin_combos <- as.data.frame(t(combn(martin, 5)))
martin_combos_vector <- paste(martin_combos$V1, martin_combos$V2, martin_combos$V3, martin_combos$V4, martin_combos$V5, sep = ",")

### INDIVIDUAL PLAYER SETUP ###
# modifications to load all players
player_train_filtered <- read_csv("NEW_all_per_minute_2017_2018.csv")

# remove players who haven't played more than 50 minutes
train_fifty <- player_train_filtered %>% filter(MIN > 50) 

train_fifty[is.na(train_fifty)] <- 0

player_from_lineup <- strsplit(martin_combos_vector, ",")
linep_1 <- rbind(train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][1]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][2]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][3]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][4]),
                 train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][5]))

linep_1 <- linep_1 %>% dplyr::select(-X1, -PLAYER_NAME, -TEAM_ABBREVIATION, -MIN, -POS) %>% arrange(FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, AST, TOV, STL, BLK, BLKA, PF, PFD, PTS, PLUS_MINUS, CONTESTED_SHOTS, CONTESTED_SHOTS_2PT, CHARGES_DRAWN, DEFLECTIONS, LOOSE_BALLS_RECOVERED, SCREEN_ASSISTS, BOX_OUTS, FG_PCT, FG3_PCT, FT_PCT)


linep_1_test <- cbind(linep_1[1] %>% arrange(FGM) %>% t,
                      linep_1[2] %>% arrange(FGA) %>% t,
                      linep_1[3] %>% arrange(FG_PCT) %>% t,
                      linep_1[4] %>% arrange(FG3M) %>% t,
                      linep_1[5] %>% arrange(FG3A) %>% t,
                      linep_1[6] %>% arrange(FG3_PCT) %>% t,
                      linep_1[7] %>% arrange(FTM) %>% t,
                      linep_1[8] %>% arrange(FTA) %>% t,
                      linep_1[9] %>% arrange(FT_PCT) %>% t,
                      linep_1[10] %>% arrange(OREB) %>% t,
                      linep_1[11] %>% arrange(DREB) %>% t,
                      linep_1[12] %>% arrange(AST) %>% t,
                      linep_1[13] %>% arrange(TOV) %>% t,
                      linep_1[14] %>% arrange(STL) %>% t,
                      linep_1[15] %>% arrange(BLK) %>% t,
                      linep_1[16] %>% arrange(BLKA) %>% t,
                      linep_1[17] %>% arrange(PF) %>% t,
                      linep_1[18] %>% arrange(PFD) %>% t,
                      linep_1[19] %>% arrange(PTS) %>% t,
                      linep_1[20] %>% arrange(PLUS_MINUS) %>% t,
                      linep_1[21] %>% arrange(CONTESTED_SHOTS) %>% t,
                      linep_1[22] %>% arrange(CONTESTED_SHOTS_2PT) %>% t,
                      linep_1[23] %>% arrange(CONTESTED_SHOTS_3PT) %>% t,
                      linep_1[24] %>% arrange(CHARGES_DRAWN) %>% t,
                      linep_1[25] %>% arrange(DEFLECTIONS) %>% t,
                      linep_1[26] %>% arrange(LOOSE_BALLS_RECOVERED) %>% t,
                      linep_1[27] %>% arrange(SCREEN_ASSISTS) %>% t,
                      linep_1[28] %>% arrange(BOX_OUTS) %>% t)

colnames(linep_1_test) <- c("FGM_1", "FGM_2", "FGM_3", "FGM_4", "FGM_5",
                            "FGA_1", "FGA_2", "FGA_3", "FGA_4", "FGA_5",
                            "FG_PCT_1", "FG_PCT_2", "FG_PCT_3", "FG_PCT_4", "FG_PCT_5",
                            "FG3M_1", "FG3M_2", "FG3M_3", "FG3M_4", "FG3M_5",
                            "FG3A_1", "FG3A_2", "FG3A_3", "FG3A_4", "FG3A_5",
                            "FG3_PCT_1", "FG3_PCT_2", "FG3_PCT_3", "FG3_PCT_4", "FG3_PCT_5",
                            "FTM_1", "FTM_2", "FTM_3", "FTM_4", "FTM_5",
                            "FTA_1", "FTA_2", "FTA_3", "FTA_4", "FTA_5",
                            "FT_PCT_1", "FT_PCT_2", "FT_PCT_3", "FT_PCT_4", "FT_PCT_5",
                            "OREB_1", "OREB_2", "OREB_3", "OREB_4", "OREB_5",
                            "DREB_1", "DREB_2", "DREB_3", "DREB_4", "DREB_5",
                            "AST_1", "AST_2", "AST_3", "AST_4", "AST_5",
                            "TOV_1", "TOV_2", "TOV_3", "TOV_4", "TOV_5",
                            "STL_1", "STL_2", "STL_3", "STL_4", "STL_5",
                            "BLK_1", "BLK_2", "BLK_3", "BLK_4", "BLK_5",
                            "BLKA_1", "BLKA_2", "BLKA_3", "BLKA_4", "BLKA_5",
                            "PF_1", "PF_2", "PF_3", "PF_4", "PF_5",
                            "PFD_1", "PFD_2", "PFD_3", "PFD_4", "PFD_5",
                            "PTS_1", "PTS_2", "PTS_3", "PTS_4", "PTS_5",
                            "PLUS_MINUS_1", "PLUS_MINUS_2", "PLUS_MINUS_3", "PLUS_MINUS_4", "PLUS_MINUS_5",
                            "CONTESTED_SHOTS_1", "CONTESTED_SHOTS_2", "CONTESTED_SHOTS_3", "CONTESTED_SHOTS_4", "CONTESTED_SHOTS_5",
                            "CONTESTED_SHOTS_2PT_1", "CONTESTED_SHOTS_2PT_2", "CONTESTED_SHOTS_2PT_3", "CONTESTED_SHOTS_2PT_4", "CONTESTED_SHOTS_2PT_5",
                            "CONTESTED_SHOTS_3PT_1", "CONTESTED_SHOTS_3PT_2", "CONTESTED_SHOTS_3PT_3", "CONTESTED_SHOTS_3PT_4", "CONTESTED_SHOTS_3PT_5",
                            "CHARGES_DRAWN_1", "CHARGES_DRAWN_2", "CHARGES_DRAWN_3", "CHARGES_DRAWN_4", "CHARGES_DRAWN_5",
                            "DEFLECTIONS_1", "DEFLECTIONS_2", "DEFLECTIONS_3", "DEFLECTIONS_4", "DEFLECTIONS_5",
                            "LOOSE_BALLS_RECOVERED_1", "LOOSE_BALLS_RECOVERED_2", "LOOSE_BALLS_RECOVERED_3", "LOOSE_BALLS_RECOVERED_4", "LOOSE_BALLS_RECOVERED_5",
                            "SCREEN_ASSISTS_1", "SCREEN_ASSISTS_2", "SCREEN_ASSISTS_3", "SCREEN_ASSISTS_4", "SCREEN_ASSISTS_5",
                            "BOX_OUTS_1", "BOX_OUTS_2", "BOX_OUTS_3", "BOX_OUTS_4", "BOX_OUTS_5")


for (i in 2:462) {
  linep_run  <- rbind(train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][1]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][2]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][3]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][4]),
                      train_fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][5]))
  linep_run <- linep_run %>% dplyr::select(-X1, -PLAYER_NAME, -TEAM_ABBREVIATION, -MIN, -POS)
  linep_run_test <- cbind(linep_run[1] %>% arrange(FGM) %>% t,
                          linep_run[2] %>% arrange(FGA) %>% t,
                          linep_run[3] %>% arrange(FG_PCT) %>% t,
                          linep_run[4] %>% arrange(FG3M) %>% t,
                          linep_run[5] %>% arrange(FG3A) %>% t,
                          linep_run[6] %>% arrange(FG3_PCT) %>% t,
                          linep_run[7] %>% arrange(FTM) %>% t,
                          linep_run[8] %>% arrange(FTA) %>% t,
                          linep_run[9] %>% arrange(FT_PCT) %>% t,
                          linep_run[10] %>% arrange(OREB) %>% t,
                          linep_run[11] %>% arrange(DREB) %>% t,
                          linep_run[12] %>% arrange(AST) %>% t,
                          linep_run[13] %>% arrange(TOV) %>% t,
                          linep_run[14] %>% arrange(STL) %>% t,
                          linep_run[15] %>% arrange(BLK) %>% t,
                          linep_run[16] %>% arrange(BLKA) %>% t,
                          linep_run[17] %>% arrange(PF) %>% t,
                          linep_run[18] %>% arrange(PFD) %>% t,
                          linep_run[19] %>% arrange(PTS) %>% t,
                          linep_run[20] %>% arrange(PLUS_MINUS) %>% t,
                          linep_run[21] %>% arrange(CONTESTED_SHOTS) %>% t,
                          linep_run[22] %>% arrange(CONTESTED_SHOTS_2PT) %>% t,
                          linep_run[23] %>% arrange(CONTESTED_SHOTS_3PT) %>% t,
                          linep_run[24] %>% arrange(CHARGES_DRAWN) %>% t,
                          linep_run[25] %>% arrange(DEFLECTIONS) %>% t,
                          linep_run[26] %>% arrange(LOOSE_BALLS_RECOVERED) %>% t,
                          linep_run[27] %>% arrange(SCREEN_ASSISTS) %>% t,
                          linep_run[28] %>% arrange(BOX_OUTS) %>% t)
  
  
  linep_1_test <- rbind(linep_1_test, linep_run_test)
}

linep_1_test <- as.data.frame(linep_1_test)

rownames(linep_1_test) <- c()

linep_1_test <- cbind(martin_combos_vector, linep_1_test)

write.csv(linep_1_test, "martin_stats.csv")
