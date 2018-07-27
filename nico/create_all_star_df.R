library(dplyr)
library(readr)

# Set Working Directory
setwd("/Users/nicoshiro/Downloads/sports_analytics/data")

# Create combinations from the players on the team
nico <- c("Stephen Curry", "Kevin Durant", "Joel Embiid", "Russell Westbrook", "Kyrie Irving", "Victor Oladipo", "Damian Lillard", "Kristaps Porzingis", "Chris Paul", "Donovan Mitchell", "Jimmy Butler")
nico_combos <- as.data.frame(t(combn(nico, 5)))
nico_combos_vector <- paste(nico_combos$V1, nico_combos$V2, nico_combos$V3, nico_combos$V4, nico_combos$V5, sep = ",")


# load all players
player_filtered <- read_csv("NEW_all_per_minute_2017_2018.csv")

# remove players who haven't played more than 50 minutes
fifty <- player_filtered %>% filter(MIN > 50) 

fifty[is.na(train_fifty)] <- 0

# Test to create the data frame
player_from_lineup <- strsplit(nico_combos_vector, ",")
lineup_1 <- rbind(fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][1]),
                 fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][2]),
                 fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][3]),
                 fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][4]),
                 fifty %>% filter(PLAYER_NAME == player_from_lineup[[1]][5]))

lineup_1 <- lineup_1 %>% dplyr::select(-X1, -PLAYER_NAME, -TEAM_ABBREVIATION, -MIN, -POS) %>% arrange(FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, AST, TOV, STL, BLK, BLKA, PF, PFD, PTS, PLUS_MINUS, CONTESTED_SHOTS, CONTESTED_SHOTS_2PT, CHARGES_DRAWN, DEFLECTIONS, LOOSE_BALLS_RECOVERED, SCREEN_ASSISTS, BOX_OUTS, FG_PCT, FG3_PCT, FT_PCT)


lineup_1_test <- cbind(lineup_1[1] %>% arrange(FGM) %>% t,
                      lineup_1[2] %>% arrange(FGA) %>% t,
                      lineup_1[3] %>% arrange(FG_PCT) %>% t,
                      lineup_1[4] %>% arrange(FG3M) %>% t,
                      lineup_1[5] %>% arrange(FG3A) %>% t,
                      lineup_1[6] %>% arrange(FG3_PCT) %>% t,
                      lineup_1[7] %>% arrange(FTM) %>% t,
                      lineup_1[8] %>% arrange(FTA) %>% t,
                      lineup_1[9] %>% arrange(FT_PCT) %>% t,
                      lineup_1[10] %>% arrange(OREB) %>% t,
                      lineup_1[11] %>% arrange(DREB) %>% t,
                      lineup_1[12] %>% arrange(AST) %>% t,
                      lineup_1[13] %>% arrange(TOV) %>% t,
                      lineup_1[14] %>% arrange(STL) %>% t,
                      lineup_1[15] %>% arrange(BLK) %>% t,
                      lineup_1[16] %>% arrange(BLKA) %>% t,
                      lineup_1[17] %>% arrange(PF) %>% t,
                      lineup_1[18] %>% arrange(PFD) %>% t,
                      lineup_1[19] %>% arrange(PTS) %>% t,
                      lineup_1[20] %>% arrange(PLUS_MINUS) %>% t,
                      lineup_1[21] %>% arrange(CONTESTED_SHOTS) %>% t,
                      lineup_1[22] %>% arrange(CONTESTED_SHOTS_2PT) %>% t,
                      lineup_1[23] %>% arrange(CONTESTED_SHOTS_3PT) %>% t,
                      lineup_1[24] %>% arrange(CHARGES_DRAWN) %>% t,
                      lineup_1[25] %>% arrange(DEFLECTIONS) %>% t,
                      lineup_1[26] %>% arrange(LOOSE_BALLS_RECOVERED) %>% t,
                      lineup_1[27] %>% arrange(SCREEN_ASSISTS) %>% t,
                      lineup_1[28] %>% arrange(BOX_OUTS) %>% t)

colnames(lineup_1_test) <- c("FGM_1", "FGM_2", "FGM_3", "FGM_4", "FGM_5",
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


# For loop to create the data frame
for (i in 2:462) {
  lineup_run  <- rbind(fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][1]),
                      fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][2]),
                      fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][3]),
                      fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][4]),
                      fifty %>% filter(PLAYER_NAME == player_from_lineup[[i]][5]))
  lineup_run <- lineup_run %>% dplyr::select(-X1, -PLAYER_NAME, -TEAM_ABBREVIATION, -MIN, -POS)
  lineup_run_test <- cbind(lineup_run[1] %>% arrange(FGM) %>% t,
                          lineup_run[2] %>% arrange(FGA) %>% t,
                          lineup_run[3] %>% arrange(FG_PCT) %>% t,
                          lineup_run[4] %>% arrange(FG3M) %>% t,
                          lineup_run[5] %>% arrange(FG3A) %>% t,
                          lineup_run[6] %>% arrange(FG3_PCT) %>% t,
                          lineup_run[7] %>% arrange(FTM) %>% t,
                          lineup_run[8] %>% arrange(FTA) %>% t,
                          lineup_run[9] %>% arrange(FT_PCT) %>% t,
                          lineup_run[10] %>% arrange(OREB) %>% t,
                          lineup_run[11] %>% arrange(DREB) %>% t,
                          lineup_run[12] %>% arrange(AST) %>% t,
                          lineup_run[13] %>% arrange(TOV) %>% t,
                          lineup_run[14] %>% arrange(STL) %>% t,
                          lineup_run[15] %>% arrange(BLK) %>% t,
                          lineup_run[16] %>% arrange(BLKA) %>% t,
                          lineup_run[17] %>% arrange(PF) %>% t,
                          lineup_run[18] %>% arrange(PFD) %>% t,
                          lineup_run[19] %>% arrange(PTS) %>% t,
                          lineup_run[20] %>% arrange(PLUS_MINUS) %>% t,
                          lineup_run[21] %>% arrange(CONTESTED_SHOTS) %>% t,
                          lineup_run[22] %>% arrange(CONTESTED_SHOTS_2PT) %>% t,
                          lineup_run[23] %>% arrange(CONTESTED_SHOTS_3PT) %>% t,
                          lineup_run[24] %>% arrange(CHARGES_DRAWN) %>% t,
                          lineup_run[25] %>% arrange(DEFLECTIONS) %>% t,
                          lineup_run[26] %>% arrange(LOOSE_BALLS_RECOVERED) %>% t,
                          lineup_run[27] %>% arrange(SCREEN_ASSISTS) %>% t,
                          lineup_run[28] %>% arrange(BOX_OUTS) %>% t)
  
  
  lineup_1_test <- rbind(lineup_1_test, lineup_run_test)
}

lineup_1_test <- as.data.frame(lineup_1_test)

rownames(lineup_1_test) <- c()

lineup_1_test <- cbind(nico_combos_vector, lineup_1_test)

write.csv(lineup_1_test, "nico_stats.csv")


