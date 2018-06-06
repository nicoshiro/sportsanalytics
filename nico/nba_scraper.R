library("nbaTools")
library(dplyr)

hustle_stats_2017_2018 <- nbaTools::GetPlayerHustleStats(SeasonType = "Regular Season")
lineup_stats_2017_2018 <- nbaTools::GetLineupStats(SeasonType = "Regular Season")
player_stats_2017_2018 <- nbaTools::GetPlayerStats(SeasonType = "Regular Season")

setwd("/Users/nicoshiro/Downloads/lbynum-lbynum-2018-thesis-e746e5f989ce")

write.csv(hustle_stats_2017_2018, "hustle_stats_2017_2018.csv")
write.csv(lineup_stats_2017_2018, "lineup_stats_2017_2018.csv")
write.csv(player_stats_2017_2018, "player_stats_2017_2018.csv")

per_minute_player <- nbaTools::GetPlayerStats(SeasonType = "Regular Season", PerMode = "PerMinute",
                                              )
write.csv(per_minute_player, "per_minute_player_2017_2018.csv")

per_minute_hustle_stats_2017_2018 <- nbaTools::GetPlayerHustleStats(SeasonType = "Regular Season", PerMode = "PerMinute")
write.csv(per_minute_hustle_stats_2017_2018, "per_minute_hustle_stats_2017_2018.csv")

per_minute_lineup_stats_2017_2018 <- nbaTools::GetLineupStats(SeasonType = "Regular Season", PerMode = "PerMinute")
write.csv(per_minute_lineup_stats_2017_2018, "per_minute_lineup_stats_2017_2018.csv")

per_minute_player_F <- nbaTools::GetPlayerStats(SeasonType = "Regular Season", PerMode = "PerMinute",
                                                PlayerPosition = "F")
per_minute_player_F$POS <- "F" 


per_minute_player_C <- nbaTools::GetPlayerStats(SeasonType = "Regular Season", PerMode = "PerMinute",
                                                PlayerPosition = "C")
per_minute_player_C$POS <- "C" 


per_minute_player_G <- nbaTools::GetPlayerStats(SeasonType = "Regular Season", PerMode = "PerMinute",
                                                PlayerPosition = "G")
per_minute_player_G$POS <- "G"


per_minute_player_wPOS <- rbind(per_minute_player_C, per_minute_player_F, per_minute_player_G)
unique_per_minute_POS <- distinct(per_minute_player_wPOS, PLAYER_NAME)

per_minute_player_stats_2017_2018_NEW <- subset(per_minute_player,
                                                select = c(PLAYER_NAME, TEAM_ABBREVIATION,
                                                           MIN, FGM, FGA, FG_PCT, FG3M,
                                                           FG3A, FG3_PCT, FTM, FTA, FT_PCT,
                                                           OREB, DREB, AST, TOV, STL, BLK, 
                                                           BLKA, PF, PFD, PTS, PLUS_MINUS))

per_minute_hustle_stats_2017_2018_NEW <- subset(per_minute_hustle_stats_2017_2018, 
                                                select = -c(PLAYER_ID, PLAYER_NAME, TEAM_ID,
                                                            TEAM_ABBREVIATION, AGE, G, MIN))



all_per_minute <- cbind(per_minute_player_stats_2017_2018_NEW, per_minute_hustle_stats_2017_2018_NEW)
write.csv(all_per_minute, "all_per_minute_2017_2018.csv")
