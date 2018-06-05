library("nbaTools")

hustle_stats_2017_2018 <- nbaTools::GetPlayerHustleStats(SeasonType = "Regular Season")
lineup_stats_2017_2018 <- nbaTools::GetLineupStats(SeasonType = "Regular Season")
player_stats_2017_2018 <- nbaTools::GetPlayerStats(SeasonType = "Regular Season")

setwd("/Users/nicoshiro/Downloads/lbynum-lbynum-2018-thesis-e746e5f989ce")

write.csv(hustle_stats_2017_2018, "hustle_stats_2017_2018.csv")
write.csv(lineup_stats_2017_2018, "lineup_stats_2017_2018.csv")
write.csv(player_stats_2017_2018, "player_stats_2017_2018.csv")

per_minute_player <- nbaTools::GetPlayerStats(SeasonType = "Regular Season", PerMode = "PerMinute")
write.csv(per_minute_player, "per_minute_player_2017_2018.csv")

per_minute_hustle_stats_2017_2018 <- nbaTools::GetPlayerHustleStats(SeasonType = "Regular Season", PerMode = "PerMinute")
write.csv(per_minute_hustle_stats_2017_2018, "per_minute_hustle_stats_2017_2018.csv")

per_minute_lineup_stats_2017_2018 <- nbaTools::GetLineupStats(SeasonType = "Regular Season", PerMode = "PerMinute")
write.csv(per_minute_lineup_stats_2017_2018, "per_minute_lineup_stats_2017_2018.csv")
