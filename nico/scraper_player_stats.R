library("nbaTools")
library(dplyr)

setwd("/Users/nicoshiro/Downloads/sports_analytics/data")

# Scrape data using nbaTools
BRef <- nbaTools::GetPlayerStats(source = "BRef", SeasonType = "Regular Season", PerMode = "PerMinute")

# Wrangling the data
BRef <- BRef[order(BRef$Player),]
BRef_subset <- subset(BRef, Tm == "TOT")
BRef_subset$Tm <- "AAA"
BRef_NEW <- rbind(BRef_subset, BRef)
BRef_NEW <- BRef_NEW[with(BRef_NEW, order(Player, Tm)),]
unique_BRef <- distinct(BRef_NEW, Player, .keep_all = TRUE)

# Adjusting player positions
unique_BRef$Pos[unique_BRef$Pos == "PG"] <- "G"
unique_BRef$Pos[unique_BRef$Pos == "SG"] <- "G"
unique_BRef$Pos[unique_BRef$Pos == "PF"] <- "F"
unique_BRef$Pos[unique_BRef$Pos == "SF"] <- "F"
unique_BRef$Pos[unique_BRef$Pos == "SF-SG"] <- "F"
unique_BRef$Pos[unique_BRef$Pos == "PG-SG"] <- "G"

# Binding positions to the data
POS <- unique_BRef$Pos
per_minute_player <- per_minute_player[order(per_minute_player$PLAYER_NAME),]
per_minute_player <- cbind(per_minute_player, POS)


# Subset player data
per_minute_player_stats_2017_2018_NEW <- subset(per_minute_player,
                                                select = c(PLAYER_NAME, TEAM_ABBREVIATION,
                                                           MIN, FGM, FGA, FG_PCT, FG3M,
                                                           FG3A, FG3_PCT, FTM, FTA, FT_PCT,
                                                           OREB, DREB, AST, TOV, STL, BLK, 
                                                           BLKA, PF, PFD, PTS, PLUS_MINUS, POS))

# Subset hustle stats
per_minute_hustle_stats_2017_2018_NEW <- subset(per_minute_hustle_stats_2017_2018, 
                                                select = -c(PLAYER_ID, PLAYER_NAME, TEAM_ID,
                                                            TEAM_ABBREVIATION, AGE, G, MIN))


# Bind and write .csv
all_per_minute <- cbind(per_minute_player_stats_2017_2018_NEW, per_minute_hustle_stats_2017_2018_NEW)
write.csv(all_per_minute, "all_per_minute_2017_2018.csv")



