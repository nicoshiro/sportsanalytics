individuals = read_csv('../data/individual_2015_2016/aggregate_individuals_2015_2016.csv')
standardized = individuals %>%
  mutate(
    FG = FG / MP,
    FGA = FGA / MP,
    `2P` = `2P` / MP,
    `2PA` = `2PA` / MP,
    `3P` = `3P` / MP,
    `3PA` = `3PA` / MP,
    FT = FT / MP,
    FTA = FTA / MP,
    ORB = ORB / MP,
    DRB = DRB / MP,
    TRB = TRB / MP,
    AST = AST / MP,
    STL = STL / MP,
    BLK = BLK / MP,
    TOV = TOV / MP,
    PF = PF / MP,
    PTS = PTS / MP
  )
write_csv(standardized, '../data/individual_2015_2016/standardized_aggregate_individuals_2015_2016.csv')
