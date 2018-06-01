library(ggvis)
library(dplyr)
library(dbplyr)


# read in all individual stats as one data frame
path_to_files <- './individual_data/'
filelist = paste0(path_to_files, list.files(path_to_files, '*.tsv'))
all_players <- lapply(filelist, function(x) read.table(x, header = TRUE, sep = '\t'))
all_players <- do.call(rbind, all_players)

clean_individual_player_data <- function(player_table) {
  player_table <- player_table %>%
    filter(MP > 0)
  return(player_table)
}

# remove players with no minutes played
all_players <- clean_individual_player_data(all_players)

aggregate_individual_player_data <- function(player_table) {
  player_table <- player_table %>%
    group_by(Player) %>%
    summarize(
      Age = as.numeric(substr(Age[1], 1, 2)), # age is invariant
      Pos = Pos[1], # position is invariant
      Date = 0, # date no longer applies, 0 instead of NA for plotting
      Tm = Tm[1], # team is invariant
      Opp = 0, # opponent no longer applies, 0 instead of NA for plotting
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

# aggregate_individual_player_data <- function(player_table) {
#   player_table <- player_table %>%
#     group_by(Player) %>%
#     summarize(
#       Age = as.numeric(substr(Age[1], 1, 2)), # age is invariant
#       Pos = Pos[1], # position is invariant
#       Date = 0, # date no longer applies
#       Tm = Tm[1], # team is invariant
#       Opp = 0, # opponent no longer applies
#       Result = mean(ifelse(Result == 'W', 1, 0)), # percentage of wins
#       GS = mean(GS, na.rm = TRUE),
#       MP = sum(MP), # net minutes played
#       FG = sum(FG) / sum(MP),
#       FGA = sum(FGA) / sum(MP),
#       FG. = sum(FG) / sum(FGA), # total fraction
#       X2P = sum(X2P) / sum(MP),
#       X2PA = sum(X2PA) / sum(MP),
#       X2P. = sum(X2P) / sum(X2PA), # total fraction
#       X3P = sum(X3P) / sum(MP),
#       X3PA = sum(X3PA) / sum(MP),
#       X3P. = sum(X3P) / sum(X3PA), # total fraction
#       FT = sum(FT) / sum(MP),
#       FTA = sum(FTA) / sum(MP),
#       FT. = sum(FT) / sum(FTA), # total fraction
#       ORB = sum(ORB) / sum(MP),
#       DRB = sum(DRB) / sum(MP),
#       TRB = sum(TRB) / sum(MP),
#       AST = sum(AST) / sum(MP),
#       STL = sum(STL) / sum(MP),
#       BLK = sum(BLK) / sum(MP),
#       TOV = sum(TOV) / sum(MP),
#       PF = sum(PF) / sum(MP),
#       PTS = sum(PTS) / sum(MP),
#       GmSc = mean(GmSc, na.rm = TRUE) # TODO figure this out
#     )
#   return(player_table)
# }

function(input, output, session) {
  # filter the players
  players <- reactive({
    # create temp variables for input values
    team <- input$team
    opponent <- input$opponent
    aggregate <- input$aggregate
    player <- input$player
    group_positions <- input$group_positions

    # apply filters
    p <- all_players

    if (aggregate) {
      p <- aggregate_individual_player_data(p)
    }

    if (team != 'All') {
      p <- p %>% filter(Tm == team)
    }
    if (opponent != 'All') {
      p <- p %>% filter(Opp == opponent)
    }

    if (player != '' && !is.null(player)) {
      p <- p %>% filter(grepl(player, Player, fixed = TRUE))
    }

    if (group_positions) {
      levels(p$Pos) <- substr(levels(p$Pos), 1, 1)
    }

    p <- as.data.frame(p)

    # TODO
    # if (input$normalize)

    return(p)
  })

  # function to pick out player
  player_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$Player)) return(NULL)

    all_players <- isolate(players())
    player <- all_players[all_players$Player == x$Player,]
    # make sure we only get one
    player_name <- unique(player$Player)
    team <- unique(player$Tm)
    position <- unique(player$Pos)
    paste0("<b>", player_name, "</b>",
           "<br>", team, " ", position, "</br>")
  }

  # create reactive expression for ggvis plot
  vis <- reactive({
    # lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]

    # set axis labels from strings
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))

    if (!is.null(input$player) && input$player != '') {
      # make plot for one player
      players %>%
        ggvis(x = xvar, y = yvar) %>%
        layer_points(size := 50, size.hover := 200,
          fillOpacity := 0.4, fillOpacity.hover := 0.5,
          fill = ~Pos) %>% #TODO: fix key to get tooltip
        add_tooltip(player_tooltip, "hover") %>%
        add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name) %>%
        set_options(width = 500, height = 500)
    } else {
      # make plot for all players
      players %>%
        ggvis(x = xvar, y = yvar) %>%
        layer_points(size := 50, size.hover := 200,
                     fillOpacity := 0.4, fillOpacity.hover := 0.5,
                     key := ~Player, fill = ~Pos) %>%
        add_tooltip(player_tooltip, "hover") %>%
        add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name) %>%
        set_options(width = 500, height = 500)
    }
  })

  vis %>% bind_shiny('plot1')

  # output number of players
  output$num_players <- renderText({ nrow(players()) })

  # output summary of selected variable
  # data_summary <- summary(players()$summary_var)
  output$data_summary <- renderTable({
    summary_var <- input$summary_var
    stat_list <- lapply(players(), summary)[summary_var][[1]]
    df <- data.frame(as.matrix(stat_list))
    names(df) <- summary_var
    t(df)
  })
}