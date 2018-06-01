library(ggvis)

# for dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

fluidPage(
  fluidRow(
    column(3,
           wellPanel(
             selectInput("yvar", "Y-axis Variable", axis_vars, selected = "TOV"),
             selectInput("xvar", "X-axis Variable", axis_vars, selected = "PTS")
           ),
           wellPanel(
             h4("Transformations"),
             selectInput("aggregate", "Aggregate observations into one for each player.",
                         c(TRUE, FALSE)
             ),
             selectInput("group_positions", "Group positions into Center, Guard, Forward.",
                         c(TRUE, FALSE)
             )
           ),
           wellPanel(
             h4("Filter"),
             selectInput("team", "Team", c(
                 "All", "ATL", "BRK", "CHI", "CHO", "CLE", "DAL", "DEN", "DET", 
                 "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", 
                 "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", 
                 "UTA", "WAS", "BOS"
               ),
               selected = "LAL"
             ),
             selectInput("opponent", "Opponent",
               c(
                 "All", "ATL", "BRK", "CHI", "CHO", "CLE", "DAL", "DEN", "DET", 
                 "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", 
                 "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", 
                 "UTA", "WAS", "BOS"
               ),
               selected = "All"
             ),
             textInput("player", "Player name contains:")
           )
    ),
    column(9,
           ggvisOutput("plot1"),
           wellPanel(
             span("Number of Observations Shown:",
                  textOutput("num_players")
             )
           )
    ),
    column(3,
           wellPanel(
             h4("Display Summary Statistics"),
             selectInput("summary_var", "Variable to Summarize", axis_vars, selected = "PTS")
           )
    ),
    column(6,
           wellPanel(
             span("Summary Statistics:",
                  tableOutput("data_summary")
             )
           )
    )
  )
)