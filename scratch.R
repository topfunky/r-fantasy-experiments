library(tidyverse)
library(rvest)

# install.packages("devtools")
#devtools::install_github("topfunky/gghighcontrast")
library(gghighcontrast)

# install.packages("RColorBrewer")                   # Install RColorBrewer package
library("RColorBrewer")

# The number of teams in the fantasy league
league_size = 12

# Return the depth of the field based on the number of teams and
# the number of rostered players at that position.
position_counts <- function(pos) {
  r = case_when(
    pos == "QB" ~ 2 * league_size,
    pos == "RB" ~ 3 * league_size,
    pos == "TE" ~ 2 * league_size,
    pos == "WR" ~ 6 * league_size,
    pos == "K" ~ 1 * league_size,
    pos == "DEF" ~ 1 * league_size,
    TRUE ~ 1
  )
  return(r)
}

calculate_replacement_points <- function(points, size) {
  # Find the top N players based on the number of teams and how many should be rostered on each team.
  # Grab 10 players in a window around the end of that list.
  # (Example: if 100 RBs are needed across the league, grab those ranked 95-105)
  # The average points for those players is the baseline that other players will be measured against.
  head(points, size + 5) %>%
    tail(10) %>%
    mean() %>%
    as.integer()
}

# Grab final season PPR points from 2020.
load_and_parse_historical_data <- function() {
  data = read.csv("data/fantasy.2020.csv") %>%
    filter(X != "Rk") %>%
    rename(
      player_name = X.1,
      team = X.2,
      fantasy_position = X.3,
      ppr_score = Fantasy.1
    ) %>%
    select(player_name, team, fantasy_position, ppr_score) %>%
    mutate_at(vars(ppr_score), as.numeric) %>%
    arrange(desc(ppr_score)) %>%
    group_by(fantasy_position) %>%
    mutate(position_index = row_number()) %>%
    ungroup() %>%
    filter(ppr_score > 100)

  data
}

load_and_parse_data <- function() {
  html <- read_html("data/nfl-player-rankings.200.html")
  name <- html_elements(html, "li a.playerName") %>% html_text2()
  projected_points <-
    html_elements(html, "li span.projected") %>% html_text2() %>% readr::parse_double()
  prev_season_total <-
    html_elements(html, "li span.playerSeasonTotal") %>% html_text2() %>% readr::parse_double()
  position_and_team <-
    html_elements(html, "li a.playerName + em") %>% html_text2()

  data.frame(name,
             prev_season_total,
             projected_points,
             position_and_team)
}

run_calculations <- function(data) {
  players <-
    data %>%

    separate(position_and_team, c("position", "team"), " - ") %>%
    # Group by position (QB, WR, RB, ...)
    group_by(position) %>%
    # Previous season
    arrange(desc(prev_season_total)) %>%
    mutate(prev_season_index = row_number()) %>%
    # Current season projection
    arrange(desc(projected_points)) %>%
    mutate(draft_size = position_counts(position)) %>%
    mutate(
      position_index = row_number(),
      replacement_points = calculate_replacement_points(projected_points, draft_size[1]),
      next_player_available_points = lead(projected_points)
    ) %>%
    ungroup() %>%

    mutate(
      value_over_replacement = as.integer(projected_points - replacement_points),
      value_over_next = as.integer(projected_points - next_player_available_points)
    ) %>%
    select(-next_player_available_points,-draft_size) %>%
    arrange(desc(value_over_replacement)) %>%
    # Indicate simulated draft round
    mutate(draft_round = as.integer((row_number() - 1) / league_size) + 1)

  # Add index number to each row for reference
  players <- tibble::rowid_to_column(players, "index")
  players
}

plot_player_projection <- function(data) {
  plot <-
    ggplot(players,
           aes(x = position_index, y = projected_points, color = position)) +
    geom_line(aes(group = position)) +
    geom_point(aes(group = position)) +
    theme_high_contrast(
      foreground_color = "white",
      background_color = "black",
      base_family = "InputMono"
    ) +
    scale_x_discrete(labels = NULL, breaks = NULL) +
    scale_colour_brewer(palette = "Set2") +
    labs(
      title = "Projected Fantasy Points by Position (Full Season)",
      subtitle = "QBs are mostly good; Only a few good TEs exist",
      caption = "Data from https://fantasy.nfl.com",
      x = "",
      y = "Projected Points"
    )

  ggsave(
    str_interp("output/player_projection.png"),
    plot = plot,
    width = 6,
    height = 4
  )
}


plot_prev_season <- function(data) {
  plot <-
    ggplot(data,
           aes(x = position_index, y = ppr_score, color = fantasy_position)) +
    geom_line(aes(group = fantasy_position)) +
    geom_point(aes(group = fantasy_position)) +
    theme_high_contrast(
      foreground_color = "white",
      background_color = "black",
      base_family = "InputMono"
    ) +
    scale_x_discrete(labels = NULL, breaks = NULL) +
    scale_colour_brewer(palette = "Set2") +
    labs(
      title = "2020 Fantasy Points by Position (Full Season)",
      subtitle = "Showing players with over 100 points",
      caption = "Data from https://www.pro-football-reference.com",
      x = "",
      y = "Fantasy Points",
      color = "pos"
    )

  ggsave(
    str_interp("output/prev_season.png"),
    plot = plot,
    width = 6,
    height = 4
  )
}

players <- load_and_parse_data() %>%
  run_calculations()

write.csv(players, file = "output/replacement_points_rank.csv")

plot_player_projection(players)
plot_prev_season(load_and_parse_historical_data())
