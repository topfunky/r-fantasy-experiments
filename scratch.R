library(tidyverse)
library(rvest)

# The number of players at a position to use when calculating averages
player_limit_for_position = 12
league_size = 8

html <- read_html("data/nfl-player-rankings.200.html")
name <- html_elements(html, "li a.playerName") %>% html_text2()
projected_points <-
  html_elements(html, "li span.projected") %>% html_text2() %>% readr::parse_double()
position_and_team <-
  html_elements(html, "li a.playerName + em") %>% html_text2()

players <-
  data.frame(name, projected_points, position_and_team) %>%
  separate(position_and_team, c("position", "team"), " - ") %>%
  group_by(position) %>%
  arrange(desc(projected_points)) %>%
  mutate(
    mean_replacement_points = mean(head(
      projected_points, player_limit_for_position
    )),
    next_player_available_points = lead(projected_points)
  ) %>%
  ungroup() %>%

  mutate(
    value_over_replacement = as.integer(projected_points - mean_replacement_points),
    value_over_next = as.integer(projected_points - next_player_available_points)
  ) %>%
  select(-next_player_available_points) %>%
  arrange(desc(value_over_replacement)) %>%

  mutate(draft_round = as.integer((row_number() - 1) / league_size) + 1)

# Add index number to each row for reference
players <- tibble::rowid_to_column(players, "index")

write.csv(players, file = "output/replacement_points_rank.csv")
