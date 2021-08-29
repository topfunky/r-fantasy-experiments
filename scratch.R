library(tidyverse)
library(rvest)

html <- read_html("data/nfl-player-rankings.html")
name <- html_elements(html, "li a.playerName") %>% html_text2()
projected_points <-
  html_elements(html, "li span.projected") %>% html_text2() %>% readr::parse_double()
position_and_team <-
  html_elements(html, "li a.playerName + em") %>% html_text2()

players <-
  data.frame(name, projected_points, position_and_team) %>%
  separate(position_and_team, c("position", "team"), " - ") %>%
  group_by(position) %>%
  mutate(mean_replacement_points = mean(projected_points)) %>%
  ungroup() %>%
  mutate(value_over_replacement = as.integer(projected_season_score - mean_replacement_points)) %>%
  arrange(desc(value_over_replacement))

write.csv(players, file="output/replacement_points_rank.csv")

## ---

download_or_cache_file <- function(slug, url) {
  local_filename <- str_interp("data/cache/${slug}")

  if (!file.exists(local_filename)) {
    download.file(url, local_filename)
  }
  # TODO: Return file contents or filename
}

