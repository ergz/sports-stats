library(rvest)
library(tidyverse)

all_tables <- read_html("https://www.basketball-reference.com/leagues/NBA_2017_standings.html#expanded_standings") %>% 
  html_table(trim = TRUE)

clean_conference_standings_tables <- function(d) {
  d %>% 
    select(
      team = ends_with("Conference"), 
      win = "W", 
      loss = "L", 
      win_loss_prop = "W/L%", 
      games_behind = "GB", 
      points_per_game = "PS/G", 
      points_against_per_game = "PA/G", 
      srs = "SRS"
    ) %>% 
    filter(!str_detect(team, "Division")) %>% 
    transmute(
      team = str_replace_all(team, "\\*", ""), 
      win = as.numeric(win),
      loss = as.numeric(loss), 
      win_loss_prop = as.numeric(win_loss_prop),
      points_per_game = as.numeric(points_per_game), 
      points_against_per_game = as.numeric(points_against_per_game), 
      srs = as.numeric(srs)
    ) %>% 
    arrange(desc(win), loss)
}

eastern_table <- all_tables[[3]] %>% clean_conference_standings_tables()
western_table <- all_tables[[4]] %>% clean_conference_standings_tables()

years <- 1980:2020

conference_standings <- map_df(years, function(y) {
  nba_link <- glue::glue("https://www.basketball-reference.com/leagues/NBA_{year}_standings.html#expanded_standings", year = y)
  tables <- read_html(nba_link) %>% html_table()
  if (length(tables) == 4) {
    eastern_index <- 3
    western_index <- 4
  } else {
    eastern_index <- 1
    western_index <- 2
  }
  print(y)
  eastern_table <- tables[[eastern_index]] %>% clean_conference_standings_tables()
  western_table <- tables[[western_index]] %>% clean_conference_standings_tables()
  
  bind_rows(
    eastern_table %>% mutate(conference = "eastern", year = y), 
    western_table %>% mutate(conference = "western", year = y)
  )
  
})

conference_standings %>% 
  ggplot(aes(year, points_per_game)) + geom_point()

conference_standings %>% 
  group_by(year) %>% 
  summarise(
    max_win = max(win),
    max_prop = max(win_loss_prop)
  ) %>% 
  ggplot(aes(year, max_prop)) + geom_point()  












