library(tidyverse)
library(janitor)

teams = data.frame( long_name = unique(playoff_results$home) ,stringsAsFactors = FALSE) %>%
  cbind.data.frame(short = c("OKC",
                             "UTA",
                             "POR",
                             "NOP",
                             "HOU",
                             "MIN",
                             "GSW",
                             "SAS",
                             "TOR",
                             "WAS",
                             "PHI",
                             "MIA",
                             "CLE",
                             "IND",
                             "BOS",
                             "MIL")) %>%
  mutate(short = as.character(short))



playoff_results = read.csv(here::here("data","raw","playoffs_results.csv"),
                           header = TRUE, stringsAsFactors = FALSE) %>%
  clean_names() %>%
  mutate(home = str_trim(str_remove(home, '@'))) %>%
  left_join(teams, by = c("away" = "long_name")) %>%
  rename(away_team = short) %>%
  left_join(teams, by = c("home" = "long_name")) %>%
  rename(home_team = short)


# TENGO QUE JUNTAR ESTA TABLE PLAYOFF RESULTS CON LA LISTA PLAYOFFS GAMES QUE TIENE LOS SCORES POR PARTIDO

aa = map(playoff_games, .f = ~as.data.frame(.x))

aa[[1]] %>%
  spread(., key = team.abbreviation.x, value = score) %>%
  spread(., key =)
