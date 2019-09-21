library(tidyverse)
library(janitor)



playoff_results = read.csv(here::here("data","raw","playoffs_results.csv"),
                           header = TRUE, stringsAsFactors = FALSE) %>%
  clean_names() %>%
  mutate(home = str_trim(str_remove(home, '@')))


# Cleaning names
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



playoff_results = playoff_results %>% 
  left_join(teams, by = c("away" = "long_name")) %>%
  rename(away_team = short) %>%
  left_join(teams, by = c("home" = "long_name")) %>%
  rename(home_team = short) %>%
  mutate(aux_ord =row_number())

# One row per list object

reformat = function(x){
  temp = as.data.frame(x)
  half1 = temp[1,]
  half2 = temp[2,]
  output = cbind.data.frame(half1, half2) %>%
    set_names("away_team_ranking", "away_ranking_score", "home_team_ranking", "home_ranking_score")
  return(output)
}


playoff_games_reformat = map_df(playoff_games, .f = reformat) %>%
  mutate(aux_ord_ranking =row_number())

# Give same order to both tables

bracket_id = function(x,y){
  temp = c(x, y) %>%
    sort() %>%
    paste(collapse = "") 
}


playoff_games_reformat = playoff_games_reformat %>%
  mutate(id_ranking = pmap_chr(list(away_team_ranking, home_team_ranking),bracket_id)) %>%
  arrange(id_ranking, aux_ord_ranking)

playoff_results = playoff_results %>%
  mutate(id = pmap_chr(list(away_team, home_team),bracket_id)) %>%
  arrange(id, aux_ord)


# Join

playoff_test = cbind.data.frame(playoff_results, playoff_games_reformat) %>%
  select(i_game, date, aux_ord, away_team, home_team, away_points, home_points,
         away_team_ranking, home_team_ranking, away_ranking_score, home_ranking_score)

