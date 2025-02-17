# Levanto los lineups de cada partido
#
# https://www.landofbasketball.com/yearbyyear/2017_2018_playoffs_brackets.htm
setwd(here::here("data","raw","playoff_lineup"))
library(tidyverse)
library(janitor)

# Leo todos
lineups <- list.files(pattern = ".rds") %>%
  map(readRDS)

setwd(here::here())
player_ranking = readRDS("output/tables/player_ranking.rds")
# Accesor para pluck

accesor_lineup <- function(x) x$api_json$teamLineups
accesor_away   <- function(x) x$api_json$game$awayTeam$id
accesor_home   <- function(x) x$api_json$game$homeTeam$id

# Me quedo con lo util
lineups2 <- lineups %>%
  map(., pluck, accesor_lineup)

# Marco el equipo visitante
# le pongo status -1
# el local va a tener 1
away_teams <- lineups %>%
  map(., pluck, accesor_away) %>%
  map(., as.data.frame) %>%
  map(., .f = ~mutate(., status = -1)) %>%
  map(., set_names, c("team.id","status")) 

# Marco el equipo local
# le pongo status 1
# lo uso solo para la parte de puntos por ahora
home_teams <- lineups %>%
  map(., pluck, accesor_home) %>%
  map(., as.data.frame) %>%
  map(., .f = ~mutate(., status = 11)) %>%
  map(., set_names, c("team.id","status")) 


# Juntos lineups con away

lineups3 <- map2(.x = lineups2, .y = away_teams, .f = left_join, by = "team.id") %>%
  map(., mutate, status = ifelse(is.na(status),1, status)) %>% 
  map(., select, -expected.lineupPositions) %>%
  map(., unnest)

player_ranking_list = list(player_ranking)

average_possessions = readRDS(here::here("data","working","average_possessions.rds")) %>%
  mutate(player = as.integer(player))

playoff_games = map2(.x = lineups3, .y = player_ranking_list, .f = left_join, by = c("player.id" = "playerid")) %>%
  map(., .f = ~ .x %>% 
        left_join(average_possessions ,by = c("player.id" = "player")) %>% 
        mutate(coef_times_avg_poss = coef * average_pos) %>%
        group_by(team.abbreviation.x) %>%
        summarise(score = sum(coef_times_avg_poss, na.rm = TRUE)))

#### Merge de los scores por equipo con los resultados reales



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
  select(i_game, date, id, aux_ord, away_team, home_team, away_points, home_points,
         away_team_ranking, home_team_ranking, away_ranking_score, home_ranking_score)

saveRDS(playoff_test, "data/working/playoff_test.rds")
