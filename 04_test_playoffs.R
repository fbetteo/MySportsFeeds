# Levanto los lineups de cada partido
# BAJAR DATA DE PLAYOFFS 2018
# Esta MAL LA LISTA DE PARTIDOS DE 2018 y POR ENDE LOS LINEUPS
# https://www.landofbasketball.com/yearbyyear/2017_2018_playoffs_brackets.htm
setwd(here::here("data","raw","playoff_lineup"))

# Leo todos
lineups <- list.files(pattern = ".rds") %>%
  map(readRDS)

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

playoff_games = map2(.x = lineups3, .y = player_ranking_list, .f = left_join, by = c("player.id" = "playerid")) %>%
  map(., .f = ~ .x %>% 
        left_join(average_possessions ,by = c("player.id" = "player")) %>% 
        mutate(coef_times_avg_poss = coef * average_pos) %>%
        group_by(team.abbreviation.x) %>%
        summarise(score = sum(coef_times_avg_poss, na.rm = TRUE)))



