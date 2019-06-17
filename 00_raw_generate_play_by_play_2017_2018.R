# Download daily team play by play for 2017-2018 regular
library(tidyverse)
source(here::here("functions.R"))


## Input Manually ##
version <- "2.0"
league <- "nba"
season <- "2017-2018-regular"
formato <- "csv"
feed <- "game_playbyplay"
output <- "play_by_play"


## Generating Meta Data to download
## Need seasonal games downloaded RDS already generated

seasonal_games <- readRDS(file = here::here("data","raw","seasonal_games_2017_2018.RDS"))
games_raw <- seasonal_games$api_json$games
games <- games_raw %>% select(game_id = schedule.id , game_start_time = schedule.startTime, 
                              away_team = schedule.awayTeam.abbreviation,
                              home_team = schedule.homeTeam.abbreviation)

games_to_download <- games %>% select(game_start_time, away_team, home_team) %>%
  mutate(time = as.numeric(str_sub(game_start_time, start = 12, end = 13))) %>%
  mutate(day = stringr::str_sub(game_start_time, start = 1, end = 10)) %>%
  mutate(day2 = ifelse(time < 7 , as.character(as.Date(day) - 1), day)) %>%
  mutate(day3 = gsub("-","",day2)) %>%
  mutate(game = paste(day3, away_team, home_team, sep = "-")) %>%
  select(game) %>%
  as.matrix()



##


while (length(games_to_download) > 0) {
  

  
  # realiza descarga. TryCatch porque rompe por limite de descargas (?) y de esta manera skipea el error y sigue corriendo
  # permite descargar todo de un tiron
  tryCatch({
    games_to_download %>% map(~ API_request_play_by_play(version, league, season, feed, formato, output =  paste0(output,.), game = .))}, error = function(e){})
  
 
 # Cuales ya baje
 already_downloaded <-  stringr::str_sub(stringr::word(list.files(here::here("data","raw","play_by_play")),
                                                       1, sep = "\\."), -16, end = -1) 
 
 # Descarto los que ya baje
 games_to_download <-  games_to_download[!games_to_download %in% already_downloaded]    
  
 Sys.sleep(1) # Pruebas para no saturar
  print("SysSleep")
  
 
}

