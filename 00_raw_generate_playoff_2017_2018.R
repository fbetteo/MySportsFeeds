# Generation of PlayOffs Games 2017-2018 Raw File #


source(here::here("functions.R"))


## Input Manually ##
version <- "2.0"
league <- "nba"
season <- "2018-2019-playoff" # asi baja los partidos de playoffs abril 2018..
formato <- "csv"
feed <- "seasonal_games"
output <- "playoff_games_2018"

API_request(version, league, season, feed, formato, output)
