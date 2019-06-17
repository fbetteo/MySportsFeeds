# Generation of Seasonal Games 2017-2018 Regular Raw File #


source(here::here("functions.R"))


## Input Manually ##
version <- "2.0"
league <- "nba"
season <- "2017-2018-regular"
formato <- "csv"
feed <- "seasonal_games"
output <- "seasonal_games_2017_2018"

API_request(version, league, season, feed, formato, output)
