# Generation of Team Stats Seasonal Games 2017-2018 Raw File #


source(here::here("functions.R"))


## Input Manually ##
version <- "2.0"
league <- "nba"
season <- "2017-2018-regular"
formato <- "csv"
feed <- "seasonal_team_stats"
output <- "team_stats_2017_2018"

API_request(version, league, season, feed, formato, output)
