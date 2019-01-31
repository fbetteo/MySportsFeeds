##

# My Sports Feed #

# Instalacióm

devtools::install_github("MySportsFeeds/mysportsfeeds-r")

# Log in

library(mysportsfeedsR)
authenticate_v2_x('4eec6849-46c9-43d2-9280-711e0c')

# Ejemplo
seas_games <- msf_get_results(version = "2.0", league = "nba", season = "2017-2018-regular", 
                              feed = "seasonal_games", params = list(team = "bos", format = "csv"))

seas_games

# Inspeccion
attributes(seas_games$response)
attributes(seas_games$api_json$games)

# Asigno tabla con data
asd <-  seas_games$api_json$games

##

# Ejemplo Boxscore partido

boxscore <- msf_get_results(version = "2.0", league = "nba", season = "2017-2018-regular", feed = "game_boxscore",
                            params = list(game = "20171017-BOS-CLE", format = "csv"))


# Inspeccion
attributes(boxscore)
attributes(boxscore$response)
attributes(boxscore$api_json$stats$away$teamStats)
attributes(boxscore$api_json$references$teamStatReferences)
boxscore$api_json$scoring

table <- boxscore$api_json$stats$away$players$playerStats[[1]]


