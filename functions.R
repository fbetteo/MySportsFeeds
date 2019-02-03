
### API REQUEST ###
## Function to get data from API of MySportsFeeds and save it as file in data/raw ##

# version = API version ("2.0")
# league = sport to download ("nba","nfl", "nhl", "mlb")
# season = season to download ("2017-2018-regular")
# feed = data to download. ("seasonal_games") https://www.mysportsfeeds.com/data-feeds/nba/feedlist/
# formato = formato de la descarga ("csv", "json", "xml")
# output = nombre del archivo rds a exportar a data/raw
 
API_request <- function(version = "2.0", league = "nba", season, feed, formato, output, ...){
  
    library(mysportsfeedsR)
    authenticate_v2_x('4eec6849-46c9-43d2-9280-711e0c')
  
      output_file <- msf_get_results( version = version, league = league, season = season, feed = feed,
                                  params = list(format = formato, ...))
      
      saveRDS(object = output_file, file = here::here("data","raw", paste0(output,".rds")))
}


# Version para daily_team_gamelogs. No se como inputar la ruta de export en el here::here si lo paso como parametro.
# Solo cambia el parametro "file" de saveRDS.

API_request_daily_team_gamelogs <- function(version = "2.0", league = "nba", season, feed, formato, output, ...){
  
  library(mysportsfeedsR)
  authenticate_v2_x('4eec6849-46c9-43d2-9280-711e0c')
  
  output_file <- msf_get_results( version = version, league = league, season = season, feed = feed,
                                  params = list(format = formato, ...))
  
  saveRDS(object = output_file, file = here::here("data","raw","daily_team_gamelogs", paste0(output,".rds")))
  
  
}

