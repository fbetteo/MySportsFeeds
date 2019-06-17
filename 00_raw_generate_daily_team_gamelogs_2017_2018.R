# Download daily team gamelogs for 2017-2018 regular

source(here::here("functions.R"))


## Input Manually ##
version <- "2.0"
league <- "nba"
season <- "2017-2018-regular"
formato <- "csv"
feed <- "daily_team_gamelogs"
output <- "daily_team_gamelogs"


dates_to_download <- seq(as.Date("2017-10-17"), as.Date("2018-04-12"), by="days") # all days between start and end of reg. season
dates_to_download2 <- gsub("-","",dates_to_download) # format requested by API

# Looping.

while (length(dates_to_download2) > 0) {

  # Cuales ya baje
  already_downloaded <-  stringr::str_sub(stringr::word(list.files(here::here("data","raw","daily_team_gamelogs")),
                                                        1, sep = "\\."), -8, end = -1)  
  # Descarto los que ya baje
  dates_to_download2 <-  dates_to_download2[!dates_to_download2 %in% already_downloaded]    
  
  # realiza descarga. TryCatch porque rompe por limite de descargas (?) y de esta manera skipea el error y sigue corriendo
  # permite descargar todo de un tiron
  tryCatch({
dates_to_download2 %>% map(~ API_request_daily_team_gamelogs(version, league, season, feed, formato, output =  paste0(output,.), date = .))}, error = function(e){})

  Sys.sleep(1) # Pruebas para no saturar
  print("SysSleep")
  
  # Creo que no actualiza suficientemente rapido el listado en carpeta y por eso descarga mas de una vez cada archivo.
}

