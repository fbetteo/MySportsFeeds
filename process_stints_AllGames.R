# Prueba play by play


# Nota. Cada Stint tiene variables temporales que denotan cuando inician.
# El ultimo "inicia" al terminar el partido pero termina ahi mismo.

library(tidyverse)

# Me paro donde estan los rds
setwd(here::here("data","raw","play_by_play"))

# Leo todos
df <- list.files(pattern = ".rds") %>%
    map(readRDS)

# Funcion para acceder a lo util de cada lista
accesor <- function(x) x$api_json$plays

# Me quedo con lo  util de cada lista
df2 <- df %>%
  map(., pluck, accesor) %>%
  map(., .f = ~select(., description, playStatus.quarter, playStatus.secondsElapsed, starts_with("substitution")) %>%
  filter(substitution.team.id != "NA")) %>%
  map(., .f = ~select(.,playStatus.quarter, playStatus.secondsElapsed, substitution.team.id, 
                      substitution.incomingPlayer.id, substitution.outgoingPlayer.id))


# Nestear por cada cambio. Una tabla con todos los jugadores presentes.
df3 <- df2 %>% 
  map(., .f = ~group_by(.,playStatus.quarter, playStatus.secondsElapsed) %>% nest)


# Creas "stints". Cada periodo de tiempo con jugadores distintos en cancha.
df4 <- df3 %>%
  map(., .f = mutate, stint = row_number())

saveRDS(object = df4, file = here::here("data","working", "df4.rds"))

# Aca podemos cargar df4 para evitar leer toda la data cada vez que corro
# df4 <- readRDS(file = here::here("data","working", "df4.rds"))

# Cantidad de stints por partido
n_stints <- df4 %>% 
  map(., .f = summarise, count = max(stint))

n_stints

# Levanto los lineups de cada partido

setwd(here::here("data","raw","lineup"))

# Leo todos
lineups <- list.files(pattern = ".rds") %>%
  map(readRDS)

# Accesor para pluck

accesor_lineup <- function(x) x$api_json$teamLineups
accesor_away   <- function(x) x$api_json$game$awayTeam$id

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

# Juntos lineups con away

lineups3 <- map2(.x = lineups2, .y = away_teams, .f = left_join, by = "team.id") %>%
  map(., mutate, status = ifelse(is.na(status),1, status)) %>% 
  map(., select, -expected.lineupPositions) %>%
  map(., unnest)

# Genero lista vacia que va a tener cada equipo en cancha durante los stints.
# Lista de listas.

match_lineups <- vector("list", length = length(df4))

match_lineups2 <- map2(.x = match_lineups, .y = n_stints, .f = function(.x, .y) map(.x, vector, mode = "list", length = length(.y)))

# Jugadores que estuvieron listados para el partido.

match_players <- lineups3 %>%
  map(., function(x) x %>% select(player.id) %>% filter(player.id != "NA")) 


# Primer stint que va del inicio hasta la primera sustitucion
# Como se lo hago con purrr?
match_lineups3 <- match_lineups2
for (i in 1:length(match_lineups2)){

  match_lineups3[[i]][[1]] <- lineups3[[i]]  %>%
    filter(str_detect(position, "Starter")) %>%
    select(position, player.id, status) %>%
    mutate(substitution.team.id = NA)
}  

# dataframe para mergear
match_lineups3 <- match_lineups3 %>% 
  map(., as.data.frame)

# Mergeo con resto de jugadores. Tengo el primer stint hecho
match_lineups3 <- map2(.x = match_lineups3, .y = match_players, .f = right_join, by = "player.id") %>%
  map(., mutate, status = ifelse(is.na(status),0,status ))
 
# Reconvierto en lista de listas
# No hay mejor manera de hacer esto?

match_lineups4 <- vector("list", length = length(df4))

for (i in 1:length(match_lineups4)){
  match_lineups4[[i]][[1]] <- match_lineups3[[i]]
}

str(match_lineups4, list.len = 3)

# Falla en el 16

# Resto de los stints
for (j in 1:length(match_lineups3)){
  print(j)
  for (i in 2:(as.integer(n_stints[[j]])+1)){
    
    
    # Me quedo con los que ya estaban en cancha
    match_lineups4[[j]][[i]] <- match_lineups4[[j]][[i-1]] %>% select(-substitution.team.id) %>% filter(status != "NA" & status != 0)
    
    # Mergeo con sustitucion
    match_lineups4[[j]][[i]] <- match_lineups4[[j]][[i]] %>% left_join(df4[[j]][(i-1),]$data[[1]], by = c("player.id" = "substitution.outgoingPlayer.id"))
   
      
    # IF porque en los entretiempos figura como que sale todo el equipo y entran otros 5. Controlo por esa situacion
    if (sum(is.na(match_lineups4[[j]][[i]]$substitution.incomingPlayer.id)) == 10) {
      # Si es el final del partido, salen todos y no entra nadie.
      if (sum(df4[[j]][(i-1),]$data[[1]][,"substitution.incomingPlayer.id"], na.rm = TRUE) == 0) {
        print("End of Match")
       } 
        else {
        # Esto es para los entretiempos normales. Remplaza los 10 en cancha por los 10 que entren.  
        match_lineups4[[j]][[i]] <- data.frame( position = rep(x = "Starter", 10),
                                         player.id = df4[[j]][(i-1),]$data[[1]][which(!is.na(df4[[j]][(i-1),]$data[[1]]$substitution.incomingPlayer.id)),"substitution.incomingPlayer.id"][[1]],
                                         substitution.team.id = df4[[j]][(i-1),]$data[[1]][which(!is.na(df4[[j]][(i-1),]$data[[1]]$substitution.incomingPlayer.id)),"substitution.team.id"][[1]]) %>%
          mutate(status = ifelse(substitution.team.id == away_teams[[j]][[1]], -1, 1)) # hardcodeado para partido de prueba
        
       }
      }
          else {
      # Si no es entre tiempo remplaza el que sale por el que entra normalmente.
      match_lineups4[[j]][[i]] <- match_lineups4[[j]][[i]] %>% mutate(player.id = ifelse(is.na(substitution.incomingPlayer.id), player.id, substitution.incomingPlayer.id)) %>%
        select(-substitution.incomingPlayer.id)
    }
    # Joinea los 10 en cancha con el resto que quedó en el banco para tener todos los jugadores en una tabla
    # Los que quedan afuera tienen status = 0
    match_lineups4[[j]][[i]] <- match_lineups4[[j]][[i]] %>%
      right_join(match_players[[j]], by = "player.id") %>%
      mutate(status = ifelse(is.na(status),0,status))
  }
}
