
# Nota. Cada Stint tiene variables temporales que denotan cuando inician.
# El ultimo "inicia" al terminar el partido pero termina ahi mismo.
rm(list = ls())
library(tidyverse)
source(here::here("functions.R"))
# Me paro donde estan los rds
setwd(here::here("data","raw","play_by_play"))

# Leo todos
df <- list.files(pattern = ".rds") %>%
    map(readRDS)

# Funcion para acceder a lo util de cada lista
accesor <- function(x) x$api_json$plays

# raw_plays, se usa para obtener los puntos
raw_plays <- df %>%
  map(., pluck, accesor)

# Me quedo con lo  util de cada lista
df2 <- df %>%
  map(., pluck, accesor) %>%
  map(., .f = ~select(., description, playStatus.quarter, playStatus.secondsElapsed, starts_with("substitution")) %>%
  filter(substitution.team.id != "NA")) %>%
  map(., .f = ~select(.,playStatus.quarter, playStatus.secondsElapsed, substitution.team.id, 
                      substitution.incomingPlayer.id, substitution.outgoingPlayer.id))

# Falla en el 16
# Detecte que en i = 20 (entretiempo) team 83 "incluye" 6 jugadores en vez de 5
# uno de esos 6. el 13869 no forma parte del match lineup.
# bug? lo remuevo. Levantar issue



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

saveRDS(lineups3, file = here::here("data","working", "lineups3.rds"))

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



# Falla el loop posterior en el 16
# Detecte que en i = 20 (entretiempo) team 83 "incluye" 6 jugadores en vez de 5
# uno de esos 6. el 13869 no forma parte del match lineup.
# bug? lo remuevo. Levantar issue

# Aplicar en order

# falla 10139 en el 3qt de partido 18
# falla 9169 en el 3qt de partido 19
# falla el partido 35, no hay outogoing players y rompe, dropeo por ahora
# drop 36 no se el error

# df4[[16]][19,]$data[[1]] <- df4[[16]][19,]$data[[1]] %>% filter(substitution.incomingPlayer.id != 13869 | is.na(substitution.incomingPlayer.id))
# df4[[18]][20,]$data[[1]] <- df4[[18]][20,]$data[[1]] %>% filter(substitution.incomingPlayer.id != 10139 | is.na(substitution.incomingPlayer.id))
# df4[[19]][13,]$data[[1]] <- df4[[19]][13,]$data[[1]] %>% filter(substitution.incomingPlayer.id != 9169 | is.na(substitution.incomingPlayer.id))
# 
# df4[35] <- NULL ; away_teams[[35]] <- NULL ; match_players[[35]] <- NULL; n_stints[[35]] <- NULL
# match_lineups3[[35]] <- NULL

# Falla el 40 tambien. Encontrar una solucion mas integral...
 
# Reconvierto en lista de listas
# No hay mejor manera de hacer esto?

match_lineups4 <- vector("list", length = length(df4))

for (i in 1:length(match_lineups4)){
  match_lineups4[[i]][[1]] <- match_lineups3[[i]]
}

str(match_lineups4, list.len = 3)


# Resto de los stints
for (j in 1:length(match_lineups3)){
  print(j)
  for (i in 2:(as.integer(n_stints[[j]])+1)){
   #if(j>900) print(i)
    
    # Me quedo con los que ya estaban en cancha
    match_lineups4[[j]][[i]] <- match_lineups4[[j]][[i-1]] %>% select(-substitution.team.id) %>% filter(status != "NA" & status != 0)
    
    # Mergeo con sustitucion
    match_lineups4[[j]][[i]] <- match_lineups4[[j]][[i]] %>% left_join(df4[[j]][(i-1),]$data[[1]], by = c("player.id" = "substitution.outgoingPlayer.id"))
   
   # Nos fijamos si un stint no tiene ningun "incoming" y lo saltea
   # Lo hacemos porque si no crashea a veces cuando pasan cosas extrañas al final del partido
   if (length(match_lineups4[[j]][[i]]$substitution.incomingPlayer.id) == 0) next;  
       
    # IF porque en los entretiempos figura como que sale todo el equipo y entran otros 5. Controlo por esa situacion
    if (sum(is.na(match_lineups4[[j]][[i]]$substitution.incomingPlayer.id)) == 10) {
      # Si es el final del partido, salen todos y no entra nadie.
      if (sum(df4[[j]][(i-1),]$data[[1]][,"substitution.incomingPlayer.id"], na.rm = TRUE) == 0) {
        print("End of Match")
       } 
        else {
        # Esto es para los entretiempos normales. Remplaza los 10 en cancha por los 10 que entren.  
        match_lineups4[[j]][[i]] <- data.frame( position = rep(x = "Starter", 10),
                                         player.id = df4[[j]][(i-1),]$data[[1]][which(!is.na(df4[[j]][(i-1),]$data[[1]]$substitution.incomingPlayer.id)),"substitution.incomingPlayer.id"][[1]][1:10],
                                         substitution.team.id = df4[[j]][(i-1),]$data[[1]][which(!is.na(df4[[j]][(i-1),]$data[[1]]$substitution.incomingPlayer.id)),"substitution.team.id"][[1]][1:10]) %>%
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

## Remuevo Duplicados, se encuentran duplicados en cancha..
## Medio bruto remover por player.id pero no puedo revisar partido a partido

remdup1 <- match_lineups4 %>% map(., map, function(x) x %>% distinct(player.id, .keep_all =TRUE) %>%
                           select(position, player.id, status, substitution.team.id))

## Chequeo que los stints de cada partido tengan 10 jugadores
## no se cumple siempre pero mayoria es por cambios extraños al final del partido
## o los ultimos stints. Gran mayoria TRUE. 1140 TRUE 89 FALSE

check1 <- remdup1  %>%
  map(., check_stints_matchlineups4)

check1 %>%
  rlist::list.rbind() %>%
  as.data.frame() %>%
  set_names(., "chequeo2") %>%
  table()

check2 <- check1 %>%
  rlist::list.rbind() %>%
  as.data.frame() %>%
  set_names(., "chequeo2")

# Genera data para el primer stint que es desde el arranque del partido
initial_stint <- vector("list", length = length(remdup1))
for (i in 1:length(remdup1)){
initial_stint[[i]] <- data.frame(playStatus.quarter = 1, playStatus.secondsElapsed = 0, stint = 0) 
}


# Junta el primer stint con el Nest del inicio y pone los lineups como una variable (lista)
# df5 <- pmap(list(x = initial_stint,y = df4,z = remdup1), function(x, y, z) {rbind(x, select(y, playStatus.quarter, playStatus.secondsElapsed, stint)) %>%
#  mutate(data = z)})


# Genera dataset con una row por stint con el lineup traspuesto, cada jugador como columna.
# Es el formato requerido para despues modelar.
df_tidy <- pmap(list(x = initial_stint,y = df4,z = remdup1), function(x, y, z) {rbind(x, 
                                                                                             select(y, playStatus.quarter, playStatus.secondsElapsed,stint)) %>%
    mutate(data = z %>% map(.f = function(x) select(x,player.id, status))) %>%
    mutate(data = data %>% map(.f = ~spread(., key = player.id, value = status)))
    })


saveRDS(object = df_tidy, file = here::here("data","working", "df_tidy.rds"))
####
## POINTS ##
####

# Proceso los puntos anotados durante el partido
# Dropeo el 35 para que sea coherente con df4 y etc que dropee.
#raw_plays[[35]] <- NULL
saveRDS(object = raw_plays, file = here::here("data","working", "raw_plays.rds"))
# raw_plays <- readRDS(file = here::here("data","working", "raw_plays.rds"))
## CAMBIAR HARDOCEDAD DE EEQUIPO VISITANTE

points <-pmap(list(.x = raw_plays , .y = away_teams, .z = home_teams) , .f = function(.x,.y,.z) select(.x,description, playStatus.quarter, playStatus.secondsElapsed, starts_with("fieldGoal"), starts_with("freeThrow")) %>%
  filter(fieldGoalAttempt.result == "SCORED" | freeThrowAttempt.result == "SCORED") %>% # jugadas o tiros libres anotados
  mutate(freeThrowAttempt.points = ifelse(freeThrowAttempt.result == "SCORED",1,0)) %>% # le agrego el valor de los FT
  mutate(abs_point = rowSums(.[,c("fieldGoalAttempt.points", "freeThrowAttempt.points")],na.rm = TRUE)) %>% # puntos anotados en la jugada
  mutate(fieldGoalAttempt.team.abbreviation = ifelse(is.na(fieldGoalAttempt.team.abbreviation),0,fieldGoalAttempt.team.abbreviation)) %>% # Clean porque si no fallaba al haber NA
  mutate( freeThrowAttempt.team.abbreviation = ifelse(is.na(freeThrowAttempt.team.abbreviation),0,freeThrowAttempt.team.abbreviation)) %>%  # Clean porque si no fallaba al haber NA
  mutate(fieldGoalAttempt.team.id = ifelse(is.na(fieldGoalAttempt.team.id),0,fieldGoalAttempt.team.id)) %>% # Clean porque si no fallaba al haber NA
  mutate( freeThrowAttempt.team.id = ifelse(is.na(freeThrowAttempt.team.id),0,freeThrowAttempt.team.id)) %>%
    # Aca anoto transformo a negativos los puntos del visitante
  mutate(multiplicador_puntos = ifelse(fieldGoalAttempt.team.id == .y$team.id | freeThrowAttempt.team.id == .y$team.id, -1, ifelse(fieldGoalAttempt.team.id == .z$team.id | freeThrowAttempt.team.id == .z$team.id, 1,0))) %>%
  # Diferencial de puntos, queda visto desde el Local. positivo es puntos a favor del local, negativos a favor del visitante
  # Va de la mano con la variable status de cada jugador. 1 para los locales, -1 para los visitantes
  mutate(diferencial = abs_point * multiplicador_puntos) %>%
  # Agregamos fix para las faltas tecnicas en el segundo 0. Rompen "merge_stint"
  mutate(playStatus.secondsElapsed = ifelse(playStatus.secondsElapsed == 0, 1 , playStatus.secondsElapsed)))


# Tabla temporal con los stints del partido y su inicio/fin para mergear con la de puntos y ubicarlos dentro de cada stint
temp_stint <- df_tidy %>% map(., function(x) select(x, stint, playStatus.quarter, playStatus.secondsElapsed) %>%
  group_by(playStatus.quarter) %>%
  mutate(end_stint = lead(playStatus.secondsElapsed, 1)) %>%
  ungroup() %>%
  mutate(end_stint = ifelse(is.na(end_stint), 721, end_stint )))

# funcion custom definida en functions.R
points_stint <- map2(.x = temp_stint, .y = points, merge_stint)

# variable dependiente
# diferencial de puntos por stints
point_diferential <- points_stint %>% map(., function(x) x %>% group_by(stint) %>%
  summarise(diferential = sum(diferencial)))

# pueden quedar stints sin puntos
# al mergear con las posesiones corregimos eso y las creamos con 0 puntos de diferencial

####
## POSSESSIONS ##
####

possessions <- raw_plays %>% map(., function(x) select(x, description, playStatus.quarter, playStatus.secondsElapsed, fieldGoalAttempt.result,
                                    rebound.type, freeThrowAttempt.result, freeThrowAttempt.attemptNum, freeThrowAttempt.totalAttempts,
                                    turnover.type) %>%
  mutate(end_possession = ifelse(fieldGoalAttempt.result == "SCORED" | rebound.type == "DEFENSIVE" |  is.na(turnover.type) == FALSE | 
                                   (freeThrowAttempt.result == "SCORED" & freeThrowAttempt.attemptNum == freeThrowAttempt.totalAttempts), 1, 0)) %>%
  filter(end_possession == 1) %>%
    mutate(playStatus.secondsElapsed = ifelse(playStatus.secondsElapsed == 0, 1 , playStatus.secondsElapsed)))


possessions_stint <- map2(.x = temp_stint, .y = possessions, merge_stint)

possesions_by_stint <-  possessions_stint %>% map(., function(x) x %>% group_by(stint) %>%
  summarise(amount_possessions = sum(end_possession))) # Sumar una mas, o ver como hacer
# para tener en cuenta la ultima posesion de cada cuarto que puede
# "no" terminar por causas tipicas y por lo tanto no estar contando una posesion


####
## Joining Possessions and Points
####

dependent_var <- possesions_by_stint %>% map2(.x = ., .y = point_diferential, function(.x, .y) left_join(x = .x, y =.y, by = "stint") %>%
  mutate(diferential = ifelse(is.na(diferential), 0, diferential)) %>%
  mutate(dif_per_100_possessions = diferential/amount_possessions*100))

####
## LIST OF TABLE TO MODEL 
####

df_model <- dependent_var %>% map2(.x =., .y = df_tidy, function(.x, .y) left_join(x = .x, y =.y, by = "stint"))
saveRDS(df_model, file = here::here("data","working", "df_model.rds"))

#

temp1 <- df_model %>%
  map(., function(x) x %>% filter(.,is.na(stint) == FALSE))

temp2 <- temp1 %>%
  map(., unnest)