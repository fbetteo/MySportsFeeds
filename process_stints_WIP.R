# Prueba play by play


# Nota. Cada Stint tiene variables temporales que denotan cuando inician.
# El ultimo "inicia" al terminar el partido pero termina ahi mismo.

library(tidyverse)
# Leer data
raw <- readRDS(here::here("data","raw","play_by_play","play_by_play20171017-BOS-CLE.RDS"))
raw_plays <- raw$api_json$plays

# filtrar por sustituciones
subs <- raw_plays %>% select(description, playStatus.quarter, playStatus.secondsElapsed, starts_with("substitution")) %>%
  filter(substitution.team.id != "NA")

head(raw_plays)

glimpse(subs)

# Clean
aa <- subs %>% select(playStatus.quarter, playStatus.secondsElapsed, substitution.team.id, substitution.incomingPlayer.id, substitution.outgoingPlayer.id)

head(aa)

# Nestear por cada cambio. Una tabla con todos los jugadores presentes.
a2 <- aa %>% 
  group_by(playStatus.quarter, playStatus.secondsElapsed) %>%
  nest()

# Creas "stints". Cada periodo de tiempo con jugadores distintos en cancha.
a2 <- a2 %>% mutate(stint = row_number())
a2

# Cantidad de stints en el partido
n_stints <- a2 %>% summarise( count = max(stint))
n_stints

# equipo inicial
lineup <- readRDS(here::here("data","raw","lineup","lineup20171017-BOS-CLE.RDS"))

raw_lineup <- lineup$api_json$teamLineups
raw_lineup[4]
str(raw_lineup)

# Equipo inicial con status correspondiente -1 away, 1 home.
bosline <- raw_lineup$actual.lineupPositions[[1]] %>%
  mutate(status = -1)
bosline
cleline <- raw_lineup$actual.lineupPositions[[2]] %>%
  mutate(status = 1)
cleline

# Genero lista vacia que va a tener cada equipo en cancha durante los stints.
match_lineup <- vector("list", length = (n_stints[[1]]+1))

# Jugadores que estuvieron listados para el partido.
match_players <- bosline %>% rbind(cleline) %>%
  select(player.id) %>% filter(player.id != "NA")

match_players

# Primer stint que va del inicio hasta la primera sustitucion
match_lineup[[1]] <- bosline %>% rbind(cleline) %>%
  filter(str_detect(position, "Starter")) %>%
  select(position, player.id, status) %>%
  mutate(substitution.team.id = NA) %>%
  right_join(match_players, by = "player.id") %>%
  mutate(status = ifelse(is.na(status),0,status ))

# Resto de los stints
for (i in 2:(n_stints[[1]]+1)){
  print(i)
  # Me quedo con los que ya estaban en cancha
  match_lineup[[i]] <- match_lineup[[i-1]] %>% select(-substitution.team.id) %>% filter(status != "NA" & status != 0)
  # Mergeo con sustitucion
  match_lineup[[i]] <- match_lineup[[i]] %>% left_join(a2[(i-1),]$data[[1]], by = c("player.id" = "substitution.outgoingPlayer.id"))
  
  # IF porque en los entretiempos figura como que sale todo el equipo y entran otros 5. Controlo por esa situacion
  if (sum(is.na(match_lineup[[i]]$substitution.incomingPlayer.id)) == 10) {
    # Si es el final del partido, salen todos y no entra nadie.
    if (sum(a2[(i-1),]$data[[1]][,"substitution.incomingPlayer.id"], na.rm = TRUE) == 0) {
      print("End of Match")
      
      } else {
    # Esto es para los entretiempos normales. Remplaza los 10 en cancha por los 10 que entren.  
    match_lineup[[i]] <- data.frame( position = rep(x = "Starter", 10),
                                     player.id = a2[(i-1),]$data[[1]][which(!is.na(a2[(i-1),]$data[[1]]$substitution.incomingPlayer.id)),"substitution.incomingPlayer.id"][[1]],
                                     substitution.team.id = a2[(i-1),]$data[[1]][which(!is.na(a2[(i-1),]$data[[1]]$substitution.incomingPlayer.id)),"substitution.team.id"][[1]]) %>%
      mutate(status = ifelse(substitution.team.id == 82, -1, 1)) # hardcodeado para partido de prueba
       
        }
      } else {
    # Si no es entre tiempo remplaza el que sale por el que entra normalmente.
    match_lineup[[i]] <- match_lineup[[i]] %>% mutate(player.id = ifelse(is.na(substitution.incomingPlayer.id), player.id, substitution.incomingPlayer.id)) %>%
    select(-substitution.incomingPlayer.id)
      }
  # Joinea los 10 en cancha con el resto que quedó en el banco para tener todos los jugadores en una tabla
  # Los que quedan afuera tienen status = 0
  match_lineup[[i]] <- match_lineup[[i]] %>%
    right_join(match_players, by = "player.id") %>%
    mutate(status = ifelse(is.na(status),0,status))
  
 
}

# Genera data para el primer stint que es desde el arranque del partido
inital_stint <- data.frame(playStatus.quarter = 1, playStatus.secondsElapsed = 0, stint = 0) 

# Junta el primer stint con el Nest del inicio y pone los lineups como una variable (lista)
a3 <- rbind(inital_stint,select(a2, playStatus.quarter, playStatus.secondsElapsed,stint)) %>%
  mutate(data = match_lineup)

# Genera dataset con una row por stint con el lineup traspuesto, cada jugador como columna.
# Es el formato requerido para despues modelar.
df_tidy <- rbind(inital_stint,select(a2, playStatus.quarter, playStatus.secondsElapsed,stint)) %>%
  mutate(data = match_lineup %>% map(.f = function(x) select(x,player.id, status))) %>%
  mutate(data = data %>% map(.f = ~spread(., key = player.id,value = status )))
  


####

# Proceso los puntos anotados durante el partido

points <- raw_plays %>% select(description, playStatus.quarter, playStatus.secondsElapsed, starts_with("fieldGoal"), starts_with("freeThrow")) %>%
  filter(fieldGoalAttempt.result == "SCORED" | freeThrowAttempt.result == "SCORED") %>% # jugadas o tiros libres anotados
  mutate(freeThrowAttempt.points = ifelse(freeThrowAttempt.result == "SCORED",1,0)) %>% # le agrego el valor de los FT
  mutate(abs_point = rowSums(.[,c("fieldGoalAttempt.points", "freeThrowAttempt.points")],na.rm = TRUE)) %>% # puntos anotados en la jugada
  mutate(fieldGoalAttempt.team.abbreviation = ifelse(is.na(fieldGoalAttempt.team.abbreviation),0,fieldGoalAttempt.team.abbreviation)) %>% # Clean porque si no fallaba al haber NA
  mutate( freeThrowAttempt.team.abbreviation = ifelse(is.na(freeThrowAttempt.team.abbreviation),0,freeThrowAttempt.team.abbreviation)) %>%  # Clean porque si no fallaba al haber NA
  # Aca anoto transformo a negativos los puntos del visitante
  mutate(multiplicador_puntos = ifelse(fieldGoalAttempt.team.abbreviation == "BOS" | freeThrowAttempt.team.abbreviation == "BOS", -1, ifelse(fieldGoalAttempt.team.abbreviation == "CLE" | freeThrowAttempt.team.abbreviation == "CLE", 1,0))) %>%
  # Diferencial de puntos, queda visto desde el Local. positivo es puntos a favor del local, negativos a favor del visitante
  # Va de la mano con la variable status de cada jugador. 1 para los locales, -1 para los visitantes
  mutate(diferencial = abs_point * multiplicador_puntos)

# Tabla temporal con los stints del partido y su inicio/fin para mergear con la de puntos y ubicarlos dentro de cada stint
temp_stint <- select(df_tidy, stint, playStatus.quarter, playStatus.secondsElapsed) %>%
  group_by(playStatus.quarter) %>%
  mutate(end_stint = lead(playStatus.secondsElapsed, 1)) %>%
  ungroup() %>%
  mutate(end_stint = ifelse(is.na(end_stint), 721, end_stint ))

# funcion custom definida en functions.R
points_stint <- merge_stint(temp_stint, points)

# variable dependiente
# diferencial de puntos por stints
dep_var <- points_stint %>% group_by(stint) %>%
  summarise(depvar = sum(diferencial))
