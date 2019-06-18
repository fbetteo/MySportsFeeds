# From list ot DF

rm(list = ls())
library(tidyverse)

df_model <- readRDS(here::here("data","working","df_model.rds"))

# Need to add 0 for all the other players not involved.

names(df_model[[1]]$data[[1]])

matrix_player_list <- vector(mode = "list") # full match matrix
matrix_player_list_col <- vector(mode = "list") # same but transposed to later add all the other league players with 0
temp <-tibble()
for (i in 1:length(df_model)){
  matrix_player_list[[i]] <-  df_model[[i]]$data %>%
    bind_rows()
  matrix_player_list_col[[i]] <- matrix_player_list[[i]]  %>%
    mutate(stint = as.integer(row.names(.))- 1 )  %>% # -1 porque empieza en 0
    gather(., key = "player", value = "included", -c("stint"))
  temp <- rbind(temp, data.frame(player = unique(matrix_player_list_col[[i]]$player)))
}

# List of all playerse of the league
 list_player_stint <- as.integer(as.character(unique(temp$player))) %>% tibble::enframe(name = NULL) %>%
   rename(player = value) %>%
   arrange(player)
  
 # Re looping to merge to each stint all the other players
 # NO FUNCA EL LIST INSIDE LIST POR LOOP
 # CORRER DE NUEVO BASES PORQUE EL 16 TIRA NULL
 
 full_matrix_player <- vector(mode = "list")
 temp <- tibble()
 for (i in 1:length(df_model)){
   for (j in 1:length(df_model[[i]]$data)){
     print(i)
     
     temp <- df_model[[i]]$data[[j]] %>%
       gather(., key = "player", value = "included") %>%
       mutate(player = as.integer(as.character(.$player))) %>%
       anti_join(x = list_player_stint, y = ., by = "player") %>%
       mutate(included = 0)
     print(j)
   full_matrix_player[[i]] <-  df_model[[i]]$data[[j]] %>% as_tibble(.) %>%
     gather(., key = "player", value = "included") #%>%
     #mutate(player = as.integer(as.character(.$player))) %>%
     #rbind.data.frame(., temp)
   }     
}
 
 full_matrix_player[1]
 df_model[[16]]$data[[34]]
 ww <- matrix_player_list_col[[1]] %>%
   mutate(player = as.integer(player))
 aa <-  full_join(ww,list_player, by = "player")
 length(df_model[[16]]$data)
 ## TENGO QUE JOINEAR TODO LOS JUGADORES DE LA LIGA CON LOS DEL PARTIDO PARA PONERLES 0
 ## Y TENGO QUE HACERLO POR STINT
 ## QUIZAS MEJOR HACERLO ANTES DE BINDEAR ROWS