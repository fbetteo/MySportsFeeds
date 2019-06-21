# From list ot DF

rm(list = ls())
library(tidyverse)

df_model <- readRDS(here::here("data","working","df_model.rds"))

# Need to add 0 for all the other players not involved.

View(df_model[[1]])

matrix_player_list <- vector(mode = "list") # full match matrix
matrix_player_list_col <- vector(mode = "list") # same but transposed to later add all the other league players with 0
temp <-tibble()
for (i in 1:length(df_model)){
  matrix_player_list[[i]] <-  df_model[[i]]$data %>%
    bind_rows() %>%
    tibble::add_column(stint = df_model[[i]]$stint)
  matrix_player_list_col[[i]] <- matrix_player_list[[i]]  %>%
    # mutate(stint = as.integer(row.names(.))- 1 )  %>% # -1 porque empieza en 0
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
 full_matrix_player <- list()
 full_matrix_player <- rep(list(list()),length(df_model))
 temp <- tibble()
 for (i in 1:length(df_model)){
   #full_matrix_player[i] <- list()
   for (j in 1:length(df_model[[i]]$data)){
     print(i)
     if (is.null(df_model[[i]]$data[[j]]) == TRUE) {next};
     temp <- df_model[[i]]$data[[j]] %>%
       gather(., key = "player", value = "included") %>%
       mutate(player = as.integer(as.character(.$player))) %>%
       anti_join(x = list_player_stint, y = ., by = "player") %>%
       mutate(included = 0)
     #browser()
     print(j)
     temp2 <- df_model[[i]]$data[[j]] %>% as_tibble(.) %>%
         gather(., key = "player", value = "included") %>%
         mutate(player = as.integer(as.character(.$player))) %>%
         rbind.data.frame(., temp)
     full_matrix_player[[i]] <- c(full_matrix_player[[i]], list(temp2))
 
   }     
}


full_matrix_player[2]

full_matrix_player_tr = list()
for (i in 1:length(full_matrix_player)){
  full_matrix_player_tr[[i]] <- map(full_matrix_player[[i]], spread, key = player, value = included )
}

## HASTA ACA GENERA LISTA DE CADA PARTIDO Y A SU INTERIOR CADA STINT TIENE EL FORMATO CORRECTo
## JUNTAR ESTO CON DF_MODEL
stints_differential <- map(df_model, select, c(stint, amount_possessions, diferential, dif_per_100_possessions ))

# One stint table per match (full playeres)
match_matrix_player_tr <- map(full_matrix_player_tr, bind_rows)
  
# Merging stints per match with points (list)
match_full_matrix <- map2(stints_differential, match_matrix_player_tr, cbind.data.frame)

# Tibble with all needed to model
matrix_model <- bind_rows(match_full_matrix)


saveRDS(matrix_model, file = here::here("data","working", "matrix_model.rds"))
#full_matrix_player_df <- map_df(full_matrix_player_tr, bind_rows) 
# head(full_matrix_player_df)
# full_matrix_player_tr[[1]]
# View(df_model[[16]])
 ## TENGO QUE JOINEAR TODO LOS JUGADORES DE LA LIGA CON LOS DEL PARTIDO PARA PONERLES 0
 ## Y TENGO QUE HACERLO POR STINT
 ## QUIZAS MEJOR HACERLO ANTES DE BINDEAR ROWS