# From list ot DF

# Parameter
# Minimum possessions required to be taken into accout
cutoff_pos <- 2000 # first quarter. Jugadores con al menos X posesiones en la temporada
cutoff_stint <- 5 # first quarter. Stints con al menos X posesiones.

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
    tibble::add_column(stint = df_model[[i]]$stint, amount_possessions = df_model[[i]]$amount_possessions)
                       
  matrix_player_list_col[[i]] <- matrix_player_list[[i]]  %>%
    # mutate(stint = as.integer(row.names(.))- 1 )  %>% # -1 porque empieza en 0
    gather(., key = "player", value = "included", -c("stint", "amount_possessions"))
  temp <- rbind(temp, data.frame(player = unique(matrix_player_list_col[[i]]$player)))
}


# List of all playerse of the league
 list_player_stint <- as.integer(as.character(unique(temp$player))) %>% tibble::enframe(name = NULL) %>%
   rename(player = value) %>%
   arrange(player)

# Removing PLayers that played too little.
 
 # Possessions in court
 possessions_in_court_player <- map(matrix_player_list_col, .f =  . %>% filter(included != 0) %>%
                                group_by(player) %>%
                                  summarise(n_possessions = sum(amount_possessions)))
 possessions_in_court_player_bind <- bind_rows(possessions_in_court_player) %>%
   group_by(player) %>%
   summarise(n_possessions = sum(n_possessions))
                                
# Plot to see distribution and pick a threshold
(g_pos <-  ggplot(data = possessions_in_court_player_bind) +
  geom_histogram(aes(x = n_possessions), binwidth = 130, color = "black", fill = "white"))
 
 summary(possessions_in_court_player_bind)
 # Pruebo dejando fuera el primer cuartil. 1011
 
 keep_players_possessions <- possessions_in_court_player_bind %>%
   filter(n_possessions > cutoff) %>% 
   select(player) %>%
   unlist() %>%
   as.integer()
 
 # Removing stints with too little possessions
  possessions_per_stint <- bind_rows(matrix_player_list_col)
  summary(possessions_per_stint$amount_possessions)
  (g_pos_stints <-  ggplot(data = possessions_per_stint) +
      geom_histogram(aes(x = amount_possessions), binwidth = 1, color = "black", fill = "white"))
  # First Q = 3
 # Re looping to merge to each stint all the other players

 full_matrix_player <- list()
 full_matrix_player <- rep(list(list()),length(df_model))
 temp <- tibble()
 for (i in 1:length(df_model)){
   #full_matrix_player[i] <- list()
   for (j in 1:length(df_model[[i]]$data)){
     if (i%%10 == 0){print(i)}
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
         rbind.data.frame(., temp) %>%
        filter(player %in% keep_players_possessions)
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
matrix_model <- bind_rows(match_full_matrix) %>%
  filter(amount_possessions > cutoff_stint)


saveRDS(matrix_model, file = here::here("data","working", "matrix_model.rds"))
#full_matrix_player_df <- map_df(full_matrix_player_tr, bind_rows) 
# head(full_matrix_player_df)
# full_matrix_player_tr[[1]]
# View(df_model[[16]])
 ## TENGO QUE JOINEAR TODO LOS JUGADORES DE LA LIGA CON LOS DEL PARTIDO PARA PONERLES 0
 ## Y TENGO QUE HACERLO POR STINT
 ## QUIZAS MEJOR HACERLO ANTES DE BINDEAR ROWS