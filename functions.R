
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


# Version para play by play. No se como inputar la ruta de export en el here::here si lo paso como parametro.
# Solo cambia el parametro "file" de saveRDS.


API_request_play_by_play <- function(version = "2.0", league = "nba", season, feed, formato, output, ...){
  
  library(mysportsfeedsR)
  authenticate_v2_x('4eec6849-46c9-43d2-9280-711e0c')
  
  output_file <- msf_get_results( version = version, league = league, season = season, feed = feed,
                                  params = list(format = formato, ...))
  
  saveRDS(object = output_file, file = here::here("data","raw","play_by_play", paste0(output,".rds")))
  
  
}


# Version para lineu`p. No se como inputar la ruta de export en el here::here si lo paso como parametro.
# Solo cambia el parametro "file" de saveRDS.


API_request_lineup <- function(version = "2.0", league = "nba", season, feed, formato, output, ...){
  
  library(mysportsfeedsR)
  authenticate_v2_x('4eec6849-46c9-43d2-9280-711e0c')
  
  output_file <- msf_get_results( version = version, league = league, season = season, feed = feed,
                                  params = list(format = formato, ...))
  
  saveRDS(object = output_file, file = here::here("data","raw","lineup", paste0(output,".rds")))
  
  
}

# Version para lineu`p. No se como inputar la ruta de export en el here::here si lo paso como parametro.
# Solo cambia el parametro "file" de saveRDS.


API_request_playoff_lineup <- function(version = "2.0", league = "nba", season, feed, formato, output, ...){
  
  library(mysportsfeedsR)
  authenticate_v2_x('4eec6849-46c9-43d2-9280-711e0c')
  
  output_file <- msf_get_results( version = version, league = league, season = season, feed = feed,
                                  params = list(format = formato, ...))
  
  saveRDS(object = output_file, file = here::here("data","raw","playoff_lineup", paste0(output,".rds")))
  
  
}


## Funcion para mergear stints ya consolidados con puntos generados en cada uno
## La idea es filtrar por cuarto y mergear las acciones cuyos momento en el tiempo condice con cada stint
## El objetivo es decir a que stint pertenece cada jugada.
## Se implemento para los puntos en primera instancia
## Acordarse que df1 es el grupo de stints
## df2 es la tabla con puntos

merge_stint <- function(df1,df2){
  quarters <- df1$playStatus.quarter %>% unique
 # print(quarters)
  full <- data.frame()
  for (i in 1:max(quarters)){
  #  print(i)
    tmp1 <- df1 %>% filter(playStatus.quarter == i)
    tmp2 <- df2 %>% filter(playStatus.quarter == i)
    df_joined <- fuzzyjoin::fuzzy_right_join(tmp1, tmp2, by = c("playStatus.secondsElapsed" = "playStatus.secondsElapsed", 
                                                              "end_stint" = "playStatus.secondsElapsed" ),
                                             match_fun = list(`<`,`>=`))
    full <- rbind(full, df_joined)
  }
  return(full)
}


## Funcion para chequear que match_lineups4 tiene la *mayoria* de los partidos OK
## Se fija que cada stint tenga 10 jugadores en cancha
## No todos los partidos lo respetan pero por lo que vi suele cagarse con cambios raros al final
## Data entry extraño al final del ultimo 4to y cosas asi

check_stints_matchlineups4 <- function(x) {
  map(x, function(x0) filter(x0, status != 0)) %>%
    map(.,function(x1) filas = nrow(x1)) %>%
    map(.,function(x2) x2 == 10) %>%
    rlist::list.rbind() %>%
    as.data.frame() %>%
    set_names(., "chequeo") %>%
    (function(x3) nrow(x3) == sum(x3$chequeo))
}

## Funcion para calcular el SE de los coeficientes de Ridge
## Obtenida de https://www.reddit.com/r/statistics/comments/1vg8k0/standard_errors_in_glmnet/
## Son SE sesgados. El modelo no tiene que tener intercepto

ridge_se <- function(xs,y,yhat,my_mod){
  # Note, you can't estimate an intercept here
  x2 <- as.matrix(xs)
  n <- dim(x2)[1]
  k <- dim(x2)[2]
  sigma_sq <- sum((y-predictions)^2)/ (n-k)
  lam <- model$lambda
  if(is.null(model$lambda)==TRUE){lam <- 0}
  i_lams <- matrix(diag(x=1,nrow=k,ncol=k))# ,sparse=TRUE)
  xpx <- t(x2)%*%x2
  xpxinvplam <- solve(xpx+lam*as.vector(i_lams))
  var_cov <- sigma_sq * (xpxinvplam %*% xpx %*% xpxinvplam)
  se_bs <- sqrt(diag(var_cov))
  print('NOTE: These standard errors are very biased.')
  return(se_bs)
}



## Funcion para calcular el SE de los coeficientes de Ridge pero devuelve la matriz de var-covar
## Obtenida de https://www.reddit.com/r/statistics/comments/1vg8k0/standard_errors_in_glmnet/
## Son SE sesgados. El modelo no tiene que tener intercepto

ridge_var_cov <- function(xs,y,yhat,my_mod){
  # Note, you can't estimate an intercept here
  x2 <- as.matrix(xs)
  n <- dim(x2)[1]
  k <- dim(x2)[2]
  sigma_sq <- sum((y-predictions)^2)/ (n-k)
  lam <- model$lambda
  if(is.null(model$lambda)==TRUE){lam <- 0}
  i_lams <- matrix(diag(x=1,nrow=k,ncol=k))# ,sparse=TRUE)
  xpx <- t(x2)%*%x2
  xpxinvplam <- solve(xpx+lam*as.vector(i_lams))
  var_cov <- sigma_sq * (xpxinvplam %*% xpx %*% xpxinvplam)
  # se_bs <- sqrt(diag(var_cov))
  # print('NOTE: These standard errors are very biased.')
  return(var_cov)
}
