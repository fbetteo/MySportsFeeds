# Prueba play by play

library(tidyverse)
raw <- readRDS(here::here("data","raw","play_by_play","play_by_play20171017-BOS-CLE.RDS"))
raw_plays <- raw$api_json$plays

subs <- raw_plays %>% select(description, playStatus.quarter, playStatus.secondsElapsed, starts_with("substitution")) %>%
  filter(substitution.team.id != "NA")

head(raw_plays)

glimpse(subs)

aa <- subs %>% select(playStatus.quarter, playStatus.secondsElapsed, substitution.team.id, substitution.incomingPlayer.id, substitution.outgoingPlayer.id)

head(aa)

a2 <- aa %>% 
  group_by(playStatus.quarter, playStatus.secondsElapsed) %>%
  nest()

a2 <- a2 %>% mutate(stint = row_number())
a2

n_stints <- a2 %>% summarise( count = max(stint))
n_stints
lineup <- readRDS(here::here("data","raw","lineup","lineup20171017-BOS-CLE.RDS"))

raw_lineup <- lineup$api_json$teamLineups
raw_lineup[4]
str(raw_lineup)

bosline <- raw_lineup$actual.lineupPositions[[1]] %>%
  mutate(status = -1)
bosline
cleline <- raw_lineup$actual.lineupPositions[[2]] %>%
  mutate(status = 1)
cleline


ex1 <- a2[2,]
str(ex1)
ex1$data[[1]]

bosupd <- bosline %>% left_join(ex1$data[[1]], by = c("player.id" = "substitution.outgoingPlayer.id")) %>%
  mutate(status = -1)

bosupd
match_lineup <- vector("list", length = (n_stints[[1]]+1))

a2[1,]$data[[1]]

match_players <- bosline %>% rbind(cleline) %>%
  select(player.id) %>% filter(player.id != "NA")

match_players

match_lineup[[1]] <- bosline %>% rbind(cleline) %>%
  filter(str_detect(position, "Starter")) %>%
  select(position, player.id, status) %>%
  mutate(substitution.team.id = NA) %>%
  right_join(match_players, by = "player.id")

for (i in 2:n_stints[[1]]){
  print(i)
  match_lineup[[i]] <- match_lineup[[i-1]] %>% select(-substitution.team.id) %>% filter(status != "NA")
  match_lineup[[i]] <- match_lineup[[i]] %>% left_join(a2[(i-1),]$data[[1]], by = c("player.id" = "substitution.outgoingPlayer.id"))
  match_lineup[[i]] <- match_lineup[[i]] %>% mutate(player.id = ifelse(is.na(substitution.incomingPlayer.id), player.id, substitution.incomingPlayer.id)) %>%
    select(-substitution.incomingPlayer.id) %>%
    right_join(match_players, by = "player.id") %>%
    mutate(status = ifelse(is.na(status),0,status))
 
}


inital_stint <- data.frame(playStatus.quarter = 1, playStatus.secondsElapsed = 0, stint = 0) 
a3 <- rbind(inital_stint,select(a2, playStatus.quarter, playStatus.secondsElapsed,stint)) %>%
  mutate(data = match_lineup)

str(a3, list.len = 4)

head(a3$data[1])

str(match_lineup[[2]])

match_lineup[[1]]
match_lineup[[2]]
match_lineup[[3]]
match_lineup[[4]]
match_lineup[[5]]
match_lineup[[1]] == match_lineup[[2]]