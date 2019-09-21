## Comparo resultados de los playoffs
## Vs prediccion segun ranking.

library(tidyverse)

playoff_test = readRDS("data/working/playoff_test.rds") %>%
  mutate(winner = ifelse(home_points > away_points, home_team, away_team),
         predicted = ifelse(home_ranking_score > away_ranking_score, home_team_ranking, away_team_ranking),
         fit = ifelse(winner == predicted, 1, 0)) 


actual_winner = playoff_test %>%
  group_by(id, winner) %>%
  summarise(avanza = n()) %>%
  filter(avanza == max(avanza))

predicted_winner = playoff_test %>%
  group_by(id, predicted) %>%
  summarise(avanza = n()) %>%
  filter(avanza == max(avanza)) %>%
  select(-avanza)

fit_table = aa %>%
  left_join(bb, by = "id") %>%
  mutate(fit = ifelse(winner == predicted, 1, 0))


table(fit_table$fit)

saveRDS(fit_table, "output/tables/fit_table.rds")
