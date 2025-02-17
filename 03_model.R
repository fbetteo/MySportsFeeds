# Regression

rm(list = ls())
library(tidyverse)
source("functions.R")

matrix_model <- readRDS(here::here("data","working","matrix_model.rds"))
lineups3 <- readRDS(here::here("data","working","lineups3.rds"))
View(head(matrix_model))

# Ridge

x <- matrix_model %>% select(-c(stint, amount_possessions, diferential)) %>%
  janitor::clean_names() %>%
  model.matrix(dif_per_100_possessions ~., .) %>%
  '['(,-1)
# Outcome variable
y <- matrix_model$dif_per_100_possessions
str(y)
str(x)

# Find the best lambda using cross-validation
set.seed(123) 
cv <- glmnet::cv.glmnet(x, y, alpha = 0, weights = matrix_model$amount_possessions, standardize = FALSE, intercept = FALSE )
# Display the best lambda value
cv$lambda.min


# Fit the final model on the training data
model <- glmnet::glmnet(x, y, alpha = 0, lambda = cv$lambda.min, weights = matrix_model$amount_possessions, standardize = FALSE, intercept = FALSE)
saveRDS(model, "data/working/model.RDS" )
# Display regression coefficients
coef(model)

model$beta

# Make predictions on the test data
x.test <- model.matrix(dif_per_100_possessions ~., matrix_model[,-c(1:3)])[,-1]
head(x.test)
predictions <- model %>% predict(x.test, s = cv$lambda.min) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = caret::RMSE(predictions, matrix_model$dif_per_100_possessions),
  Rsquare = caret::R2(predictions, matrix_model$dif_per_100_possessions)
)

# Desvios estandar
se_of_ridge = ridge_se(x,y,predictions,model) 

var_cov_ridge  = ridge_var_cov(x,y,predictions,model)
saveRDS(var_cov_ridge, "data/working/var_cov_ridge.RDS" )

# Alpha = 0
bb <- coef(model)@x %>% 
  as.matrix() %>%
  as_tibble() %>%
  add_column(playerid = coef(model)@Dimnames[[1]][-1]) %>% # if we have intercept in the model remove the [-1]
  rename(coef = V1)%>%
  add_column(sd = se_of_ridge) %>%
  arrange(desc(coef)) %>%
  mutate(playerid = str_remove(playerid, "x"), playerid = as.integer(playerid)) 
str(bb)
str(coef(model))
coef(model)
coef(model)@Dimnames[[1]][1:3]
# Alpha = 1
# bb <- coef(model)@x %>% 
#   as.matrix() %>%
#   as_tibble() %>%
#   #add_column(playerid = coef(model)@Dimnames[[1]][coef(model)@i]) %>%
#   add_column(playerid = c("intercept",coef(model)@Dimnames[[1]][coef(model)@i[-1]+1])) %>%
#   rename(coef = V1)%>%
#   arrange(desc(coef)) %>%
#   mutate(playerid = str_remove(playerid, "x"), playerid = as.integer(playerid)) 


list2 <- lineups3 %>%
  bind_rows() %>%
  distinct(player.id, player.firstName, player.lastName, team.abbreviation) %>%
  rename(playerid = player.id) %>%
  group_by(playerid) %>% arrange(playerid) %>%
  filter(row_number() == n()) %>% # para quedarse con el ultimo equipo 
  ungroup() %>%
  arrange(team.abbreviation)
  


player_ranking <- bb %>%
  inner_join(., list2, by = "playerid") %>%
  select(playerid, player.firstName, player.lastName, team.abbreviation, coef, sd)
saveRDS(player_ranking, "output/tables/player_ranking.rds")

average_possessions = readRDS(here::here("data","working","average_possessions.rds")) %>%
  mutate(player = as.integer(player))

team_ranking <- player_ranking %>%
  left_join(average_possessions ,by = c("playerid" = "player")) %>% 
  mutate(coef_times_avg_poss = coef * average_pos) %>%
  group_by(team.abbreviation) %>%
  summarise(coef_total =sum(coef_times_avg_poss)) %>%
  arrange(coef_total)
saveRDS(team_ranking, here::here("output/tables/team_ranking.rds"))



