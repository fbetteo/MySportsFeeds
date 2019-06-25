# Regression

rm(list = ls())
library(tidyverse)

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
cv <- glmnet::cv.glmnet(x, y, alpha = 1, weights = matrix_model$amount_possessions )
# Display the best lambda value
cv$lambda.min


# Fit the final model on the training data
model <- glmnet::glmnet(x, y, alpha = 1, lambda = cv$lambda.min, weights = matrix_model$amount_possessions)
# Display regression coefficients
coef(model)

model$beta
head(x.test)
# Make predictions on the test data
x.test <- model.matrix(dif_per_100_possessions ~., matrix_model[,-c(1:3)])[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = caret::RMSE(predictions, matrix_model$dif_per_100_possessions),
  Rsquare = caret::R2(predictions, matrix_model$dif_per_100_possessions)
)


# Alpha = 0
bb <- coef(model)@x %>% 
  as.matrix() %>%
  as_tibble() %>%
  add_column(playerid = coef(model)@Dimnames[[1]]) %>%
  rename(coef = V1)%>%
  arrange(desc(coef)) %>%
  mutate(playerid = str_remove(playerid, "x"), playerid = as.integer(playerid)) 
str(bb)
str(coef(model))
coef(model)
coef(model)@Dimnames[[1]][1:3]
# Alpha = 1
bb <- coef(model)@x %>% 
  as.matrix() %>%
  as_tibble() %>%
  #add_column(playerid = coef(model)@Dimnames[[1]][coef(model)@i]) %>%
  add_column(playerid = c("intercept",coef(model)@Dimnames[[1]][coef(model)@i[-1]+1])) %>%
  rename(coef = V1)%>%
  arrange(desc(coef)) %>%
  mutate(playerid = str_remove(playerid, "x"), playerid = as.integer(playerid)) 


list2 <- lineups3 %>%
  bind_rows() %>%
  distinct(player.id, player.firstName, player.lastName, team.abbreviation) %>%
  rename(playerid = player.id) %>%
  group_by(playerid) %>% arrange(playerid) %>%
  filter(row_number() == n()) %>% # para quedarse con el ultimo equipo 
  ungroup() %>%
  arrange(team.abbreviation)
  


cc <- bb %>%
  inner_join(., list2, by = "playerid")


dd <- cc %>% group_by(team.abbreviation) %>%
  summarise(coef_total =sum(coef)) %>%
  arrange(coef_total)

### DA COMO EL ORTO
## VER COMO SACAR LOS QUE JUEGAN MENOS DE X MINUTOS

View(df_model[[1]])
