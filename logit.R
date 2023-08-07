library(ggplot2)
library(dplyr)
library(tidymodels)
library(glmnet)

# Following the datacamp guide for logistic regression in R 
# https://www.datacamp.com/tutorial/logistic-regression-R

source("cleaning.R")
load_clean_data()

# Determine the hyperparameters -> mixture and penalty.
# Below gives you 0.0000000001  0.333
log_reg <- logistic_reg(mixture = tune(), penalty = tune(), engine = "glmnet")

# define grid search for hyperparameters
grid <- grid_regular(mixture(), penalty(), levels = c(mixture = 4, penalty = 3))

log_reg_wf <- workflow() %>% 
  add_model(log_reg) %>%
  add_formula(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked)

# define resampling method for grid search. 
folds <- vfold_cv(train, v = 5)

log_reg_tuned <- tune_grid(
  log_reg_wf,
  resamples = folds,
  grid = grid,
  control = control_grid(save_pred = T)
)

select_best(log_reg_tuned, metric = "roc_auc")

# Create model with the above hyperparameters
log_reg <- logistic_reg(penalty = 0.0000000001, mixture = 0.333) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train)

# largest coefficients are Sexmale (-2.69) and Pclass3 (-2.11)
pred_class <- predict(log_reg, new_data = test, type = "class")

results <- cbind(test$PassengerId, pred_class)
names(results) <- c("PassengerId", "Survived")
results$Survived <- as.numeric(as.character(results$Survived))

write.csv(results, "data/results.csv", row.names=F)
