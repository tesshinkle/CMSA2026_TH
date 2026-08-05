#CMSA Lecture 11 Code
#supervized learning: regularization
#ridge vs lasso vs elastic net
#ridge doesn't allow coefficents to be shrunken to zero
#lasso allows for coefficients to be shrunken to zero
#lasso can also handle the p > n case
#elastic net is considered best of both worlds


library(tidyverse)
theme_set(theme_bw())

nfl_teams_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/regression_examples/nfl_team_season_summary.csv")
nfl_model_data <- nfl_teams_data %>%
  mutate(score_diff = points_scored - points_allowed) %>%
  filter(season >= 2006) %>% # Only use rows with air yards
  dplyr::select(-wins, -losses, -ties, -points_scored, -points_allowed, -season, -team)

library(glmnet)

#creating a matrix of predictor variables
model_x <- nfl_model_data |> 
  dplyr::select(-score_diff) |>
  as.matrix()

#pulling out the response variable 
model_y <- nfl_model_data |> 
  pull(score_diff)

#initial linear regression, modeling the response variable with all predictor variable
init_reg_fit <- lm(score_diff ~ ., data = nfl_model_data)

library(broom)
init_reg_fit |> 
  tidy() |> 
  mutate(term = fct_reorder(term, estimate)) |> 
  ggplot(aes(x = estimate, y = term, 
             fill = estimate > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) #manually changing colors

#ridge regression, specified with aplha = 0
init_ridge_fit <- glmnet(model_x, model_y, alpha = 0)
plot(init_ridge_fit, xvar = "lambda")

fit_ridge_cv <- cv.glmnet(model_x, model_y, alpha = 0)
plot(fit_ridge_cv)

#same ridge regression but tidy
tidy_ridge_coef <- tidy(fit_ridge_cv$glmnet.fit)
tidy_ridge_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fit_ridge_cv$lambda.min) +
  geom_vline(xintercept = fit_ridge_cv$lambda.1se, 
             #red line at 1 stand. error which is typically the best lambda value
             linetype = "dashed", color = "red")+
  labs(y = "Coefficients")

tidy_ridge_cv <- tidy(fit_ridge_cv)
tidy_ridge_cv |> 
  ggplot(aes(x = lambda, y = estimate)) +
  geom_line() + 
  scale_x_log10() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2) +
  geom_vline(xintercept = fit_ridge_cv$lambda.min) +
  geom_vline(xintercept = fit_ridge_cv$lambda.1se,
             linetype = "dashed", color = "red")+
  labs(y = "MSE")

#ridge_final <- glmnet(
#  model_x, model_y, alpha = 0,
#  lambda = fit_ridge_cv$lambda.1se,
#)
#library(vip)
#ridge_final |>
#  vip()

tidy_ridge_coef |>
  filter(lambda == fit_ridge_cv$lambda.1se) |>
  mutate(term = fct_reorder(term, estimate)) |>
  ggplot(aes(x = estimate, y = term, 
             fill = estimate > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue"))

#lasso regression, denoted by alpha = 1
fit_lasso_cv <- cv.glmnet(model_x, model_y, 
                          alpha = 1)
tidy_lasso_coef <- tidy(fit_lasso_cv$glmnet.fit)
tidy_lasso_coef |> 
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = fit_lasso_cv$lambda.min) +
  geom_vline(xintercept = fit_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red")

tidy_lasso_cv <- tidy(fit_lasso_cv)
tidy_lasso_cv |>
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fit_lasso_cv$lambda.min) +
  geom_vline(xintercept = fit_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10()

# this will only print out non-zero coefficient estimates
# tidy_lasso_coef |>
#   filter(lambda == fit_lasso_cv$lambda.1se)

lasso_final <- glmnet(
  model_x, model_y, 
  alpha = 1,
  lambda = fit_lasso_cv$lambda.1se,
)
library(vip)
lasso_final |> 
  vi() |> 
  mutate(Variable = fct_reorder(Variable, Importance)) |>
  ggplot(aes(x = Importance, y = Variable, 
             fill = Importance > 0)) +
  geom_col(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  labs(x = "estimate", y = NULL)

set.seed(100)
fold_id <- sample(rep(1:10, length.out = nrow(model_x)))

cv_enet_25 <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 0.25)
cv_enet_50 <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 0.5)
cv_ridge <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 0)
cv_lasso <- cv.glmnet(model_x, model_y, foldid = fold_id, alpha = 1)

which.min(c(min(cv_enet_25$cvm), min(cv_enet_50$cvm), min(cv_ridge$cvm), min(cv_lasso$cvm)))

cv_enet_50 |> 
  tidy() |> 
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cv_enet_50$lambda.min) +
  geom_vline(xintercept = cv_enet_50$lambda.1se, 
             linetype = "dashed", 
             color = "red") +
  scale_x_log10()

set.seed(101)
N_FOLDS <- 5
nfl_model_data <- nfl_model_data |>
  mutate(test_fold = sample(rep(1:N_FOLDS, length.out = n())))

get_test_pred <- function(x) {
  test_data <- nfl_model_data |> filter(test_fold == x)                     # get test and training data
  train_data <- nfl_model_data |> filter(test_fold != x)
  test_x <- as.matrix(select(test_data, -score_diff, -test_fold))           # get test and training matrices
  train_x <- as.matrix(select(train_data, -score_diff, -test_fold))
  
  lm_fit <- lm(score_diff ~ ., data = select(train_data, -test_fold))       # fit models to training data
  ridge_fit <- cv.glmnet(train_x, train_data$score_diff, alpha = 0)
  lasso_fit <- cv.glmnet(train_x, train_data$score_diff, alpha = 1)
  enet_fit <- cv.glmnet(train_x, train_data$score_diff, alpha = 0.5)
  
  tibble(lm_pred = predict(lm_fit, newdata = test_data),              # return test results
         ridge_pred = as.numeric(predict(ridge_fit, newx = test_x)),
         lasso_pred = as.numeric(predict(lasso_fit, newx = test_x)),
         enet_pred = as.numeric(predict(enet_fit, newx = test_x)),
         test_actual = test_data$score_diff,
         test_fold = x)
}

test_pred_all <- map(1:N_FOLDS, get_test_pred) |> 
  bind_rows()

test_pred_all |>
  pivot_longer(lm_pred:enet_pred, 
               names_to = "type", 
               values_to = "test_pred") |>
  group_by(type, test_fold) |>
  summarize(
    rmse = sqrt(mean((test_actual - test_pred)^2))
  ) |> 
  ggplot(aes(x = type, y = rmse)) + 
  geom_point(size = 4) +
  stat_summary(fun = mean, geom = "point", 
               color = "red", size = 4) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               color = "red", width = 0.2)
