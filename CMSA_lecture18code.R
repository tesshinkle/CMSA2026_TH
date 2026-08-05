#CMSA Lecture 18 code
#Boosting and Bagging

#Bagging----
#working with 2022 men's fifa world cup data
library(tidyverse)
theme_set(theme_light())

mwc_shots = read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/mwc_shots.csv")
head(mwc_shots)

library(ranger) #create an untuned random forest model
set.seed(0630)
prop.table(table(mwc_shots$goal))
mwc_shots_rf = ranger(goal ~ location_x + location_y + angle_to_goal + play_pattern_name 
                       + avevelocity + angle_to_keeper + dist_to_keeper + density_incone,
                       num.trees = 500, importance = "impurity", data = mwc_shots, 
                      #num.trees is either 500 or 1000 typically
                       probability = TRUE)
mwc_shots_rf #shows brier score of 0.083

library(vip)
vip(mwc_shots_rf) #gives variable importance percentage bar plot 

#mwc_shots_rf$prediction.error #another way to get the brier score
mwc_shots |> #also get brier score
  mutate(goal_prob = mwc_shots_rf$predictions[,"1"]) |> 
  summarize(brier_score = mean((goal_prob - goal)^2))

#gives the AUC and plots the ROC curve (Rate of coverage)
library(pROC)
mwc_shots_rfroc = mwc_shots |> 
  mutate(goal_prob = mwc_shots_rf$predictions[,"1"]) |> 
  roc(goal, goal_prob)

mwc_shots_rfroc$auc #gives AUC, the closer to one the better

rf_roc = tibble(threshold = mwc_shots_rfroc$thresholds,
                 specificity = mwc_shots_rfroc$specificities,#a non-goal is detected as a non-goal
                 sensitivity = mwc_shots_rfroc$sensitivities)#a goal is actually detected as a goal 

rf_roc |> 
  ggplot(aes(x = 1 - specificity , y = sensitivity)) + #1-specificity (false pos. rate)
  geom_path() +
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed")
#best possible ROC curve is a right angle


#Boosting----
library(xgboost)

x_train = mwc_shots |> 
  select(location_x, location_y, angle_to_goal, play_pattern_name,
         avevelocity, angle_to_keeper, dist_to_keeper, density_incone) 

dtrain = xgb.DMatrix(data = model.matrix(~ . - 1, data = x_train), #data of all predictors, -1 removes intercept
                      label = as.numeric(mwc_shots$goal == 1))

# Define a grid of hyperparameters
#define all combinations of paramenters we are interested in
#gives 126 combinations
xg_grid = expand.grid(
  nrounds = seq(20, 150, 10),#20 to 150 going up by 10 each time
  max_depth = c(2, 3, 4),
  eta = c(0.01, 0.05, 0.1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

library(purrr)

cv_results <- map(1:nrow(xg_grid), function(i) {
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc", # cv metric 
    max_depth = xg_grid$max_depth[i],
    eta = xg_grid$eta[i],
    gamma = xg_grid$gamma[i],
    colsample_bytree = xg_grid$colsample_bytree[i],
    min_child_weight = xg_grid$min_child_weight[i],
    subsample = xg_grid$subsample[i]
  )
  # Run CV for the current parameter set
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nfold = 5,
    # how many boosting rounds
    nrounds = xg_grid$nrounds[i],
    maximize = TRUE, # want to maximize cv metric (since auc) 
    verbose = FALSE,
  )
  
  return(list(params = params,
              nrounds = xg_grid$nrounds[i],
              best_auc = max(cv$evaluation_log$test_auc_mean)))
})

#best AUC is 0.807
best_result = cv_results[[which.max(purrr::map_dbl(cv_results, "best_auc"))]]
print(best_result)

#fitting xgboost model
xg_fit = xgb.train(
  params = best_result$params,
  data = dtrain, # ideally, we would use new data (completely unseen)
  nrounds = best_result$nrounds,
  verbose = FALSE
)

library(vip)
xg_fit |> 
  vip()

mwc_shots |> 
  mutate(goal_prob = predict(xg_fit, newdata = dtrain, type = "response")) |> 
  summarize(briar_score = mean((goal_prob - goal)^2))

mwc_shots_xgroc <- mwc_shots |> 
  mutate(goal_prob = predict(xg_fit, newdata = dtrain, type = "response")) |> 
  roc(goal, goal_prob)

mwc_shots_xgroc$auc #0.887

#ROC curve plot where it shows that xgboost is a better fit than random forest model
tibble(threshold = c(mwc_shots_xgroc$thresholds),
       specificity = mwc_shots_xgroc$specificities,
       sensitivity = mwc_shots_xgroc$sensitivities,
       method = "xgboost") |> 
  rbind(mutate(rf_roc, method = "random forest")) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = method)) +
  geom_path() +
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed") 

