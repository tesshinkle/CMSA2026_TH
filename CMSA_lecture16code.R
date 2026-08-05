#Chapter 16 lecture code
#non-parametric regression
#generalized additive models

library(tidyverse)
require(data.table)

#fread("https://raw.githubusercontent.com/36-SURE/2026/main/data/savant.csv") #supposedly faster read-in

savant = read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/savant.csv")
batted_balls = savant |> 
  filter(type == "X") |> 
  mutate(is_hr = as.numeric(events == "home_run")) |> #returns 1 or 0 for if event is a home run
  filter(!is.na(launch_angle), !is.na(launch_speed), !is.na(is_hr))#filtering out na's 
# glimpse(batted_balls)

#scatterplot looking at bat launch angle and launch speed if it results in a homerun or not
batted_balls |> 
  ggplot(aes(x = launch_speed, 
             y = launch_angle, 
             color = as.factor(is_hr))) +
  geom_point(alpha = 0.2)

#density plot/heat map of launch angle and speed and proportion of homeruns
batted_balls |> 
  group_by(
    launch_angle_bucket = round(launch_angle * 2, -1) / 2,
    launch_speed_bucket = round(launch_speed * 2, -1) / 2
  ) |> 
  summarize(hr = sum(is_hr == 1), #gives true or false of home runs and then sums the trues
            n = n()) |> 
  ungroup() |> 
  mutate(pct_hr = hr / n) |> #gives the percent of home runs
  ggplot(aes(x = launch_speed_bucket, 
             y = launch_angle_bucket, 
             fill = pct_hr)) +
  geom_tile() + #creates the heat map, similar to geom_rect()
  scale_fill_gradient2(labels = scales::percent_format(),
                       low = "blue", 
                       high = "red", 
                       midpoint = 0.2)

library(mgcv)
#REML stands for Restricted Maximum Likelihood
hr_gam = gam(is_hr ~ s(launch_speed) + s(launch_angle),
              family = binomial,
              method = "REML", # more stable solution than default
              data = batted_balls)

library(broom)
tidy(hr_gam)

tidy(hr_gam, parametric = TRUE) #gives the intercept values

library(gratia)
#launch speed has a linear partial effect (the smooth function of launch speed fit a linear function)
#launch angle has a nonlinear partial effect. there is a peack around 45 degrees. 
#the log odds of a home run increases with a high launch angle
draw(hr_gam)

#model comparison: GAM vs GLM----
set.seed(25)
N_FOLDS = 10
games_folds = batted_balls |> 
  distinct(game_date, home_team, away_team) |> #gives the home and away team shown in the data
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

#Joining the games_folds into the main data
batted_balls = batted_balls |> 
  left_join(games_folds, by = join_by(game_date, home_team, away_team)) 

hr_cv = function(x) {
  hr_train = batted_balls |> filter(fold != x)
  hr_test = batted_balls |> filter(fold == x)
  
  gam_fit = gam(is_hr ~ s(launch_speed) + s(launch_angle),
                 family = binomial,
                 method = "REML",
                 data = hr_train)
  logit_fit = glm(is_hr ~ launch_speed + launch_angle, 
                   family = binomial, data = hr_train)
  
  hr_out = tibble(
    gam_pred = predict(gam_fit, newdata = hr_test, type = "response"),
    logit_pred = predict(logit_fit, newdata = hr_test, type = "response"),
    test_actual = hr_test$is_hr, #get the actual home run status
    test_fold = x
  )
  return(hr_out)  
}

hr_preds = map(1:N_FOLDS, hr_cv) |> 
  list_rbind()

#calculating test accuracy, GAM model has an accuracy of 0.973 
#while GLM had an accuracy of 0.959
hr_preds |> 
  pivot_longer(cols = gam_pred:logit_pred, 
               names_to = "model", 
               values_to = "test_pred") |> 
  mutate(test_pred_class = round(test_pred)) |> 
  group_by(model, test_fold) |>
  summarize(accuracy = mean(test_pred_class == test_actual)) |> 
  group_by(model) |> 
  summarize(cv_accuracy = mean(accuracy),
            se_accuracy = sd(accuracy) / sqrt(N_FOLDS))
