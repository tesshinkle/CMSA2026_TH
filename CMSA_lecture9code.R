#CMSA Lecture 9 Code
#Linear Regression notes
library(tidyverse)
theme_set(theme_bw())
nfl_teams_data = read_csv("http://www.stat.cmu.edu/cmsac/sure/2021/materials/data/regression_projects/nfl_team_season_summary.csv") # throwback using CMSACamp 2021/2022 data

glimpse(nfl_teams_data)

nfl_teams_data = nfl_teams_data |>
  mutate(score_diff = points_scored - points_allowed)
#histogram to check distribution, normal distribution is shown
nfl_teams_data |>
  ggplot(aes(x = score_diff)) +
  geom_histogram(color = "black", 
                 fill = "darkblue",
                 alpha = 0.3) +
  labs(x = "Score differential", y = "Count")

#scatterplot to visualize relationship
nfl_teams_data |>
  ggplot(aes(x = offense_ave_epa_pass,
             y = score_diff)) +
  geom_point(alpha = 0.5) +
  labs(x = "EPA gained per pass attempt",
       y = "Score differential")

#linear regression model 1
simple_lm = lm(score_diff ~ offense_ave_epa_pass, data = nfl_teams_data) 
summary(simple_lm)

confint(simple_lm)

library(broom)
require(gt)
glance(simple_lm)

tidy(simple_lm) |>
  gt()
tidy(simple_lm, conf.int = TRUE, conf.level = 0.95) #adds conf interval columns

var(predict(simple_lm)) / var(nfl_teams_data$score_diff) #gives the r-squared
#variance in prediction of Y divided by the variance of Y

cor(nfl_teams_data$offense_ave_epa_pass, nfl_teams_data$score_diff)^2 #also gives correlation

head(predict(simple_lm)) # also can input new data to predict()
head(fitted(simple_lm))

nfl_teams_data |>
  mutate(pred = predict(simple_lm)) |> 
  ggplot(aes(x = pred, y = score_diff)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed",
              color = "blue",
              linewidth = 2)

nfl_teams_data = simple_lm |> 
  augment(nfl_teams_data) 

nfl_teams_data |>
  ggplot(aes(x = .fitted, y = score_diff)) + 
  geom_point(alpha = 0.5, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "blue", 
              linetype = "dashed", linewidth = 2)

#looking at assumptions
#errors have normality, linearity, homoscedasticity, and errors are independent

#residual plot
#assesses linearity and homoscedasticity
nfl_teams_data |>
  ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point(alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "red2", linewidth = 1.5) +
  # plot the residual mean
  geom_smooth(se = TRUE, fill = "skyblue2")

#checking normality, Q-Q plot
nfl_teams_data |>
  ggplot(aes(sample = .std.resid)) +
  geom_qq_line(distribution = stats::qnorm) +
  geom_qq(distribution = stats::qnorm) 

#categorical predictors
nfl_teams_subset = nfl_teams_data |>
  filter(team %in% c("PIT", "SEA", "CHI"))

team_lm = lm(score_diff ~ team, data = nfl_teams_subset)
summary(team_lm)
tidy(team_lm, conf.int = TRUE)

nfl_teams_subset |>
  mutate(team_pred = predict(team_lm)) |> 
  ggplot(aes(x = score_diff, y = team_pred)) +
  geom_point(size = 1.5, alpha = 0.5) +
  facet_wrap(~ team, ncol = 3) +
  labs(x = "Actual score differential", 
       y = "Predicted score differential")

nfl_teams_subset |> 
  ggplot(aes(x = offense_ave_epa_pass, y = score_diff, color = team)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 2) +
  ggthemes::scale_color_colorblind()

#interaction in regression
interaction_lm = lm(score_diff ~ offense_ave_epa_pass * team, 
                     data = nfl_teams_subset)
tidy(interaction_lm)
summary(interaction_lm)

multiple_lm = lm(score_diff ~ offense_ave_epa_pass + defense_ave_epa_pass, data = nfl_teams_subset)
tidy(multiple_lm)
summary(multiple_lm)

glance(multiple_lm) # notice the diff. between unadj. and adj. R-squared

# predict(simple_lm, interval = "confidence")
lm_plot = nfl_teams_subset |>
  ggplot(aes(x = offense_ave_epa_pass, y = score_diff)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")
lm_plot

# predict(simple_lm, interval = "prediction")
lm_plot +
  geom_ribbon(
    data = augment(simple_lm, interval = "prediction"),
    aes(ymin = .lower, ymax = .upper),
    color = "red", fill = NA
  )
