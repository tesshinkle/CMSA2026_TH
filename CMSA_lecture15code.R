#CMSA Lecture 15 code
#poisson regression

library(tidyverse)
theme_set(theme_bw())
scoring = read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/nhl_offense_2021.csv")
glimpse(scoring)

#distribution of number of goals, slightly skewed
scoring |> 
  count(goals) |> 
  ggplot(aes(goals, n)) +
  geom_col(width = 0.5) 

#distribution of number of goals separated by home and away team
scoring |> 
  count(goals, is_home) |> 
  mutate(is_home = factor(is_home)) |> 
  ggplot(aes(goals, n, fill = is_home, group = is_home)) +
  geom_col(position = "dodge", width = 0.5, col = "black") +
  labs(fill = "Home advantage?")

#relationship between shot distance and number of goals scored
a = scoring |> 
  mutate(goals_group = case_when(goals <2 ~ "0-1", #grouping goals
                                 goals >= 2 & goals <=3 ~ "2-3", 
                                 goals == 4 ~ "4", 
                                 goals > 4 ~ "5+")) |>
  ggplot(aes(x=avg_dist, color = goals_group))+
  geom_density()+ #density plot
  theme(legend.position = "bottom")

b = scoring |> 
  ggplot(aes(x=avg_dist, y = goals))+
  geom_point(position = "jitter", alpha=0.5)+ #scatterplot
  geom_smooth()

library(patchwork)
a+b

#none of the division variables are significant in comparison to the reference division atlantic
goals_poisson = glm(goals ~ division + is_home + avg_dist, family = "poisson", data = scoring)

summary(goals_poisson)
table(scoring$division) #reference level is atlantic

#scoring = scoring |> mutate(division = fct_relevel(division, "Central")) #divisions still aren't significant
#used to change the reference level of the variable division

library(broom)
tidy(goals_poisson)

tidy(goals_poisson, exponentiate = TRUE, conf.int = TRUE) #fixes the estimates

#Deviance Test
fit_league = glm(goals ~ division + is_home + avg_dist, family = "poisson", data = scoring)
fit_simple = glm(goals ~ is_home + avg_dist, family = "poisson", data = scoring)

anova(fit_simple, fit_league, test = "Chisq") #comparing two models
#the reduced model is better and the variable division does not provide evidence toward goals

library(regressinator)
#checking assumptions: linearity between predictor and the log of the outcome
scoring |>
  bin_by_quantile(avg_dist, breaks = 10) |>
  summarize(
    mean_avg_dist = mean(avg_dist),
    log_goals = empirical_link(
      goals,
      family = poisson(link = "log"),
      n = n()
    )) |>
  ggplot(aes(x = mean_avg_dist, y = log_goals)) +
  geom_point()+
  geom_smooth() +
  labs(x = "Avg shot distance", y = "Log goals")

#checking residuals
augment_quantile(goals_poisson) %>%
  ggplot(aes(x=avg_dist, y = .quantile.resid))+
  geom_point(alpha=0.7)+
  theme_bw(base_size = 22)+
  labs(x="Shot distance", y="Randomized quantile residual")+
  geom_smooth()

#offset
goals_offset = glm(goals ~ division + is_home + avg_dist, 
                    offset = log(shots), # include with coefficient 1
                    data = scoring, family = poisson(link = "log"))
summary(goals_offset)
tidy(goals_offset, exponentiate = T)

#checking overdispersion
var(scoring$goals)
mean(scoring$goals)

#negative binomial regression
goals_nb <- MASS::glm.nb(goals ~ division + is_home + avg_dist, data = scoring)
summary(goals_nb)
