#CMSA Lecture 21 code
#data pooled from the Chicago marathon elite field
#building a model to quantify the effect of super shoes on elite marathon performance
#Level one variable: variables measured at the most frequently occurring observational unit (each individual’s race)
##Weather (wind, temperature), athlete age, year, super shoe era indicator
#Level two variables: variables measured runner level
##Division (men vs. women), country


library(tidyverse)
library(gt)

elite = read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/chicago_marathon_elite.csv") |>
  dplyr::select(Name, Year, Division = Gender, Age, Country, time_min, super_era_elite, temp9, Gender_place, Overall)
head(elite) |> gt()

#bar chart of how many times a runner has ran the chicago marathon
elite |> count(Name) |> #first line gives how many time each runner has done the marathon
  ggplot(aes(x=n))+
  geom_bar(fill = "blue", col = "black")+
  theme_classic()+
  labs(x="Number of times competiting in Chicago elite field",
       y = "Number of athletes")+
  theme(axis.title = element_text(size = 15), 
        legend.position = "none")

#trajectory of marathon times by division of Mens and womens
#We notice that the mens times are lower than womens but both divisions 
#typically increase in time with an increase in temp 
elite |>
  ggplot(aes(x = temp9, y=time_min, color = Division))+
  geom_point(alpha=0.5)+
  geom_smooth()+
  facet_wrap(~Division, scales = "free_x")+
  scale_color_manual(values = c("darkorange", "royalblue"))+
  labs(x="Temperature at 9am on raceday", y= "Finish time (min)")+
  theme(legend.position = "none")

#fitting a multiple linear regression
pooled_model = lm(time_min ~ Year + super_era_elite + Division + temp9, data = elite)

library(gtsummary)
tbl_regression(pooled_model)
#checking residuals of multiple linear regression model
broom::augment(pooled_model) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Fitted value", y = "Residual")+
  geom_hline(linetype = "dashed", color = "red", yintercept = 0)

#fitting a linear mixed model with Name as as the varying intercept and random effect
library(lme4)

mod = lmer(time_min ~ super_era_elite + Year + Age + temp9 + Division +
              (1 | Name), data = elite |>  mutate(Year = Year - 2017))

tbl_regression(mod)

#computing the interclass correlation coefficient
#closer to 0, the responses are more independent
#closer to 1, repeated obs provide no new observations
#Name is at 0.66 so if we fit a normal regression model we would be breaking independence
VarCorr(mod) |>
  as_tibble() |>
  mutate(icc = vcov / sum(vcov)) |>
  dplyr::select(grp, icc) |>
  gt()

#fitting random slopes
#data actually cant fit varying slopes
#if we only have one race we cant estimate a slope
mod2 = lmer(time_min ~ super_era_elite + Year + Age + temp9 + Division +
              (1 + super_era_elite | Name), data = elite |>  mutate(Year = Year - 2017))

library(merTools)

#estimating each runners random intercept
#every vertical line is a runner and the dots are their estimated random effects
runner_effects = REsim(mod)
plotREsim(runner_effects)

#SE of random effects
#uncertainty around each random intercept is strongly correlated with number of races completed
races = elite |> count(Name)

runner_effects |>
  left_join(races, by = c("groupID" = "Name")) |>
  ggplot(aes(x=as.factor(n), y=sd))+
  geom_boxplot()+
  labs(x="Races")

#best and worst runners by effects
runner_effects |>
  as_tibble() |>
  arrange(desc(mean)) |>
  slice(1:5, (n() - 4):n()) %>%
  ggplot(aes(x = reorder(groupID, mean))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - 2 * sd,
                    ymax = mean + 2 * sd)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "red") +
  coord_flip() +
  theme_bw(base_size = 20)+
  labs(x = "", y = "Effect")

#checking diagnostics (linearity, normality, etc)
qqnorm(ranef(mod)$Name[,1])

#are random effects needed?
mod_lm <- lm(time_min ~ super_era_elite + Year + Age + temp9 + Division, data = elite |> mutate(Year = Year - 2017))

mod_lmer <- lmer(time_min ~ super_era_elite + Year + Age + temp9 + Division +
                   (1 | Name), data = elite |> mutate(Year = Year - 2017),
                 REML = FALSE)

anova(mod_lmer, mod_lm)

#predict the probability for the fastest man in the pre-super shoe and 
#breaking 2 hours with super shoes
#elite |> arrange(time_min) |> filter(Year < 2017) |> slice(1)

runner_effect <- ranef(mod)$Name["Dennis Kimetto", "(Intercept)"]

dennis <- elite[elite$Name == "Dennis Kimetto", ]

new_pre <- data.frame(
  super_era_elite = "No",
  Age = dennis$Age,
  Division = "M",
  temp9 = dennis$temp9,
  Year = -4
)

new_post <- new_pre
new_post$super_era_elite <- "Yes"
new_post$Year <- 6

pred_pre_fixed <- predict(mod, newdata = new_pre, re.form = NA)
pred_post_fixed <- predict(mod, newdata = new_post, re.form = NA)

pred_pre <- pred_pre_fixed + runner_effect
pred_post <- pred_post_fixed + runner_effect

resid_sd <- sigma(mod) # residual standard deviation of the errors

prob_pre <- pnorm(120, mean = pred_pre, sd = resid_sd)
prob_post <- pnorm(120, mean = pred_post, sd = resid_sd)
#prob_post/prob_pre