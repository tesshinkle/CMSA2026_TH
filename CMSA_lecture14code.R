#CMSA Lecture 14 code
#Logistic Regression
#binary respopnse variable

library(tidyverse)
theme_set(theme_light())

cricket = read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/cricket_asia_cup.csv") |>
  janitor::clean_names() |>
  mutate(selection = as_factor(if_else(selection == 1, "Bowling" , "Batting")), #naming the binary variables
         result = as_factor(if_else(result == 1, "Win", "Loss")))

cricket_logit = glm(result ~ selection + fours + sixes +
                       extras_scored + given_extras, data = cricket, family = binomial)
summary(cricket_logit)

#likelihood ratio test
require(car)
Anova(cricket_logit,type="II",test="LR")

exp(confint(cricket_logit)) # 95% CI for the odds ratio

#a more simple regression
cricket_logit2 = glm(result ~ selection + fours + sixes, data = cricket, family = binomial)
summary(cricket_logit2)

library(broom)
tidy(cricket_logit) #does the same function as summary
tidy(cricket_logit, exponentiate = TRUE, conf.int = TRUE) #std. error still on log scale but estimate not

library(gtsummary)
tbl_regression(cricket_logit, exponentiate = TRUE) #gives the 95% OR CI

library(regressinator)
rate_by_fours = cricket |>
  mutate(win = (result == "Win")) |>
  bin_by_quantile(fours, breaks = 10) |>
  summarize(
    mean_fours = mean(fours),
    prob = mean(win),
    log_odds = empirical_link(
      win,
      family = binomial(link = "logit")),
    n = n()
  )

rate_by_fours |>
  ggplot(aes(x = mean_fours, y = log_odds)) +
  geom_point(aes(size = n)) +
  geom_smooth() +
  labs(x = "Number of fours scored", y = "log-odds of winning")

augment(cricket_logit) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point()

library(DHARMa)
dh = simulateResiduals(cricket_logit)

#creating residual points
cricket_aug = augment(cricket_logit) |>
  mutate(.quantile.resid = residuals(dh))

#residual plot to check linearity
cricket_aug |>
  ggplot(aes(x = .fitted, y = .quantile.resid)) +
  geom_point() +
  geom_hline(yintercept = 0.5, color = "orangered",
             linetype = "dashed") +
  geom_smooth()

cricket_pred_prob = predict(cricket_logit, type = "response") 
#for every obs the pred prob of winning

cricket_pred_class = ifelse(cricket_pred_prob > 0.5, "Win", "Loss") 
#labeling the predictions as a win or loss

cricket_pred_binary = ifelse(cricket_pred_prob > 0.5, 1, 0) 
#changing the labeled wins or losses to 1 and 0

library(ggeffects) #install.packages("ggeffects")

predict_response(cricket_logit,
                 terms = c("fours", "selection")) |>
  plot() +
  labs(x = "Number of fours scores",
       y = "Probability of winning the match",
       color = "Selection", title = NULL) 

glance(cricket_logit)

#misclassification rate
mean(cricket_pred_class != cricket$result) #32.9%
#proportion of times that the predicted class doesn't match the true class
prop.table(table(cricket$result)) # should always check the class balance!

#Brier Score, the lower the better calibrated predictions
mean((cricket_pred_binary - cricket_pred_prob)^2)

#Confusion matrix, looks at predicted vs observed outcome
#want diagonal cells to be large and off-diagonal cells to be small.
table("Predicted" = factor(cricket_pred_class, levels = c("Win", "Loss")), 
      "Observed" = factor(cricket$result, levels = c("Win", "Loss")))

library(pROC)

#want the area under the roc curve to be as large as possible
cricket_roc = cricket |> 
  mutate(pred_prob = predict(cricket_logit, 
                             type = "response")) |> 
  roc(result, pred_prob)
# str(cricket_roc)
cricket_roc$auc #gives area under curve = 0.703
tibble(threshold = cricket_roc$thresholds,
       specificity = cricket_roc$specificities,
       sensitivity = cricket_roc$sensitivities) |> 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed")

#calibration produce predicted probabilities for each class
cricket |> 
  mutate(pred = predict(cricket_logit, type = "response"),
         obs = ifelse(result == "Win", 1, 0)) |> 
  ggplot(aes(pred, obs)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))
#model starts to deviate for higher probability values, 
#considering that there are few data points above 0.8

#another calibration method named binning
cricket |> 
  mutate(
    pred_prob = predict(cricket_logit, type = "response"),
    bin_pred_prob = cut(pred_prob, breaks = seq(0, 1, .1))
  ) |> 
  group_by(bin_pred_prob) |> 
  summarize(n = n(),
            bin_actual_prob = mean(result == "Win")) |> 
  mutate(mid_point = seq(0.15, 0.95, 0.1)) |> 
  ggplot(aes(x = mid_point, y = bin_actual_prob)) +
  geom_point(aes(size = n)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, 
              color = "black", linetype = "dashed") +
  scale_x_continuous(breaks = seq(0.15, 0.95, 0.1)) 
# expand_limits(x = c(0, 1), y = c(0, 1))