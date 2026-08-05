#CMSA Lecture 10 code
#variable selection
library(tidyverse)
batting_2025 <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/player_hitting_2025.csv")

batting_2025 |> 
  ggplot(aes(x = woba)) +
  geom_histogram(color = "black", 
                 fill = "darkblue",
                 alpha = 0.3) +
  theme_bw() +
  labs(x = "wOBA")+
  theme(axis.text = element_text(size =15),
        axis.title =element_text(size = 18))

library(ggcorrplot)
#creating a correlation heat map
mlb_model_data = batting_2025 |>
  dplyr::select(woba,
                barrel_batted_rate,
                hard_hit_percent, 
                whiff_percent, k_percent, swing_percent, bb_percent)
mlb_cor_matrix = cor(mlb_model_data)
ggcorrplot(mlb_cor_matrix)

#creating a correlation heat map with numbers available
round_cor_matrix =  round(cor(mlb_model_data), 2)
ggcorrplot(round_cor_matrix, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

library(gtsummary) #creates tables similar to gt()
#linear regression model
hh_mod = lm(data= mlb_model_data, woba ~ hard_hit_percent)
#table of regression
#in the table beta stands for slope
tbl_regression(hh_mod, estimate_fun = label_style_sigfig(digits = 3))

barrel_mod = lm(data= mlb_model_data, woba ~ barrel_batted_rate)
tbl_regression(barrel_mod, estimate_fun = label_style_sigfig(digits = 3))

#combined lm model with barrel percent and hard hit percent
combined = lm(data = mlb_model_data, woba ~ barrel_batted_rate + hard_hit_percent)
tbl_regression(combined, estimate_fun = label_style_sigfig(digits = 4))

mlb_model_data |>
  ggplot(aes(hard_hit_percent,barrel_batted_rate)) +
  geom_point()

#implementing cross-validation
set.seed(2026)
N_FOLDS = 5
batting_2025 = batting_2025 |>
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

table(batting_2025$fold)

batting_cv = function(x) {
  
  # get test and training data
  test_data = batting_2025 |> filter(fold == x) #29 observations each                     
  train_data = batting_2025 |> filter(fold != x)
  
  # fit models to training data
  barrel_fit = lm(woba ~ barrel_batted_rate, data = train_data)
  combined_fit = lm(woba ~ barrel_batted_rate + hard_hit_percent, data = train_data)
  
  # return test results
  out = tibble(
    barrel_pred = predict(barrel_fit, newdata = test_data),
    combined_pred = predict(combined_fit, newdata = test_data),
    test_actual = test_data$woba,
    test_fold = x
  )
  return(out)
}

batting_test_preds = map(1:N_FOLDS, batting_cv) |> 
  bind_rows()

batting_test_summary = batting_test_preds |>
  pivot_longer(barrel_pred:combined_pred, names_to = "model", values_to = "test_pred") |>
  group_by(model, test_fold) |>
  summarize(rmse = sqrt(mean((test_actual - test_pred)^2)))

batting_test_summary |> 
  group_by(model) |> 
  summarize(avg_cv_rmse = mean(rmse),
            sd_rmse = sd(rmse),
            k = n()) |>
  mutate(se_rmse = sd_rmse / sqrt(k),
         lower_rmse = avg_cv_rmse - se_rmse,
         upper_rmse = avg_cv_rmse + se_rmse) |> 
  select(model, avg_cv_rmse, lower_rmse, upper_rmse)
#the rmse is almost equivalent between the barrel and combined model showing 
#that you can choose either model. I would typically choose just one variable 
#instead of two for parsimony

batting_test_summary |> 
  ggplot(aes(x = model, y = rmse)) + 
  geom_point(size = 4, alpha=0.6) +
  stat_summary(fun = mean, geom = "point", 
               color = "red", size = 4) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               color = "red", width = 0.2)

library(gt)
library(broom)
#not working
all_fit |> 
  tidy() |> 
  gt() |> 
  fmt_number(columns = where(is.numeric), decimals = 2) |> 
  cols_label(term = "Term",
             estimate = "Estimate",
             std.error = "SE",
             statistic = "t",
             p.value = md("*p*-value"))

library(gtsummary)
all_fit |> 
  tbl_regression() |> 
  bold_p() |> 
  bold_labels()

# https://bradcongelio.com/nfl-analytics-with-r-book/04-nfl-analytics-visualization.html
cpoe <- read_csv("http://nfl-book.bradcongelio.com/pure-cpoe")
cpoe_gt <- cpoe |> 
  select(passer, season, total_attempts, mean_cpoe) |> 
  gt(rowname_col = "passer") |> 
  fmt_number(columns = c(mean_cpoe), decimals = 2) |>
  data_color(columns = c(mean_cpoe),
             fn = scales::col_numeric(palette = c("#FEE0D2", "#67000D"), domain = NULL)) |> 
  cols_align(align = "center", columns = c("season", "total_attempts")) |> 
  tab_stubhead(label = "Quarterback") |> 
  cols_label(season = "Season",
             total_attempts = "Attempts",
             mean_cpoe = "Mean CPOE") |> 
  tab_header(title = md("**Average CPOE in Pure Passing Situations**"),
             subtitle = md("*For seasons between 2010 and 2022*")) |> 
  tab_source_note(source_note = md("Example adapted from the book<br>*An Introduction to NFL Analytics with R*")) |> 
  gtExtras::gt_theme_espn()

# gtsave(cpoe_gt, "cpoe_gt.png")