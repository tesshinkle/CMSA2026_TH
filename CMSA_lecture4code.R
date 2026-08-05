#CMSA lecture 4 code

library(tidyverse)
ggplot2::theme_set(ggplot2::theme_light(base_size = 20)) # setting the ggplot theme

runs <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/ultra_running.csv") 
race_stats_2019 <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/ultra_races_2019.csv")

view(runs)
view(race_stats_2019)

wser = runs |>
  filter(event == "Western States Endurance Run")

summary(wser$time_in_hours)

sd(wser$time_in_hours, na.rm = TRUE) #na.rm removes na's

#boxplot (no outliers)
wser |> 
  ggplot(aes(x = time_in_hours)) +
  geom_boxplot() +
  theme(axis.text.y = element_blank())+
  labs(x= 'Time (hours)')

#histogram (bimodal shape)
wser |> 
  ggplot(aes(x = time_in_hours)) +
  geom_histogram(fill = "lightblue", col = "blue")+
  labs(x= "Time (hours)")

#density plot
wser |> 
  ggplot(aes(x = time_in_hours)) +
  geom_density()+
  labs(x= "Time (hours)")

library(ggbeeswarm)
wser |> 
  ggplot(aes(x = time_in_hours, y = "")) +
  geom_beeswarm(cex = 1, size = 0L)+
  labs(x= "Time (hours)", y="")

#violin plot
wser |> 
  ggplot(aes(x = time_in_hours, y = "")) +
  geom_violin()+
  labs(x= "Time (hours)", y ="")

#adding box plot to violin plot
wser |> 
  ggplot(aes(x = time_in_hours, y = "")) +
  geom_violin() +
  geom_boxplot(width = 0.25)+
  labs(x= "Time (hours)", y = "")

#ECDF plot, allows us to estimate CDF P(X <= x)
wser |> 
  ggplot(aes(x = time_in_hours)) +
  stat_ecdf()+
  labs(x= "Time (hours)")

wser |> 
  ggplot(aes(x = time_in_hours)) +
  stat_ecdf()+
  coord_cartesian(xlim = c(16, 17), ylim = c(0.01, 0.03))+
  labs(x= "Time (hours)")

#rug plots

wser |> 
  ggplot(aes(x = time_in_hours)) +
  geom_rug(alpha = 0.3)+
  labs(x= "Time (hours)")

wser |> 
  ggplot(aes(x = time_in_hours, y = "")) +
  geom_violin() +
  geom_rug(alpha = 0.3)+
  labs(x= "Time (hours)")

wser |> 
  ggplot(aes(x = time_in_hours)) +
  stat_ecdf() +
  geom_rug(alpha = 0.3)+
  labs(x= "Time (hours)")

#2d quantitative data

race_stats_2019 |> 
  ggplot(aes(x = elevation_gain, y = mean_finish_time)) +
  geom_point(color = "navy", size = 4, alpha = 0.5)+
  scale_x_continuous(labels = scales::label_comma())+
  labs(x = "Elevation Gain (m)", y = "Avg finish time (hrs)")

cor(race_stats_2019$elevation_gain, 
    race_stats_2019$mean_finish_time, 
    use = "complete.obs") # to ensure no missing data

race_stats_2019 |> 
  ggplot(aes(x = elevation_gain, y = mean_finish_time)) +
  geom_point(color = "navy", size = 4, alpha = 0.5)+
  geom_smooth(method = "lm", linewidth = 2)+
  labs(x = "Elevation Gain (m)", y = "Avg finish time (hrs)")

race_stats_2019 |> 
  ggplot(aes(x = elevation_gain, y = mean_finish_time)) +
  geom_point(color = "navy", size = 4, alpha = 0.5) +
  geom_rug(alpha = 0.4)+
  labs(x = "Elevation Gain (m)", y = "Avg finish time (hrs)")

library(GGally)
race_stats_2019 |> 
  ungroup() |>
  select(elevation_gain, elevation_loss, mean_finish_time, n_finishers) |> 
  ggpairs(aes(alpha=0.3)) # note: alpha in aes() here, even though it is not a variable

#continuous by categorical

wser <- wser |>
  mutate(age_grp = case_when(age <= 39 ~ "22-39", 
                             age > 39 & age <= 49 ~ "40-49", 
                             age > 49 & age <= 59 ~ "50-59", 
                             age > 59 ~ "60+"))
#boxplot
wser |> 
  ggplot(aes(x = time_in_hours, y = age_grp))+
  geom_boxplot()+
  labs(x = "Time (hours)", y = "Age group")

#ecdf
wser |> 
  ggplot(aes(x = time_in_hours, color = age_grp)) +
  stat_ecdf(linewidth = 1) +
  theme(legend.position = "bottom")+
  scale_color_viridis_d()+
  labs(x = "Time (hours)", color = "Age group")

#density
wser |> 
  ggplot(aes(x = time_in_hours, color = age_grp)) +
  geom_density(linewidth = 0.8) + 
  theme(legend.position = "bottom") +
  labs(color = "Age group", x = "Time (hours)")

library(ggridges)
wser |> 
  ggplot(aes(x = time_in_hours, y = age_grp, fill = age_grp)) +
  geom_density_ridges(scale = 1.5)+
  scale_fill_viridis_d()+
  labs(x = "Time (hours)", y = "Age group")+
  theme(legend.position = "none")

#histograms
wser |> 
  ggplot(aes(x = time_in_hours, fill = age_grp)) +
  geom_histogram(bins = 15, col = "black") +
  scale_fill_viridis_d(alpha = 0.6, option= "mako")+
  labs(fill = "Age group", x = "Time (hours)")

wser |>
  ggplot(aes(x = time_in_hours)) +
  geom_histogram(bins = 15, col = "blue", fill = "lightblue") +
  facet_wrap(~ age_grp, nrow = 1)+
  labs(x= "Time (hours)")

wser |> 
  ggplot(aes(x = time_in_hours)) +
  geom_histogram(bins = 15, fill = "lightblue", col = "blue") +
  facet_grid(age_grp ~ ., margins = TRUE)+
  labs(x = "Time (hours)")

