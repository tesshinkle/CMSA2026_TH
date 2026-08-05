#CMSA Lecture 5 code 
#Data visualization:vdensity estimation
library(tidyverse)
theme_set(theme_light())
wilson_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/wilson_shots.csv")
glimpse(wilson_shots)

wilson_shots |>
  ggplot(aes(x = shot_distance)) +
  geom_histogram(fill = "lightblue", col = "blue")

#small binwidth
wilson_shots |>
  ggplot(aes(x = shot_distance)) +
  geom_histogram(binwidth = 0.5)

#large binwidth
wilson_shots |>
  ggplot(aes(x = shot_distance)) +
  geom_histogram(binwidth = 5)

wilson_shots |>
  ggplot(aes(x = shot_distance)) +
  geom_histogram(binwidth = 1)

#creating closed intervals # ensure it isn't covering any negative shot distance
wilson_shots |>
  ggplot(aes(x = shot_distance)) +
  geom_histogram(binwidth = 1, center = 0.5,
                 closed = "left")

#looking at density plots and their similarity to histograms
#can determine that the shooter is primarily a two-point shooter
wilson_shots |>
  ggplot(aes(x = shot_distance)) + 
  geom_density() +
  geom_rug(alpha = 0.3)

wilson_shots |>
  ggplot(aes(x = shot_distance)) + 
  geom_density(adjust = 0.5) + #similar to changing binwidth for histograms
  geom_rug(alpha = 0.3)

wilson_shots |>
  ggplot(aes(x = shot_distance)) + 
  geom_density(adjust = 2) +
  geom_rug(alpha = 0.3)

wilson_shots |>
  mutate(shot_cat = case_when(shot_distance < 10 ~ "<10",
                              shot_distance >=10 & shot_distance < 20 ~"10-20",
                              TRUE ~ ">20")) |>
  count(shot_cat)


require(cowplot)

wilson_shot_dens <- wilson_shots |>
  ggplot(aes(x = shot_distance)) + 
  geom_density() +
  labs(x = "Shot distance (in feet)",
       y = "Number of shot attempts")

wilson_shot_ecdf <- wilson_shots |>
  ggplot(aes(x = shot_distance)) + 
  stat_ecdf() +
  labs(x = "Shot distance (in feet)",
       y = "Proportion of shot attempts")

# library(patchwork)
# wilson_shot_dens + wilson_shot_ecdf
# plot_grid(wilson_shot_dens, wilson_shot_ecdf) #not working

wilson_shot_dens_made <- wilson_shots |>
  ggplot(aes(x = shot_distance, 
             color = scoring_play)) + 
  geom_density() +
  geom_rug(alpha = 0.3) +
  labs(x = "Shot distance (in feet)",
       y = "Number of shot attempts")

wilson_shot_ecdf_made <- wilson_shots |>
  ggplot(aes(x = shot_distance,
             color = scoring_play)) + 
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  labs(x = "Shot distance (in feet)",
       y = "Proportion of shot attempts")

library(patchwork)
wilson_shot_dens_made + wilson_shot_ecdf_made + plot_layout(guides = "collect")

library(ggridges)
wilson_shots |>
  group_by(shot_type) |>
  mutate(n_shots = n()) |> 
  filter(n_shots >= 30) |> #setting a cut-off
  ggplot(aes(x = shot_distance, y = fct_reorder(shot_type, n_shots))) + 
  geom_density_ridges(rel_min_height = 0.01) 

#2d density estimation
wilson_shots |>
  ggplot(aes(x = shot_x, y = shot_y)) +
  geom_point(size = 4, alpha = 0.3)

wilson_shots |>
  ggplot(aes(x = shot_x, y = shot_y)) + 
  geom_point(size = 4, alpha = 0.3) + 
  geom_density_2d(col = "red", size = 1) +
  coord_fixed() +
  theme(legend.position = "bottom")

wilson_shots |>
  ggplot(aes(x = shot_x, y = shot_y)) + 
  geom_point(size = 4, alpha = 0.3) + 
  geom_density2d(adjust = 2, col = "red", size = 1) + #the default of 1 is probably best
  coord_fixed() +
  theme(legend.position = "bottom")

#
wilson_shots |>
  ggplot(aes(x = shot_x, y = shot_y)) + 
  stat_density2d(aes(fill = after_stat(density)),
                 h = c(0.6, 0.6), bins = 100, contour = FALSE,
                 geom = "raster") +
  scale_fill_gradient(low = "midnightblue", 
                      high = "gold") +
  theme(legend.position = "bottom", 
        legend.key.size = unit(1.5, "cm")) +
  coord_fixed()

library(hexbin)
wilson_shots |>
  ggplot(aes(x = shot_x, y = shot_y)) + 
  geom_hex(binwidth = c(1, 1)) +
  scale_fill_gradient(low = "navy", 
                      high = "red") + 
  theme(legend.position = "bottom") +
  coord_fixed()

wilson_shots |>
  ggplot(aes(x = shot_x, y = shot_y, 
             z = scoring_play, group = 1)) +
  stat_summary_hex(binwidth = c(2, 2), fun = mean, 
                   color = "black", linewidth = 0.05) +
  scale_fill_gradient(low = "midnightblue", 
                      high = "green") + 
  theme(legend.position = "bottom", 
        legend.key.size = unit(1.5, "cm")) +
  coord_fixed()

library(sportyR)
wnba_court <- geom_basketball("wnba", display_range = "offense", rotation = 270, x_trans = -41.5)
wnba_court +
  geom_hex(data = wilson_shots, aes(x = shot_x, y = shot_y), binwidth = c(1.5, 1.5)) + 
  scale_fill_gradient(low = "midnightblue", high = "red")
