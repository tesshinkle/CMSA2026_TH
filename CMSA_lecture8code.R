#CMSA Lecture Code 8
# Gaussian Mixture Models
#soft assignments

library(tidyverse)
theme_set(theme_bw())

nba_players <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/nba_players_2024.csv")

glimpse(nba_players)

library(mclust)
# x3pa: 3pt attempts per 100 possessions
# trb: total rebounds per 100 possessions

nba_mclust = nba_players |> 
  select(x3pa, trb) |> 
  Mclust()

summary(nba_mclust)

#more than two variables


library(broom)
library(gt)

gt(tidy(nba_mclust)) #provides table

nba_mclust |> #shows uncertainty
  augment()

nba_mclust |> #the larger points show a larger uncertainty in the cluster that they are in
  augment() |> 
  ggplot(aes(x = x3pa, y = trb, color = .class, size = .uncertainty)) +
  geom_point(alpha = 0.6) +
  ggthemes::scale_color_colorblind()

nba_mclust |> #graphing BIC with different geometry values (EVV etc.)
  plot(what = "BIC", 
       legendArgs = list(x = "bottomright", ncol = 4))

nba_mclust |> #graph with clusters with circles
  plot(what = "classification")

table("Clusters" = nba_mclust$classification, "Positions" = nba_players$pos)

nba_player_probs = nba_mclust$z
colnames(nba_player_probs) = str_c("cluster", 1:3)

nba_player_probs <- nba_player_probs |>
  as_tibble() |>
  mutate(player = nba_players$player) |>
  pivot_longer(-player, names_to = "cluster", values_to = "prob")

nba_player_probs |>
  ggplot(aes(prob)) +
  geom_histogram() +
  facet_wrap(~cluster)

nba_players |>
  mutate(cluster = nba_mclust$classification,
         uncertainty = nba_mclust$uncertainty) |> 
  group_by(cluster) |>
  slice_max(uncertainty, n = 8) |> 
  mutate(player = fct_reorder(player, uncertainty)) |> 
  ggplot(aes(x = uncertainty, y = player)) +
  geom_point(size = 3) +
  facet_wrap(~cluster, scales = "free_y", nrow = 3)


#practice

#2 point attempts and steals
nba_mclust2 = nba_players |>
  select(x2pa, stl) |>
  Mclust()

summary(nba_mclust2)

gt(tidy(nba_mclust2))

nba_mclust2 |> 
  augment() |> 
  ggplot(aes(x = x2pa, y = stl, color = .class, size = .uncertainty)) +
  geom_point(alpha = 0.6) +
  ggthemes::scale_color_colorblind()

nba_mclust2 |> 
  plot(what = "BIC", 
       legendArgs = list(x = "bottomright", ncol = 4))

nba_mclust2 |> 
  plot(what = "classification")

#table looking at the type of positions in each cluster
table("Clusters" = nba_mclust2$classification, "Positions" = nba_players$pos)

#creating histogram of clusters and their probability with counts in the respective clusters
nba_player_probs2 = nba_mclust2$z
colnames(nba_player_probs2) = str_c("cluster", 1:3)

nba_player_probs2 = nba_player_probs2 |>
  as_tibble() |>
  mutate(player = nba_players$player) |>
  pivot_longer(-player, names_to = "cluster", values_to = "prob")

nba_player_probs2 |>
  ggplot(aes(prob)) +
  geom_histogram() +
  facet_wrap(~cluster)


#visualization of players and the uncertainty in their clusters
nba_players |>
  mutate(cluster = nba_mclust2$classification,
         uncertainty = nba_mclust2$uncertainty) |> 
  group_by(cluster) |>
  slice_max(uncertainty, n = 8) |> 
  mutate(player = fct_reorder(player, uncertainty)) |> 
  ggplot(aes(x = uncertainty, y = player)) +
  geom_point(size = 3) +
  facet_wrap(~cluster, scales = "free_y", nrow = 3) #free_y chooses just the players in the cluster

#nba_mclust2$z #show the probability what each player is in what cluster


#more than two variables

nba_mclust3 = nba_players |> 
  select(x3pa, trb, ast, stl) |> 
  Mclust()

summary(nba_mclust3)

gt(tidy(nba_mclust3))

nba_mclust3 |> 
  augment() |> 
  ggplot(aes(x = x3pa, y = trb, color = .class, size = .uncertainty)) +
  geom_point(alpha = 0.6) +
  ggthemes::scale_color_colorblind()


nba_mclust3 |> 
  plot(what = "BIC", 
       legendArgs = list(x = "bottomright", ncol = 4))

nba_mclust3 |> 
  plot(what = "classification")

table("Clusters" = nba_mclust3$classification, "Positions" = nba_players$pos)

nba_player_probs3 = nba_mclust3$z
colnames(nba_player_probs2) = str_c("cluster", 1:4)

nba_player_probs3 = nba_player_probs3 |>
  as_tibble() |>
  mutate(player = nba_players$player) |>
  pivot_longer(-player, names_to = "cluster", values_to = "prob")

nba_player_probs3 |>
  ggplot(aes(prob)) +
  geom_histogram() +
  facet_wrap(~cluster)

nba_players |>
  mutate(cluster = nba_mclust3$classification,
         uncertainty = nba_mclust3$uncertainty) |> 
  group_by(cluster) |>
  slice_max(uncertainty, n = 5) |> 
  mutate(player = fct_reorder(player, uncertainty)) |> 
  ggplot(aes(x = uncertainty, y = player)) +
  geom_point(size = 3) +
  facet_wrap(~cluster, scales = "free_y", nrow = 4) #free_y chooses just the players in the cluster
