#new project
#WTA Tennis matches 2021-2026

library(tidyverse)
require(mosaic)
require(performance)
require(ggplot2)

wta_2021_2026_matches <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/wta_matches_2021_2026.csv")


wta_2021_2026_matches <-
  map_dfr(c(2021:2026),
          function(year) {
            read_csv(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_",
                            year, ".csv")) %>%
              mutate(winner_seed = as.character(winner_seed),
                     loser_seed = as.character(loser_seed),
                     surface = as.factor(surface),
                     winner_entry = as.factor(winner_entry),
                     loser_entry = as.factor(loser_entry),
                     tourney_level = as.factor(tourney_level))
          })

view(wta_2021_2026_matches)

str(wta_2021_2026_matches)
summary(wta_2021_2026_matches)

#getting simple stats of various variables
favstats(~winner_ht, data=wta_2021_2026_matches)
favstats(~winner_age, data = wta_2021_2026_matches)
favstats(~loser_age, data=wta_2021_2026_matches)
favstats(~w_ace, data = wta_2021_2026_matches)
favstats(~l_ace,data = wta_2021_2026_matches)
favstats(~minutes, data = wta_2021_2026_matches)

#several outliers in aces. potentially able to throw out aces > 40 
wta_2021_2026_matches |>
  select(l_ace) |>
  drop_na() |>
  ggplot(aes(x = l_ace)) +
  geom_boxplot()

wta_2021_2026_matches |>
  select(w_ace) |>
  drop_na() |>
  ggplot(aes(x = w_ace)) +
  geom_boxplot()

wta_2021_2026_matches |>
  select(w_ace) |>
  drop_na() |>
  ggplot(aes(x = w_ace)) +
  geom_histogram()

table(wta_2021_2026_matches$tourney_name)

#combining the two clays
#dplyr:: used due to an error from a potential other package used
wta_2021_2026_matches = wta_2021_2026_matches |>
  mutate(surface = factor(tolower(surface)))

str(wta_2021_2026_matches)

#proportion of surfaces
levels(wta_2021_2026_matches$surface)
prop.table(table(wta_2021_2026_matches$surface))
table(wta_2021_2026_matches$surface)

#bar chart of surfaces
wta_2021_2026_matches|>
  select(surface, winner_name, loser_name) |>
  drop_na() |>
  ggplot(aes(x = surface)) +
  geom_bar(fill = "lightblue", col = "blue") + theme_bw() + coord_flip()

class(wta_2021_2026_matches$tourney_level)



#creating new column with full tournament names
wta_2021_2026_matches <- wta_2021_2026_matches |>
  mutate(tournament = case_when(tourney_level == "O" ~ "Olympics", 
                                tourney_level == "P" ~ "Premier",
                                tourney_level == "PM" ~ "Premier Mandatory", 
                                tourney_level == "I"~ "International",
                                tourney_level == "G" ~ "Grand Slams", 
                                tourney_level == "F" ~ "Tour Finals",
                                tourney_level == "D" ~ "Bille Jean King Cup",
                                tourney_level == "W" ~ "ITF Tournament",
                                tourney_level == "35+H" ~ "ITF Tournament",
                                tourney_level == "50+H" ~ "ITF Tournament")) |>
  mutate(tournament = as.factor(tournament))

#table
wta_2021_2026_matches |> 
  select(surface, tournament) |> 
  table() |> 
  prop.table()

ace_table = wta_long |>
  dplyr::select(tournament, surface, ace) |>
  drop_na() |>
  group_by(tournament, surface) |>
  summarise(sum_aces = sum(ace, na.rm = TRUE),
            .groups = "drop") |>
  pivot_wider(names_from = surface, values_from = sum_aces, values_fill = 0)
  

#visualization for tournament levels and surfaces used
wta_2021_2026_matches |>
  select(surface, tournament) |>
  drop_na() |>
  ggplot(aes(x = tournament, fill = surface)) +
  geom_bar(col = "black") +
  labs(x= "Tournament Level", y = "", 
       title = "Visualization of Surfaces used per Tournament Level") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 1, hjust = 1)) 

chisq.test(table(wta_2021_2026_matches$surface))
#Reject null hypothesis, significantly different proportion in types of surfaces played on.

chisq.test(table(wta_2021_2026_matches$surface, wta_2021_2026_matches$tourney_level))
#reject null hypothesis, significantly different proportion between surfaces and tourney level

wta_2021_2026_matches |> 
  select(surface, tourney_level) |> 
  table() |> 
  chisq.test()

#looking at potential upsets (amount)
require(ggridges)
wta_2021_2026_matches |>
  select(surface, minutes, winner_rank, loser_rank, tournament, round) |>
  mutate(rank_difference = loser_rank-winner_rank) |>
  filter(rank_difference < -50) |>
  filter(round == "SF" | round == "F" | round == "QF") |>
  group_by(round) |>
  ggplot(aes(x = rank_difference, y = round)) + 
  geom_density_ridges(rel_min_height = 0.01) +
  theme_bw() +
  labs(title = "Frequency of Upsets among WTA matches", y = "Rounds (SF, QF, and F)",
       x = "Difference in Rank between Winner and Loser")

# Is there more number of Aces in specific type of surface? 


#Filtering datasets for ace/surface analysis
ace_data <- wta_2021_2026_matches |>
  filter(!is.na(surface),!is.na(w_ace), !is.na(l_ace)) |>
  mutate(total_aces = w_ace + l_ace)


#Distribution of Matches by Court Surface
ace_data |>
  count(surface) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = surface, y = prop)) +
  geom_col(fill="skyblue", col="blue", na.rm = TRUE) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, hjust = 1))+
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of Matches by Court Surface",
    x = "Court Surface",
    y = "Percentage of Matches"
  )

# Boxplot Visualization
#distribution of aces by court surface


wta_long |>
  select(ace, surface) |>
  drop_na() |>
  filter(ace < 30) |>
  ggplot(aes(x = ace, y = surface, fill = surface)) +
  geom_boxplot() +
  labs(title = "Distribution of Aces by Court Surface",
       x = "Total Aces per Match", y = "Surface type") +
  theme_bw() + theme(legend.position = "none")

summary(aov(total_aces~surface, data = ace_data))


#repeated measures ANOVA?
#between subjects: each subject is measured once (total_aces)
#within subjects: each subject is measured many times (surfaces?)

#rm.ANOVA2 = aov(ace~surface+Error(name/surface),data=wta_long)
#summary(rm.ANOVA2)

## combining the winner and loser columns
#essentially pivoting the data set----
wta_long <- wta_2021_2026_matches |>
  rename_with(~ sub("^w_", "winner_", .x), starts_with("w_")) |>
  rename_with(~ sub("^l_", "loser_", .x), starts_with("l_")) |>
  pivot_longer(
    cols = matches("^(winner|loser)_"),
    names_to = c("outcome", ".value"),
    names_pattern = "(winner|loser)_(.*)"
  ) 
wta_long
glimpse(wta_long)
str(wta_long)

wta_long = wta_long |>
  rename( firstIn = `1stIn`,
          firstWon = `1stWon`,
          secWon = `2ndWon`)

wta_long_table = wta_long |>
  #as.data.frame() |>
  select(surface, tournament, name, outcome, ht, ace, df, rank, age )|>
  head(5) |>
  gt() |>
  tab_header(title = "WTA 2021-2026 Data") 

wta_long = wta_long |>
  select(surface, outcome:rank_points)

wta_long = wta_long |>
  select(-id, -seed, -entry)
wta_long

wta_aces = wta_long |>
  drop_na() |>
  group_by(name, surface) |>
  summarise(mean_aces = mean(ace),
    .groups = "drop")
wta_aces

rm.ANOVA = aov(mean_aces~surface+Error(name/surface),data=wta_aces)
summary(rm.ANOVA)
#there is a significantly statistical difference in mean_aces by surface


ggplot(wta_long, aes(x = ace, y = ht)) + geom_point()

#clustering 
#kmeans----
set.seed(47)
#potential two variable clustering between height and aces
wta_data = wta_long |>
  select(ace, ht) |>
  drop_na() |>
  mutate(
    std_ace = as.numeric(scale(ace, center = TRUE, scale = TRUE)),
    std_ht = as.numeric(scale(ht, center = TRUE, scale = TRUE)))

wta_kmeans = wta_data |>
  dplyr::select(std_ace, std_ht) |>
  drop_na() |>
  kmeans(algorithm = "Hartigan-Wong", centers = 3, nstart = 1, iter.max = 40)

wta_data |>
  drop_na() |>
  mutate(wta_clusters = as.factor(wta_kmeans$cluster)) |>
  ggplot(aes(x = ht, y = ace, color = wta_clusters)) +
  geom_point() + coord_fixed()


#multipe variables clustering

wta_match_features = wta_2021_2026_matches |>
  select( w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_df, w_bpSaved, w_bpFaced) |> 
  drop_na() 
#scaling volleyball features so you don't have to mutate everything individually
std_wta_match_features = wta_match_features |> 
  scale(center = TRUE, scale = TRUE)

kmeans_many_features = std_wta_match_features |> 
  kmeans(algorithm = "Hartigan-Wong", centers = 3, nstart = 50) 

library(gt)
#creating table of tennis data
wta_2021_2026_matches |>
  select( w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_df, w_bpSaved, w_bpFaced) |> 
  drop_na() |>
  mutate(wta_clusters = as.factor(kmeans_many_features$cluster)) |> 
  pivot_longer(-wta_clusters, names_to = "feature", values_to = "value") |>
  group_by(wta_clusters, feature) |>
  summarize(avg_value = base::mean(value), .groups = "drop") |>
  pivot_wider(id_cols = c(wta_clusters), names_from = feature, values_from = avg_value) |>
  gt() |>
  fmt_number( decimals = 2)

#doing kmean clustering with combined data ----
wta_match_features2 = wta_long |>
  select( svpt, firstIn, firstWon, secWon, df, bpSaved, bpFaced) |> 
  drop_na() 
#scaling volleyball features so you don't have to mutate everything individually
std_wta_match_features2 = wta_match_features2 |> 
  scale(center = TRUE, scale = TRUE)

kmeans_many_features2 = std_wta_match_features2 |> 
  kmeans(algorithm = "Hartigan-Wong", centers = 3, nstart = 50) 

library(gt)
#creating table of tennis data
wta_long |>
  select( svpt, firstIn, firstWon, secWon, df, bpSaved, bpFaced) |> 
  drop_na() |>
  mutate(wta_clusters2 = as.factor(kmeans_many_features2$cluster)) |> 
  pivot_longer(-wta_clusters2, names_to = "feature", values_to = "value") |>
  group_by(wta_clusters2, feature) |>
  summarize(avg_value = base::mean(value), .groups = "drop") |>
  pivot_wider(id_cols = c(wta_clusters2), names_from = feature, values_from = avg_value) |>
  gt() |>
  fmt_number( decimals = 2)

#kmeans clustering with just bpFaced, bpSaved, df, and firstWon

wta_match_features3 = wta_long |>
  select(df, bpSaved, bpFaced) |> 
  drop_na() 
#scaling volleyball features so you don't have to mutate everything individually
std_wta_match_features3 = wta_match_features3 |> 
  scale(center = TRUE, scale = TRUE)

kmeans_many_features3 = std_wta_match_features3 |> 
  kmeans(algorithm = "Hartigan-Wong", centers = 4, nstart = 50) 

library(gt)
#creating table of tennis data
wta_long |>
  select( df, bpSaved, bpFaced) |> 
  drop_na() |>
  mutate(wta_clusters3 = as.factor(kmeans_many_features3$cluster)) |> 
  pivot_longer(-wta_clusters3, names_to = "feature", values_to = "value") |>
  group_by(wta_clusters3, feature) |>
  summarize(avg_value = base::mean(value), .groups = "drop") |>
  pivot_wider(id_cols = c(wta_clusters3), names_from = feature, values_from = avg_value) |>
  gt() |>
  fmt_number( decimals = 2)



#clustering analysis
#hierarchical clustering----

wta_serves = wta_long |>
  select(SvGms, rank) |>
  drop_na()

wta_serves = wta_serves |>
  mutate(std_SvGms = as.numeric(scale(SvGms)),
    std_rank = as.numeric(scale(rank)))

wta_serves |> 
  ggplot(aes(x = std_SvGms, y = std_rank)) +
  geom_point(size = 2) +
  coord_fixed()

wta_dist = wta_serves |> 
  select(std_SvGms, std_rank) |> 
  dist()

wta_dist_matrix = as.matrix(wta_dist)
rownames(players_dist_matrix) = wta_serves$name #assigns names to rows and columns
colnames(players_dist_matrix) = wta_serves$name
players_dist_matrix[1:4, 1:4]

wta_complete = wta_dist |> 
  hclust(method = "complete")

wta_serves |> 
  mutate(
    cluster = as.factor(cutree(wta_complete, k = 3)) #choose number of clusters
  ) |>
  ggplot(aes(x = std_SvGms, y = std_rank,
             color = cluster)) +
  geom_point(size = 2) + 
  ggthemes::scale_color_colorblind() +
  coord_fixed()
theme(legend.position = "bottom")

#Clustering from Swayam of avg_rank and avg_ace
player_summary <- wta_long |>
  group_by(name) |>
  summarize(avg_rank = mean(rank, na.rm = TRUE),
            avg_bpFaced = mean(bpFaced, na.rm = TRUE),
            avg_ace = mean(ace, na.rm = TRUE),
            matches = n()) |>
  filter(avg_rank < 350, avg_ace<8) |>
  drop_na()

wta_players <- player_summary |>
  mutate(std_rank = as.numeric(scale(avg_rank, center = TRUE, scale = TRUE)),
         std_ace = as.numeric(scale(avg_ace, center = TRUE, scale = TRUE)))

players_dist <- wta_players |>
  select(std_rank, std_ace) |>
  dist()

wta_complete <- players_dist |>
  hclust(method = "complete")

wta_players |>
  mutate(
    cluster = as.factor(cutree(wta_complete, k = 3))
  ) |>
  ggplot(aes(
    x = avg_ace,
    y = avg_rank,
    color = cluster
  )) +
  geom_point(size = 1) +
  coord_flip() +
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom")

#soft clustering ----
library(mclust)
wta_mclust = player_summary |> 
  select(avg_ace, avg_rank) |> 
  Mclust()

summary(wta_mclust)

library(broom)
library(gt)

gt(tidy(wta_mclust)) |>
  cols_label(mean.avg_ace = "Mean avg aces",
             mean.avg_rank = "Mean avg rank",
             component = "Clusters",
             size = "Size",
             proportion = "Proportion")

wta_mclust |> #the larger points show a larger uncertainty in the cluster that they are in
  augment() |> 
  ggplot(aes(x = avg_ace, y = avg_rank, color = .class, size = .uncertainty)) +
  geom_point(alpha = 0.6) + labs(title = "Clustering of Average Rank and Average Aces",
                                 x = "Average Ace", y = "Average Rank")

wta_mclust |> #graph with clusters with circles
  plot(what = "classification")

#table("Clusters" = wta_mclust$classification, "Positions" = player_summary$outcome)


wta_new = wta_long |>
  select(ace, rank, outcome, name) |>
  drop_na() |>
  group_by(name, outcome) |>
  summarize(avg_rank = mean(rank, na.rm = TRUE),
                        avg_ace = mean(ace, na.rm = TRUE),
                        matches = n()) |>
  filter(avg_rank < 350, avg_ace<8)

wta_mclust2 = wta_new |> 
  select(avg_ace, avg_rank) |> 
  Mclust()

summary(wta_mclust2)

table("Clusters" = wta_mclust2$classification, "outcome" = wta_new$outcome)

wta_mclust2 |> #the larger points show a larger uncertainty in the cluster that they are in
  augment() |> 
  ggplot(aes(x = avg_ace, y = avg_rank, color = .class, size = .uncertainty)) +
  geom_point(alpha = 0.6) + labs(title = "Clustering of Average Rank and Average Aces",
                                 x = "Average Ace", y = "Average Rank")

wta_mclust2 |> #graph with clusters with circles
  plot(what = "classification")
