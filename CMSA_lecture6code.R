#CMSA Lecture code 6
#unsupervised learning:k-means clustering
#hard clustering: strictly assign observations to only one cluster
#soft/fuzzy clustering: assign an observation a probability of belonging to each cluster
#loyd's algorithm
#default r algorithm is Hartigan-Wong method

conflicted::conflicts_prefer(dplyr::select)

library(tidyverse)
theme_set(theme_bw())

d1_volleyball = read_csv("https://data.scorenetwork.org/data/volleyball_ncaa_div1_2022_23.csv")

glimpse(d1_volleyball)

set.seed(47)

init_kmeans = d1_volleyball |> 
  dplyr::select(kills_per_set, blocks_per_set) |> 
  kmeans(algorithm = "Lloyd", centers = 4,
         nstart = 1, iter.max = 20)

#vizualizing results
#cluster 1 possibly under-performing
d1_volleyball |>
  mutate(
    team_clusters = as.factor(init_kmeans$cluster)
  ) |>
  ggplot(aes(x = blocks_per_set, y = kills_per_set,
             color = team_clusters)) +
  geom_point(size = 2) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom")

#respects that the x-axis range is smaller than the y-axis range
#kills_per_set impacts the clustering more because the range is larger
d1_volleyball |>
  mutate(
    team_clusters = as.factor(init_kmeans$cluster)
  ) |>
  ggplot(aes(x = blocks_per_set, y = kills_per_set, 
             color = team_clusters)) +
  geom_point(size = 2) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") +
  coord_fixed()

#standardizing the variables
#center = TRUE (subtract the mean)
#scale = TRUE (divide by the std)
clean_d1_volleyball = d1_volleyball |>
  mutate(
    std_kills_per_set = as.numeric(scale(kills_per_set, center = TRUE, scale = TRUE)),
    std_blocks_per_set = as.numeric(scale(blocks_per_set, center = TRUE, scale = TRUE))
  )

std_kmeans <- clean_d1_volleyball |> 
  select(std_kills_per_set, std_blocks_per_set) |> 
  kmeans(algorithm = "Lloyd", centers = 4, nstart = 1, iter.max = 30)

clean_d1_volleyball |>
  mutate(
    team_clusters = as.factor(std_kmeans$cluster)
  ) |>
  ggplot(aes(x = blocks_per_set, y = kills_per_set,
             color = team_clusters)) +
  geom_point(size = 1) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") +
  coord_fixed()

clean_d1_volleyball |>
  mutate(
    team_clusters = as.factor(std_kmeans$cluster)
  ) |>
  ggplot(aes(x = std_blocks_per_set, y = std_kills_per_set,
             color = team_clusters)) +
  geom_point(size = 2) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom") +
  coord_fixed()

another_kmeans <- clean_d1_volleyball |> 
  select(std_kills_per_set, std_blocks_per_set) |> 
  kmeans(algorithm = "Lloyd", centers = 4, nstart = 1)

clean_d1_volleyball |>
  mutate(
    team_clusters = as.factor(another_kmeans$cluster)
  ) |>
  ggplot(aes(x = blocks_per_set, y = kills_per_set,
             color = team_clusters)) +
  geom_point(size = 2) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom")

nstart_kmeans <- clean_d1_volleyball |> 
  select(std_blocks_per_set, std_kills_per_set) |> 
  kmeans(algorithm = "Lloyd", centers = 4,
         nstart = 30, iter.max = 50)

clean_d1_volleyball |>
  mutate(
    team_clusters = as.factor(nstart_kmeans$cluster)
  ) |> 
  ggplot(aes(x = blocks_per_set, y = kills_per_set,
             color = team_clusters)) +
  geom_point(size = 2) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom")

#transitioning to hartigan wong method
default_kmeans <- clean_d1_volleyball |> 
  select(std_blocks_per_set, std_kills_per_set) |> 
  kmeans(algorithm = "Hartigan-Wong", centers = 4,
         nstart = 30, iter.max = 20) 

clean_d1_volleyball |>
  mutate(
    team_clusters = as.factor(default_kmeans$cluster)
  ) |> 
  ggplot(aes(x = blocks_per_set, y = kills_per_set,
             color = team_clusters)) +
  geom_point(size = 2) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom")

#using multiple variables
d1_volleyball_features <- d1_volleyball |>
  select(blocks_per_set, digs_per_set, kills_per_set, aces_per_set) |> 
  drop_na() 
#scaling volleyball features so you don't have to mutate everything individually
std_d1_volleyball_features <- d1_volleyball_features |> 
  scale(center = TRUE, scale = TRUE)

kmeans_many_features = std_d1_volleyball_features |> 
  kmeans(algorithm = "Hartigan-Wong", centers = 4, nstart = 30) 

library(gt)
#creating table of volleyball data
clean_d1_volleyball |>
  select(blocks_per_set, digs_per_set, kills_per_set, aces_per_set) |> 
  mutate(team_clusters = as.factor(kmeans_many_features$cluster)) |> 
  pivot_longer(-team_clusters, names_to = "feature", values_to = "value") |>
  group_by(team_clusters, feature) |>
  summarize(avg_value = base::mean(value), n_teams = n(), .groups = "drop") |>
  pivot_wider(id_cols = c(team_clusters, n_teams), names_from = feature, values_from = avg_value) |>
  gt() |>
  fmt_number(columns = -n_teams, decimals = 2)

#kmeans++
library(flexclust)
init_kmeanspp = clean_d1_volleyball |> 
  select(std_blocks_per_set, std_kills_per_set) |> 
  kcca(k = 4, control = list(initcent = "kmeanspp"))

clean_d1_volleyball |>
  mutate(
    team_clusters = as.factor(init_kmeanspp@cluster)
  ) |>
  ggplot(aes(x = blocks_per_set, y = kills_per_set,
             color = team_clusters)) +
  geom_point(size = 2) + 
  ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom")

#elbow plot
# function to perform clustering for each value of k
d1_volleyball_kmeans = function(k) {
  
  kmeans_results = clean_d1_volleyball |>
    select(std_blocks_per_set, std_kills_per_set) |>
    kmeans(centers = k, nstart = 30)
  
  kmeans_out = tibble(
    clusters = k,
    total_wss = kmeans_results$tot.withinss
  )
  return(kmeans_out)
}

# number of clusters to search over
n_clusters_search = 2:15

# iterate over each k to compute total wss
kmeans_search = n_clusters_search |> 
  map(d1_volleyball_kmeans) |> 
  bind_rows()

# plot the results
kmeans_search |> 
  ggplot(aes(x = clusters, y = total_wss)) +
  geom_line() + 
  geom_point(size = 4) +
  scale_x_continuous(breaks = n_clusters_search)

remove.packages("rlang")
library(factoextra)

clean_d1_volleyball |> 
  dplyr::select(std_blocks_per_set, std_kills_per_set) |> 
  fviz_nbclust(kmeans, method = "wss")

clean_d1_volleyball |> 
  select(std_blocks_per_set, std_kills_per_set) |> 
  fviz_nbclust(kmeans, method = "silhouette")

library(cluster)

d1_volleyball_kmeans_gap_stat = clean_d1_volleyball |> 
  select(std_blocks_per_set, std_kills_per_set) |> 
  clusGap(FUN = kmeans, nstart = 30, K.max = 10)

# view the result 
#d1_volleyball_kmeans_gap_stat |> 
#  print(method = "firstmax")

d1_volleyball_kmeans_gap_stat |> 
  fviz_gap_stat(maxSE = list(method = "firstmax"))

d1_volleyball_kmpp = function(k) {
  
  kmeans_results = clean_d1_volleyball |>
    select(std_blocks_per_set, std_kills_per_set) |>
    kcca(k = k, control = list(initcent = "kmeanspp"))
  
  kmeans_out = tibble(
    clusters = k,
    total_wss = sum(kmeans_results@clusinfo$size * 
                      kmeans_results@clusinfo$av_dist)
  )
  return(kmeans_out)
}

n_clusters_search = 2:12

kmpp_search = n_clusters_search |> 
  map(d1_volleyball_kmpp) |> 
  bind_rows()

kmpp_search |> 
  ggplot(aes(x = clusters, y = total_wss)) +
  geom_line() + 
  geom_point(size = 2) +
  scale_x_continuous(breaks = n_clusters_search)
