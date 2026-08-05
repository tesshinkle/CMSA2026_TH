#CMSA Lecture 7 code
#hierarchical clustering
#no randomness compared to kmeans

library(tidyverse)
theme_set(theme_bw())
nba_players <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/nba_players.csv")
glimpse(nba_players)

nba_players |> 
  ggplot(aes(x = x3pa, y = trb)) +
  geom_point(size = 2, alpha=0.7)

nba_players = nba_players |> 
  mutate(
    std_x3pa = as.numeric(scale(x3pa)),
    std_trb = as.numeric(scale(trb))
  )

nba_players |> 
  ggplot(aes(x = std_x3pa, y = std_trb)) +
  geom_point(size = 2) +
  coord_fixed()

players_dist = nba_players |> 
  select(std_x3pa, std_trb) |> 
  dist()

players_dist_matrix = as.matrix(players_dist)
rownames(players_dist_matrix) = nba_players$player #assigns names to rows and columns
colnames(players_dist_matrix) = nba_players$player
players_dist_matrix[1:4, 1:4] #gives first four rows and columns

#Clustering_Analysis-------
#Complete linkage method
#maximum distance between between any two cases in the cluster
#merge the points closest together into a new cluster
#update the distances using complete linkage (new distance will be the furthest distnace)

#single linkage method = lowkey trash
#average = slightly better that single

nba_complete = players_dist |> 
  hclust(method = "complete")

#can determine number of cluster by height h or by k (clusters)
nba_players |> 
  mutate(
    cluster = as.factor(cutree(nba_complete, k = 3)) #choose number of clusters
  ) |>
  ggplot(aes(x = std_x3pa, y = std_trb,
             color = cluster)) +
  geom_point(size = 2) + 
  ggthemes::scale_color_colorblind() +
  coord_fixed()
  theme(legend.position = "bottom")

library(ggdendro)
nba_complete |> 
  ggdendrogram(labels = FALSE, 
               leaf_labels = FALSE, #makes sure there is no player labels on the dendogram
               theme_dendro = FALSE) + 
  labs(y = "Dissimilarity between clusters") +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid = element_blank())

#post-clustering analysis----
table("Cluster" = cutree(nba_complete, k = 3), "Position" = nba_players$pos)
#creates a table of the clusters by position

#including multiple variables----
nba_players_features = nba_players |> 
  select(x3pa, x2pa, fta, trb, ast, stl, blk, tov)

player_dist_mult_features = nba_players_features |> 
  dist(method = "euclidean") # can try out other methods

nba_players_hc_complete = player_dist_mult_features |> 
  hclust(method = "complete") # can try out other methods

nba_players_hc_complete |> 
  ggdendrogram(labels = FALSE, 
               leaf_labels = FALSE,
               theme_dendro = FALSE) +
  labs(y = "Dissimilarity between clusters") +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        panel.grid = element_blank())

nba_players |> 
  mutate(
    cluster = as.factor(cutree(nba_players_hc_complete, k = 3)) 
  ) |>
  ggplot(aes(x = ast, y = x2pa,
             color = cluster)) +
  geom_point(size = 2)

  

library(factoextra)
#shows optimal number of clusters
nba_players_features |> 
  fviz_nbclust(FUN = hcut, method = "silhouette")
