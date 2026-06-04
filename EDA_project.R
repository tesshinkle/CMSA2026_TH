#CMSA EDA WTA Project

library(tidyverse)
require(mosaic)

wta_2021_2026_matches <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/wta_matches_2021_2026.csv")

wta_2021_2026_matches <-
  map_dfr(c(2021:2026),
          function(year) {
            read_csv(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_",
                            year, ".csv")) %>%
              mutate(winner_seed = as.character(winner_seed),
                     loser_seed = as.character(loser_seed))
          })

view(wta_2021_2026_matches)

str(wta_2021_2026_matches)
summary(wta_2021_2026_matches)

favstats(~winner_ht, data=wta_2021_2026_matches)

cor(wta_2021_2026_matches[,c(4,6,7,8,13,15,16,21)])
table(wta_2021_2026_matches$tourney_name)

wta_matches = wta_2021_2026_matches |>
  select(surface, winner_name, minutes, winner_ht, 
         winner_hand,winner_age, best_of, round) |>
  drop_na()
wta_matches


wta_matches2 = wta_2021_2026_matches |>
  select(minutes:w_bpFaced, winner_rank, winner_rank_points) |>
  filter(winner_rank <=100)
  drop_na()
wta_matches2

ggplot(wta_matches2, aes(x = winner_rank, y = winner_rank_points)) + 
  geom_point() 



#cor(wta_2021_2026_matches[,c(4,6,7,8,13,15,16,21)])


