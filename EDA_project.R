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


