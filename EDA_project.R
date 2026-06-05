#CMSA EDA WTA Project

library(tidyverse)
require(mosaic)
require(performance)

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
  drop_na()
wta_matches2

#negative exponenital or log function
ggplot(wta_matches2, aes(x = winner_rank, y = winner_rank_points)) + 
  geom_point() 


model1 = lm(winner_rank_points~winner_rank, data= wta_2021_2026_matches)
summary(model1)



wta_2021_2026_matches = wta_2021_2026_matches |>
  mutate(surface = recode(tolower(surface),
                          "Clay" = "clay")) |>
  mutate(surface = as.factor(surface))

str(wta_2021_2026_matches)

#proportion of surfaces
levels(wta_2021_2026_matches$surface)
prop.table(table(wta_2021_2026_matches$surface))
table(wta_2021_2026_matches$surface)

wta_2021_2026_matches|>
  select(surface, winner_name, loser_name) |>
  drop_na() |>
  ggplot(aes(x = surface)) +
  geom_bar(fill = "lightblue") + theme_bw() + coord_flip()

wta_2021_2026_matches |>
  select(surface,tourney_level,winner_name, loser_name) |>
  drop_na() |>
  count(surface) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = prop, y = surface, fill = tourney_level)) +
  geom_col() + theme_bw()

ggplot(wta_2021_2026_matches, aes(x = w_ace, y = l_ace)) + geom_point()

chisq.test(table(wta_2021_2026_matches$surface))
#Reject null hypothesis, significantly different proportion in types of surfaces played on.

levels(wta_2021_2026_matches$tourney_level)

# win by surface/ popularity of surface

#cor(wta_2021_2026_matches[,c(4,6,7,8,13,15,16,21)])


