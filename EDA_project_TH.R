#new project


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
  mutate(surface = dplyr::recode(tolower(surface), 
                          "Clay" = "clay")) |>
  mutate(surface = as.factor(surface))

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


wta_2021_2026_matches |> 
  select(surface, tournament) |> 
  table() |> 
  prop.table()

#visualization for tournament levels and surfaces used
wta_2021_2026_matches |>
  select(surface, tournament) |>
  drop_na() |>
  ggplot(aes(x = tournament, fill = surface)) +
  geom_bar(col = "black") +
  labs(x= "Tournament Level", y = "") +
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

#potential repeated measures anova for number of aces by surface level

#looking at potential upsets (amount)

wta_2021_2026_matches |>
  select(surface, minutes, winner_rank, loser_rank, tournament, round) |>
  mutate(rank_difference = loser_rank-winner_rank) |>
  filter(rank_difference < -50) |>
  filter(round == "SF" | round == "F" | round == "QF") |>
  group_by(round) |>
  ggplot(aes(x = rank_difference, y = round)) + 
  geom_density_ridges(rel_min_height = 0.01) +
  labs(title = "Frequency of Upsets among WTA matches")





