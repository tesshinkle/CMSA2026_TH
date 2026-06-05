library(tidyverse)


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


no_of_games_played <- wta_2021_2026_matches |>
  group_by(winner_name) |>
  count(winner_name) |>
  arrange(desc(n))



wta_2021_2026_matches |>
  select(w_1stIn, w_1stWon) |>
  filter(w_1stIn <200) |>
  ggplot(aes(x=w_1stIn, y=w_1stWon)) +
  geom_point()

# very weird 
view (wta_2021_2026_matches |>
        filter(w_1stWon>w_1stIn))

# clay vs Clay


wta_2021_2026_matches |>
  select(winner_name, w_1stIn, w_1stWon, w_bpSaved,w_bpFaced, loser_name, l_1stIn, l_1stWon, l_bpSaved, l_bpFaced) |>
  filter(!(w_1stIn==0 & w_1stWon==0), !( l_1stIn==0 & l_1stWon==0))|>
  mutate(
    w_1stWonPct = w_1stWon / w_1stIn,  # 
    l_1stWonPct = l_1stWon / l_1stIn,
    w_bp_save_rate = w_bpSaved / w_bpFaced,
    l_bp_save_rate = l_bpSaved / l_bpFaced
  )|>
  
  
  wta_2021_2026_matches |>
  filter(!is.na(surface)) |>
  mutate(total_aces = w_ace + l_ace) |>
  filter(total_aces<60) |>
  ggplot(aes(x = surface, y = total_aces, fill = surface)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "Distribution of Total Aces by Court Surface",
       x = "Surface Type", 
       y = "Total Aces per Match") +
  theme_minimal() +
  theme(legend.position = "none")

wta_2021_2026_matches|>
  ggplot(aes(x = winner_ht, y = w_ace)) +
  geom_point(alpha = 0.3, color = "darkblue", na.rm = TRUE) +
  geom_smooth(color = "red", na.rm = TRUE) +
  labs(title = "Winner's Height vs. Aces Served",
       x = "Winner Height (cm)", 
       y = "Aces Served by Winner") +
  theme_minimal()


wta_2021_2026_matches |>
  filter(winner)

table(wta_2021_2026_matches$surface)

wta_2021_2026_matches|>
  filter(!is.na(minutes), w_SvGms > 0, minutes>0, minutes<250, w_df<30 ) |>
  # mutate(w_df_rate = w_df / w_SvGms) |>
  ggplot(aes(x = minutes, y = w_df)) +
  geom_point(alpha = 0.2, color = "purple", na.rm = TRUE) +
  geom_smooth(color = "black") +
  labs(title = "Does Match Length Increase Errors in Winners?",
       x = "Match Duration (Minutes)",
       y = "Winner's Double Faults") +
  theme_minimal()


#winner's age and minutes played

wta_2021_2026_matches |>
  filter(winner_age>0) |>
  mutate(w_agegrp= case_when(winner_age <= 25 ~ "<25", 
                             winner_age > 25 ~ ">25"))|>
  ggplot(aes(x = minutes)) +
  geom_histogram(alpha = 0.5) +
  facet_grid(w_agegrp ~.)+
  scale_fill_viridis_d(option ="inferno")+
  theme(legend.position = "none")







