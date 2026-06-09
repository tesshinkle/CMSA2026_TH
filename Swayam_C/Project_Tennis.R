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

wta_2021_2026_matches = wta_2021_2026_matches |>
  mutate(surface = recode(tolower(surface),
                          "Clay" = "clay")) |>
  mutate(surface = as.factor(surface))

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


wta_2021_2026_matches |>
  select(winner_name, w_1stIn, w_1stWon, w_bpSaved,w_bpFaced, loser_name, l_1stIn, l_1stWon, l_bpSaved, l_bpFaced) |>
  filter(!(w_1stIn==0 & w_1stWon==0), !( l_1stIn==0 & l_1stWon==0))|>
  mutate(
    w_1stWonPct = w_1stWon / w_1stIn,  # 
    l_1stWonPct = l_1stWon / l_1stIn,
    w_bp_save_rate = w_bpSaved / w_bpFaced,
    l_bp_save_rate = l_bpSaved / l_bpFaced
  )|>
  
  

      
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



# Does Match Length Increase Errors in Winners?

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




# Does players ranking difference affect the length of the game?

view(wta_2021_2026_matches |>
  select(surface, minutes, winner_rank, loser_rank, tourney_name, round ) |>
  mutate(rank_difference = loser_rank - winner_rank) |>
  filter(rank_difference<(-50), round==c("SF","F", "QF"))) |>
  filter(!is.na(surface), !is.na(minutes), !is.na(rank_difference), rank_difference<300, rank_difference>0, minutes>0, minutes<300) |>
  ggplot(aes(x=minutes, y= rank_difference)) +
  geom_point(color = "navy", size = 1, alpha = 0.5)+
  scale_x_continuous(labels = scales::label_comma())



# Winner's age group and minutes played

wta_2021_2026_matches |>
  filter(winner_age>0, minutes>0, minutes<250) |>
  mutate(w_agegrp= case_when(winner_age < 25 ~ "Age group less than 25", 
                             winner_age >= 25 ~ "Age group greater than 25"))|>
  ggplot(aes(x = minutes)) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(w_agegrp ~.)+
  scale_fill_viridis_d(option ="inferno")+
  theme(legend.position = "none")



# Loser's age group and minutes played

wta_2021_2026_matches |>
  filter(loser_age>0, minutes>0, minutes<250) |>
  mutate(l_agegrp= case_when(loser_age < 25 ~ "Age group less than 25", 
                             loser_age >= 25 ~ "Age group greater than 25"))|>
  ggplot(aes(x = minutes)) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(l_agegrp ~.)+
  scale_fill_viridis_d(option ="inferno")+
  theme(legend.position = "none")


table(wta_2021_2026_matches$surface)






#height and rank difference 


height_rank <- wta_2021_2026_matches |> 
  filter(!is.na(winner_ht),  !is.na(loser_ht), !is.na(winner_rank), !is.na(winner_rank),
         !(winner_rank == loser_rank), !(winner_ht == loser_ht)) |>
  mutate(category = case_when( winner_rank < loser_rank & winner_ht >loser_ht ~ "Better rank + taller",
                               winner_rank < loser_rank & winner_ht <loser_ht ~ "Better rank + shorter",
                               winner_rank > loser_rank & winner_ht >loser_ht ~ "Worst rank + taller",
                               winner_rank > loser_rank & winner_ht <loser_ht ~ "Worst rank + shorter"))


table(height_rank$category)


height_rank |>
  count(category) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = category, y = prop)) +
  geom_col(col = "skyblue", fill = "lightgreen", na.rm = TRUE) +
  labs(x= "", y = "") +
  theme(axis.text.x = element_text(
    vjust = 1, hjust = 1)) +
  coord_flip()





..................................................................................



#  EDA project Hypothesis Test-1
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

ace_data |>
  filter(total_aces<30) |>
  ggplot(aes(x = total_aces, y = surface, fill = surface)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "Distribution of Total Aces by Court Surface",
       x = "Total Aces per Match", 
       y = "Surface Type") +
  theme_minimal() +
  theme(legend.position = "none")

  
# ANOVA Test 

aov(total_aces ~ surface, data = ace_data)|>
summary()


.............................................................................................

............................................................................

# EDA project Hypothesis Test-2
# At which case does the winner have more advantage if he/she's rank,height or age?

adv <- wta_2021_2026_matches |>
  filter(!is.na(winner_rank), !is.na(loser_rank),
         !is.na(winner_ht), !is.na(loser_ht),
         !is.na(winner_age), !is.na(loser_age)) |>
  summarise(Rank = mean(winner_rank < loser_rank),
            Height = mean(winner_ht > loser_ht),
            Age = mean(winner_age > loser_age))

ggplot(data.frame(category = c("Rank", "Height", "Age"),
                  prop = c(adv$Rank, adv$Height, adv$Age)),
       aes(x = category, y = prop)) +
  geom_col(fill="skyblue", col="blue", na.rm = TRUE) +
  coord_flip() +
  theme_minimal() +
  labs(title = "How Often Does the Winner Have the Advantage?",
       x = "", y = "Probability")



# Do height and age matter more in close matches?


wta_2021_2026_matches |>
  filter(!is.na(winner_rank), !is.na(loser_rank),
    !is.na(winner_ht), !is.na(loser_ht),
    !is.na(winner_age), !is.na(loser_age) ) |>
  
  mutate(rank_df = abs(winner_rank - loser_rank),
    rank_grp = case_when(rank_df <= 10 ~ "0–10",
                         rank_df <= 25 ~ "11–25",
                         rank_df <= 50 ~ "26–50",
                         rank_df >50 ~ "50+"),
    height_adv = winner_ht > loser_ht,
    age_adv    = winner_age > loser_age)  |>
  
  group_by(rank_grp) |>
  summarise(height_adv = mean(height_adv),
            age_adv = mean(age_adv))  |>
  
  pivot_longer(c(height_adv, age_adv),
               names_to = "variable",
               values_to = "prob")|>
  
  ggplot(aes(rank_grp, prob, color = variable, group = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(x = "Rank difference group",
       y = "Probability winner has advantage",
       title = "Do height and age matter more in close matches?")

.......................................................................


