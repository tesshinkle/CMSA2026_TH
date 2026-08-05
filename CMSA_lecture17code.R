#CMSA lecture 17 code
#Decision trees
#working with hockey data

library(tidyverse)
theme_set(theme_bw())
nhl_goals = read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/nhl-shots-2021.csv") 

nhl_goals = nhl_goals|> 
  mutate(is_goal = as.factor(case_when(shot_outcome == "GOAL" ~ 1, 
                                       TRUE ~ 0)),
         period = as.factor(period)) |>
  dplyr::select(period, strength_code, shot_distance, shot_angle, is_goal) |>
  filter(!is.na(shot_distance), !is.na(shot_angle), !is.na(strength_code)) 

glimpse(nhl_goals)

table(nhl_goals$period)
table(nhl_goals$strength_code)

#how to build regression tree
#rpart is short for recursive partitioning
#tuneLength parameter tries 20 different values of the complexity parameter to 
#determine which works best
library(caret)
set.seed(10)
goal_tree = train(is_goal ~ ., method = "rpart", tuneLength = 20,
                   trControl = trainControl(method = "cv", number = 10),
                   data = nhl_goals)
# str(goal_tree)
ggplot(goal_tree)

#gives the tree plot
library(rpart.plot)
goal_tree |> 
  pluck("finalModel") |> 
  rpart.plot()

#gives bar plot of variables and importance
#shot_distance and shot_angle appear to be the most important 
library(vip)
goal_tree |> 
  vip()

#treemap for visualizing categorical data
library(treemapify)
penguins |>
  count(species, island) |>
  ggplot(aes(area = n, subgroup = island, label = species,
             fill = interaction(species, island))) +
  # draw species borders and fill colors
  geom_treemap() +
  # draw island borders
  geom_treemap_subgroup_border() + #creates borders around island distinction
  # print island text
  geom_treemap_subgroup_text(
    place = "center", grow = TRUE, alpha = 0.5, #grow = TRUE makes the labels fit in the box
    color = "black", fontface = "italic", min.size = 0
  )+
  # print species text
  geom_treemap_text(color = "white", place = "topleft", 
                    reflow = TRUE) +
  guides(color = "none", fill = "none") #removes legend
