#CMSA Lecture 3
#Data Visualization: categorical data
library(tidyverse)
require(janitor)
library(gt) # for nicely outputted tables

theme_set(theme_bw()) # setting the ggplot theme

# set seed for reproducibility
set.seed(4747)

# import data from current working directory
tennis_shots = read_csv("tennis-w-shots-wim.csv.gz") |>
  # tidy names
  janitor::clean_names() |>
  # remove serves
  filter(shot_type != "serve") |>
  # NA = shot did not produce an outcome (i.e., returned)
  mutate(outcome_type = if_else(is.na(outcome_type),"returned", outcome_type)) |>
  # replace "_" with " " in all character variables 
  mutate(across(where(is.character), ~ str_replace_all(., "_", " "))) |>
  # convert date into ymd format
  mutate(date = ymd(date)) |>
  # random sample of sample 10,000 observations
  dplyr::slice_sample(n = 10000)

head(tennis_shots)
view(tennis_shots)
str(tennis_shots)
summary(tennis_shots)



tennis_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/tennis_w_shots_wim.csv") |>
  # filter out NAs for shot_type and outcome_type
  filter(!is.na(shot_type), !is.na(outcome_type)) |>
  # convert characters into factors
  mutate(across(where(is.character), as.factor)) #mutate anytime variable is a character into a factor
head(tennis_shots)
view(tennis_shots)

names(tennis_shots) #gives column names

class(tennis_shots$shot_type) #tells that its a factor
levels(tennis_shots$shot_type) #gives the different factor levels

table(tennis_shots$shot_type)
#tennis_shots |> 
#  group_by(shot_type) |>
#  summarize(n = n(), .groups = "drop")

tennis_shots |> 
  count(shot_type) |>
  arrange(desc(n))

prop.table(table(tennis_shots$shot_type))

tennis_shots |> 
  count(shot_type) |> 
  mutate(prop = n / sum(n))

 
ggplot(tennis_shots,aes(x = shot_type)) +
  geom_bar(fill = "blue") +
  # angling axis text for readability
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, hjust = 1))

#gives the same as geom_bar
tennis_shots |>
  count(shot_type, name = "count") |> 
  ggplot(aes(x = shot_type, y = count)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, hjust = 1))

tennis_shots |> 
  ggplot(aes(y = shot_type)) +
  geom_bar(fill = "blue") 

tennis_shots |> 
  ggplot(aes(x = shot_type)) +
  geom_bar(fill = "blue") +
  coord_flip() 

#provides the precise number for each bar as well as the proportion instead of counts
tennis_shots |> 
  count(shot_type) |> 
  mutate(prop = n / sum(n)) |> 
  ggplot(aes(x = prop, y = shot_type)) +
  geom_col()  + geom_label(aes(label = n), hjust = 1)

#provides standard error bars
tennis_shots |> 
  count(shot_type) |> 
  mutate(prop = n / sum(n),
         se = sqrt(prop * (1 - prop) / sum(n)),
         lower = prop - 2 * se,
         upper = prop + 2 * se) |> 
  ggplot(aes(x = prop, y = shot_type)) +
  geom_col() +
  geom_errorbar(aes(xmin = lower, xmax = upper), 
                color = "blue", 
                width = 0.2, 
                linewidth = 1)

#ordering bar chart based off of length of shot type
tennis_shots |> 
  count(shot_type) |> 
  mutate(prop = n / sum(n),
         se = sqrt(prop * (1 - prop) / sum(n)),
         lower = prop - 2 * se,
         upper = prop + 2 * se,
         shot_type = fct_reorder(shot_type, prop)) |> 
  ggplot(aes(x = prop, y = shot_type)) +
  geom_col() +
  geom_errorbar(aes(xmin = lower, xmax = upper), 
                color = "blue", width = 0.2, linewidth = 1)


#chi-square test
#checking to see if the shot types all have the same proportions
chisq.test(table(tennis_shots$shot_type))
#reject null hypothesis, at least one category has a different proportion

table(tennis_shots$shot_type)

table(tennis_shots$outcome_type)

tennis_shots <- mutate(tennis_shots, outcome_type = fct_relevel(outcome_type, "unforced error"))
table(tennis_shots$outcome_type)
#fct_relevel is for factor relevel, place the named level as the first level

table("Shot type" = tennis_shots$shot_type, 
      "Outcome type" = tennis_shots$outcome_type)

xtabs(~ shot_type + outcome_type, data = tennis_shots)

tennis_shots |>
  count(shot_type, outcome_type) |>
  ggplot(aes(x = shot_type, y = n, fill = outcome_type)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, hjust = 1))

tennis_shots |>
  count(shot_type, outcome_type) |>
  ggplot(aes(x = shot_type, y = n,
             fill = outcome_type)) +
  geom_col(position = "fill") + #changes from count to proportion
  labs(y = "prop") +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, hjust = 1))

tennis_shots |>
  count(shot_type, outcome_type) |>
  ggplot(aes(x = shot_type, y = n,
             fill = outcome_type)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, hjust = 1))

tennis_shots |>
  group_by(shot_type, outcome_type) |>
  summarize(joint = n() / nrow(tennis_shots)) |>
  pivot_wider(names_from = outcome_type, values_from = joint, values_fill = 0)

# The base `R` way to get a two-way proportion table
tennis_shots |> 
  select(shot_type, outcome_type) |> 
  table() |> 
  prop.table()



tennis_shots |>
  group_by(shot_type, outcome_type) |>
  summarize(
    freq = n(), 
    joint = n() / nrow(tennis_shots)
  ) |> 
  ggplot(aes(x = shot_type, y = outcome_type)) +
  geom_tile(aes(fill = freq), color = "white") + #creates heat map
  geom_text(aes(label = scales::percent(joint, accuracy = 0.01))) +
  scale_fill_gradient2() +
  coord_equal() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, hjust = 1))

tennis_shots |> 
  select(shot_type, outcome_type) |> 
  table() |> 
  mosaicplot(main = "Relationship between shot type and outcome type",
             las=2, cex.axis = 1.2, cex.lab = 1.5, cex.main = 2) 

devtools::install_github("haleyjeppson/ggmosaic") #NOT WORKING
library(ggmosaic)
tennis_shots |> 
  ggplot() +
  geom_mosaic(aes(x = product(outcome_type, shot_type),
                  fill = outcome_type)) +
  theme_classic() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,
                                   vjust = 1, hjust = 1))


tennis_shots |>
  filter(!(round %in% c("Q1", "Q2", "Q3"))) |>
  mutate(round_grouped = case_when(
    round == "F" ~ "Finals",
    round == "SF" ~ "Semi-finals",
    round == "QF" ~ "Quarter-finals",
    TRUE ~ "Early rounds")) |>
  mutate(round_grouped = fct_relevel(round_grouped, "Early rounds", "Quarter-finals", "Semi-finals", "Finals")) |>
  ggplot(aes(x = shot_type, fill = outcome_type)) + 
  geom_bar(position = "fill") + # could remove position = "fill" to get raw counts
  labs(x = "Shot type", y = "Proportion", fill = "Outcome Type") +
  facet_wrap(~ round_grouped, ncol = 4) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

chisq.test(table(tennis_shots$outcome_type, tennis_shots$shot_type))

tennis_shots |> 
  select(outcome_type, shot_type) |> 
  table() |> 
  chisq.test()

# before
iris |> 
  head()
# after
#library(janitor)
iris |> 
  clean_names() |> 
  head()


tennis_shots |> 
  tabyl(shot_type) |>
  gt()

tennis_shots |> 
  tabyl(shot_type, outcome_type) |> 
  adorn_percentages("row") |> 
  adorn_pct_formatting(digits = 2) |> 
  adorn_ns() |>
  gt()
