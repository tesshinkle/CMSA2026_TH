#CMSA Lecture 2 code
require(ggplot2)
require(Lahman)

library(tidyverse)

theme_set(theme_bw())

yearly_batting <- Batting |>
  filter(lgID %in% c("AL", "NL")) |>
  group_by(yearID) |>
  summarize(total_h = sum(H, na.rm = TRUE),
            total_hr = sum(HR, na.rm = TRUE),
            total_so = sum(SO, na.rm = TRUE),
            total_bb = sum(BB, na.rm = TRUE),
            total_ab = sum(AB, na.rm = TRUE)) |>
  mutate(batting_avg = total_h / total_ab)

view(yearly_batting)

 #coord_cartesian() for changing limits
ggplot(data = yearly_batting, aes(x=yearID, y=total_hr)) + geom_point() +
  coord_cartesian() + geom_line() + theme_bw() +
  scale_x_continuous(limits = c(2000,2015)) + #looks at downward trend from 2000-2015
  labs(title = "Yearly Batting for the MLB", y = "Total Home Runs", x = "Year")

ggplot(yearly_batting, aes(x= yearID, y = total_hr)) + geom_point()+
  geom_line() + scale_x_reverse() +scale_y_log10() + theme_bw()

ggplot(yearly_batting, aes(x= yearID, y = total_hr)) + geom_point()+
  geom_line() + geom_smooth() + theme_bw()

#color = total strike outs and size = total walks
ggplot(yearly_batting, aes(x= yearID, y = total_hr)) +
  geom_point(aes(color = total_so, size = total_bb)) + 
  geom_line(color = "red", linetype = "dashed")


ggplot(yearly_batting, aes(x = yearID, y = total_hr)) + 
  geom_point(aes(color = total_so, size = total_bb)) +
  geom_line(color = "darkred", linetype = "dashed") +
  scale_color_gradient(low = "darkblue", high = "gold") +
  scale_size_continuous(breaks = seq(0, 20000, 2500)) + theme_bw()



ggplot(yearly_batting, aes(x = yearID, y = total_hr)) + 
  geom_point(aes(color = total_so, size = total_bb)) +
  geom_line(color = "darkred", linetype = "dashed") +
  scale_color_gradient(low = "darkblue", high = "gold") +
  labs(
    x = "Year",
    y = "Homeruns",
    color = "Strikeouts",
    size = "Walks",
    title = "The rise of three true outcomes in baseball",
    caption = "Data courtesy of Lahman"
  )


ggplot(yearly_batting, aes(x = yearID, y = total_hr)) + 
  geom_point(aes(color = total_so, size = total_bb)) +
  geom_line(color = "darkred", linetype = "dashed") +
  scale_color_gradient(low = "darkblue", high = "gold") +
  labs(
    x = "Year",
    y = "Homeruns",
    color = "Strikeouts",
    size = "Walks",
    title = "The rise of three true outcomes in baseball",
    caption = "Data courtesy of Lahman"
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold"))

#pivoting
yearly_batting |> 
  select(yearID, HRs = total_hr, Strikeouts = total_so, Walks = total_bb) |> # renaming while also selecting
  pivot_longer(HRs:Walks, # can also do !yearID (to select everything but yearID)
               names_to = "stat",
               values_to = "val")

#faceting
yearly_batting |>
  select(yearID, HRs = total_hr, 
         Strikeouts = total_so, Walks = total_bb) |>
  pivot_longer(HRs:Walks, 
               names_to = "stat", 
               values_to = "val") |>
  ggplot(aes(yearID, val)) +
  geom_line(color = "darkblue") +
  geom_point(alpha = 0.8, color = "darkblue") +
  facet_wrap(~ stat, scales = "free_y", ncol = 1) +
  labs(
    x = "Year", 
    y = "Total of statistic",
    title = "The rise of three true outcomes in baseball",
    caption = "Data courtesy of Lahman"
  ) +
  theme_bw(base_size = 20) +
  theme(strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold"))

#excersise
require(babynames)
view(babynames)
str(babynames)

tess_babynames = babynames|>
  filter(name == "Tess" , sex == "F")
head(tess_babynames)

ggplot(tess_babynames, aes(x = year, y = n)) + geom_point() + 
  geom_vline(xintercept=2006, color = "red" ,size = 0.8, linetype = "dashed") +
  labs(title = "Frequency of the name Tess throughout the Years", x = "Year", y = "Frequency")


leanne_babynames = babynames|>
  filter(name == "Leanne" , sex == "F")
head(leanne_babynames)

ggplot(leanne_babynames, aes(x = year, y = n)) + geom_point() + 
  geom_vline(xintercept=2006, color = "red" ,size = 0.8, linetype = "dashed") +
  labs(title = "Frequency of the name Leanne throughout the Years", x = "Year", y = "Frequency")


don_babynames = babynames|>
  filter(name == "Don" , sex == "M")
head(don_babynames)

ggplot(don_babynames, aes(x = year, y = n)) + geom_point() + 
  geom_vline(xintercept=2006, color = "red" ,size = 0.8, linetype = "dashed") +
  labs(title = "Frequency of the name Don throughout the Years", x = "Year", y = "Frequency")

#the name don is highly more popular than either Leanne or Tess especially in 
#the 1940s-1960s, however it has become very unpopular today. the name Tess has 
#only become popular in the late 1990s and early 2000s unlike the name Don and Leanne.
#The name Leanne became popular in the 1960s but not as popular as the name Don.

babynames |>
  filter(name %in% c("Erin", "Sara", "Lily") & sex == "F") |>
  ggplot(aes(x = year, y = n, color = name)) +
  geom_point() + geom_vline(xintercept = 2006, linetype = "dashed")
