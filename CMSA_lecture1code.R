require(tidyverse)
require(Lahman)
require(gt)
require(dplyr)
#require(mosaic) maybe for favstats

Batting <- as_tibble(Batting)
view(Batting)

dim(Batting)
nrow(Batting)
ncol(Batting)

head(Batting)

str(Batting)

summary(Batting)
summary(Batting$G)

table(Batting$lgID)

#filtering

mlb_batting = filter(Batting, lgID %in% c("AL", "NL")) 
# or filter(Batting, lgID == "AL" | lgID == "NL")
nrow(Batting) - nrow(mlb_batting) # difference in row counts

view(mlb_batting)
summary(mlb_batting)

# filtering missing values
#filter(Batting, !is.na(IBB))
#Batting |>
#  drop.na()

# multiple conditions (pirates year 2022)
pirates_batting = filter(Batting, yearID == 2022 & teamID == "PIT")
head(pirates_batting, n = 3) # difference in row counts


select(Batting, playerID, G, AB, R, H, HR, BB)

select(Batting, contains("ID"), G, AB, R, H, HR, BB)


#mutating
new_batting <- mutate(Batting, batting_avg = H / AB, so_bb_ratio = SO / BB)
head(new_batting, n = 5)

#arrange
hr_batting <- arrange(Batting, desc(HR)) # desc() for descending order
head(hr_batting, n = 3)

arrange(Batting, desc(AB), HR)

#pipe operator
Batting |> 
  filter(yearID == 2022, teamID == "PIT", AB >= 50) |> 
  mutate(batting_avg = H / AB) |> 
  arrange(desc(batting_avg)) |> 
  select(playerID, AB, batting_avg)

#summarize()
Batting |> 
  summarize(median_at_bats = median(AB))
Batting |> 
  summarize(cor_ab_hr = cor(AB, HR))

favstats(~AB,data = Batting)


Batting |> 
  filter(yearID %in% 2015:2019) |> 
  group_by(teamID) |> 
  summarize(total_hr = sum(HR),
            total_so = sum(SO),
            total_bb = sum(BB)) |> 
  arrange(desc(total_hr))

Batting |> 
  count(lgID, name = "freq")

# note: count is a "shortcut" of this
Batting |> 
  group_by(lgID) |> 
  summarize(freq = n()) |> 
  ungroup()

Batting |> 
  slice(c(1, 99, 101, 500)) #sliced by row index

tail(Batting)

# single-season home run record (top 5)
Batting |> 
  slice_max(HR, n = 5)

yearly_batting = Batting |>
  filter(lgID %in% c("AL", "NL")) |>
  group_by(yearID) |>
  summarize(total_hits = sum(H, na.rm = TRUE), #total hits
            total_hr = sum(HR, na.rm = TRUE), #total home runs
            total_so = sum(SO, na.rm = TRUE), #total strikeouts
            total_bb = sum(BB, na.rm = TRUE), #total walks
            total_ab = sum(AB, na.rm = TRUE)) |> #total at bats
  mutate(batting_avg = total_hits / total_ab)
yearly_batting

yearly_batting|>
  arrange(desc(total_hr))|>
  slice(1:3)

yearly_batting |>
  mutate(so_bb_ratio = total_so/total_bb) |>
  select(-batting_avg) |>
  arrange(so_bb_ratio) |>
  slice(c(1,n()))


yearly_batting |>
  select(yearID, batting_avg) |>
  rename(Year = yearID, `Batting Average` = batting_avg) |>
  arrange(desc(`Batting Average`)) |>
  slice(c(1:3, (n()-2):n())) |>
  gt() |>
  fmt_number(columns = `Batting Average`, decimals = 3)

yearly_batting |>
  select(yearID, batting_avg) |>
  rename(Year = yearID,
         `Batting Average` = batting_avg) |>
  arrange(desc(`Batting Average`)) |>
  slice(c(1:3, (n()-2):n())) |>
  gt(rowname_col = "Year") |>
  # add table header
  tab_header(title = "Best / Worst MLB Seasons by Batting Average") |>
  # create row groups by location 
  tab_row_group(
    label = "Bottom 3 Years",
    rows = 4:6
  ) |>
  tab_row_group(
    label = "Top 3 Years",
    rows = 1:3
  ) |>
  # recolor row group labels
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_row_groups(groups = "Bottom 3 Years")
  ) |>
  tab_style(
    style = cell_fill(color = "lightpink"),
    locations = cells_row_groups(groups = "Top 3 Years")) |>
  # round "Batting Average" to 3 decimals 
  fmt_number(columns = `Batting Average`, decimals = 3)
