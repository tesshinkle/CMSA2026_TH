#CMSA Lecture 20 code
#using liverpool FC men's injury data from the 2017-2018 and 2018-2019 seasons
#also use the injurytools package

library(tidyverse)

injury_data = read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/liverpool_injury_data_17-19.csv") |>
  mutate(positionb = str_split_i(position, "_", 1)) |>
  filter(positionb != "Goalkeeper") 
#glimpse(injury_data)

library(survival) #do a survival analysis, fitting the Kaplan-meier curves
#gives two curves, one for each season
#median is the median days until injury
km_fit = survfit(Surv(tstop_day, status) ~ seasonb,
                  data = injury_data) 
km_fit

library(survminer) #plotting the kaplan-meier curves
ggsurvplot(km_fit,  palette = c("#E7B800", "#2E9FDF"),
           surv.median.line = "hv", #provides the horizontal and vertical location of the median
           xlab = "Time (in calendar days)") 

library(gtsummary)
injury_data_sub = injury_data |> filter(season == "18-19") 
#looking at just the 18-19 data to ensure that players are only counted once

cfit = coxph(Surv(tstop_day, status) ~ positionb + age + yellows,
              data = injury_data_sub) #fitting a cox PH model

tbl_regression(cfit, exponentiate = TRUE) #so we don't get the log hazard ratios
#for liverpool's FC men's team, midfielders were estimated to have 3.79 times the 
#hazard of injury with attackers in the 17-18 and 18-19 seasons holding all other 
#model variables constant

#Schoenfeld residual diagnostics
#we fail to reject that the variables are constant over time
ggcoxzph(cox.zph(cfit))

ggsurvplot(survfit(Surv(tstop_day, status) ~ positionb,
                   data = injury_data_sub), fun="cloglog",
           title = "Complementary Log-Log") #transformed version of the survival curve

#fitting a model with time-dependent covariate
library(broom)
cfit_new = coxph(Surv(tstop_day, status) ~ positionb + age + tt(age) + yellows,
                  data = injury_data_sub, tt = function(x, t, ...) x * t)
# tidy(cfit_new, exponentiate = TRUE) #values not statistically significant

#diagnosing non-linearity, using martingale residuals
ggcoxdiagnostics(cfit, type = "martingale") + theme_light(base_size = 20)

#computing the concordance index
#index = 0.63, not the best but not the worst, a good fit would be >=0.7
concordance(cfit) #glance(cfit), # could also compute on held-out data
