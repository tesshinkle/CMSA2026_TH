#CMSA lecture 19 code
#Advanced factor analysis
#using decathlon data 

library(tidyverse)
ggplot2::theme_set(ggplot2::theme_light(base_size = 20))

dec_performances = read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/dec_performances.csv")
decathlon = dec_performances |>
  dplyr::select(men_100, men_lj, men_sp, men_hj, men_400, men_110h, men_dt, men_pv, men_jt, men_1500) |>
  separate(men_1500, into = c("min", "sec", "remove"), sep = ":", convert = TRUE) %>%
  mutate(men_1500 = min * 60 + sec) |>
  select(-min, -sec, -remove) 

#exploring event similarities
round_cor_matrix =  round(cor(decathlon), 2) #get the full correlation matrix

library(ggcorrplot)
#correlation matrix heat map
ggcorrplot(round_cor_matrix, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

#fitting factor model
library(psych)
fit = fa(decathlon)
#h2 is similar to an r^2
#u2 is variance that is unexplained by the common factors

#scree plot that helps choose the number of factors to use
#use two factors for the decathlon data
#vss tells us that the Velicer MAP achieves a minimum using 2 factors
#however we could use 1 to 3 factors
scree(decathlon) #vss(decathlon)


#factor rotation
fit_varimax = fa(decathlon, nfactors = 2, rotate = "varimax")
fit_oblimin = fa(decathlon, nfactors = 2, rotate = "oblimin") 
#two factors explain 55% of variance of men_100 for both rotations

fit_varimax$Phi #uncorrelated
fit_oblimin$Phi #shows how correlated the MR1 and MR2 factors are


#factor scores
fit$scores |> as.data.frame() |>
  ggplot(aes(x=MR1))+
  geom_histogram(col = "blue", fill = "lightblue", alpha = 0.7)

cbind(decathlon, fit$scores) |>
  ggplot(aes(men_100, MR1)) +
  geom_point(alpha = 0.5)
