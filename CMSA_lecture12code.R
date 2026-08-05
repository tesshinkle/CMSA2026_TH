#CMSA Lecture 12 Code
#dimension reduction: principal component analysis
#focus on reducing the demensionality (no. of columns) while retaining the most information

#looking at the national women's soccer league from 2016-2022 with 2020 being cancelled

library(tidyverse)
theme_set(theme_bw())
nwsl_team_stats = read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/nwsl-team-stats.csv")
glimpse(nwsl_team_stats)

nwsl_team_feat = nwsl_team_stats |> 
  select(cross_accuracy:tackle_success_pct) # extracting relevant metrics

#Principal Component Analysis----
#prcomp() is the function to run PCA, 
#specify centering and scaling to standardize
nwsl_team_pca = prcomp(nwsl_team_feat, center = TRUE, scale. = TRUE) 
summary(nwsl_team_pca) #gives importance of each component (PCj) look at prop of variance

nwsl_team_pc_matrix = nwsl_team_pca$x
head(as.data.frame(nwsl_team_pc_matrix))
cov(nwsl_team_pc_matrix) 
#diagonals are in descending order and shows that the PCs are uncorrelated

as.data.frame(nwsl_team_pca$rotation) |> rownames_to_column("statistic")

#creates PC scores for each observation of the nwsl
nwsl_team_stats = nwsl_team_stats |> 
  mutate(pc1 = nwsl_team_pc_matrix[,1], #gives first row
         pc2 = nwsl_team_pc_matrix[,2])

#plotting the PC1 and PC2 and labeling the proportion of variance explained by the PCj
nwsl_team_stats |> 
  ggplot(aes(x = pc1, y = pc2)) +
  geom_point(alpha = 0.5) +
  labs(x = "PC1 (38.5%) ", y = "PC2 (25.7%)")

library(factoextra)
# fviz_pca_var(): projection of variables
# fviz_pca_ind(): display observations with first two PCs

#creating a biplot: displays space of observations and space of variables
#look at direction, length, and angle of each arrow
#arrow direction(shot_accuracy):as the percentage of shots on target increase, PC1 and PC2 increase
#arrow angle: variables are positively correlated (shot_accuracy & goal_conversion_pct)
#arrow length: cross_accuracy has relatively small coefficeints in the matrix
nwsl_team_pca |> 
  fviz_pca_biplot(label = "var",
                  alpha.ind = 0.25,
                  alpha.var = 0.75,
                  labelsize = 5,
                  col.var = "darkblue",
                  repel = TRUE)

summary(nwsl_team_pca)

#Scree plot
#look at the proportion of variance explained by each PC
#y-int shows to use 3 PCs
nwsl_team_pca |> 
  fviz_eig(addlabels = TRUE) +
  geom_hline(
    yintercept = 100 * (1 / ncol(nwsl_team_pca$x)), #y-int to determine how many PCs to use
    linetype = "dashed", 
    color = "green",
  )


#Principal Component Regression----
nwsl_model_data = nwsl_team_stats |>
  select(-team_name, -season, -games_played, -goals, -goals_conceded)

# goal_differential will be our outcome (we will talk soon about how to model count data)

library(pls)
#function pcr() specify scale and center to standardize and number of PC you want to use
#pcr() for principal component regression
nwsl_pcr_fit = pcr(goal_differential ~ ., ncomp = 2,
                    center = TRUE, scale = TRUE, data = nwsl_model_data)
summary(nwsl_pcr_fit)

set.seed(0622)
library(caret)

cv_model_pcr = train(
  goal_differential ~ ., 
  data = nwsl_model_data, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("center", "scale"),
  tuneLength = ncol(nwsl_model_data) - 1)

ggplot(cv_model_pcr) 

summary(cv_model_pcr$finalModel) #best number of PC is 6

set.seed(0622)

cv_model_pcr_onese = train(
  goal_differential ~ ., 
  data = nwsl_model_data, 
  method = "pcr",
  trControl = 
    trainControl(method = "cv", number = 10,
                 selectionFunction = "oneSE"),
  preProcess = c("center", "scale"),
  tuneLength = ncol(nwsl_model_data) - 1)

summary(cv_model_pcr_onese$finalModel)

#Partial Least Squares----
set.seed(0622)

cv_model_pls = train(
  goal_differential ~ ., 
  data = nwsl_model_data, 
  method = "pls",
  trControl = 
    trainControl(method = "cv", number = 10,
                 selectionFunction = "oneSE"), 
  preProcess = c("center", "scale"),
  tuneLength = 6)

ggplot(cv_model_pls)

# alternative with vip
#library(vip) #install.packages("vip")
#vip::vip(cv_model_pls$finalModel) 

as.data.frame(varImp(cv_model_pls)$importance) |>
  rownames_to_column("statistic") |>
  filter(!grepl("pc1|pc2", statistic)) |>
  ggplot(aes(x = Overall, y = fct_reorder(statistic, Overall))) +
  geom_col() +
  labs(x = "Overall importance", y = "Statistic")

library(pdp) #install.packages("pdp")
partial(cv_model_pls, "goal_conversion_pct", plot = TRUE)

# str(nwsl_team_pca)
library(broom)
tidy(nwsl_team_pca, matrix = "eigenvalues") # equivalent to nwsl_team_pca$sdev
tidy(nwsl_team_pca, matrix = "rotation") # equivalent to nwsl_team_pca$rotation
tidy(nwsl_team_pca, matrix = "scores") # equivalent to nwsl_team_pca$x

nwsl_team_pca |>
  tidy(matrix = "eigenvalues") |>
  ggplot(aes(x = PC, y = percent)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 1 / ncol(nwsl_team_feat),
             color = "darkred", linetype = "dashed") +
  scale_x_continuous(breaks = 1:ncol(nwsl_team_pca$x)) +
  scale_y_continuous(labels = scales:: label_percent())

nwsl_team_pca |>
  tidy(matrix = "eigenvalues") |>
  ggplot(aes(x = PC, y = cumulative)) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = 1:ncol(nwsl_team_pca$x)) +
  scale_y_continuous(labels = scales:: label_percent())

nwsl_team_pca |> 
  fviz_eig(addlabels = TRUE) +
  geom_hline(
    yintercept = 100 * (1 / ncol(nwsl_team_pca$x)), 
    linetype = "dashed", 
    color = "darkred",
  )
