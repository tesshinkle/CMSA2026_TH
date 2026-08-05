#CMSA Lecture 13 Code
#linear discriminant analysis
#maximizes the component axes for class-separation
#first axes provides the greatest separation among the classes relative to within class variation

#trying to classify what pitch the guy is throwing
#pitch type is outcome variable
ohtani = read_csv("https://raw.githubusercontent.com/36-SURE/2026/main/data/ohtani.csv")
theme_set(theme_bw())
str(ohtani)

#bar plot of pitch type and proportions
ohtani |> count(pitch_type) |> mutate(prop = n/sum(n)) |>
  ggplot(aes(x=fct_reorder(pitch_type, -prop), y = prop))+ #reordering of bars/cols
  geom_col(col = "blue", fill = "lightblue")+
  labs(x = "Pitch type")+
  theme_bw(base_size = 20)

#boxplot to show different pitches and the speed of each pitch
ohtani |>
  ggplot(aes(x=release_speed, y=pitch_type, color=pitch_type))+
  geom_boxplot()+
  labs(x="Release speed (mph)", y="")+
  theme_light(base_size = 20)+
  theme(legend.position = "none") #getting rid of legend

ohtani_subset = ohtani |> dplyr::select(pitch_type, release_speed, release_pos_x, release_pos_y, release_pos_z)
str(ohtani_subset)

#install.packages("rsample")
#create train and testing data
library(rsample)

set.seed(623)
#give data, 3/4th data is training data
data_split = initial_split(ohtani_subset, prop = 0.75, strata = pitch_type)
train = training(data_split)
test = testing(data_split)

#checking multivariate normality assumption
train |>
  filter(pitch_type == "FF") |> 
  pivot_longer(cols = c(release_speed:release_pos_z), names_to = "measure", values_to = "value") |>
  ggplot(aes(x=value, fill = measure))+
  geom_histogram(col = "black")+
  facet_wrap(~measure, scale = "free_x")+
  theme_bw(base_size = 20)+
  theme(legend.position = "none")

#checking constant covariance assumption
#covariance matrix for fastballs and splitters(?)
cov_ff_actual = cov(subset(train, pitch_type == "FF", select = c(release_speed:release_pos_z)))
cov_st_actual = cov(subset(train, pitch_type == "ST", select = c(release_speed:release_pos_z)))

cov_ff_actual
cov_st_actual

#comparing covariance matrices
#tells us that the covariance matrices might differ
#with large data sets differeing matrices isn't really meaningful
lirary (biotools)
train_sub <- train |> filter(pitch_type %in% c("FF", "ST"))
biotools::boxM(train_sub[, c("release_speed", "release_pos_x", "release_pos_y", "release_pos_z")], train_sub$pitch_type)

#fitting the lda
library(MASS)
lda_mod = lda(pitch_type~., train)
lda_mod #gives prior probabilities

#plotting the lda
prop = lda_mod$svd^2 / sum(lda_mod$svd^2)
percent = round(100 * prop, 1)

scores = predict(lda_mod)$x
scores_df = data.frame(LD1 = scores[, 1], LD2 = scores[, 2], pitch_type = train$pitch_type)

ggplot(scores_df, aes(x = LD1, y = LD2, color = pitch_type)) +
  geom_point(alpha = 0.5) +
  theme_bw()+
  labs(x = paste0("LD1 (", percent[1], "%)"),
       y = paste0("LD2 (", percent[2], "%)"))+
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

#predicting with the lda
test_preds = predict(lda_mod, test)$class

mean(test_preds == test$pitch_type)
#confusion matrix comparing predicted vector to actual
#can see where miss-classifications are occurring
#want all numbers to be within the diagonal
table(Predicted = test_preds, Actual = test$pitch_type)

#correlation of each variable with LD scores
cor(train |> dplyr::select(-pitch_type), scores)
