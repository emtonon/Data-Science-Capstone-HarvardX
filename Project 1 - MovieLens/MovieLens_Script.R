#################### HarvardX ####################
#################### About ####################
# Data Science - Capstone - MovieLens
# Author: Erik Martins Tonon
# Date: September 2022

#################### Packages ####################
if (!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(ggplot2))
  install.packages("ggplot2")
if (!require(knitr))
  install.packages("knitr")
if (!require(kableExtra))
  install.packages("kableExtra")
if (!require(dplyr))
  install.packages("dplyr")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(knitr)
library(kableExtra)
library(dplyr)

#################### Data Sets ####################
# Create edx set, validation set (final hold-out test set)
# Note: this process could take a couple of minutes

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip",
              dl)

ratings <-
  fread(
    text = gsub("::", "\t", readLines(unzip(
      dl, "ml-10M100K/ratings.dat"
    ))),
    col.names = c("userId", "movieId", "rating", "timestamp")
  )

movies <-
  str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))

# if using R 4.0 or later:
movies <-
  as.data.frame(movies) %>% mutate(
    movieId = as.numeric(movieId),
    title = as.character(title),
    genres = as.character(genres)
  )

movielens <- left_join(ratings, movies, by = "movieId")

#################### edX / Validation ####################
# Validation set will be 10% of MovieLens data
set.seed(1) # if using R 3.5 or earlier, use `set.seed(1)`
test_index <-
  createDataPartition(
    y = movielens$rating,
    times = 1,
    p = 0.1,
    list = FALSE
  )
edx <- movielens[-test_index, ]
temp <- movielens[test_index, ]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Removing unused objects
rm(dl, ratings, movies, test_index, temp, movielens, removed)
# Cleaning up memory
gc()

#################### Functions ####################
#Converting Epoch date to Human Date
to_date <- function(x) {
  as.Date(as.POSIXct(x, origin = "1970-01-01"))
}

get_year <- function(x) {
  format(as.Date(as.POSIXct(x, origin = "1970-01-01")), "%Y")
}

#################### Data set Mutation ####################

#Split train & test set
set.seed(1)
test_index <-
  createDataPartition(
    y = edx$rating,
    times = 1,
    p = 0.1,
    list = FALSE
  )
train_set <- edx[-test_index, ]
temp <- edx[test_index, ]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed)

##  Review Year mutation
train_set <- train_set %>%
  mutate(review_year = get_year(timestamp))

test_set <- test_set %>%
  mutate(review_year = get_year(timestamp))

##  Review Year (edx vs validation)
edx <- edx %>%
  mutate(review_year = get_year(timestamp))

validation <- validation %>%
  mutate(review_year = get_year(timestamp))

#################### Exploratory Analysis ####################


first10 <- head(edx, 10) %>%
  select(userId, movieId, rating, timestamp, title, genres)

knitr::kable(first10, caption = "First 10 rows of edx dataset.",
             booktabs = T) %>%
  kable_styling(
    latex_options = c("striped", "HOLD_position", "scale_down"),
    position = "center"
  )

####################  Users ####################

hist_users <- edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 25,
                 color = I("black"),
                 fill = "darkseagreen4") +
  scale_x_log10() +
  labs(x = "Users",
       y = "Number of ratings")

hist_users

####################  Movies ####################

hist_movies <- edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 25,
                 color = I("black"),
                 fill = "darkseagreen4") +
  scale_x_log10() +
  labs(x = "Movies",
       y = "Number of ratings")

hist_movies


avg_movies <- edx %>%
  group_by(movieId) %>%
  summarise(avg = sum(rating / n())) %>%
  ggplot(aes(avg)) +
  geom_histogram(bins = 25,
                 color = I("black"),
                 fill = "darkseagreen4") +
  labs(x = "Average Rating",
       y = "Number of Movies")

avg_movies

####################  Movie Genres ####################

movie_genres <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(count = n(), rating = round(mean(rating), 2)) %>%
  arrange(desc(count))


knitr::kable(head(movie_genres, 3),
             caption = "Most Rated",
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")


knitr::kable(tail(movie_genres, 3),
             caption = "Least Rated",
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")

high_rates <- movie_genres %>%
  arrange(desc(rating)) %>% head(., 10)

knitr::kable(high_rates, caption = "Highest rates",
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")

####################  Movie Title ####################

top_10_rated <- edx %>%
  group_by(title) %>%
  summarise(n = n()) %>%
  slice_max(n, n = 10)

knitr::kable(top_10_rated, caption = "Top 10 Most Rated Movies",
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")


#################### Rating ####################

hist_rating <- edx %>%
  ggplot(aes(rating)) +
  geom_histogram(bins = 25,
                 color = I("black"),
                 fill = "darkseagreen4") +
  labs(x = "Ratings", y = "Count", fill = element_blank())

hist_rating

#################### Review Date ####################

hist_date_review <- edx %>%
  ggplot(aes(get_year(timestamp))) +
  geom_bar(color = I("black"), fill = "darkseagreen4") +
  labs(x = "Year", y = "Count", fill = element_blank())

hist_date_review


rm(hist_date_review,
   hist_movies,
   hist_rating,
   hist_users,
   movie_genres)

#################### Methods ####################

#################### Naive Recommendation ####################

rmse_results <- data.frame(Method = "Objective", RMSE = "0.86490")


knitr::kable(rmse_results,
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")


mu_hat <- mean(train_set$rating)

naive_rmse <- round(RMSE(train_set$rating, mu_hat), 5)

rmse_results <- rmse_results %>%
  rbind(c("Simple Average", naive_rmse))

knitr::kable(rmse_results,
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")

#################### Movie Effect (b_i) ####################

#Dropping hat to represent estimates
mu <- mean(train_set$rating)

b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

pred_ratings <- mu + test_set %>%
  left_join(b_i, by = "movieId") %>%
  pull(b_i)

b_i_rmse <-
  format(round(RMSE(pred_ratings, test_set$rating), 5), nsmall = 5)

rmse_results <- rmse_results %>%
  rbind(c("Movie Effect (b_i)", b_i_rmse))

knitr::kable(rmse_results,
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")

#################### Movie, User effect (b_u) ####################

b_u <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

pred_ratings <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

b_u_rmse <-
  format(round(RMSE(pred_ratings, test_set$rating), 5), nsmall = 5)

rmse_results <- rmse_results %>%
  rbind(c("Movie + User Effect (b_u)", b_u_rmse))

knitr::kable(rmse_results,
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")

#################### Movie, Users and Genre Effect (b_g) ####################

b_g <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = 'userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

pred_ratings <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = 'userId') %>%
  left_join(b_g, by = c("genres")) %>%
  mutate(pred = mu + b_i  + b_u + b_g) %>%
  pull(pred)

b_g_rmse <-
  format(round(RMSE(pred_ratings, test_set$rating), 5), nsmall = 5)

rmse_results <- rmse_results %>%
  rbind(c("Movie + User + Genre Effect (b_g)", b_g_rmse))

knitr::kable(rmse_results,
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")

#################### Movie, Users, Genre  + Review Year Effect (b_y) ####################

b_y <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = 'userId') %>%
  left_join(b_g, by = "genres") %>%
  group_by(review_year) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u - b_g))

pred_ratings <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = 'userId') %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "review_year") %>%
  mutate(pred = mu + b_i  + b_u + b_g + b_y) %>%
  pull(pred)

b_y_rmse <-
  format(round(RMSE(pred_ratings, test_set$rating), 5), nsmall = 5)

rmse_results <- rmse_results %>%
  rbind(c("Movie + Users + Genre + Review Year Effect (b_y)", b_y_rmse))

knitr::kable(rmse_results,
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")

#################### Regularization ####################


lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l) {
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + l))
  
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i) / (n() + l))
  
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = 'userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + l))
  
  b_y <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = 'userId') %>%
    left_join(b_g, by = "genres") %>%
    group_by(review_year) %>%
    summarize(b_y = sum(rating - mu - b_i - b_u - b_g) / (n() + l))
  
  pred_ratings <-
    test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "review_year") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
    pull(pred)
  
  return(RMSE(pred_ratings, test_set$rating))
})


qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
knitr::kable(lambda,
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")

#################### Results ####################

b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu) / (n() + lambda))

b_u <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))

b_g <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + lambda))

b_y <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  group_by(review_year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u - b_g) / (n() + lambda))

pred_ratings <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = 'userId') %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "review_year") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  pull(pred)

reg_rmse <-
  format(round(RMSE(pred_ratings, test_set$rating), 5), nsmall = 5)

rmse_results <- rmse_results %>%
  rbind(c("Regularized Movie + User + Genre + Year Effect", reg_rmse))

knitr::kable(rmse_results,
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")

#################### Final Validation ####################

mu <- mean(edx$rating)

b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu) / (n() + lambda))

b_u <- edx %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i) / (n() + lambda))

b_g <- edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + lambda))

b_y <- edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  group_by(review_year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u - b_g) / (n() + lambda))

pred_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = 'userId') %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "review_year") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  pull(pred)

reg_rmse <-
  format(round(RMSE(pred_ratings, validation$rating), 5), nsmall = 5)

rmse_results <- rmse_results %>%
  rbind(c("Final Validation", reg_rmse))

knitr::kable(rmse_results,
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")


top_10_validation <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = 'userId') %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "review_year") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  arrange(-pred) %>%
  group_by(title) %>%
  select(title) %>%
  head(10)


knitr::kable(top_10_validation, caption = "Top 10 Most Rated Movies - Validation data set",
             booktabs = T) %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center")
