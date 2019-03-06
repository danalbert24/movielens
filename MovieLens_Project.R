# Create edx set, validation set, and submission file
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# The above is the code given to us. Below is the code I wrote.

# create an algorithm to predict movie reviews
# we have a training set, named edx
# we have a test set, named validation
# the columns of these sets are, in order, userId, movieId, rating, timestamp, title, and genres
# we will want to consider the average rating for the movie in question, and the average ratings of the user


# define our RMSE function
RMSE <- function(true_rating, predicted_rating) {
  sqrt(mean((true_rating - predicted_rating)^2))
}

# we'll start by calculating b_i, a value that tells us how much better/worse the ratings of a particular
# movie are than the average rating
# we do this by calculating the average rating across all movies, then subtracting it from the average rating 
# for each movie in particular
mu <- mean(edx$rating)
movie_avg <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))

# we will now do something similar for the ratings of each user, b_u
user_avg <- edx %>% left_join(movie_avg, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


#now we put it all together and test it using the validation set
predicted_ratings <- validation %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>% .$pred

# calculate adn print the RMSE
rmse <- RMSE(predicted_ratings, validation$rating)


#let's see how good our predictions were
#I'll start by calculating how far off each prediction was from the actual and call it delta
#I use absolute value because I see no difference being over vs under


delta <- abs(predicted_ratings-validation$rating)

#I'll define a few ranges based on how far off our predictions were from the actual
#excellent means less than .25 away
# very good means between .25 and .5
#good means between .5 and .75
#not good means between .75 and 1
#bad means between 1 and 2
#very bad means between 2 and 3
#super bad means between 3 and 4
# and extremely bad means over 4

excellent <- sum(delta <= .25)
very_good <- sum(delta <= .5 & delta > .25)
good <- sum(delta <= .75 & delta > .5)
not_good <- sum(delta <= 1 & delta > .75)
bad <- sum(delta < 2 & delta >= 1)
very_bad <- sum(delta <3 & delta >= 2)
super_bad <- sum(delta <4 & delta >= 3)
extremely_bad <- sum(delta >= 4)

#we'll create a table showing how many of our predictions were in each range and the precent of them it represents
categories <- c("excellent", "very good", "good", "not good", "bad", "very bad", "super bad", "extremely bad")
numbers <- c(excellent, very_good, good, not_good, bad, very_bad, super_bad, extremely_bad)
percents <- numbers/nrow(validation)
percents <- round(percents, digits = 3)*100
ranges <- c("[0,.25]", "(.25, .5]", "(.5, .75]", "(.75, 1]", "(1, 2]", "(2, 3]", "(3, 4]", "(4, 5]")
table2 <- data.frame(categories, numbers, percents, ranges)



# I will now output some of htis information to files that can be accessed by the R Markdown file I will be writing.
# I used ~/ because I think that should work on just about any system (not just my own). 
# In my original code, I had it in a further subfolder.
path_table2 <- "~/table2.csv"
write_csv(table2, path_table2)

path_delta <- "~/delta.csv"
write(delta, path_delta)

rmse