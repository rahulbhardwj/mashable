# TO567 Group Project
# Analysing popularity of news articles on Mashable

# Loading libraries
library(MASS)
library(caret)
library(Metrics)

# Reading news popularity data
news_data <- read.csv("OnlineNewsPopularity.csv")

# Viewing data type of each column
str(news_data)

# Recoding column datatypes
news_data$data_channel_is_lifestyle <- as.factor(news_data$data_channel_is_lifestyle)
news_data$data_channel_is_entertainment <- as.factor(news_data$data_channel_is_entertainment)
news_data$data_channel_is_bus <- as.factor(news_data$data_channel_is_bus)
news_data$data_channel_is_socmed <- as.factor(news_data$data_channel_is_socmed)
news_data$data_channel_is_tech <- as.factor(news_data$data_channel_is_tech)
news_data$data_channel_is_world <- as.factor(news_data$data_channel_is_world)
news_data$weekday_is_monday <- as.factor(news_data$weekday_is_monday)
news_data$weekday_is_tuesday <- as.factor(news_data$weekday_is_tuesday)
news_data$weekday_is_wednesday <- as.factor(news_data$weekday_is_wednesday)
news_data$weekday_is_thursday <- as.factor(news_data$weekday_is_thursday)
news_data$weekday_is_friday <- as.factor(news_data$weekday_is_friday)
news_data$weekday_is_saturday <- as.factor(news_data$weekday_is_saturday)
news_data$weekday_is_sunday <- as.factor(news_data$weekday_is_sunday)
news_data$is_weekend <- as.factor(news_data$is_weekend)

# Viewing data type of each column
str(news_data)

# Calculating average shares on weekdays and weekend
aggregate(shares ~ is_weekend, data = news_data, mean)

# Visualizing relationship of shares with various parameters
plot(news_data$num_imgs, news_data$shares)
plot(news_data$num_videos, news_data$shares)
plot(news_data$num_keywords, news_data$shares)
plot(news_data$rate_negative_words, news_data$shares)

# Preparing training and test data
set.seed(567)
train_indices <- sample(1:nrow(news_data), 0.7*nrow(news_data))
train_data <- news_data[train_indices, ]
val_data <- news_data[-train_indices, ]
x_val_data <- val_data[ , -61]
y_val_data <- val_data[ , 61]

# Linear Regression Model
linear.regression.model <- lm(shares ~ . - url - timedelta, data = train_data)
summary(linear.regression.model)

# Removing independent variables causing multicollinearity
stepAIC(linear.regression.model, direction = "both")

# Building updated Linear Regression Model with only selected variables
linear.regression.updated <- lm(shares ~ n_tokens_title + n_tokens_content + n_unique_tokens + 
     n_non_stop_words + num_hrefs + num_self_hrefs + num_imgs + 
     average_token_length + num_keywords + data_channel_is_lifestyle + 
     data_channel_is_entertainment + data_channel_is_bus + data_channel_is_socmed + 
     data_channel_is_tech + data_channel_is_world + kw_min_min + 
     kw_max_min + kw_min_max + kw_min_avg + kw_max_avg + kw_avg_avg + 
     self_reference_min_shares + self_reference_max_shares + weekday_is_monday + 
     weekday_is_saturday + LDA_00 + LDA_03 + LDA_04 + global_subjectivity + 
     global_rate_positive_words + min_positive_polarity + avg_negative_polarity + 
     abs_title_subjectivity + abs_title_sentiment_polarity, data = train_data)
summary(linear.regression.updated)

# Making predictions and evaluating performance
val.predictions <- predict(linear.regression.updated, x_val_data)
result <- postResample(val.predictions, y_val_data)
result
mape(y_val_data, val.predictions)
