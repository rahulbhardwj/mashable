# TO567 Group Project
# Analysing popularity of news articles on Mashable

# Installing libraries

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
