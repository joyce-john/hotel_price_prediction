# load  libraries
library(tidyverse)

# load the clean tables from ym GitHub repo
price <- read_csv('https://raw.githubusercontent.com/joyce-john/hotel_price_prediction/main/data/clean/hotels-europe_price.csv')
features <- read_csv('https://raw.githubusercontent.com/joyce-john/hotel_price_prediction/main/data/clean/hotels-europe_features.csv')

# join the clean tables
data <- left_join(price, features, by = 'hotel_id')

# remove the tables after they have been joined
rm(price, features)


