################################################################################
################################################################################
###############                                                  ###############
###############                    PREPARATION                   ###############
###############                                                  ###############
################################################################################
################################################################################



##################################
###                            ###
###         BASIC SETUP        ###
###                            ###
##################################


##############################
##  loading data, libraries ##
##############################

# load  libraries
library(tidyverse)
library(skimr)
library(GGally)
library(caret)
library(rpart)
library(rattle)
library(ranger)
library(ggthemes)


#load helper functions - used for price_diff_by_variables
source('https://raw.githubusercontent.com/joyce-john/hotel_price_prediction/main/helper_functions/da_helper_functions.R')

# load the clean tables from ym GitHub repo
price <- read_csv('https://raw.githubusercontent.com/joyce-john/hotel_price_prediction/main/data/clean/hotels-europe_price.csv')
features <- read_csv('https://raw.githubusercontent.com/joyce-john/hotel_price_prediction/main/data/clean/hotels-europe_features.csv')

# join the clean tables
data <- left_join(price, features, by = 'hotel_id')

# remove the tables after they have been joined
rm(price, features)

#####################
##  filter sample  ##
#####################

# filter cities
valid_cities <- c("Berlin", "Munich", "Vienna", "Budapest", "Prague", "Warsaw")
data <-
  data %>% 
  filter(city %in% valid_cities)

# filter data to: weekend in April 2018
data <- 
  data %>% 
  filter(year == 2018 & month == 4 & weekend == 1)

# filter data to only include hotels
data <-
  data %>% 
  filter(accommodation_type == 'Hotel')

# let's focus on non-luxury hotels and just drop everything over 500+
data <-
  data %>% 
  filter(price <= 500)

######################
##  factors/levels  ##
######################

# set character vars to factors
data <- 
  data %>% 
  mutate_if(is.character, factor)

# set levels for the discount categories
data <- 
  data %>% 
  mutate(offer_cat = factor(offer_cat, levels = c("0% no offer", "1-15% offer", "15-50% offer", "50%-75% offer", "75%+ offer")))


##################################
###         EXPLORATORY        ###
###            DATA            ###
###          ANALYSIS          ###
##################################


# let's do some EDA + summary stats
skimr::skim(data)

# how many NAs?
sum(is.na(data))

# just 54 NAs, let's drop them
data <-
  data %>% 
  drop_na()

####################
##     price      ##
####################

# check distribution of price
data %>% 
  ggplot(aes(x = price)) + 
  geom_histogram()
# based on the distribution, log is worth considering... 
# ...but may not be necessary after dropping obs with price > 500

# choose variables for which its pratical to check correlation (i.e. not factors with a bajillion levels)
check_cor <- c("price", "offer", "offer_cat", "nnights", "scarce_room", 
               "city", "distance", "stars", "rating", "rating_reviewcount", 
               "ratingta", "ratingta_count", "distance_alter")

# check correlations with price (and also between these variables)
data %>% 
  select(all_of(check_cor)) %>% 
  ggcorr()
# stars and ratings seem to be the strongest correlated

#########################
##     ALL numerics    ##
#########################

# check distribution of all other numeric variables
data %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
# possible log transformations: distance, distance_alter, rating_reviewcount, ratingta_count

####################
##    distance    ##
####################

# level distance and level price
data %>% 
  ggplot(aes(x = distance, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')

# log distance and level price
data %>% 
  ggplot(aes(x = log(distance), y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')
# more linear

# log distance and log price
data %>% 
  ggplot(aes(x = log(distance), y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')
# not much of an upgrade

##########################
##    distance_alter    ##
##########################

# level distance_alter and level price
data %>% 
  ggplot(aes(x = distance_alter, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')

# log distance_alter and level price
data %>% 
  ggplot(aes(x = log(distance_alter), y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')
# this is the best one, despite the bump which regression won't capture

# log distance_alter and log price
data %>% 
  ggplot(aes(x = log(distance_alter), y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')
# not helpful at all

##############################
##    rating_reviewcount    ##
##############################

# level rating_reviewcount and level price
data %>% 
  ggplot(aes(x = rating_reviewcount, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')

# log rating_reviewcount and level price
data %>% 
  ggplot(aes(x = log(rating_reviewcount), y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')
# outstanding - it looks *really* linear now!

# log rating_reviewcount and log price
data %>% 
  ggplot(aes(x = log(rating_reviewcount), y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')
# also great, but doesn't suggest the log(price) is necessary

##########################
##    ratingta_count    ##
##########################

# level ratingta_count and level price
data %>% 
  ggplot(aes(x = ratingta_count, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')

# log ratingta_count and level price
data %>% 
  ggplot(aes(x = log(ratingta_count), y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')

# log ratingta_count and log price
data %>% 
  ggplot(aes(x = log(ratingta_count), y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')

# log ratingta_count squared and log price
data %>% 
  ggplot(aes(x = log(ratingta_count)^2, y = log(price))) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')
# this is the most linear-looking loess for this variable

# log ratingta_count squared and level price
data %>% 
  ggplot(aes(x = log(ratingta_count)^2, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')

# log ratingta_count cubed and level price
data %>% 
  ggplot(aes(x = log(ratingta_count)^2, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = 'red')
# this is the second-most linear-looking and doesn't involve log()ing price


# well, I don't want to take log of price.
# that would be useful for linear regression, but not anything else
# and it would make model comparison slightly inconvenient

####################
##     cities     ##
####################

# check boxplots for cities
data %>% 
  ggplot(aes(x = city, y = price)) +
  geom_boxplot()

#######################
##     offer_cat     ##
#######################

# check boxplots for offer category
data %>% 
  ggplot(aes(x = offer_cat, y = price)) +
  geom_boxplot()
# fascinating - the means and quantiles are quite similar!

# could cities * offer_cat be worth an interaction?
price_diff_by_variables(data, "city", "offer_cat")
# there are some differences within the cities... but I'm not convinced these are meaningful patterns
# preliminary decision: "no" to this interaction for regression models

###################
##     stars     ##
###################

# check boxplots for stars
data %>% 
  ggplot(aes(x = as.factor(stars), y = price)) +
  geom_boxplot()

# worth an interaction?
price_diff_by_variables(data, "city", "stars")
# no, same pattern in every city

#########################
##     city_actual     ##
#########################

# what about properties in the cities VS in nearby cities etc?
# look at the numbers
data %>% 
  mutate(is_neighbor_city = ifelse(as.character(city) != as.character(city_actual), 1, 0)) %>% 
  group_by(city, is_neighbor_city) %>% 
  summarize(mean_price = mean(price))
# generally, smaller neighbor cities are cheaper than the big city markets they belong to

# look at the boxplots
temp <-
  data %>% 
  mutate(is_neighbor_city = ifelse(as.character(city) != as.character(city_actual), 1, 0))
price_diff_by_variables(temp, "city", "is_neighbor_city")
# with the exception of Berlin, neighboring cities are usually cheaper
# this could be worth an interaction in the regression

# remove temporary dataframe
rm(temp)


##################################
###                            ###
###     FEATURE ENGINEERING    ###
###                            ###
##################################


# make the variable transformations that EDA suggested would be useful for regression
data <- 
  data %>% 
  mutate(ln_distance = log(distance + 0.001),
         ln_distance_alter = log(distance_alter + 0.001),
         ln_rating_reviewcount = log(rating_reviewcount + 0.001),
         ln2_ratingta_count = log(ratingta_count + 0.001)^2)


# based on the EDA, create a new binary variable which indicates whether
# the hotel is in the city in the "city" column or whether it's in a smaller city nearby
data <-
  data %>% 
  mutate(is_neighbour_city = ifelse(as.character(city) != as.character(city_actual), 1, 0))


################################################################################
################################################################################
###############                                                  ###############
###############                     PREDICTION                   ###############
###############                                                  ###############
################################################################################
################################################################################


##################################
###                            ###
###      PREDICTION SETUP      ###
###                            ###
##################################


#############################
##     train/test sets     ##
#############################

#split data into training and test sets. let's do an 80/20 split
set.seed(1413)
index <- createDataPartition(data$price, times = 1, p = 0.8, list = FALSE)

# assign training and test sets
train_set <- slice(data, index)
test_set <- slice(data, -index)

#################################
##     training parameters     ##
#################################

# set the trainControl variable for 5-fold cross validation and allow parallel processing
train_control <- trainControl(method = "cv",
                              number = 5,
                              allowParallel = TRUE)

# set the tuneGrid params for random forest
# setting mtry as the sqrt of the number of basic vars through sqrt of the number of all vars
tune_grid_rf <- expand.grid(
  .mtry = c(2, 3, 4),
  .splitrule = "variance", # for regression
  .min.node.size = c(5, 10)
)

####################################################
##     variable sets of increasing complexity     ##
####################################################

# create variable sets FOR LINEAR REGRESSION 
# includes functional form adjustments to accommodate linear patterns
# includes the is_neighbour_city*city interaction

# property stats 
linear_vars_basic <- c("city", "stars", "rating", "ln_distance", "ln_rating_reviewcount")

# property stats + circumstances (offer, holiday) + neighbor_city & interaction
linear_vars_mid <- c("offer",  "holiday", "city", "stars", "rating", "ln_distance", "ln_rating_reviewcount", "is_neighbour_city", "city*is_neighbour_city")
 
# all potential variables
linear_vars_all <- c("offer","offer_cat", "holiday",
                     "scarce_room", "city", "stars", 
                     "rating", "city_actual", "neighbourhood", 
                     "ln_distance", "ln_distance_alter", "ln_rating_reviewcount",
                     "ln2_ratingta_count", "is_neighbour_city", "city*is_neighbour_city")

# create variable sets FOR NON-LINEAR METHODS
# no functional form adjustments, because these models can find non-linear patterns
# does not includ the interaction term, because these models can find interactions automatically

# property stats
nonlinear_vars_basic <- c("city", "stars", "rating", "distance", "rating_reviewcount")

# property stats + circumstances + neighbor_city
nonlinear_vars_mid <- c("offer",  "holiday", "city", "stars", "rating", "distance", "rating_reviewcount", "is_neighbour_city")

# all potential variables
nonlinear_vars_all <- c("offer", "offer_cat", "holiday", 
                        "scarce_room", "city", "stars",
                        "rating", "city_actual", "neighbourhood",
                        "distance", "distance_alter", "rating_reviewcount",
                        "ratingta_count", "is_neighbour_city")

##################################
###                            ###
###        CREATE MODELS       ###
###                            ###
##################################

#############################
##           ols           ##
#############################

# basic
lm_1 <- train(formula(paste0("price ~", paste0(linear_vars_basic, collapse = "+"))),
              method = "lm",
              data = train_set,
              trControl = train_control)

# mid
lm_2 <- train(formula(paste0("price ~", paste0(linear_vars_mid, collapse = "+"))),
              method = "lm",
              data = train_set,
              trControl = train_control)

# complex
lm_3 <- train(formula(paste0("price ~", paste0(linear_vars_all, collapse = "+"))),
              method = "lm",
              data = train_set,
              trControl = train_control)

#############################
##   rpart decision tree   ##
#############################

# basic
set.seed(1413)
tree_1 <- train(formula(paste0("price ~", paste0(nonlinear_vars_basic, collapse = "+"))),
                method = "rpart",
                data = train_set,
                trControl = train_control,
                tuneLength = 20)

# mid
set.seed(1413)
tree_2 <- train(formula(paste0("price ~", paste0(nonlinear_vars_mid, collapse = "+"))),
                method = "rpart",
                data = train_set,
                trControl = train_control,
                tuneLength = 20)

# complex
set.seed(1413)
tree_3 <- train(formula(paste0("price ~", paste0(nonlinear_vars_all, collapse = "+"))),
                method = "rpart",
                data = train_set,
                trControl = train_control,
                tuneLength = 20)

#############################
##      random forest      ##
#############################

# basic
set.seed(1413)
rf_1 <- train(formula(paste0("price ~", paste0(nonlinear_vars_basic, collapse = "+"))),
                method = "ranger",
                data = train_set,
                importance = "impurity",
                trControl = train_control,
                tuneGrid = tune_grid_rf)

# mid
set.seed(1413)
rf_2 <- train(formula(paste0("price ~", paste0(nonlinear_vars_mid, collapse = "+"))),
              method = "ranger",
              data = train_set,
              importance = "impurity",
              trControl = train_control,
              tuneGrid = tune_grid_rf)

# complex
set.seed(1413)
rf_3 <- train(formula(paste0("price ~", paste0(nonlinear_vars_all, collapse = "+"))),
              method = "ranger",
              data = train_set,
              importance = "impurity",
              trControl = train_control,
              tuneGrid = tune_grid_rf)

##################################
###                            ###
###      EVALUATE MODELS       ###
###                            ###
##################################

########################################
##      cross-validation results      ##
########################################

# vector of model names
models <- c('lm_1', 'lm_2', 'lm_3', 'tree_1', 'tree_2', 'tree_3', 'rf_1', 'rf_2', 'rf_3')

# vector of variable descriptors, manually ordered to match
variable_detail <- c('basic', 'mid', 'complex','basic', 'mid', 'complex','basic', 'mid', 'complex')

# dataframe of RMSE / minimum mean cross-validated RMSE for optimal tuning values for all the models
cv_results <- data.frame("Model" = models, "Variables" = variable_detail, "Mean_RMSE" = c(lm_1$results$RMSE, lm_2$results$RMSE, lm_3$results$RMSE,
                                                                                          min(tree_1$results$RMSE), min(tree_2$results$RMSE), min(tree_3$results$RMSE),
                                                                                          min(rf_1$results$RMSE), min(rf_2$results$RMSE), min(rf_3$results$RMSE)))

# rf_2 has the lowest RMSE in cross-validation, therefore I select it as my model


########################################
##          test-set results          ##
########################################

#use rf_2 model to create a vector of predictions on test set obs
rf_2_predictions <- predict(rf_2, test_set)

# calculate RMSE for predictions on test set
test_set_rmse <- RMSE(rf_2_predictions, test_set$price) # 48.08499
test_set_mae <- caret::MAE(rf_2_predictions, test_set$price) # 32.63675

########################################
##         model diagnostics          ##
########################################

# add prediction and RMSE column to test set data so we can analyze errors
test_set <- 
  test_set %>% 
  mutate(prediction = rf_2_predictions)

# do errors differ by city? look at the numbers
test_set %>% 
  group_by(city) %>% 
  summarize(rmse = RMSE(price, prediction))

# do errors differ by city? look at the graph
test_set %>% 
  group_by(city) %>% 
  summarize(rmse = RMSE(price, prediction)) %>% 
  ggplot(aes(x = reorder(city, rmse), y = rmse)) +
  geom_bar(stat = 'identity') +
  theme_few() +
  labs(x = 'City', y = "RMSE", title = "RMSE on the Test Set") +
  theme(plot.title = element_text(hjust = 0.5))

# top ten positive prediction errors as a table
test_set %>% 
  mutate(error = prediction - price) %>% 
  arrange(desc(error)) %>% 
  select(hotel_id, city, distance, stars, rating, rating_reviewcount,price, prediction, error) %>% 
  slice_head(n = 10)
# 8/10 are in Prague!!

# top 10 negative prediction errors as a table
test_set %>% 
  mutate(error = prediction - price) %>% 
  arrange(error) %>% 
  select(hotel_id, city, distance, stars, rating, rating_reviewcount,price, prediction, error) %>% 
  slice_head(n = 10)
# no real pattern here, but the two biggest errors are in Budapest, which has the highest RMSE in general


# look at the most important variables (varImp plot)
plot(varImp(rf_2))

# partial dependence plot for stars on price
# calculate partial dependence
pdp_stars <- pdp::partial(rf_2, pred.var = "stars", pred.grid = distinct_(test_set, "stars"), train = train_set)

# plot partial dependence and save to an object
pdp_stars_plot <- pdp_stars %>%
  autoplot( ) +
  geom_point(size=3) +
  geom_line(size=1) +
  ylab("Predicted Price") +
  xlab("Stars") +
  labs(title = "Partial Dependence Plot: Stars and Price") +
  scale_x_continuous(limit=c(1.5,5), breaks=seq(1.5,5,0.5)) +
  theme_few() +
  theme(plot.title = element_text(hjust = 0.5))

# show partial dependence plot
pdp_stars_plot

