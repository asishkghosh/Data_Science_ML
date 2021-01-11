library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(broom.mixed) # for converting bayesian models to tidy tibbles

# Data set
bike_data_tbl <- readRDS("C:/Users/rueta/Desktop/Data_Science_ML/data/bike_orderlines.rds")


ggplot(bike_data_tbl,
       aes(x = price, 
           y = weight, 
           group = category_1, 
           col = category_1)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_manual(values=c("#2dc6d6", "#d65a2d", "#d6af2d", "#8a2dd6","#d6afee"))

weight ~ price * category_1

linear_reg()
## Linear Regression Model Specification (regression)

lm_mod <- linear_reg() %>% 
  set_engine("lm")

lm_mod
## Linear Regression Model Specification (regression)
## 
## Computational engine: lm


lm_fit <- lm_mod %>% 
  fit(weight ~ price * category_1, 
      data = bike_data_tbl)

tidy(lm_fit)

new_points <- expand.grid(price = 2000, 
                          category_1 = c("E-Bikes", "Hybrid / City", "Mountain", "Road", "Gravel"))
new_points
##   price    category_1
## 1       2000       E-Bikes
## 2       2000 Hybrid / City
## 3       2000      Mountain
## 4       2000          Road

mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred


conf_int_pred <- predict(lm_fit, 
                         new_data = new_points, 
                         type = "conf_int")
conf_int_pred


# Now combine: 
plot_data <- new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

# and plot:
ggplot(plot_data, aes(x = category_1)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "Bike weight", x = "Category") 




# set the prior distribution
prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)


# make the parsnip model
bayes_mod <- linear_reg() %>% 
  set_engine("stan",
             prior_intercept = prior_dist, 
             prior = prior_dist) 

# train the model
bayes_fit <-  bayes_mod %>% 
  fit(weight ~ price * category_1, 
      data = bike_data_tbl)

print(bayes_fit, digits = 5)

tidy(bayes_fit, conf.int = TRUE)



bayes_plot_data <- 
  new_points %>%
  bind_cols(predict(bayes_fit, new_data = new_points)) %>% 
  bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))

ggplot(bayes_plot_data, aes(x = category_1)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = .2) + 
  labs(y = "Bike weight") + 
  ggtitle("Bayesian model with t(1) prior distribution")


#Preprocessing


library(nycflights13)
library(skimr)

set.seed(123)

flight_data <- 
  flights %>% 
  mutate(
    # Convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    # We will use the date (not date-time) in the recipe below
    date = as.Date(time_hour)
  )%>% 
  # Include the weather data
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  # Only retain the specific columns we will use
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)


flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))

flight_data %>% 
  skimr::skim(dest, carrier) 

# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(555)
# Put 3/4 of the data into the training set 
data_split <- initial_split(flight_data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)


flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") 

summary(flights_rec)

flight_data %>% 
  distinct(date) %>% 
  mutate(numeric_date = as.numeric(date))

flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>% 
  step_rm(date)%>% 
  step_dummy(all_nominal(), -all_outcomes())%>% 
  step_zv(all_predictors())

test_data %>% 
  distinct(dest) %>% 
  anti_join(train_data)

lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

flights_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(flights_rec)
flights_wflow

flights_fit <- 
  flights_wflow %>% 
  fit(data = train_data)

flights_fit %>% 
  pull_workflow_fit() %>% 
  tidy()





