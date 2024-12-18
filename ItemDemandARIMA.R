library(tidymodels)
library(tidyverse)
library(vroom)
library(embed)
library(lme4)
library(parsnip)
library(discrim)
library(kernlab)
library(themis)
library(modeltime) #Extensions of tidymodels to time series1
library(timetk) #Some nice time series functions


trainData <- vroom("trainStoreData.csv")
testData <- vroom("testStoreData.csv")


my_recipe <- recipe(formula = sales ~ ., data = trainData) %>%
  step_mutate_at(store, fn = factor) |> 
  step_mutate_at(item, fn = factor) |> 
  step_date(date, features = "doy") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) |> 
  step_date(date, features = "month") |> 
  step_mutate(date_month) |> 
  step_date(date, features = "dow") |> 
  step_date(date, features = "year") |> 
  step_date(date, features = "decimal") |> 
  step_normalize(all_numeric_predictors())


baked <- bake(prep(my_recipe), trainData)


pieceOfData <- trainData %>% filter(store==3, item==17)

cv_split <- time_series_split(pieceOfData, assess="3 months", cumulative = TRUE)
cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)



## Read in the Data and filter to store/item
storeItemTrain1 <- trainData %>%
  filter(store==3, item==17)
storeItemTest1 <- testData %>%
  filter(store==3, item==17)

## Read in the Data and filter to store/item
storeItemTrain2 <- trainData %>%
  filter(store==4, item==20)
storeItemTest2 <- testData %>%
  filter(store==4, item==20)


## Create the CV split for time series
cv_split1 <- time_series_split(storeItemTrain1, assess = "3 months", cumulative = TRUE)

cv_split2 <- time_series_split(storeItemTrain2, assess = "3 months", cumulative = TRUE)


## Create a recipe for the linear model part
arima_recipe <- recipe(formula = sales ~ ., data = trainData) %>%
  step_mutate_at(store, fn = factor) |>
  step_mutate_at(item, fn = factor) |>
  step_date(date, features = "doy") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) |>
  step_date(date, features = "month") |>
  step_mutate(date_month) |>
  step_date(date, features = "dow") |>
  step_date(date, features = "year") |>
  step_date(date, features = "decimal") |>
  step_normalize(all_numeric_predictors())

## Define the ARIMA Model
arima_model <- arima_reg(seasonal_period = "3 months",
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
                         ) %>%
                         set_engine("auto_arima")



## Merge into a single workflow and fit to the training data
arima_wf1 <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split1))

## Merge into a single workflow and fit to the training data
arima_wf2 <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split2))


## Calibrate (tune) the models (find p,d,q,P,D,Q)
cv_results1 <- modeltime_calibrate(arima_wf1,
                                  new_data = testing(cv_split1))

cv_results2 <- modeltime_calibrate(arima_wf2,
                                  new_data = testing(cv_split2))

## Visualize results
plot1 <- cv_results1 %>%
  modeltime_forecast(
                   new_data = testing(cv_split1),
                   actual_data = training(cv_split1)) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plot2 <- cv_results2 %>%
  modeltime_forecast(
    new_data = testing(cv_split2),
    actual_data = training(cv_split2)) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## Now that you have calibrated (tuned) refit to whole dataset
fullfit1 <- cv_results1 %>%
  modeltime_refit(data = storeItemTrain1)

fullfit2 <- cv_results2 %>%
  modeltime_refit(data = storeItemTrain2)


## Predict for all the observations in storeItemTest
plot3 <- fullfit1 %>%
  modeltime_forecast(new_data = storeItemTest1,
                     actual_data = storeItemTrain1) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plot4 <- fullfit2 %>%
  modeltime_forecast(new_data = storeItemTest2,
                     actual_data = storeItemTrain2) %>%
  plot_modeltime_forecast(.interactive=FALSE)


plotly::subplot(plot1,plot2,plot3,plot4, nrows=2)




