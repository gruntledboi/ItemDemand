library(vroom)
library(timetk)
library(tidyverse)
library(patchwork)
library(tidymodels)
library(forecast)
library(modeltime)
library(embed)
# library(bonsai)
# library(lightgbm)

train <- vroom("trainStoreData.csv")
test <- vroom("testStoreData.csv")

n.stores <- max(train$store)
n.items <- max(train$item)

item_recipe <- recipe(sales~., data = train) %>%
  step_date(date, features=c("dow", "month", "doy", "year")) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY = cos(date_doy)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(sales)) %>%
  step_rm(date, item, store) %>%
  step_normalize(all_numeric_predictors())

baked <- bake(prep(item_recipe), train)

boosted_model <- boost_tree(tree_depth = 2,
                            trees = 1000,
                            learn_rate = 0.01) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

boostwf <- workflow() %>%
  add_recipe(item_recipe) %>%
  add_model(boosted_model)


## Double Loop over all store-item combos
for(s in 1:n.stores){
  for(i in 1:n.items){
    
    # Subset the data
    thisTrain <- train %>%
      filter(store == s, item == i)
    thisTest <- test %>%
      filter(store == s, item == i)
    
    ## Fit the data and forecast
    fitboostwf <- boostwf %>%
      fit(data=thisTrain)
    
    preds <- predict(fitboostwf, new_data = thisTest) %>%
      bind_cols(thisTest) %>%
      rename(sales = .pred) %>%
      select(id, sales)
    
    ## Save the results
    if(s==1 && i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds,
                             preds)
    }
    
  }
}

write_csv(all_preds, "./submission.csv")
