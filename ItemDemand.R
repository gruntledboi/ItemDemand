library(tidymodels)
library(vroom)
library(forecast)
library(patchwork)

install.packages("forecast")


trainData <- vroom("trainStoreData.csv")
testData <- vroom("testStoreData.csv")

trainData$sales <- as.factor(trainData$sales)

my_recipe <- recipe(formula = sales ~ ., data = trainData) %>%
  step_mutate_at(date, fn = factor) %>% ## Turn color to factor then dummy encode color
  step_mutate_at(store, fn = factor) |> 
  step_mutate_at(item, fn = factor) |> 
  step_normalize(all_numeric_predictors())


## Filter down to just 1 store item for exploration and model building5
storeItem1 <- trainData %>%
  filter(store == 1, item == 1)


plot1 <- storeItem1 %>%
  ggplot(mapping = aes(x = date, y = sales)) +
  geom_line() +
  geom_smooth(se = FALSE)


plot2 <- storeItem1 %>%
  pull(sales) |> 
  forecast::ggAcf()

plot3 <- storeItem1 %>%
  pull(sales) |> 
  forecast::ggAcf(lag.max=2*365)
  





storeItem2 <- trainData %>%
  filter(store == 2, item == 1)


plot4 <- storeItem2 %>%
  ggplot(mapping = aes(x = date, y = sales)) +
  geom_line() +
  geom_smooth(se = FALSE)


plot5 <- storeItem2 %>%
  pull(sales) |> 
  forecast::ggAcf()

plot6 <- storeItem2 %>%
  pull(sales) |> 
  forecast::ggAcf(lag.max=2*365)

(plot1 + plot4) / (plot2 + plot5) / (plot3 + plot6)
