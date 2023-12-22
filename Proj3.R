library("tidyverse")
library("DT")

setwd("/Users/hrithikchavva/Desktop/HRK/Data Mining/Project3")


cases <- read_csv("COVID-19_cases_plus_census.csv")

cases



cases <- cases %>% mutate_if(is.character, factor)
dim(cases)


cases <- cases %>% filter(confirmed_cases > 0)


cases <- cases %>% 
  arrange(desc(confirmed_cases)) #%>%    
#Normalize the data
cases <- cases %>% mutate(
  cases_per_10000 = confirmed_cases/total_pop*10000, 
  deaths_per_10000 = deaths/total_pop*10000, 
  death_per_case = deaths/confirmed_cases)

cases


cases_selected1 <- cases %>% select(
  county_name, state, total_pop,
  owner_occupied_housing_units,
  civilian_labor_force,
  employed_pop,
  families_with_young_children,
  two_parent_families_with_young_children,
  two_parents_in_labor_force_families_with_young_children,
  nonfamily_households,
  white_pop,
  vacant_housing_units_for_rent,
  income_per_capita,
  bachelors_degree,
  in_school,
  median_rent,
  black_pop,
  asian_pop,
  hispanic_pop,
  commuters_by_public_transportation,
  commute_30_34_mins,
  commute_60_more_mins,
  million_dollar_housing_units,
  median_income,
  in_school,
  cases_per_10000,
  deaths_per_10000
)
summary(cases_selected1)
cases_selected <- cases_selected  %>% mutate(
  owner_occupied_housing_units = owner_occupied_housing_units / total_pop,
  civilian_labor_force = civilian_labor_force / total_pop,
  employed_pop = employed_pop / total_pop,
  families_with_young_children = families_with_young_children / total_pop,
  two_parent_families_with_young_children = two_parent_families_with_young_children / total_pop,
  two_parents_in_labor_force_families_with_young_children = two_parents_in_labor_force_families_with_young_children / total_pop,
  nonfamily_households = nonfamily_households / total_pop,
  white_pop = white_pop / total_pop,
  vacant_housing_units_for_rent = vacant_housing_units_for_rent / total_pop,
  income_per_capita = income_per_capita / total_pop,
  bachelors_degree = bachelors_degree / total_pop,
  in_school = in_school / total_pop,
  median_rent = median_rent / total_pop,
  black_pop = black_pop / total_pop,
  asian_pop = asian_pop / total_pop,
  hispanic_pop = hispanic_pop / total_pop,
  commuters_by_public_transportation = commuters_by_public_transportation / total_pop,
  commute_30_34_mins = commute_30_34_mins / total_pop,
  commute_60_more_mins = commute_60_more_mins / total_pop,
  million_dollar_housing_units = million_dollar_housing_units / total_pop,
  median_income = median_income / total_pop,
)



summary(cases_selected)

table(complete.cases(cases_selected))



install.packages("seriation")
str(cases_selected)

library(seriation)

cm <- cor(cases_selected %>% select_if(is.numeric) %>% na.omit)

cm

hmap(cm, margins = c(14,14))





install.packages("dbscan")
install.packages("devtools")

library(ggplot2)
lof_scores <- lof(cm)
plot(sort(lof_scores), type = "l", xlab = "Observations", ylab = "LOF", abline(h = 1.22, lty = 1.2))
abline(h = 2.25, lty = 2)


lof_scores




cases_selected <- cases_selected %>% mutate(bad = as.factor(deaths_per_10000 > 10))


cases_selected %>% pull(bad) %>% table()


cases_selected %>% group_by(state) %>% 
  summarize(bad_pct = sum(bad == TRUE)/n()) %>%
  arrange(desc(bad_pct))



cases_train <- cases_selected %>% filter(state %in% c("TX", "CA", "FL", "NY", "CH", "GA", "DC", "MI", "OH", "AR"))
cases_train %>% pull(bad) %>% table()



cases_test <-  cases_selected %>% filter(!(state %in% c("TX", "CA", "FL", "NY", "CH", "GA", "DC", "MI", "OH", "AR")))
cases_test %>% pull(bad) %>% table()


counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties  

counties_all <- counties %>% left_join(cases_train %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))



ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))




# check variable importance

install.packages("FSelector")
library(FSelector)


cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()

cases_train <- cases_train %>% select(-c(deaths_per_10000))
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()


cases_train <- cases_train %>% select( -cases_per_10000)

cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(n = 10)




#classification models


#Ctree fit

library(dplyr)
install.packages("caret")
library(caret)


fit <- cases_train %>%
  train(bad ~ . - county_name - state,
        data = . ,
        #method = "rpart",
        method = "rf",
        #method = "nb",
        trControl = trainControl(method = "cv", number = 10)
  )
fit


varImp(fit)

train_index <- createFolds(cases_train$bad, k = 10)


ctreeFit <- cases_train %>% train(bad ~ .,
                                  method = "ctree",
                                  data = .,
                                  tuneLength = 5,
                                  trControl = trainControl(method = "cv", indexOut = train_index))
ctreeFit

library(caret)
cases_train <- na.omit(cases_train)



plot(ctreeFit$finalModel, main = "ctreeFit training model diagram", cex.main = 10)

first_test <- cases_test %>% na.omit
first_test$bad_predicted <- predict(ctreeFit, first_test)

counties_test1 <- counties %>% left_join(first_test %>% 
                                           mutate(county = county_name %>% str_to_lower() %>% 
                                                    str_replace('\\s+county\\s*$', '')))

ggplot(counties_test1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'green', 'FALSE' = 'white'))


ggplot(counties_test1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'green', 'FALSE' = 'white'))




#xgboost


xgboostFit <- cases_train %>% train(bad ~.,
                                    method = "xgbTree",
                                    data = .,
                                    tuneLength = 5,
                                    trControl = trainControl(method = "cv", indexOut = train_index),
                                    tuneGrid = expand.grid(
                                      nrounds = 20,
                                      max_depth = 3,
                                      colsample_bytree = .6,
                                      eta = 0.1,
                                      gamma=0,
                                      min_child_weight = 1,
                                      subsample = .5
                                    ))
xgboostFit
xgboostFit$finalModel

extra_test <- cases_test %>% na.omit
extra_test$bad_predicted <- predict(xgboostFit, extra_test)

counties_test4 <- counties %>% left_join(extra_test %>% 
                                           mutate(county = county_name %>% str_to_lower() %>% 
                                                    str_replace('\\s+county\\s*$', '')))

ggplot(counties_test4, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'green', 'FALSE' = 'white'))


ggplot(counties_test4, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'green', 'FALSE' = 'white'))



#SVM
svmFit <- cases_train |> train(bad ~.,
                               method = "svmLinear",
                               data = _,
                               tuneLength = 5,
                               trControl = trainControl(method = "cv", indexOut = train_index))
svmFit


third_test <- cases_test %>% na.omit
third_test$bad_predicted <- predict(svmFit, third_test)

counties_test1 <- counties %>% left_join(third_test %>% 
                                           mutate(county = county_name %>% str_to_lower() %>% 
                                                    str_replace('\\s+county\\s*$', '')))

#plot(ctreeFit$finalModel, main = "Svmfit training model diagram", cex.main = 10)


ggplot(counties_test1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'green', 'FALSE' = 'white'))


ggplot(counties_test1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'green', 'FALSE' = 'white'))




#C45

C45Fit <- cases_train %>% train(bad ~ .,
                                method = "J48",
                                data = .,
                                tuneLength = 5,
                                trControl = trainControl(method = "cv", indexOut = train_index))
C45Fit

C45Fit$finalModel
library(party)
plot(C45Fit$finalModel)
install.packages("partykit")


second_test <- cases_test %>% na.omit
second_test$bad_predicted <- predict(C45Fit, second_test)

counties_test2 <- counties %>% left_join(second_test %>% 
                                           mutate(county = county_name %>% str_to_lower() %>% 
                                                    str_replace('\\s+county\\s*$', '')))

ggplot(counties_test2, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'blue', 'FALSE' = 'white'))


ggplot(counties_test2, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'blue', 'FALSE' = 'white'))




#Comparing m odels


resamps <- resamples(list(
  ctree = ctreeFit,
  C45 = C45Fit,
  SVM = svmFit,
  xgboost = xgboostFit
  
))
resamps

summary(resamps)
library(lattice)
bwplot(resamps, layout = c(3, 1))

difs <- diff(resamps)
difs

summary(difs)























