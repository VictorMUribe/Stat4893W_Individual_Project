library(tidyverse)
library(tidymodels)
library(data.table)
library(timetk)
library(modeltime)

members <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
#peaks <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')






members %>% glimpse()
expeditions %>%  glimpse()
#peaks %>% glimpse()

data_temp <- full_join(members, expeditions)

data_temp %>% glimpse()


VIM::aggr(data_temp, col = c("skyblue", "orange"))

prop_na <- function(col){
  sum(is.na(col))/length(col)  
}

# all the values that can be used for prediction since we can only impute 10%
as.data.frame(sapply(data_temp, prop_na)) %>% 
  filter(sapply(data_temp, prop_na) <= .10)


everest <- data_temp %>% 
  filter(peak_name == "Everest") %>% 
  select(
    c(year, season, sex, age, citizenship, expedition_role, hired,
      success, solo, oxygen_used, died, injured)
  ) %>% 
  mutate(
    season = factor(season),
    sex = factor(sex),
    citizenship = factor(citizenship),
    expedition_role = factor(expedition_role),
    hired = factor(hired), 
    success = factor(success),
    solo = factor(solo),
    oxygen_used = factor(oxygen_used),
    died = factor(died),
    injured = factor(injured)#,
  )
  

everest %>% names()




## Original Data Split
set.seed(123)

everest_split <- initial_split(everest, prop = .7)
everest_train <- training(everest_split)
everest_test <- testing(everest_split)

everest_fold <- vfold_cv(everest_train, strata = success)
















  




