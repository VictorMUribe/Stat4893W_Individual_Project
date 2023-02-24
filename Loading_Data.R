library(tidyverse)
library(tidymodels)
library(data.table)
library(timetk)
library(modeltime)

members <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
#peaks <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')






#members %>% glimpse()
#expeditions %>%  glimpse()
#peaks %>% glimpse()

#data_temp <- full_join(members, expeditions)

#data_temp %>% glimpse()


#VIM::aggr(data_temp, col = c("skyblue", "orange"))

#prop_na <- function(col){
#  sum(is.na(col))/length(col)  
#}

# all the values that can be used for prediction since we can only impute 10%
#as.data.frame(sapply(data_temp, prop_na)) %>% 
#  filter(sapply(data_temp, prop_na) <= .10)



everest <- members %>% # should have an age
  filter(age != 'NA' & peak_name == "Everest") %>% 
  select(-c(peak_id,peak_name,expedition_id,member_id, death_height_metres,
            injury_height_metres, highpoint_metres, death_cause,injury_type)) %>%
  mutate(
    #death_height_metres = ifelse(death_height_metres == NA, 0, death_height_metres), # issue wiith levels for some reason
    #injury_height_metres = ifelse(injury_height_metres == NA, 0, injury_height_metres), # gives leveling issue 
    #highpoint_metres = ifelse(highpoint_metres == 'NA', 0, highpoint_metres), # thows off the p values
    season = factor(season),
    sex = factor(sex),
    #citizenship = factor(citizenship), # not significant after testing
    #expedition_role = factor(expedition_role), # not significant after testing
    hired = factor(hired), 
    success = factor(success), # value being predicted
    solo = factor(solo),
    oxygen_used = factor(oxygen_used),
    died = factor(died),
    #death_cause = factor(death_cause), # issue
    injured = factor(injured)#,
    #injury_type = factor(injury_type) # issue with levels
  )
  

#everest %>% names()




## Original Data Split
set.seed(123)

everest_split <- initial_split(everest, prop = .7)
everest_train <- training(everest_split)
everest_test <- testing(everest_split)

everest_fold <- vfold_cv(everest_train, strata = success)
















  




