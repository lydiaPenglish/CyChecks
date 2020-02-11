########################
# author: Lydia 
#
# purpose: Combined raw salary data with ISU directory data
# 
# inputs: 
#
# outputs: 
#
# notes: 
#   - I got rid of base_salary_date and base_salary_year since base_salary_year was identical to fiscal_year. 
#
########################

library(tidyverse)


# i. ---- Clean up salary data ----

cyd_salsraw <- read_csv("data-raw/_tidy/td_rawsals.csv")

cyd_salstidy <- cyd_salsraw %>% 
  
  # -- get rid of unwanted columns -- 
  select(fiscal_year, name, gender, position, total_salary_paid, travel_subsistence, base_salary) %>%
  
  # -- eliminate things that are per hour
  filter(!grepl("HR", base_salary)) %>%
  
  # -- update columns
  mutate(base_salary = as.numeric(gsub(",", "", base_salary)))              # note: warning about NAs introduced by coercion



,
    total_salary_paid = as.numeric(total_salary_paid),
    travel_subsistence = as.numeric(travel_subsistence),
    
    #--get rid of any white space in character cols
    position = stringr::str_trim(position, side = "right"),
    name = stringr::str_trim(name, side = "right"),
    
    #--make name lower case, get rid of commas
    name = tolower(name),
    name = gsub(",", "", name),
    
    position = tolower(position)) %>%
  
  #--eliminate people who don't have a position or a gender
  filter(!grepl("\\*", position),
         !grepl("\\*", gender))  %>%
  
  #--only keep professors
  filter(grepl("prof", position)) %>%
  
  
  #--expand the name out, I think we have to
  separate(name, into = c("last_name", "first_name", "other"), sep = " ") %>% 
  
  #--remove punctuation from the first name (?not sure if this is a good way to do this....)
  
  mutate(first_name = str_remove_all(first_name, "[[:punct:]]"))
