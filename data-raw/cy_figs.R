########################
# author: Gina
# created: Sept 11 2019
#
# last updated: 
#
# purpose: make a graph for kendall
# 
# inputs: 
# outputs: 
#
# notes: 
#
########################


rm(list = ls())
library(tidyverse)
library(janitor) #--cleans things nicely
library(readxl) #--reads excel spreadsheets
library(here) #--helps w/wd things
#library(fuzzyjoin) ##--merge by inexact matches (like smith jill with smith jill h)

library(digest) #--for anonymizing names

## NOTE all this shit is just bc I don't have internet at my house so I don't have the updated code
# Ignore this and just get the figure code at the end


# FIXED SEPARATION OF NAMES, don't separate hyphenated names (like Maria Salas-Fernandez)
# BUT, she's listed as Mari?
saltidy <- read_csv("../_data/_tidy/td_rawsals.csv") %>%
    #--deal with dates
  mutate(fiscal_year = as.numeric(fiscal_year),
         base_salary_date = lubridate::ymd_hms(base_salary_date),
         base_salary_year = lubridate::year(base_salary_date)) %>%
  
  #--keep only desired columns
  select(fiscal_year, base_salary_year, 
         name, gender, position, 
         total_salary_paid, travel_subsistence, base_salary) %>%
  
  #--eliminate entries that are per hour (concentrate on profs)
  filter(!grepl("HR", base_salary)) %>%
  
    mutate(
      #--get rid of stupid commas in base_salary, make everything monetary into numeric    
      base_salary = as.numeric(gsub(",", "", base_salary)),
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
  
  #--expand the name out, I think we have to
  
  separate(name, into = c("last_name", "first_name", "other"), sep = " ") 


cyd_depts %>%
  filter(last_name == "salas-fernandez")

cyd_depts <- 
  read_csv("../_data/_tidy/td_empl-dept-key.csv") %>%
  
  # why are there 3 ag/biosys eng? simplify them
  mutate(dept = recode(dept,
                       "ag/biosys eng-a" = "ag/biosys eng",
                       "ag/biosys eng-e" = "ag/biosys eng"))

cyd_orgs <- read_csv("../_data/_tidy/td_org-dept-key.csv")

#--PROBLEM: we only have 2 names from Cynthia - so adams sarah l and adams sarah k both get grouped with adams sarah.
# how many times is this a problem?
# 66 people have the same name as someone else. I have no idea what to do with them. 
# I guess remove them? Keep them? No idea. For now we keep them. 
dupes <- 
  cyd_depts %>%
  group_by(fiscal_year, last_name, first_name) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(last_name, first_name, dept) %>%
  ungroup() %>%
  unite(last_name, first_name, col = "name", sep = "_") %>%
  select(name) %>%
  distinct() 
  
# remove dupes
dupes <- 
  cyd_depts %>%
  group_by(fiscal_year, last_name, first_name) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(last_name, first_name, dept) %>%
  ungroup() %>%
  unite(last_name, first_name, col = "name", sep = "_") %>%
  select(name) %>%
  distinct() 

dupesv <- as_vector(dupes)
#dupesv2 <- c("abasht_behnam", "abate_sarah")

saltidy2 <- 
  saltidy %>%
  unite(last_name, first_name, col = "name", sep = "_") %>%
  filter(!name %in% dupesv) %>%
  separate("name", into = c("last_name", "first_name"))


saldept <- 
  saltidy2 %>%
  left_join(cyd_depts, by = c("fiscal_year", "last_name", "first_name")) %>%
  select(-base_salary_year) %>%
  #--anonymize names
  unite(last_name, first_name, other, col = "name") %>%
  mutate(anon = name %>% map(digest)) %>%
  unnest() %>%
  
  # add college
  left_join(cyd_orgs, by = "dept") %>%
  select(fiscal_year, college, dept, anon, name,
         position, gender, 
         base_salary, total_salary_paid, travel_subsistence) %>%
  arrange(fiscal_year, college, dept, position, base_salary)



# NOTE: profs are effed. need to allow for more as 'prof' -----------------


# what things are allowed?
myprofs = c("asst prof", "assoc prof", "prof", 
            "distg prof", "prof & chair", "univ prof", "prof emeritus")

saldeptprofs <- 
  saldept %>%
  filter(grepl("prof", position)) %>%
  mutate(
    prof_simp = ifelse(position %in% myprofs, position, "other")) %>%
  mutate(
    pos_simp = prof_simp) %>%
  mutate(
    pos_simp = ifelse(prof_simp == "distg prof", "prof", pos_simp),
    pos_simp = ifelse(prof_simp == "prof & chair", "prof", pos_simp),
    pos_simp = ifelse(prof_simp == "prof emeritus", "prof", pos_simp),
    pos_simp = ifelse(prof_simp == "univ prof", "prof", pos_simp)
    ) %>%
  filter(pos_simp != "other") %>%
  mutate(pos_simp = factor(pos_simp, levels = c("asst prof", "assoc prof", "prof")))


# look at agroomy dept ----------------------------------------------------

saldeptprofs %>%
  filter(dept == "agronomy") %>%
  filter(gender == "F") ->a
  
  group_by(fiscal_year, pos_simp, gender) %>%
  summarise(n = n()) %>%
  complete(fiscal_year, pos_simp, gender, fill = list(n = 0)) %>%
  
  
  ggplot(aes(fiscal_year, n, group = gender)) + 
  geom_point(aes(color = gender)) + 
  geom_line(aes(color = gender)) +
  facet_grid(~pos_simp)



