########################
# author: Gina
# created: July 10 2019
#
# last updated: July 25 2019 (what the hell was I doing?)
#               July 31 2019 (Cindy updated the tabs)
#
# purpose: wrangle data received from Cindy Van Loon on July 10 2019
#          into clean consistent formats to store in CyChecks package
# 
# inputs: Employees with Department and Org
#         January snapshot data for GN 2012-2019
#
# outputs: 
#
# notes: I'm assuming that dept-person data taken 1-31-2012 is valid for the year 2011, etc.
#
########################


rm(list = ls())
library(tidyverse)
library(janitor) #--cleans things nicely
library(readxl) #--reads excel spreadsheets
library(here) #--helps w/wd things

library(fuzzyjoin) #--so Jon H. Lee is the same as Jon Lee, not used but could be useful
setwd(here())



# Clean and write dept-org info -------------------------------------------

orgraw <- read_excel("data-raw/_raw/Employees with Department and Org 4-8-19.xlsx")

# just keep org and dept as a key
org <- 
  orgraw %>%
  clean_names() %>%
  select(org_short_name, drcty_dept_name) %>%
  rename("college" = 1,
         "dept" = 2) %>%
  mutate_if(is_character, tolower) %>%
  distinct() %>%
  arrange(college, dept)

org %>% write_csv("data-raw/_tidy/td_org-dept-key.csv")


# read name-dept data from tabs-------------------------------------------


#-- the last tab is not in the same format as the others, must be dealt with separately, or we get a 

tyear <- c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
rawdat <- read_excel("data-raw/_raw/January snapshot data for GN 2012-2019.xlsx", sheet = 1) %>%
  mutate(year  = tyear[1])

for (i in 2:length(tyear)){
  
  tdat <- read_excel("data-raw/_raw/January snapshot data for GN 2012-2019.xlsx", sheet = i) %>%
    mutate(year  = tyear[i])

  rawdat <- rawdat %>%
    rbind(tdat)
}
  
  

dat <- 
  rawdat %>%
  select(year, DEPT_SHORT_NAME, LAST_NAME, FIRST_NAME) %>%
  arrange(year, DEPT_SHORT_NAME, LAST_NAME) %>%
  rename("dept" = DEPT_SHORT_NAME,
         "last_name" = LAST_NAME,
         "first_name" = FIRST_NAME,
         "fiscal_year" = year) %>%
  filter(!is.na(dept)) %>%
  mutate_if(is_character, tolower) %>%
  arrange(fiscal_year, dept, last_name, first_name)

dat %>%
  write_csv("data-raw/_tidy/td_empl-dept-key.csv")

# What if there is more than one sarah adams?
filter(dat, first_name == "sarah" & last_name == "adams")
