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

library(tidyverse)
library(janitor) #--cleans things nicely
library(readxl) #--reads excel spreadsheets
# library(fuzzyjoin) #--so Jon H. Lee is the same as Jon Lee, not used but could be useful




# Clean and write dept-org info -------------------------------------------

orgraw <- readxl::read_excel("data-raw/_raw/Employees with Department and Org 4-8-19.xlsx")

# just keep org and dept as a key
org <- 
  orgraw %>%
  janitor::clean_names() %>%
  dplyr::select(org_short_name, drcty_dept_name) %>%
  dplyr::rename("college" = 1,
         "dept" = 2) %>%
  dplyr::mutate_if(is_character, tolower) %>%
  dplyr::distinct() %>%
  arrange(college, dept)

org %>% readr::write_csv("data-raw/_tidy/td_org-dept-key.csv")


# read name-dept data from tabs-------------------------------------------


#-- the last tab is not in the same format as the others, must be dealt with separately, or we get a 
# JBN:  ^^^^ first?
# JBN: how about using readxl::excel_sheets
# JBN: or since this would be a one time change perhaps just fix the sheet to be like the others?

tyear <- 2011:2018
rawdat <- readxl::read_excel("data-raw/_raw/January snapshot data for GN 2012-2019.xlsx", 
                             sheet = 1) %>%
  dplyr::mutate(year  = tyear[1])

for (i in 2:length(tyear)){
  
  tdat <- readxl::read_excel("data-raw/_raw/January snapshot data for GN 2012-2019.xlsx", 
                             sheet = i) %>%
    dplyr::mutate(year  = tyear[i])

  rawdat <- rawdat %>%
    rbind(tdat) # maybe use dplyr::bind_rows
}

dat <- 
  rawdat %>%
  dplyr::select(year, DEPT_SHORT_NAME, LAST_NAME, FIRST_NAME) %>%
  dplyr::arrange(year, DEPT_SHORT_NAME, LAST_NAME) %>%
  dplyr::rename("dept" = DEPT_SHORT_NAME,
         "last_name" = LAST_NAME,
         "first_name" = FIRST_NAME,
         "fiscal_year" = year) %>%
  
  dplyr::filter(!is.na(dept)) %>%  # JBN: do we want to filter at this point?
  
  dplyr::mutate_if(is_character, tolower) %>%
  
  # merging name columns together
  tidyr::unite(name, last_name, first_name, sep = " ") %>%
  
  # truncating names after 20 characters
  dplyr::mutate(name = str_trunc(name, 20, ellipsis = "")) %>%
  tidyr::separate(name, c("last_name", "first_name"), sep = " ") %>% # JBN: I received a bunch of warnings messages here
  dplyr::arrange(fiscal_year, dept, last_name, first_name)

dat %>%
  readr::write_csv("data-raw/_tidy/td_empl-dept-key.csv")
