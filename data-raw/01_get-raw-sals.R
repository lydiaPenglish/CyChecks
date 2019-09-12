########################
# author: Gina, but really Lydia
# created: July 31 2019
#
# last updated: July 31 2019
#               September 6 2019 by LE
#
# purpose: get raw sal data and write it
# 
# inputs: 
# outputs: 
#
# notes: 
########################


rm(list = ls())
library("jsonlite")
library("tidyverse")


# Right now this is Lydia's token...
token <- "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj"
limit <- 150000 # Max number of entries
offset <- 0 # Where to start gathering from (0 = the beginning)
fiscal_year <- "2018" # Will we just have different dataframes for each year?

#--is there a way to just get one year at a time?
#-- answer: Yes. We can add a fiscal year to the url parse (see commented url below)
#-- I'm sure there's also a way for us to choose which years we get
#url <- sprintf("https://data.iowa.gov/resource/s3p7-wy6w.json?%s&$limit=%d&$offset=%d&$order=:id&department=Iowa%%20State%%20University&fiscal_year=%d", token, limit, offset, fiscal_year)

url <- sprintf("https://data.iowa.gov/resource/s3p7-wy6w.json?%s&$limit=%d&$offset=%d&$order=:id&department=Iowa%%20State%%20University", token, limit, offset)

salraw <- tibble::as_tibble(fromJSON(url))

salraw %>% write_csv("data-raw/_tidy/td_rawsals.csv")
