# Merge salaries and affiliations based on a key constructed from the 
# last_name first_name middle_initial
# 

library("dplyr") # for %>%

key_from_name <- function(x) {
  tolower(sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", x)) # keep up to 3rd space
}

load("../data/affiliation.rda")
load("../data/departments.rda")
load("../data/salaries.rda")


professors <- salaries %>%
  mutate(title = tolower(title)) %>%
  
  filter(grepl("prof", title)) %>% 

  mutate(key = key_from_name(name)) %>%
  # select(-name) %>% 
  left_join(affiliation  %>%
              mutate(key = key_from_name(name)) %>% 
              select(-name),
            by = c("key","year")) 

# Check to make sure professors are unique
tmp <- professors %>%
  group_by(year,key,name) %>%
  summarize(n = n()) %>%
  ungroup()

tmp  %>%
  summarize(maxn = max(n)) # should be 1

tmp %>%
  arrange(-n)

# LE (3/30) I think for simplicity's sake...we should get rid of all the 
# ppl in temp who have more than one data entry (i.e. all the duplicates)
# it's 9 people

dupes <- tmp %>%
  filter(n > 1) %>%
  distinct(key)

professors <- 
  professors %>%
  anti_join(dupes, by = "key")

# LE (3/31) If we want to track people before 2012 (when we have directory data)
# then we neeed to interpolate..

tmp2 <-
  professors %>%
  group_by(name) %>%
  tidyr::fill(DEPT1:DEPT_SHORT_NAME, .direction = "up")

unkns <- 
  tmp2 %>%
  filter(is.na(DEPT1)) %>%
  filter(year > 2011)    # NA values for dept before this time are bc ppl aren't in directory

unkns %>%
  distinct(name)

# 657 people don't still don't have a department...??!?

# Ok it seems like an issue is that some people still don't have middle names so we should make a 
# function that does a partial join...?

# OR just join by first and last name....? Issues with duplicates again, but could
# also just delete those...

# I think names from affiliation will always(?) be nested within names from salaries

# usethis::use_data(professors, overwrite = TRUE)
