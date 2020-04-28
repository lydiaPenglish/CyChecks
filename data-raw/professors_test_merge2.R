# Merge salaries and affiliations based on a key constructed from the 
# last_name first_name middle_initial

library("dplyr") # for %>%
library("ggplot2")
library(fuzzyjoin) #--what's the difference betweeen loading w/quotes?

key_from_name <- function(x) {
  tolower(sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", x)) # keep up to 3rd space
}

load("../data/affiliation.rda")
load("../data/departments.rda")
load("../data/salaries.rda")

# LE - going to try fuzzy join on 4 largest departments
dept_nums <- readr::read_csv("departments/department_numbers.csv")
  
top_depts <- dept_nums %>%
  arrange(-TOTAL) %>%
  top_n(4) %>%              
  select(DEPT_SHORT_NAME) %>%
  unlist() %>% unname()


# filter salaries to just profs in yrs > 2011 -----------------------------

sal_profs <- 
  salaries %>%
  
  mutate(title = tolower(title)) %>%
  
  filter(grepl("prof", title)) %>% 

  mutate(key = key_from_name(name))  %>% 
  
  # LE I found hyphens to be a problem, they are in the salary data, 
  # but not in affiliation, I'm making a second key where they are replace by a space
  mutate(key2 = stringr::str_replace_all(key, "-", " ")) %>%
  
  # GN get rid of "special" profs, not interested in them
  filter(!grepl("emer|vstg|res|adj|affil|collab|clin", title),
         year > 2011)                                 # LE - let's just focus on where we have directory data from 

# getting rid of endings that distinguish college
patterns <- c("-AGLS|-LAS|-HSCI|-A|-E")

profs_affil  <- 
  affiliation %>%
  mutate(key = key_from_name(name),
    # same here, second key without hyphens     
         key2 = stringr::str_replace_all(key, "-", " "),
         DEPT_SHORT_NAME = stringr::str_remove_all(DEPT_SHORT_NAME, patterns)) %>% 
  select(-name)

# trying with top 4 departments  ------------------------------------------

dept_affs <- 
  profs_affil  %>%
  filter(DEPT_SHORT_NAME %in% top_depts)

# testing regular, non-fuzzy join...
test_join <- 
  left_join(dept_affs, sal_profs, by = c("year", "key2"))

# numbers
test_join %>%
  filter(!(is.na(name))) %>%
  group_by(year, DEPT_SHORT_NAME) %>%
  tally() %>%
  filter(year == 2019)                 
# compared with...
dept_nums %>%
  filter(DEPT_SHORT_NAME %in% top_depts)

# overall not bad! 

# try a fuzzy join..
dept_affs_FJ <- 
  dept_affs %>%
  mutate(key_regex = paste0(key2, "*")) %>%
  select(-key, -key2)

test_fj <- 
  regex_left_join(sal_profs, dept_affs_FJ, by = c(key2 = "key_regex", year = "year"))

# get rid of everyone who didn't join
fj_joins <- test_fj %>% filter(!(is.na(DEPT_SHORT_NAME)))

fj_joins %>%                              # we get two more people from the fuzzy join, worth it!
  group_by(year.x, DEPT_SHORT_NAME) %>%
  tally() %>%
  filter(year.x == 2019)

# numbers are still off by 1 sometimes (and 3 extra in MECH ENG?, oh well)

# any duplicates?
tmp <- fj_joins %>%
  group_by(year.x,key2, total_salary_paid) %>%
  summarize(n = n()) %>%
  ungroup()

tmp  %>%
  summarize(maxn = max(n)) # 1! good! 

# LE- I say we progress onwards with fuzzy join! 

# going to try with even MORE departments... ----------------------------------

#Codes for depts
top_10_depts <- dept_nums %>%          # it's actually 11...
  arrange(-TOTAL) %>%
  top_n(10) %>%              
  select(DEPT_SHORT_NAME) %>%
  unlist() %>% unname()

# filter profs affiliation for these depts and make ready for join
top10_dept_affs <- 
  profs_affil  %>%
  filter(DEPT_SHORT_NAME %in% top_10_depts) %>%
  mutate(key_regex = paste0(key2, "*")) %>%
  select(-key, -key2)

# join 'em!
top10_fj <- 
  regex_left_join(sal_profs, top10_dept_affs, by = c(key2 = "key_regex", year = "year"))

# get rid of everyone else...
top10_joins <- top10_fj %>% filter(!(is.na(DEPT_SHORT_NAME)))

top10_joins %>%                              # we get two more people from the fuzzy join, worth it!
  group_by(year.x, DEPT_SHORT_NAME) %>%
  tally() %>%
  filter(year.x == 2019) %>%
  arrange(-n) %>%
  left_join(dept_nums)
# most of our numbers are over...we are missing 3 in school of ed. Idk i think it's ok

# checking for dupes
tmp <- top10_joins %>%
  group_by(year.x, key , total_salary_paid) %>%
  summarize(n = n()) %>%
  ungroup()

tmp  %>%
  summarize(maxn = max(n)) # Uh, oh....

tmp %>%
  filter(n == 2)

# hmm Marc Anderson is a prof in MANAGEMENT and shouldn't have been chosen so this isn't great...
# somehow Mary Anderson and E anderson are getting linked to him...

# LE - I will come back to this


# example of problem merge ------------------------------------------------

ralph <- 
  sal_profs %>% 
  filter(key == "ackerman ralph a") %>% 
  select(year, name, key) 

ralph

#--mostly, affiliations will be missing a middle 'name'
ralph_aff <- 
  profs_affil %>% 
  filter(grepl("ackerman ralph", key)) %>% 
  mutate(key_regex = paste0(key, "*")) %>% 
  select(year, DEPT1, DEPT_SHORT_NAME, key_regex)

ralph_aff  

# try fuzzy join? ---------------------------------------------------------

ralph %>% 
  regex_left_join(ralph_aff, by = c(key = "key_regex", year = "year"))


