# Merge salaries and affiliations based on a key constructed from the 
# last_name first_name middle_initial

# I changed things so this just worked in the project directory

library("dplyr") # for %>%
library("ggplot2")
library(fuzzyjoin) #--what's the difference betweeen loading w/quotes?

key_from_name <- function(x) {
  tolower(sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", x)) # keep up to 3rd space
}

data("affiliation")
data("departments")
data("salaries")

# LE - going to try fuzzy join on 4 largest departments
dept_nums <- readr::read_csv("data-raw/departments/department_numbers.csv")
  
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
  # I'm going to mannually recode this E. Walter Anderson, who is a problem in the fuzzy join
  mutate(name = recode(name, "Anderson E" = "Anderson E Walter"),
         key = key_from_name(name),
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
  regex_inner_join(sal_profs, dept_affs_FJ, by = c(key2 = "key_regex", year = "year"))

test_fj %>%                              # we get two more people from the fuzzy join, worth it!
  group_by(year.x, DEPT_SHORT_NAME) %>%
  tally() %>%
  filter(year.x == 2019) %>%
  left_join(dept_nums, by = "DEPT_SHORT_NAME")

# numbers are still off by 1 sometimes (and 3 extra in MECH ENG?, oh well)

# any duplicates?
tmp <- test_fj %>%
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
  # adding a starts with regex...
  mutate(key_regex = paste0("^", key2, "*+")) %>%
  select(-key, -key2)

# join 'em! 
top10_fj <- 
  regex_inner_join(sal_profs, top10_dept_affs, by = c(key2 = "key_regex", year = "year"))

top10_fj %>%                              
  group_by(year.x, DEPT_SHORT_NAME) %>%
  tally() %>%
  filter(year.x == 2019) %>%
  arrange(-n) %>%
  left_join(dept_nums)
# most of our numbers are over...we are missing 3 in school of ed. Idk i think it's ok

# checking for dupes
tmp <- top10_fj2 %>%
  group_by(year.x, key , total_salary_paid) %>%
  summarize(n = n()) %>%
  ungroup()

tmp  %>%
  summarize(maxn = max(n)) # Uh, oh....

tmp %>%
  filter(n == 2)

# Current issues:
# David Zimmerman L is also getting matched with David Zimmerman C....which is weird
#     We just want David Zimmerman L who is an assoc prof in ENG, DCZ is not a prof but in Agronomy
# The correct Hongwei Zhang should be in Electrical Enginering, NOT AGRONOMY, that person is a postdoc (this is a duplicate name)
# Jianming Yu is also being joined with Jie Yu...so that's a problem with the join
# Zhao Zhang is joined with Zhan Zhang in one year....that's a problem with the join


# So in general we need fuzzy join to be a little more exact. I wonder if we should just go through 
# duplicates by hand and fix them...?

# 5/11/20 ^ I now think this is our best move

# Going to move forward with all departments with at least 20 faculty in them

dept_focus <- dept_nums %>%
  select(DEPT_SHORT_NAME) %>%
  unlist() %>% unname() %>%
  stringr::str_remove("-AGLS") %>%
  stringr::str_remove("-LAS")

# quick trying two different ways to fuzzy join first with "*" and second without;
# trying to figure out where I can get less errors 

dept_affs <- 
  profs_affil  %>%
  filter(DEPT_SHORT_NAME %in% dept_focus) %>%
  # adding a starts with regex...
  mutate(key_regex = paste0("^", key2)) %>%
  select(-key, -key2) %>%
  mutate(key_regex = stringr::str_trim(key_regex, side = "right"),
         key_regex = paste0(key_regex, "*"))

prof_depts_fj <- 
  regex_inner_join(sal_profs, dept_affs, by = c(key2 = "key_regex", year = "year"))

tmp <- prof_depts_fj %>%
  group_by(year.x, key , total_salary_paid) %>%
  summarize(n = n()) %>%
  ungroup()

problems <- tmp %>%
  filter(n == 2) %>%
  arrange(key)
# 14 people we need to fix 

# second type

dept_affs2 <- 
  profs_affil  %>%
  filter(DEPT_SHORT_NAME %in% dept_focus) %>%
  # adding a starts with regex...
  mutate(key_regex = paste0("^", key2)) %>%
  select(-key, -key2) %>%
  mutate(key_regex = stringr::str_trim(key_regex, side = "right"))

prof_depts_fj2 <- 
  regex_inner_join(sal_profs, dept_affs2, by = c(key2 = "key_regex", year = "year"))

# how many problems are there? 

tmp2 <- prof_depts_fj2 %>%
  group_by(year.x, key , total_salary_paid) %>%
  summarize(n = n()) %>%
  ungroup()

problems2 <- tmp2 %>%
  filter(n == 2) %>%
  arrange(key)
# 4 people we need to fix - huzzah! 

# i. get rid of andrews james, only want to keep andrews james t  
# ii. wang li - only want to keep STATS
# iii. wang lizhi - get rid of, matched with wang li but a prof in DEPT we aren't considering...
# iv. zhang hongwei keep ELEC ENG/CP ENG, get rid of Agronomy, post doc! 

# let's just look at where the two joins differ
discrep <- 
  anti_join(prof_depts_fj %>% select(year.x, title, key2, DEPT_SHORT_NAME, key_regex) %>%
              mutate(key_regex = stringr::str_remove_all(key_regex, "\\*")),
          prof_depts_fj2 %>% select(year.x, title, key2, DEPT_SHORT_NAME, key_regex))
# ^ Awesome, so it looks like all the discrepancies between the two types of fuzzy joins
# are actually spelling errors. 


anti_join(prof_depts_fj2 %>% select(year.x, title, key2, DEPT_SHORT_NAME),
          prof_depts_fj %>% select(year.x, title, key2, DEPT_SHORT_NAME)) # ok! 

# compare our numbers with the university 
prof_depts_fj2 %>%                              
  group_by(year.x, DEPT_SHORT_NAME, title) %>%
  tally() %>%
  filter(year.x == 2019) %>%
  arrange(-n) %>%
  left_join(dept_nums)
# in most cases we have higher numbers than the official record 

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


