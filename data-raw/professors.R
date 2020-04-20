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


sal_profs <- 
  salaries %>%
  
  mutate(title = tolower(title)) %>%
  
  filter(grepl("prof", title)) %>% 

  mutate(key = key_from_name(name))  %>% 
  # GN I know this is ugly but it works
  filter(!grepl("emer", title),
         !grepl("vstg", title),
         !grepl("res", title),
         !grepl("adj", title),
         !grepl("affil", title),
         !grepl("collab", title),
         !grepl("clin", title)) 

# do we have only what we want?
sal_profs %>% 
  select(title) %>% 
  arrange(title) %>% 
  distinct()
# yes


professors <- 
  sal_profs %>% 
  left_join(affiliation  %>%
              mutate(key = key_from_name(name)) %>% 
              select(-name),
            by = c("key","year")) %>% 
  select(year, key, gender, place_of_residence, title, base_salary, total_salary_paid, everything())

# Check to make sure professors are unique
tmp <- professors %>%
  group_by(year,key,name, total_salary_paid) %>%
  summarize(n = n()) %>%
  ungroup()

tmp  %>%
  summarize(maxn = max(n)) # should be 1

tmp %>%
  arrange(-n)

# GN - add DEPT1 to grouping - shows it's 100% a department code problem
tmp2 <- professors %>%
  group_by(year,key,name, total_salary_paid, DEPT1) %>%
  summarize(n = n()) %>%
  ungroup()

tmp2  %>%
  summarize(maxn = max(n)) # should be 1 (GN it is if you include DEPT1)

#--so this is problem that needs to be addressed earlier
tmp %>%
  filter(n > 1) %>% 
  left_join(professors) %>% 
  select(year, key, name, total_salary_paid, DEPT1, DEPT_SHORT_NAME)

# LE (3/30) I think for simplicity's sake...we should get rid of all the 
# ppl in temp who have more than one data entry (i.e. all the duplicates)
# it's 9 people
# GN I think we could fix it!

#--these are probably people w/dept codes listed incorrectly?
# GN definitely, I can look into this

dupes <- tmp %>%
  filter(n > 1) %>%
  distinct(key)

professors <- 
  professors %>%
  anti_join(dupes, by = "key")

tmp %>% 
  filter(n > 1) 
 
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
# Now it is 467

# GN If we take the ppl w/unkn dept and merge by first and last, we get

affiliation2 <- 
  affiliation %>% 
  tidyr::separate(name, into = c("last", "first", "middle")) %>%
  tidyr::unite(last, first, col = "name", sep = " ", remove= TRUE) %>% 
  mutate(key = key_from_name(name)) %>% 
  select(-name, -middle)


unkns %>% 
  select(-(DEPT1:DEPT_SHORT_NAME)) %>% 
  tidyr::separate(name, into = c("last", "first", "middle")) %>% 
  mutate(name = paste(last, first, sep = " "),
         key = key_from_name(name)) %>%
  left_join(affiliation2,
            by = c("key","year")) %>% 
  filter(is.na(DEPT1)) %>% 
  arrange(total_salary_paid) %>% 
  select(year, last, first, middle, gender, place_of_residence, title, base_salary, key, DEPT1) %>% 
  filter(base_salary > 0)

#--31 ppl w/unknown depts and more than $0 salaries



# Ok it seems like an issue is that some people still don't have middle names so we should make a 
# function that does a partial join...?

# OR just join by first and last name....? Issues with duplicates again, but could
# also just delete those...

# I think names from affiliation will always(?) be nested within names from salaries

# usethis::use_data(professors, overwrite = TRUE)
