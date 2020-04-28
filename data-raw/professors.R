# Merge salaries and affiliations based on a key constructed from the 
# last_name first_name middle_initial

library("dplyr") # for %>%
library("ggplot2")

# form: last first middle
key_from_name <- function(x) {
  tolower(sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", x)) # keep up to 3rd space
}

load("../data/affiliation.rda")
load("../data/departments.rda")
load("../data/salaries.rda")


############### sick of doing this manually every time
affiliation <- affiliation  %>%
  mutate(key = key_from_name(name)) %>% 
  select(-name)
#######################

sal_profs <- 
  salaries %>%
  
  mutate(title = tolower(title)) %>%
  
  filter(grepl("prof", title)) %>% 

  mutate(key = key_from_name(name))  %>% 
  # GN get rid of "special" profs, not interested in them
  filter(!grepl("emer|vstg|res|adj|affil|collab|clin", title),
         year > 2011)                                 # LE - let's just focus on where we have directory data from 

# do we have only what we want?
sal_profs %>% 
  select(title) %>% 
  arrange(title) %>% 
  distinct()
# yes

# try joining based on key
professors <- 
  sal_profs %>% 
  left_join(affiliation,
            by = c("key","year")) %>% 
  select(year, key, gender, place_of_residence, title, base_salary, total_salary_paid, everything())

################ NOTE
#--a lot of people are just missing DEPT info for one year - we have info for other years. 
#--ex mcnicholl timothy
professors %>% 
  filter(key == "mcnicholl timothy") %>% 
  select(key, DEPT1)

# Could we do a group_by and fill at some point in this?
professors %>% 
  group_by(key, gender, place_of_residence, title) %>% 
  tidyr::fill(DEPT1, .direction = c("updown")) %>% 
  filter(key == "mcnicholl timothy") %>% 
  select(key, DEPT1)

#####################


# LE - OK! so step 1 is picking a cutoff threshold for number of profs and 
# filtering by those departments...
 
# Step 2 is dealing with any merge issues, for example:
#      i. Duplicate names
#      ii. Professors in affiliation who don't have a department (easy to figure out)
#      iii. Professors not listed in affiliation (not as easy to figure out...)
#             iiib. Or professors who's names aren't matching 


#. i. Duplicates - update this section once we filter for departments first

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

#  -- so this is problem that needs to be addressed earlier
tmp %>%
  filter(n > 1) %>% 
  left_join(professors) %>% 
  select(year, key, name, total_salary_paid, DEPT1, DEPT_SHORT_NAME)

#--this seems like a list of people where in the affiliation there are at least two people with that name?

dupes <- tmp %>%
  filter(n > 1) %>% 
  left_join(professors) %>% 
  select(key) %>%
  distinct() %>% 
  pull()

#--No idea how to fix this one. 
affiliation %>% 
  filter(key == dupes[2])

sal_profs %>% 
  filter(key == dupes[2]) %>% 
  select(year, key, gender, title, total_salary_paid)


# ii. Professors in affiliation who don't have a department FIXED! GN 4/26

no_depts <-
  affiliation %>%
  filter(is.na(DEPT_SHORT_NAME)) %>%
  mutate(key = stringr::str_trim(key))

# join no_depts with sal_profs - i.e. how many profs have no dept listed
unkn_profs <- inner_join(no_depts, sal_profs, by = c("year", "key")) 

no_depts_list <- distinct(unkn_profs, key) %>% pull() # ok, if I did this right then there are only 6 "profs" who are
# in the affiliation dataframe but have `NA` as their department.
no_depts_list
# This is good! We have to be careful here though - David Peterson
# is on this list  and we already know he's a duplicate and that
# he is listed in POL SCI as a prof
# GN- I filtered out the 'typo' dept in the affilition code to get rid of the wrog David Peterson
# GN - I assigned DEPT1 7090 to be "ART/VISUAL CULT" in affiliation data. See note in that code.

#--Prashant Jha
affiliation %>%   
  mutate(key = key_from_name(name)) %>% 
  filter(grepl("prashant", key)) %>% 
  select(name, key)

# He is in the salary database. 
sal_profs %>% 
  filter(grepl("jha prashant", key)) %>% 
  select(name, key)
# He might be missing from affiliation bc of his hire date. 


# iii. How many people have been merged unsuccesfully?

unkns <- 
  professors %>%
  filter(is.na(DEPT_SHORT_NAME)) 

# GN If we take the ppl w/unkn dept and merge by first and last, we get
affiliation2 <- 
  affiliation %>% 
  tidyr::separate(name, into = c("last", "first", "middle")) %>%
  tidyr::unite(last, first, col = "name", sep = " ", remove= TRUE) %>% 
  mutate(key = key_from_name(name)) %>% 
  select(-name, -middle)

# then we can try joining the subset of unknowns by this new "key"
new_unkns <- 
  unkns %>% 
  select(-(DEPT1:DEPT_SHORT_NAME)) %>% 
  tidyr::separate(name, into = c("last", "first", "middle")) %>% 
  mutate(name = paste(last, first, sep = " "),
         key = key_from_name(name)) %>%
  select(-name, -middle, -last, -first) %>%
  left_join(affiliation2,
            by = c("key","year")) %>% 
  filter(is.na(DEPT1)) 

still_unknown <-                
  distinct(new_unkns, key) %>%   # these are unknown people 
  # LE - but now I'm going to join them with affiliation without looking at year bc it seems like
  # some people were just not listed in the correct year
  left_join(affiliation2 %>% select(-year) %>% distinct()) %>%
  # filter again for unknowns --- these are the REAL problems
  filter(is.na(DEPT_SHORT_NAME))

# ^ seems like these people are actually absent from affiliation, but perhaps they are new like 
# Prashant Jha...or they were just never listed...

#--how many seem to be a middle name merge problem?
# 1783. 
# let me investigate fuzzy joining them in professors_test_merge2
# GN UPDATE: I think we can do a fuzzy join using for each to do it on multiple cores. 
# let me play with it, but the fuzzy join using regex seems to work?
unkns %>% 
  select(-(DEPT1:DEPT_SHORT_NAME)) %>% 
  tidyr::separate(name, into = c("last", "first", "middle")) %>% 
  mutate(name = paste(last, first, sep = " "),
         key = key_from_name(name)) %>%
  select(-name, -middle, -last, -first) %>%
  left_join(affiliation2,
            by = c("key","year")) %>% 
  select(year, key, DEPT_SHORT_NAME)


# ackerman, ralph
professors %>% 
  filter(grepl("ackerman", key)) %>% 
  select(year, key)
# usethis::use_data(professors, overwrite = TRUE)
