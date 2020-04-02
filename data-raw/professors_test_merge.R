# PROF test merge by first and last name, instead of full name

library("dplyr") # for %>%

load("../data/affiliation.rda")
load("../data/departments.rda")
load("../data/salaries.rda")

# salary data for merge
sal_dat <- salaries %>%
  filter(grepl("PROF", title)) %>% 
  mutate(name = stringr::str_to_lower(name)) %>%
  tidyr::separate(name, into = c("last", "first", "middle"), remove = FALSE,
                  sep = "\\s") 
aff_dat <- affiliation %>%
  mutate(name = stringr::str_to_lower(name)) %>%
  tidyr::separate(name, into = c("last", "first", "middle"), 
                  sep = "\\s", remove = FALSE) %>%
  mutate(first = na_if(first, ""))

# I don't understand why there are more entries now in test than there were in tt...  
test_join <- 
  left_join(sal_dat, aff_dat, by = c("year", "first", "last"))  %>%
  # right now this duplicates columns...fix later
  group_by(last, first, middle.x) %>%
  # fill in info for pre-2012 and 2019, if necessary
  tidyr::fill(DEPT1:DEPT_SHORT_NAME, .direction = "updown")

tmp <- test_join %>%
  group_by(year, last, first, middle.x) %>%
  summarize(n = n()) %>%
  ungroup()

tmp  %>%
  summarize(maxn = max(n)) # should be 1

tmp %>%
  arrange(-n)

# bleh just get rid of these for now...
dupes <- tmp %>%
  filter(n > 1) %>%
  distinct(last, first, middle.x)

profs <- 
  test_join %>%
  anti_join(dupes, by = c("last", "first", "middle.x"))
  
# unkns = ppl who still don't have an affiliation match
unkns <- 
  profs %>%
  filter(is.na(DEPT1)) %>%
  filter(year > 2011)

unkns %>%
  distinct(name.x)
# only 106 ppl with no deparment info, better than joining by full name!

# ISSUES 
# 1. hyphens in people's first name? Sometimes there, sometimes not
# 2. People's middle name changing at some point (i.e. from just "K" to "Kirk")
# 3. People actually not in directory data 