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

  
# usethis::use_data(professors, overwrite = TRUE)
