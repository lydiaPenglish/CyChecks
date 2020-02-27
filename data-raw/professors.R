# Merge salaries and affiliations based on a key constructed from the 
# last_name first_name middle_initial
# 
key_from_name <- function(x) {
  tolower(sub("^(\\S*\\s+\\S+).*", "\\1", x)) # keep up to 2nd space
}

load("../data/affiliation.rda")
load("../data/departments.rda")
load("../data/salaries.rda")

professors <- salaries %>%
  mutate(position = tolower(position)) %>%
  
  # filter(grepl("prof", position)) # could do this
  filter(position %in% c("asst prof","assoc prof","prof")) %>%
  rename(rank = position) %>%

  mutate(key = key_from_name(name)) %>%
  # select(-name) %>% 
  left_join(affiliation %>% 
              mutate(key = key_from_name(name)),
            by = "key") 

professors %>% 
  group_by(year,key) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
