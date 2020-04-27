# Merge salaries and affiliations based on a key constructed from the 
# last_name first_name middle_initial

library("dplyr") # for %>%
library("ggplot2")

key_from_name <- function(x) {
  tolower(sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", x)) # keep up to 3rd space
}

load("../data/affiliation.rda")
load("../data/departments.rda")
load("../data/salaries.rda")


# filter salaries to just profs in yrs > 2011 -----------------------------

sal_profs <- 
  salaries %>%
  
  mutate(title = tolower(title)) %>%
  
  filter(grepl("prof", title)) %>% 

  mutate(key = key_from_name(name))  %>% 
  # GN get rid of "special" profs, not interested in them
  filter(!grepl("emer|vstg|res|adj|affil|collab|clin", title),
         year > 2011)                                 # LE - let's just focus on where we have directory data from 

profs_affil  <- 
  affiliation %>%
  mutate(key = key_from_name(name)) %>% 
  select(-name)

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


