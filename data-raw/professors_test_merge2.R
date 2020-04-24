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




# example of problem merge ------------------------------------------------

#--ugh I need to find a good example


# try fuzzy join? ---------------------------------------------------------

library(dplyr)
library(fuzzyjoin)
library(ggplot2)
data(diamonds)

d <- tibble(approximate_name = c("Idea", "Premiums", "Premioom",
                                 "VeryGood", "VeryGood", "Faiir"),
            type = 1:6)

# no matches when they are inner-joined:
diamonds %>%
  inner_join(d, by = c(cut = "approximate_name"))

# but we can match when they're fuzzy joined
diamonds %>%
  stringdist_inner_join(d, by = c(cut = "approximate_name"))

affil_tim %>% 
  stringdist_left_join(sal_tim, by = c(key = "key"))