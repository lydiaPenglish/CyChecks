# Merge salaries and affiliations based on a key constructed from the 
# last_name first_name middle_initial; use fuzzy join to account for missing middle initials

library(dplyr) # for %>%
library(fuzzyjoin)

# form: last first middle
key_from_name <- function(x) {
  tolower(sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", x)) # keep up to 3rd space
}

# Load the data ------------------------------------------------------------

data("affiliation")
data("salaries")

# Breakdown of professor numbers from departments with over 20 professors (from Institutional Research)
dept_nums <- readr::read_csv("data-raw/departments/department_numbers.csv")

# salary data filtered for professors positions only
sal_profs <- 
  salaries %>%
  
  mutate(title = tolower(title)) %>%
  
  filter(grepl("prof", title)) %>% 
  
  mutate(key = key_from_name(name),
  
  # LE I found hyphens to be a problem, they are in the salary data, 
  # but not in affiliation, so I'm getting rid of them
         key = stringr::str_replace_all(key, "-", " ")) %>%
  
  # GN get rid of "special" profs, not interested in them
  filter(!grepl("emer|vstg|res|adj|affil|collab|clin", title),
         year > 2011)                                 # LE - let's just focus on where we have directory data from 

# getting rid of department endings that distinguish college, let's lump them together
patterns <- c("-AGLS|-LAS|-HSCI|-A|-E")

# department data 
profs_affil  <- 
  affiliation %>%
  # I'm going to mannually recode these ppl who I found to be issues
  mutate(name = recode(name, "Anderson E" = "Anderson E Walter",
                             "Windus Theresa Lynn" = "Windus Theresa L"),
         key = key_from_name(name),
         # same here, key without hyphens     
         key = stringr::str_replace_all(key, "-", " "),
         # getting rid of patterns listed above
         DEPT_SHORT_NAME = stringr::str_remove_all(DEPT_SHORT_NAME, patterns),
         # recoding BBMB to be the same name (las and agls named them differently)
         DEPT_SHORT_NAME = recode(DEPT_SHORT_NAME, "BIOCH/BIOPH" = "BBMB")) %>% 
  # need to recode the people who are housed in centers so they are in the correct tenure home - sorry gross manual edits
  mutate(
    DEPT_SHORT_NAME = if_else(key == "goggi alcira susana", "AGRONOMY", DEPT_SHORT_NAME),
    DEPT_SHORT_NAME = if_else(key == "johnson lawrence", "FOOD SC/HN", DEPT_SHORT_NAME),
    DEPT_SHORT_NAME = if_else(key == "niederhauser dale s" | key == "thompson elizabeth a", 
                              "SCHOOL OF ED", DEPT_SHORT_NAME)) %>%
  select(-name)

# Getting a merged dataset for all departments with over 20 professors -----

dept_affs <- 
  profs_affil  %>%
  # filter for select departments
  filter(DEPT_SHORT_NAME %in% dept_nums$DEPT_SHORT_NAME) %>%
  # add to regex for fuzzy joining
  mutate(key_regex = paste0("^", key)) %>%
  select(-key) %>%
  # trim any white space on the end of a name
  mutate(key_regex = stringr::str_trim(key_regex, side = "right"))

# join together - inner join (nb this takes a minute)
professors_fj <- 
  regex_inner_join(sal_profs, dept_affs, by = c(key = "key_regex", year = "year"))

# Trouble shooting - is this data correct??!? --------------------------------

# Duplicates - 4 people will need to be fixed
dupes <- professors_fj %>%
  group_by(year.x, key , total_salary_paid) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(n > 1)
# Manual fixes for these four people: 
professors <- 
  professors_fj %>%
# i. get rid of andrews james, only want to keep andrews james t  
  filter(!(key == "andrews james t" & DEPT_SHORT_NAME == "ENGLISH"),
# ii. wang li - get rid of EEOB, keep stats
         !(key == "wang li" & DEPT_SHORT_NAME == "EEOB"),
# iii. wang lizhi - get rid of this person altogether, they matched with wang li but they are a prof in DEPT we aren't considering...
         !(key == "wang lizhi"),
# iv. zhang hongwei keep ELEC ENG/CP ENG, get rid of Agronomy (post doc!)
         !(key == "zhang hongwei" & DEPT_SHORT_NAME == "AGRONOMY"))


# Was anything joined incorrectly bc of using fuzzy? Comparing key and key_regex...
name_diffs <- 
  professors %>% 
  select(year.x, key, key_regex) %>%
  mutate(key_regex = stringr::str_remove_all(key_regex, "\\^"),
         same_name = if_else(key == key_regex, "yes", "no")) %>%
  filter(same_name == "no")
# I looked through these and I think they look good. Different bc of a middle initial


# Did we get everyone?? (This will be the hardest...focusing on 2019 first)

# We already know we need to add Prashant Jha to Agronomy - haven't done this yet!! (5/13 LE)

# Compare our numbers with institutional research 
# our numbers
fj_nums <- 
  professors %>%
  filter(year.x == 2019) %>%
  group_by(DEPT_SHORT_NAME, title) %>%
  tally() %>%
  tidyr::pivot_wider(id_cols = DEPT_SHORT_NAME, names_from = title, values_from = n) %>%
  rowwise(.) %>%
  mutate(prof = sum(c(prof, `distg prof`, `morrill prof`, `prof & chair`,
                      `univ prof`), na.rm = TRUE)) %>%
  select(DEPT_SHORT_NAME, prof, assoc_prof = `assoc prof`, asst_prof = `asst prof`) %>%
  mutate(tot = sum(prof, assoc_prof, asst_prof, na.rm = TRUE))

compare_nums <- left_join(fj_nums, dept_nums, by = "DEPT_SHORT_NAME")
# The majority of cases we have more ppl in our dataset than reported by IR, hmmm, ok!

# Here is where we are missing 2 or more ppl 
# school of ed - missing 3 ppl
# chemistry - missing 3 ppl (fixed this so now only missing 2, idk!)
# (BBMB - missing half of profs (!!), probably a bug somewhere) - LE fixed this earlier in the script
# MUSIC & THEATRE - missing 2 ppl 

# selecting columns for dataset
professors <- 
  professors %>%
  select(year = year.x, ORG_SHORT_NAME, DEPT_SHORT_NAME, name = key, gender, title, base_salary, 
         total_salary_paid, travel_subsistence)

usethis::use_data(professors, overwrite = TRUE)



# Comparing this dataset with our very initial attempt at joining -----------------

data("cyd_salprofs")

# tweaking our initial attempt to match new professors dataframe
init_merge <- cyd_salprofs %>%
  mutate(DEPT_SHORT_NAME = stringr::str_to_upper(dept),
         DEPT_SHORT_NAME = recode(DEPT_SHORT_NAME, "BIOCH/BIOPH" = "BBMB")) %>%
  filter(DEPT_SHORT_NAME %in% dept_nums$DEPT_SHORT_NAME) %>%
  filter(fiscal_year > 2011) %>%
  select(year = fiscal_year, name, DEPT_SHORT_NAME, title = position) %>%
  mutate(name = key_from_name(name),
         name = stringr::str_replace_all(name, "-", " "),
         name = stringr::str_remove_all(name, "jr"))

6024 - 5938   # our dataset has 86 more ppl, which is good! 

not_joined <- anti_join(professors %>% select(year, name, DEPT_SHORT_NAME, title),
          init_merge)

# We didn't include chairs in our initial attempt at joining so I'm filtering those out
nj2 <- not_joined %>%
  filter(!(stringr::str_detect(title, "chair")))
# ^ idk why these ppl aren't (for the most part) in our initial attempts, but glad we have them now! 

# Past work worth keeping -------------------------------------------------------
# Professors in affiliation who don't have a department FIXED! GN 4/26

affiliation <- affiliation  %>%
  mutate(key = key_from_name(name)) %>% 
  select(-name)

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


