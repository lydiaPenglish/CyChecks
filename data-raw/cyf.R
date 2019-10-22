########################
# author: Gina
# created: July 29 2019
#
# last updated: July 31 2019
#               Aug 29 2019
#               September 6 2019 (barely, by LE)
#               Sept 9 2019 (Gina, removing dupes, fixing Maria/Mari Salas-Fernandez)
#               Sept 13 updated github code to match cybox code
#               Sept 29 added 'named prof' category in prof4_simp column (4 cats now)
#                        de-anonymized things bc it's easier to trouble shoot at this point
#               Oct 2 GN; looked at non-merges, there are all kinds of problems including:
#                           spelling errors, punctuation in names, collaborating professor title
#                           (probably doesn't have a dept assignment?)
#               Oct 16 GN: updated prof categories based on Akelo mtg
#
# purpose: make CyChecks2 functions in a safe place
# 
# inputs: 
# outputs: cyd_salprofs in _tidy folder
#
# notes: Is there a way to make Lydia's function just give me a single year?
#        The website give us middle names. Cindy did not. 
#        I'd like to fix the dupes! (LE)
#        Make it remove the dupes for now, fix name separation to by by spaces only
#           Maria Salas-Fernandez is Mari Salas-Fernandez in the database...WTF
#
########################


rm(list = ls())
library(tidyverse)
library(janitor) #--cleans things nicely
library(readxl) #--reads excel spreadsheets
library(here) #--helps w/wd things
#library(fuzzyjoin) ##--merge by inexact matches (like smith jill with smith jill h)

library(digest) #--for anonymizing names

library("jsonlite")
library("tidyverse")

# cyf_getsals -------------------------------------------------------------
#--this one will get raw data, can it do just one year at a time?

# # Right now this is Lydia's token...
# token <- "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj"
# limit <- 150000 # Max number of entries
# offset <- 0 # Where to start gathering from (0 = the beginning)
# fiscal_year <- "2018" # Will we just have different dataframes for each year?
# 
# 
# url <- sprintf("https://data.iowa.gov/resource/s3p7-wy6w.json?%s&$limit=%d&$offset=%d&$order=:id&department=Iowa%%20State%%20University", token, limit, offset)


# cyf_tidysals ------------------------------------------------------------
# this one will:
#-make dates into just years (rather than hours?!)
#-get rid of place of residency and 'dept' column that is just ISU
#-eliminate non-numeric base salaries (hourly)
#-make salaries numeric
#-separate names into first and last (to merge w/our dept info) *not sure about this yet*
#-filter out when we don't have a position or a gender
#-only keep professors (makes trouble shooting name issues easier)

cyd_salsraw <- read_csv("data-raw/_tidy/td_rawsals.csv")
#salraw2 <- salraw %>% filter(fiscal_year == 2015)


cyf_TidySals <- function(mydata = cyd_salsraw){
  
  cyd_salstidy <- 
    
    mydata %>% 
    
    #--deal with dates
    mutate(fiscal_year = as.numeric(fiscal_year),
           base_salary_date = lubridate::ymd_hms(base_salary_date),
           base_salary_year = lubridate::year(base_salary_date)) %>%
    
    #--get rid of unwanted columns
    select(fiscal_year, base_salary_year,
           name, gender, position, 
           total_salary_paid, travel_subsistence, base_salary) %>%
    
    #--eliminate things that are per hour
    filter(!grepl("HR", base_salary)) %>%
    
    mutate(
      #--get rid of stupid commas in base_salary, make everything monetary into numeric    
      base_salary = as.numeric(gsub(",", "", base_salary)),
      total_salary_paid = as.numeric(total_salary_paid),
      travel_subsistence = as.numeric(travel_subsistence),
      
      #--get rid of any white space in character cols
      position = stringr::str_trim(position, side = "right"),
      name = stringr::str_trim(name, side = "right"),
      
      #--make name lower case, get rid of commas
      name = tolower(name),
      name = gsub(",", "", name),
      
      position = tolower(position)) %>%
    
    #--eliminate people who don't have a position or a gender
    filter(!grepl("\\*", position),
           !grepl("\\*", gender))  %>%
    
    #--only keep professors
    filter(grepl("prof", position)) %>%
    
    
    #--expand the name out, I think we have to
    separate(name, into = c("last_name", "first_name", "other"), sep = " ")
  
  return(cyd_salstidy)
  
}


cyd_salstidy <- cyf_TidySals() %>%
  mutate(first_name = str_remove_all(first_name, "[[:punct:]]"))


# cyf_salsdeptmerge -------------------------------------------------------
# this one will
#-associate each employee with a department
#-anonymize names, with a consistent identification throughout time

# proof the digest is working:
tibble(name = c("Mary D", "Mary D", "Mary A")) %>%
  mutate(anon = name %>% map(digest)) %>%
  unnest()

# removing these from departments
patterns <- c("-agls|-las|-hsci")

cyd_dept <- 
  read_csv("data-raw/_tidy/td_empl-dept-key.csv") %>%
  
  # recoding redundant departments
  mutate(dept = recode(dept,
                       "ag/biosys eng-a" = "ag/biosys eng",
                       "ag/biosys eng-e" = "ag/biosys eng")) %>%
  mutate(dept = str_remove_all(dept, patterns)) %>%
  
  #--remove punctuation from people's names (e. anderson should be e anderson)
  mutate(first_name = str_remove_all(first_name, "[[:punct:]]"))%>%
  

depts <- cyd_dept %>%
  pull(dept) %>%
  unique() 


######################
# Data cleaning to merge cyd_dept and cyd_salstidy
# PROBLEMS: 
#   1. spelling doens't match between directory and salstidy
#         a. names are truncated in the des moines register data - i.e. they have a max of 20 characters in their single column
#   2. Missing from the directory for no good reason - ex. Lloyd Anderson, should be listed in Animal Science - SEEMS LIKE A LOT OF PEOPLE
####################

problems <- cyd_salstidy %>%
  filter(fiscal_year > 2010) %>%
  select(last_name, first_name, position) %>%
  unique() %>%
  full_join(cyd_dept, by = c("last_name", "first_name")) %>%
  filter(is.na(dept)) %>%
  arrange(last_name)

fixme_last <- cyd_salstidy %>%
  filter(fiscal_year > 2010) %>%
  select(last_name, first_name) %>%
  unique() %>%
  full_join(cyd_dept, by = c("last_name", "first_name")) %>%
  filter(is.na(dept)) %>%
  pull(last_name)

fixme_first <- cyd_salstidy %>%
  filter(fiscal_year > 2010) %>%
  select(last_name, first_name) %>%
  unique() %>%
  full_join(cyd_dept, by = c("last_name", "first_name")) %>%
  filter(is.na(dept)) %>%
  pull(first_name)

# Tackling the spelling errors first...I truncated the directory so that it stops after the same amount of characters as
# the register, but there are still 187 people that aren't merging...are all of them missing from the directory??

# --- messing around w/filtering people -- #

# This person changed her name spelling
cyd_salstidy %>%
  filter(str_detect(last_name, "^garasky")) %>%
  select(fiscal_year, last_name, first_name, other, position)
cyd_dept %>%
  filter(str_detect(last_name, "^garasky")) %>%
  select(fiscal_year, last_name, first_name, dept)

cyd_dept %>%
  filter(last_name == fixme_last[3]) %>%
  mutate(fl = str_sub(first_name, 1, 1)) %>%
  filter(fl == "l") %>%
  arrange(first_name) %>%
  filter(first_name != "laura")

andersons <- cyd_dept %>%
  filter(last_name == "")

# alekel d, a professor, no record of her
cyd_salstidy %>%
  filter(last_name == "alekel") 
cyd_dept %>%
  filter(str_detect(last_name, "^alek")) %>%
  select(fiscal_year, last_name, first_name)

# What the hell is a collab assoc prof? She is probably missing for a good reason then. 
cyd_salstidy %>%
  filter(last_name == "wickrama") %>%
  select(fiscal_year, last_name, first_name, position)
cyd_dept %>%
  filter(last_name == "wickrama") 

# she doesn't exist
cyd_salstidy %>%
  filter(last_name == fixme_last[6],
         first_name == fixme_first[6]) %>%
  select(fiscal_year, last_name, first_name, position)

cyd_dept %>%
  filter(last_name == fixme_last[6]) %>%
  mutate(first_name_letter = str_sub(first_name, 1, 1)) %>%
  filter(first_name_letter == "b") %>%
  arrange(first_name)

# he doesn't exist
cyd_salstidy %>%
  filter(last_name == fixme_last[7],
         first_name == fixme_first[7]) %>%
  select(fiscal_year, last_name, first_name, position)

cyd_dept %>%
  filter(last_name == fixme_last[7])

cyd_dept %>%
  filter(first_name == "gaya")


  
##FUUUUUUUUUUUUUUUUUUUUUUUUUUUUCK
# NOTE: future improvement, if it doesn't appear in this list it should get lumped into another one (ex. ag/biosys eng)


cyd_college <- read_csv("data-raw/_tidy/td_org-dept-key.csv")


#--PROBLEM: we only have 2 names from Cynthia - so adams sarah l and adams sarah k both get grouped with adams sarah.
# how many times is this a problem?
# 66 people have the same name as someone else. I have no idea what to do with them. 
# I guess remove them? Keep them? No idea. For now we keep them. 
# NO!!!!! Let's remove them. 

### solving the duplicate issue ### 

# get duplicates from the department directory
dupes <- 
  cyd_dept %>%
  group_by(fiscal_year, last_name, first_name) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(last_name, first_name, dept) %>%
  ungroup() %>%
  # LE - I added dept here in case we wanted to google them...
  select(last_name, first_name, dept) %>%
  distinct() 

# dupes are then fixed and added back into sals_tidy

good_dupes <- read_csv("data-raw/_raw/duplicate_name_reconciliation.csv")%>%
  # getting rid of ones I couldn't figure out
  filter(!grepl(pattern = "^\\**", reason_to_delete)) %>%
  select(-c(n, reason_to_delete, fiscal_year))

# right now separately joining by middle initial and by position, since they are different

name_pos <- right_join(cyd_salstidy, good_dupes, by = c("last_name", "first_name", "position"), copy = FALSE, keep = FALSE) %>%
  filter(total_salary_paid > 0) %>%
  distinct()

name_mid <- right_join(cyd_salstidy, select(good_dupes, -position), by = c("last_name", "first_name", "other")) %>%
  filter(total_salary_paid > 0) %>%
  filter(!is.na(other)) %>%
  distinct()

all_prof_dupes <- bind_rows(name_pos, name_mid) %>%
  arrange(fiscal_year, last_name, first_name)


# create list of names w/duplicates to filter against
dupes2 <- 
  cyd_dept %>%
  group_by(fiscal_year, last_name, first_name) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(last_name, first_name, dept) %>%
  ungroup() %>%
  #--make one 'name' column
  unite(last_name, first_name, col = "name", sep = "_") %>%
  select(name) %>%
  distinct() %>%
  as_vector()

cyd_salstidy2 <- 
  cyd_salstidy %>%
  unite(last_name, first_name, col = "name", sep = "_") %>%
  filter(!name %in% dupes2) %>%
  separate("name", into = c("last_name", "first_name"), sep = "_")

# --- Merging everything together

cyf_SalsDeptMerge <- function(mydata = cyd_salstidy, mydept = cyd_dept, mycollege = cyd_college) {
  
  
  cyd_saldept <-
    mydata %>%
    left_join(mydept, by = c("fiscal_year", "last_name", "first_name")) %>%
    
    select(-base_salary_year) %>%
    
    #--anonymize names
    unite(last_name, first_name, col = "name", sep = "_") %>%
    #mutate(anon = name %>% map(digest)) %>%
    #unnest() %>%
    
    # add college
    left_join(mycollege, by = "dept") %>%
    select(
      fiscal_year,
      college,
      dept,
      #anon,
      name,
      position,
      gender,
      base_salary,
      total_salary_paid,
      travel_subsistence
    ) %>%
    
    arrange(fiscal_year, college, dept, position, base_salary)
  
  return(cyd_saldept)
  
}

cyd_saldept <- cyf_SalsDeptMerge(mydata = cyd_salstidy2)


cyd_saldept %>%
  filter(name == "salas-fernandez_maria")



# cyf_simpprofs -------------------------------------------------------
# this one will
#-make 4 general categories of professors positions:
#  awarded prof, asst, assoc, and prof

cyd_saldept %>%
  filter(!grepl("chair|adj|affil|emer|vstg|chr|clin|collab|res", position)) %>%
  filter(grepl("prof", position)) %>%
  pull(position) %>% as.factor() %>% summary()

cy_SimpProfs <- function(mydata = cyd_saldept){
  
  # assertthat::assert_that(is.data.frame(data))
  # assertthat::assert_that("position" %in% names(data))
  # 
  # Define simple categories to keep
  
  # NOTE: eliminate department chairs, they are weird
  #       eliminate adjuncts, things with 'adj' or 'affil' or 'emer' or 'vstg', they are paid strangely
  
  awardprof <- c("distg prof", "univ prof", "morrill prof")
  # 
  cyd_salprofs <- 
    cyd_saldept %>%
    filter(!grepl("chair|adj|affil|emer|vstg|chr|clin|collab|res", position)) %>%
    filter(grepl("prof", position)) %>%
    
    mutate(
      prof_simp = ifelse(position %in% awardprof, "awarded prof", position)) 
  
  return(cyd_salprofs)
  
  
}


cyd_salprofs <- cy_SimpProfs() %>%
filter(!is.na(dept)) %>%
mutate(prof_simp = factor(prof_simp, levels = c("asst prof", "assoc prof", "prof", "awarded prof")))


cyd_salprofs %>% write_csv("data-raw/_tidy/cyd_salprofs.csv")


cyd_salprofs %>%
  filter(dept == "agronomy",
         fiscal_year == 2018)


# let's look at this shit -------------------------------------------------

cyd_salprofs %>%
  
  filter(dept == "agronomy") %>%
  filter(prof_simp != "OTHER") %>%
  
  group_by(gender, fiscal_year, prof_simp) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(fiscal_year, nesting(prof_simp, gender),
           fill = list(n = 0)) %>%
  
  ggplot(aes(fiscal_year, n)) + 
  geom_point(aes(color = gender)) + 
  geom_line(aes(color = gender)) + 
  facet_grid(~prof_simp)

# they hired an asst prof in 2018?
cyd_saldept %>%
  filter(dept == "agronomy") %>%
  filter(fiscal_year == 2018) %>%
  filter(gender == "F") %>%
  filter(grepl("prof", position))

# how is sarah jones? the dept lists her as a research associate
cyd_salstidy %>%
  filter(fiscal_year == 2018) %>%
  filter(grepl("prof", position)) %>%
  filter(position == "asst prof") %>%
  filter(base_salary == 70878)


cyd_dept %>%
  filter(last_name == "jones", first_name == "sarah")



# stats -------------------------------------------------------------------


# where can I even take a ratio?


cyd_salsraw %>%
  filter(grepl("JONES SARAH", name))


cyd_salstidy %>%
  filter(grepl("jones", last_name)) %>%
  filter(grepl("sarah", first_name))


cyd_saldept %>%
  group_by(fiscal_year, college, dept, pos_simp, gender) %>%
  summarise(base_mean = mean(base_salary),
            #base_var = var(base_salary)
  ) %>%
  spread(gender, value = base_mean) %>%
  mutate(genderdiv = ifelse(
    is.na(`F`), "noF",
    ifelse(
      is.na(`M`), "noM", "both-present")
  )) %>%
  
  filter(college == "college of agriculture & life sciences") %>%
  filter(pos_simp != "OTHER") %>%
  
  ggplot(aes(pos_simp, dept)) + 
  geom_tile(aes(fill = genderdiv), color = "black") + 
  scale_fill_manual(values = c("both-present" = "gray50",
                               "noF" = "yellow",
                               "noM" = "dodgerblue")) +
  facet_wrap(~college, scales = "free") + 
  theme(axis.text.x = element_text(angle = 45),
  ) + 
  theme_classic()



# look at ratios ----------------------------------------------------------

# by position, separate points for each dept
saldeptprofs %>%
  group_by(fiscal_year, college, dept, pos_simp, gender) %>%
  summarise(base_mean = mean(base_salary),
            n = n(),
            #base_var = var(base_salary)
  ) %>%
  mutate(n = sum(n)) %>%
  spread(gender, value = base_mean) %>%
  mutate(base_rat = M/ `F`,
         base_lrat = log(base_rat)) %>%
  
  ggplot(aes(dept, base_lrat)) + 
  geom_point(aes(size = n, color = pos_simp)) + 
  geom_hline(yintercept = 0) +
  geom_label(x = 50, y = 1.5, label = "Men Make More") +
  geom_label(x = 50, y = -1, label = "Women Make More") +
  
  facet_grid(~pos_simp) + 
  theme_classic() + 
  theme(axis.text.x = element_blank())


# what is going on with the negative values?
# are there professors in the library? or is it pulling 'professionals'?
saldeptprofs %>%
  group_by(fiscal_year, college, dept, pos_simp, gender) %>%
  summarise(base_mean = mean(base_salary),
            n = n(),
            #base_var = var(base_salary)
  ) %>%
  mutate(n = sum(n)) %>%
  spread(gender, value = base_mean) %>%
  mutate(base_rat = M/ `F`,
         base_lrat = log(base_rat)) %>%
  filter(base_lrat < -0.8)



# by position and college

saldeptprofs %>%
  filter(dept != "library") %>%
  group_by(fiscal_year, college, pos_simp, gender) %>%
  summarise(base_mean = mean(base_salary),
            n = n(),
            #base_var = var(base_salary)
  ) %>%
  mutate(n = sum(n)) %>%
  spread(gender, value = base_mean) %>%
  mutate(base_rat = M/ `F`,
         base_lrat = log(base_rat)) %>%
  
  ggplot(aes(college, base_lrat)) + 
  geom_point(aes(size = n)) + 
  geom_hline(yintercept = 0) +
  geom_label(x = 50, y = 1.5, label = "Men Make More") +
  geom_label(x = 50, y = -1.25, label = "Women Make More") +
  
  facet_grid(~pos_simp) + 
  theme_classic() + 
  theme(axis.text.x = element_blank())

# by position only

all_mn <- saldeptprofs %>%
  filter(dept != "library") %>%
  group_by(fiscal_year, pos_simp, gender) %>%
  summarise(base_mean = mean(base_salary),
            n = n(),
            #base_var = var(base_salary)
  ) %>%
  mutate(n = sum(n)) %>%
  spread(gender, value = base_mean) %>%
  mutate(base_rat = M/ `F`,
         base_lrat = log(base_rat))

all_var <- saldeptprofs %>%
  filter(dept != "library") %>%
  group_by(fiscal_year, pos_simp, gender) %>%
  summarise(base_var = var(base_salary)) %>%
  spread(gender, value = base_var) %>%
  rename(varF = `F`,
         varM = M) 

# Need to get covariance so I can get a standard error bar!

#all <- 
all_mn %>%
  left_join(all_var)




ggplot(aes(fiscal_year, base_lrat)) + 
  geom_point(aes(size = n)) + 
  geom_hline(yintercept = 0) +
  geom_label(x = 50, y = 1.5, label = "Men Make More") +
  geom_label(x = 50, y = -1.25, label = "Women Make More") +
  
  facet_grid(~pos_simp) + 
  theme_classic() + 
  theme(axis.text.x = element_blank())
