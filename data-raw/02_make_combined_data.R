########################
# author: Lydia 
#
# purpose: Combined raw salary data with ISU directory data
# 
# inputs: 
# outputs: Two datasets
#         1. cyd_saldept  - only professors
#         2. cyd_salprofs - only professors, but even more simplified names
#
# notes: 
#   - (LE) I got rid of the columns `base_salary_date` and `base_salary_year` since `base_salary_year`
#     was identical to `fiscal_year`. 

library(tidyverse)
# loading organization key 
cyd_college <- read_csv("data-raw/_tidy/td_org-dept-key.csv")

# i.   ---- Clean up raw salary data ----

cyd_salsraw <- read_csv("data-raw/_tidy/td_rawsals.csv")

cyd_salstidy <- cyd_salsraw %>% 
  
  # -- get rid of unwanted columns 
  dplyr::select(fiscal_year, name, gender, position, total_salary_paid, 
                travel_subsistence, base_salary) %>%
  
  # -- eliminate things that are per hour and people who don't have a position or a gender 
  dplyr::filter(!grepl("HR", base_salary),
         !grepl("\\*", position),
         !grepl("\\*", gender)) %>%
  
  # -- update columns
  dplyr::mutate(
    base_salary = as.numeric(gsub(",", "", base_salary)),            # note: warning about NAs introduced by coercion, JBN: and?
    position    = stringr::str_trim(position, side = "right"),       # get rid of any white space in character cols
    name        = stringr::str_trim(name, side = "right"),           # 
    name        = tolower(name),                                     # make name/position lower case
    position    = tolower(position),                                 #
    name        = gsub(",", "", name)) %>%                           # getting rid of commas
  
  # -- only keep professors (!!! this can be commented out if anyone wants to make a larger dataset...)
  dplyr::filter(grepl("prof", position)) %>%
  
  # -- Keep name column, but also expand into first, last, and middle     # note: warning about some columns with 4 things (usually ppl with Jr at end of name)
  tidyr::separate(name, into = c("last_name", "first_name", "other"), sep = " ", remove = FALSE) %>% 
  
  #--remove punctuation from the first name 
  dplyr::mutate(first_name = str_remove_all(first_name, "[[:punct:]]"))

# ii.  ---- Clean up directory data from Cindy ----

# -- Endings to remove from departments that span two different colleges (we want to combine these into one department!)
# JBN: do we want to combine them? 
patterns <- c("-agls|-las|-hsci|-a|-e")

cyd_dept <- 
  readr::read_csv("data-raw/_tidy/td_empl-dept-key.csv") %>%
  
  # recoding redundant departments
  dplyr::mutate(dept = str_remove_all(dept, patterns)) %>%
  
  # remove punctuation from people's names (e. anderson should be e anderson)
  dplyr::mutate(first_name = str_remove_all(first_name, "[[:punct:]]"))

# iii. ---- Addressing problems pre merge: duplicate names ----
#     problem: In the directory data there is no middle name initial so `adams sarah l` and `adams sarah k` 
#     both get grouped with `adams sarah`. 

# first - get duplicates from the department directory
dupes <- 
  cyd_dept %>%
  dplyr::group_by(fiscal_year, last_name, first_name) %>%
  dplyr::mutate(n = n()) %>%
  dplyr::filter(n > 1) %>%
  dplyr::arrange(last_name, first_name, dept) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() 
# write.csv(dupes, "data-raw/_raw/duplicate_name_reconciliation.csv")

# second - reconcile duplicate names to the best of our ability. This was done outside of R (sorry...). 
#          The process of how duplicates were reconciled can be found in "~/data-raw/_raw/duplicate_reconciliation_process.txt"

good_dupes <- readr::read_csv("data-raw/_raw/duplicate_name_reconciliation.csv")%>%
  # getting rid of names that couldn't be reconciled
  dplyr::filter(!grepl(pattern = "^\\**", reason_to_delete)) %>%
  dplyr::select(-c(n, reason_to_delete, fiscal_year))

# -- Given that the dupes were reconciled by either middle initial or position we need to join them in two different ways
# ppl we joined by posiiton
name_pos <- dplyr::right_join(cyd_salstidy, 
                              good_dupes, 
                              by = c("last_name", "first_name", "position"),
                              copy = FALSE, keep = FALSE) %>%
  dplyr::filter(total_salary_paid > 0) %>%
  dplyr::distinct()

# ppl we joined by middle name
name_mid <- dplyr::right_join(cyd_salstidy, 
                       select(good_dupes, -position), 
                       by = c("last_name", "first_name", "other")) %>%
  dplyr::filter(total_salary_paid > 0) %>%
  dplyr::filter(!is.na(other)) %>%
  dplyr::distinct()

# Combining the two types of joins - now we have a dataframe that can be added to our merge by binding rows
all_prof_dupes <- dplyr::bind_rows(name_pos, name_mid) %>%
  dplyr::arrange(fiscal_year, last_name, first_name) %>%
  dplyr::select(-c(other.x, other.y))

dupe_names <- dupes %>%
  tidyr::unite(last_name, first_name, col = "short_name", sep = " ") %>%
  dplyr::select(short_name) %>%
  dplyr::distinct() %>%
  as_vector()

# iv.  ---- Merging datasets ----
#      Before merging we need to add back in the reconciled duplicates 

# dataset that deleted duplicate names and then added back in the reconciled ones
cyd_salstidy2 <- 
  cyd_salstidy %>%
  tidyr::unite(last_name, first_name, col = "short_name", sep = " ", remove = FALSE) %>%
  # get rid of all duplicate names...we will add back in the good ones
  dplyr::filter(!short_name %in% dupe_names) %>%
  dplyr::select(-short_name) 

# merging with directory data, then adding in duplicates
cyd_saldept_x <-
  cyd_salstidy2 %>%
  dplyr::left_join(cyd_dept, by = c("fiscal_year", "last_name", "first_name")) %>%
  # adding in duplicates who have been reconciled 
  dplyr::bind_rows(., all_prof_dupes) %>%
  # add college
  dplyr::left_join(cyd_college, by = "dept") %>%

  # fill in department info for years before/after we had directory info (i.e before 2011 and after 2018)
  dplyr::group_by(name) %>%
  tidyr::fill(dept, college, .direction = "downup") %>%             # this is obviously assuming people haven't changed departments...
  dplyr::ungroup()

# v.   ---- Addressing problems post merge: people listed in center, people missing from the directory ----

# -- 7 people who are professors in a center, updating their department info 
#    List of ppl can be found at `~/data-raw/_raw/ctr-faculty-reconciled.csv`

cyd_saldept_x <- cyd_saldept_x %>%
  # Ugh sorry did this manually because there were only 7 instances...
  dplyr::mutate(
    dept = if_else(name == "brown_robert_c", "mechanical eng", dept),
    dept = if_else(name == "dalal_vikram_l" | name == "tuttle_gary_l", "elec eng/cp eng", dept),
    dept = if_else(name == "goggi_alcira_s", "agronomy", dept),
    dept = if_else(name == "johnson_lawrence_a", "food sci/hn", dept),
    dept = if_else(name == "niederhauser_dale_s", "curr/instr", dept),
    dept = if_else(name == "thompson_elizabeth_a", "school of ed", dept)) %>%
  # recoding anthro old dept to anthropology, confused as to the difference
  # curr/inst becomes school of ed
  dplyr::mutate(dept = dplyr::recode(dept, 
                                     "anthro old"      = "anthropology",
                                     "curr/instr"      = "school of ed",
                                     "ed ldshp pol st" = "school of ed"))

# TO DO: are there other deparments that have changed names? Are there other departments that are missing from the database?

# -- People who still don't have a department listed (i.e. PROFS in salary database, but not in directory) 

no_dept <- cyd_saldept_x %>%
  dplyr::filter(is.na(dept), fiscal_year > 2010) %>%                  # while we've imputed departments, anyone who has left before 2010 will not have a dept listed      
  dplyr::select(last_name, first_name, other, name, position) %>%
  unique() %>%
  dplyr::arrange(last_name, first_name)

# TO DO: how to deal with these... it's a lot. 

# -- Addressing people who have two different departments listed throughout their tenure. Ok if they change departments, 
#    but looking for departments that change names (eg. curr/inst becomes school of ed, as seen above)

two_depts <- cyd_saldept_x %>%
  distinct(last_name, first_name, other, dept) %>%
  group_by(last_name, first_name, other) %>%
  mutate(numDept = n()) %>%
  ungroup() %>%
  filter(numDept > 1) %>%
  arrange(last_name, first_name, dept)
# ^^ seems like art/design becomes a couple of different things....not sure how to fix this. Going to leave the rest for now...

# vi.  ---- Anonymizing people ----

# -- function to anonymize
cy_Anonymize <- function(df, col_to_anon = "name", algo = "crc32"){
  
  assertthat::not_empty(df)
  assertthat::see_if(col_to_anon %in% names(df), msg = "The selected column isn't in the dataframe")
  assertthat::assert_that(is.data.frame(df),
                          is.character(algo),
                          nrow(df) > 0)
  
  to_anon <- dplyr::select(df, col_to_anon)
  
  ids <- unname(apply(to_anon, 1, digest::digest, algo = algo))
  
  df2 <- df %>%
    dplyr::mutate(id = ids)
  
  return(df2)
}

cyd_saldept <- cy_Anonymize(cyd_saldept_x) %>%
  # taking all identifying names
  select(-c(name, last_name, first_name, other))

#looks like it worked and made one for everyone - this should go into the tests eventually...
unique(cyd_saldept_x$name) %>% length(.)
unique(cyd_saldept$id) %>% length(.)

# vii  ---- Making cy_simpprof, simplified professor titles -----

awardprof <- c("distg prof", "univ prof", "morrill prof")

cyd_salprofs <- 
  
  cyd_saldept %>%
  
  # eliminate department chairs, they are weird
  # eliminate adjuncts, things with 'adj' or 'affil' or 'emer' or 'vstg', they are paid strangely
  
  filter(!grepl("chair|adj|affil|emer|vstg|chr|clin|collab|res", position)) %>%
  filter(grepl("prof", position)) %>%
  
  mutate(prof_simp = ifelse(position %in% awardprof, "awarded prof", position)) %>% 
  filter(!is.na(dept))

cyd_salprofs %>% write_csv("data-raw/_tidy/cyd_salprofs.csv")

# viii.---- Save datasets as .rdas ----
usethis::use_data(cyd_saldept, overwrite = TRUE)
usethis::use_data(cyd_salprofs, overwrite = TRUE)
