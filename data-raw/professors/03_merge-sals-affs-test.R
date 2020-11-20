# Merge salaries and affiliations based on a key constructed from the 
# merge and munge

library(dplyr) # for %>%
library(readr)


# Load the data ------------------------------------------------------------
#--try with just 2019 first

affs <- 
  read_csv("data-raw/professors/munged-affs.csv") %>% 
  rename(aff_last_name = last_name,
         aff_first_name = first_name,
         aff_mid_name = mid_name,
         aff_lfm20 = name_lfm20) %>% 
  filter(year == 2019)

sals <- 
  read_csv("data-raw/professors/sals-profs.csv") %>% 
  rename(sal_lfm20 = name_lfm20) %>% 
  filter(year == 2019) 

#--merge only 2 depts to start
# big_depts <- 
#   readr::read_csv("data-raw/departments/department_numbers.csv") %>%
#   filter(TOTAL > 39) %>% 
#   janitor::clean_names() %>% 
#   mutate_if(is.character, stringr::str_to_lower) %>% 
#   pull(dept_short_name)


# join big depts ----------------------------------------------------------

# PROBLEMS:
# the affiliation data came with last, first, middle names separated
# salary data the names are listed as one column. Separation by space may not be accurate
# Pasting tog the lfm name from affiliation might not exactly match what is in salary data
# there might be a liang wang in eeob and electrical eng. In the salary data, they will not have unique ids. 
# affs doesn't include their position, only their dept. So it will include non-profs. 
# theoretically sals data only includes profs 

j19 <- 
  left_join(sals, affs, by = c("sal_lfm20" = "aff_lfm20", "year" = "year")) %>% 
  arrange(sal_lfm20) 
         
# munging -----------------------------------------------------------------
j19_simp <- 
  j19 %>% 
  select(year, org_short_name, dept_short_name, sal_lfm20, 
         aff_last_name, aff_first_name, aff_mid_name, gender, title, total_salary_paid) 


#--first, people with sals but no affil. Fixed!
j19_simpnas <- 
  j19_simp %>% 
  filter(is.na(dept_short_name))

#--duplicates. Fixed!
j19_simp %>% 
  group_by(year, sal_lfm20) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

j19




# make title_simp ----------------------------------------------------------------

clean_punc <- function(x) {
  stringr::str_replace_all(x, "[^[:alnum:]]", " ") %>% 
    stringr::str_trim(.) %>% 
    stringr::str_squish(.)
}


p1 <- 
  j19 %>%  
  mutate(dept_chair = ifelse(grepl("chair|chr", title), "Y", "N"),
         title_simp = case_when(
           grepl("distg|univ|morrill", title) ~ "awarded prof",
           TRUE ~ title),
         title_simp = stringr::str_replace_all(title_simp, "chair", ""),
         title_simp = clean_punc(title_simp))


# clean cols ----------------------------------------------------------------

p2 <- 
  p1 %>% 
  rename(
    name = sal_lfm20,
    college = org_short_name,
    dept = dept_short_name
  ) %>% 
  select(year, college, dept, gender, name, 
         title, title_simp, dept_chair,
         base_salary, travel_subsistence, total_salary_paid)
  



# look at issues ----------------------------------------------------------

# 9 people make less than $50,000. 
# linda allen's total salary is double her base, something weird there
# diane birt is a dist. prof, money is prob  weird
# nancy grudens, her colleagues make double, maybe half her salary comes from somewhere else?
# ronald leonard is a professor of practice
# alan murdoch is old but is an assistant prof. all his collegues make more
#  I think these are all not acurate reflections of a professor salary
p2 %>% 
  filter(base_salary < 50000, base_salary != 0) %>% 
  select(dept, gender, name, title, base_salary)

p3 <- p2 %>% 
  mutate(base50 = ifelse(base_salary > 50000, "Y", "N")) 



# write it ----------------------------------------------------------------
professors <- p3
  
usethis::use_data(professors, overwrite = TRUE)
