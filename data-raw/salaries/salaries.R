# Creates these data/
#   salaries



clean_punc <- function(x) {
  stringr::str_replace_all(x, "\\'", "") %>% 
    stringr::str_replace_all("\\.", " ") %>% 
    stringr::str_replace_all("[^[:alnum:]]", " ") %>% 
    stringr::str_trim(.) %>% 
    stringr::str_squish(.)
}



b <- "j, -gordon o'connor. "
clean_punc(b)


# See salaries/salaries.R if you want to update the csv

library("dplyr") # for %>%, should it be mag
library("readr") # for %>%, should it be mag

sals_raw <- readr::read_csv("data-raw/salaries/salaries.csv",
                            col_types = readr::cols(
                              fiscal_year        = readr::col_integer(),
                              department         = readr::col_character(),
                              name               = readr::col_character(),
                              gender             = readr::col_character(),
                              place_of_residence = readr::col_character(),
                              position           = readr::col_character(),
                              base_salary_date   = readr::col_datetime(format = ""),
                              total_salary_paid  = readr::col_double(),
                              travel_subsistence = readr::col_double(),
                              base_salary        = readr::col_character()
                            )) 

# Deal with hourly pay and non-numeric characters in base_salary

sals1 <- 
  sals_raw %>% 
  mutate_if(is.character, stringr::str_to_lower) %>% 
  mutate(
    pay_period = ifelse(is.na(base_salary), NA, "year"),
    pay_period = ifelse(grepl("hr", base_salary), "hour", pay_period),
    base_salary = stringr::str_remove_all(base_salary, "hr"),
    base_salary = parse_number(base_salary))


# clean up names
# names in 2017 and later have commas between last name and first name
#hyphens in salary data but not aff
# got rid of "iii" and jr

sals2 <- 
  sals1 %>% 
  mutate(
    name = clean_punc(name),
    name = stringr::str_remove_all(name, "jr"),
    name = stringr::str_remove_all(name, "iii"),
    name = stringr::str_remove_all(name, "ii"),
    name = clean_punc(name),
    ) 

# rename cols
sals3 <- 
  sals2 %>% 
  dplyr::rename(
    year = fiscal_year,
    name_lfm20 = name, #--note this is limited to 20 characters, not true for aff
    title = position) %>%
  dplyr::select(-department)  # department is "Iowa State University" for everyone


# write it ----------------------------------------------------------------

salaries <- sals3

usethis::use_data(salaries, overwrite = TRUE)

