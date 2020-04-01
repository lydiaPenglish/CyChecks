# Creates these data/
#   salaries

# See salaries/salaries.R if you want to update the csv

library("dplyr") # for %>%, should it be mag

salaries <- readr::read_csv("salaries/salaries.csv",
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
                            )) %>%
  
  # Deal with hourly pay and non-numeric characters in base_salary
  dplyr::mutate(
    pay_period = ifelse(is.na(base_salary), NA, "year"),
    pay_period = ifelse(grepl("HR", base_salary), "hour", pay_period),
    
    base_salary = gsub("/HR", "", base_salary),
    base_salary = gsub(" HR", "", base_salary),
    base_salary = gsub(",","", base_salary),    # deal with commas, e.g. 99,999.99
    base_salary = gsub("-","", base_salary),    # deal with dashes, e.g. -0-
    base_salary = as.numeric(base_salary),
    
    name = gsub(",", "", name), # names in 2017 and later have commas between last name and first name
    name = stringr::str_remove_all(name, "JR")) %>% # got rid of "JR" because it's not in affiliation
  
  dplyr::rename(
    year = fiscal_year,
    title = position) %>%
  
  dplyr::select(-department) # department is "Iowa State University" for everyone

usethis::use_data(salaries, overwrite = TRUE)
