library("dplyr") # for %>%

read_affiliation_csv = function(f, into) {
  readr::read_csv(f,
                  col_types = readr::cols(
                    DEPT1           = readr::col_integer(),
                    ORG_SHORT_NAME  = readr::col_character(),
                    DEPT_SHORT_NAME = readr::col_character(),
                    LAST_NAME       = readr::col_character(),
                    FIRST_NAME      = readr::col_character()
                  )) %>%
    dplyr::mutate(file=f) %>%
    tidyr::separate(file, into) 
}

read_affiliation_dir = function(path, pattern, into) {
  files = list.files(path = path,
                     pattern = pattern,
                     recursive = TRUE,
                     full.names = TRUE)
  plyr::ldply(files, read_affiliation_csv, into = into)
}



##############################################################################

middle_names <- readr::read_delim("data-raw/affiliation/middle_names.txt",
                                  delim = "\t",
                                col_types = readr::cols(
                                  DEPT1           = readr::col_integer(),
                                  ORG_SHORT_NAME  = readr::col_character(),
                                  DEPT_SHORT_NAME = readr::col_character(),
                                  LAST_NAME       = readr::col_character(),
                                  FIRST_NAME      = readr::col_character(),
                                  MID_NAME        = readr::col_character()
                                )) %>%
  janitor::clean_names() %>% #--I hate all uppercase
  tibble::as_tibble(.) %>%  
  dplyr::mutate_if(is.character, ~stringr::str_trim(.)) %>% 
  dplyr::mutate_if(is.character, stringr::str_to_lower)
                                            
middle_names



# read in every year's affiliations ---------------------------------------

aff_raw <- read_affiliation_dir("data-raw/affiliation/",
                                    pattern = "*csv",
                                    into = c("x1", "x2", "affiliation",
                                             "year","month","day",
                                             "extension")) %>% 
  select(DEPT1, ORG_SHORT_NAME, DEPT_SHORT_NAME, LAST_NAME, FIRST_NAME, year) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate_if(is.character, stringr::str_to_lower) %>% 
  dplyr::mutate_if(is.character, ~stringr::str_trim(.)) %>% 
  tibble::as_tibble()


# merge in middle names ---------------------------------------------------

affiliation <- 
  aff_raw %>% 
  left_join(middle_names, by = c("dept1", 
                                 "org_short_name",
                                 "dept_short_name",
                                 "last_name",
                                 "first_name")) %>% 
  select(year, dept1, org_short_name, dept_short_name, 
         first_name, mid_name, last_name)



usethis::use_data(affiliation, overwrite = TRUE)
