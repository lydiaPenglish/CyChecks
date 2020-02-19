read_affiliation_csv = function(f, into) {
  readr::read_csv(f,
                  col_types = readr::cols(
                    DEPT1           = col_character(),
                    ORG_SHORT_NAME  = col_character(),
                    DEPT_SHORT_NAME = col_character(),
                    LAST_NAME       = col_character(),
                    FIRST_NAME      = col_character()
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

affiliation <- read_affiliation_dir("affiliation/",
                                    pattern = "*csv",
                                    into = c("affiliation",
                                             "year","month","day",
                                             "extension")) %>%
  
  dplyr::rename(`NUMERIC CODE` = DEPT1) %>% # To match departments data.frame
  
  dplyr::mutate(name           = paste(LAST_NAME, FIRST_NAME), # To match salaries data.frame
                year           = as.integer(year),
                `NUMERIC CODE` = as.integer(`NUMERIC CODE`)) %>% 
  
  select(year, `NUMERIC CODE`, name)

usethis::use_data(affiliation, overwrite = TRUE)
