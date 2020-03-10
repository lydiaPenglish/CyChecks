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

middle_names <- readr::read_delim("affiliation/middle_names.txt",
                                  delim = "\t",
                                col_types = readr::cols(
                                  DEPT1           = readr::col_integer(),
                                  ORG_SHORT_NAME  = readr::col_character(),
                                  DEPT_SHORT_NAME = readr::col_character(),
                                  LAST_NAME       = readr::col_character(),
                                  FIRST_NAME      = readr::col_character(),
                                  MID_NAME        = readr::col_character()
                                )) %>%
  as.data.frame(.) %>%                      # LE - not sure this helps, but it doesn't hurt!
  mutate_if(is.character, ~stringr::str_trim(.))         
                                            # LE - this seemed to be the issue. Got rid of white space


affiliation <- read_affiliation_dir("affiliation/",
                                    pattern = "*csv",
                                    into = c("affiliation",
                                             "year","month","day",
                                             "extension")) %>%
  
  left_join(middle_names, by = c("DEPT1", "ORG_SHORT_NAME", "DEPT_SHORT_NAME",
                                     "LAST_NAME", "FIRST_NAME")) %>%
   
  # dplyr::rename(`NUMERIC CODE` = DEPT1) %>% # To match departments data.frame, except that it doesn't
  
  dplyr::mutate(name = paste(LAST_NAME, FIRST_NAME), # To match salaries data.frame
                name = ifelse(is.na(MID_NAME), name, paste(name, MID_NAME)),     
                year = as.integer(year)) %>% 
  
  select(year, name, DEPT1, ORG_SHORT_NAME, DEPT_SHORT_NAME)

usethis::use_data(affiliation, overwrite = TRUE)
