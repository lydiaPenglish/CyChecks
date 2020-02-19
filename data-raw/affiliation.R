affiliation <- readr::read_csv("affiliation/affiliation.csv",
                               col_types = readr::cols(
                                 ORG_SHORT_NAME  = col_character(),
                                 DRCTY_DEPT_NAME = col_character(),
                                 NAME            = col_character()
                               )) %>%
  
  rename(`DIRECTORY NAME` = DRCTY_DEPT_NAME,  # To match departments data.frame
         name             = NAME)             # To match salaries data.frame

