# Creates the following data/
#    departments
#    department_rename


if (!require(readr))
  stop("Install `readr` package.")

departments <- readr::read_csv("departments/dept_title.csv", 
                               skip = 3, #n_max =261,        # LE - Why are we not reading all the depts?
                               
                               col_types = readr::cols(
                                 `NUMERIC CODE`             = readr::col_integer(),
                                 `ALPHA CODE`               = readr::col_character(),
                                 `DIRECTORY NAME`           = readr::col_character(),
                                 `FULL NAME`                = readr::col_character(),
                                 ADDRESS                    = readr::col_character(),
                                 PHONE                      = readr::col_character(),
                                 `RMM RESOURCE UNIT NUMBER` = readr::col_integer(),
                                 `PARENT DEPT NUMBER`       = readr::col_integer()
                               )) %>%
  # LE I want to rename these columns so they are the same as affiliation.rda
  dplyr::rename(DEPT1 = `NUMERIC CODE`,
                DEPT_SHORT_NAME = `DIRECTORY NAME`)

# There doesn't seem to be anything wrong, but I get the following warning for 
# the EOADV (30145)

# Warning: 1 parsing failure.
# row                col   expected actual                         file
# 261 PARENT DEPT NUMBER an integer        'departments/dept_title.csv'

usethis::use_data(departments, overwrite = TRUE)



################################################################################
# department_rename
department_rename <- readr::read_csv("departments/department_rename.csv",
                                     col_types = readr::cols(
                                       old_name = readr::col_character(),
                                       current_name = readr::col_character()
                                     ))

usethis::use_data(department_rename, overwrite = TRUE)

