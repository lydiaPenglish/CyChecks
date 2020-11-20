# Gina -I don't actually get the point of this

# Creates the following data/
#    departments
#    department_rename


if (!require(readr))
  stop("Install `readr` package.")

departments <- readr::read_csv("data-raw/departments/dept_title.csv", 
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


# I propose we copy-paste 7040 and assign it a value of 7090

usethis::use_data(departments, overwrite = TRUE)



################################################################################
# department_rename
department_rename <- readr::read_csv("data-raw/departments/department_rename.csv",
                                     col_types = readr::cols(
                                       old_name = readr::col_character(),
                                       current_name = readr::col_character()
                                     ))

usethis::use_data(department_rename, overwrite = TRUE)


################################################################################
# why are some depts missing
#--John Cunnally seems like a 'dept transition' problem, 7090 in 2013-2015 is blank
# 7050 before that, 7040 afterwards
# 
# Beginning July 1, 2012, the department of Art and Design was divided into four departments – Integrated Studio Arts, Industrial
# Design, Interior Design and Graphic Design. There was not sufficient peer data available to separate these departments so they are
# combined in this report.

departments %>% 
  filter(DEPT1 %in% c(7090, 7040, 7050))

# NOTE: did nothing to this data, assigned 7090 to "ART/VISUAL CULT" in affiliation data         