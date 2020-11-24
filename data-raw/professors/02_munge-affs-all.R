# Merge salaries and affiliations based on a key constructed from the 
# last_name first_name middle_initial; use fuzzy join to account for missing middle initials

library(dplyr) # for %>%
library(readr)


clean_punc <- function(x) {
  stringr::str_replace_all(x, "\\'", "") %>% 
    stringr::str_replace_all("\\.", " ") %>% 
    stringr::str_replace_all("[^[:alnum:]]", " ") %>% 
    stringr::str_trim(.) %>% 
    stringr::str_squish(.)
}


test <- "j.  -gordon "
clean_punc(test)


# Load the data ------------------------------------------------------------

data("affiliation")

# keep only colelges, simplify depts----------------------------------------

patterns <- c("-agls|-las|-hsci-a|-e")

aff0 <-
  affiliation %>%
  mutate(
    dept_short_name = stringr::str_remove_all(dept_short_name, patterns)
    ) %>% 
  filter(grepl("college", org_short_name))


# 1-3 clean up names ------------------------------------

aff1 <-
  aff0 %>%
  dplyr::mutate(year = as.integer(year)) %>%
  dplyr::mutate(
    last_name = clean_punc(last_name),
    mid_name = clean_punc(mid_name),
    first_name = clean_punc(first_name)) 

#--fix middle initials (make aff match sals) when I know they are problems
aff2 <- 
  aff1 %>% 
  mutate(
    first_name = case_when(
      grepl("sternberg", last_name) ~ "henrik",
      grepl("dalessandro", last_name) ~ "domenic",
      TRUE ~ first_name),
    mid_name = case_when(
      grepl("arritt", last_name) & grepl("raymond", first_name) ~ "w",
      grepl("booth", last_name) & grepl("larry", first_name) ~ "c",
      grepl("cerfogli", last_name) & grepl("jennifer", first_name) ~ "ann",
      grepl("constant", last_name) & grepl("kristen", first_name) ~ "p",
      grepl("crum", last_name) & grepl("michael", first_name) ~ "robert",
      grepl("ellingson", last_name) & grepl("laura", first_name) ~ "d",
      grepl("jeong", last_name) & grepl("hyungseok", first_name) ~ "davi",
      grepl("lewis", last_name) & grepl("calvin", first_name) ~ "f",
      grepl("oconnor", last_name) & grepl("annette", first_name) ~ "m",
      grepl("oneal", last_name) & grepl("matthew", first_name) ~ "ellio",
      grepl("pineyro", last_name) & grepl("pablo", first_name) ~ "enriqu",
      grepl("quam", last_name) & grepl("andrea", first_name) ~ "l",
      grepl("sternberg", last_name) ~ "ste",
      grepl("stevens", last_name) & grepl("julie", first_name) ~ "l",
      grepl("stevenson", last_name) & grepl("gregory", first_name) ~ "w",
      grepl("taylor", last_name) & grepl("gary", first_name) ~ "d",
      grepl("anderson", last_name) & grepl("e", first_name) 
      & dept1 == 4140 ~ "walter",
      grepl("windus", last_name) & grepl("theresa", first_name) ~ "l",
      TRUE ~ mid_name)
  ) 

# create what might look like the salary names
aff3 <- 
  aff2 %>% 
  dplyr::mutate(
    name_lf = paste(last_name, first_name, sep = " "),
    name_lfm = ifelse(is.na(mid_name), name_lf, paste(name_lf, mid_name, sep = " ")),
    name_lfm = clean_punc(name_lfm),
    name_lfm20 = stringr::str_sub(name_lfm, start = 1, end = 20),
    ) %>% 
  select(-name_lf, -name_lfm)

aff3 %>% 
  filter(grepl("zhang hongwei", name_lfm20))

# 4-X fix people in 2019 ------------------------------------------------------

# some people have affs 2012-2018 but not 2019. 
# aff_na19 <- read_csv("data-raw/professors/aff19-nas.csv") %>% 
#   pull(sal_lfm20)
# 
# #--these people are nas in 2019, but exist in 2018
# aff_here18 <- 
#   aff4 %>% 
#   filter(year == 2018) %>%
#   filter(name_lfm20 %in% aff_na19) %>% 
#   pull(name_lfm20)
# 
# aff_here19 <- 
#   aff4 %>% 
#   filter(year == 2019) %>%
#   filter(name_lfm20 %in% aff_na19) %>% 
#   pull(name_lfm20)

#--check, but these could be copy-pasted from 2018?
#--yes. 11/15
#--write it so if the nas change, I still have this happening for hard coded peeps
#copythese <- setdiff(aff_here18, aff_here19)
#tibble(here18 = copythese) %>% write_csv("data-raw/professors/aff19-nas-here18.csv")
# aff4 %>% 
#   filter(year == 2018|2019) %>%
#   filter(name_lfm20 %in% copythese[16]) 

copythese <- read_csv("data-raw/professors/aff19-nas-here18.csv") %>% pull(here18)

aff_replace19 <- 
  aff3 %>% 
  filter(year == 2018) %>% 
  filter(name_lfm20 %in% copythese) %>% 
  mutate(year = 2019)

#--laura has aff data 2012-2015, but salary data from 2012-2019.
#--kristen has aff data up to 2017
aff_smar <- 
  aff3 %>% 
  filter(year == 2015) %>% 
  filter(grepl("smarandescu laura", name_lfm20)) %>%
  select(-year) %>% 
  tidyr::crossing(year = c(2016, 2017, 2018, 2019))

aff_constant <- 
  aff3 %>% 
  filter(year == 2015) %>% 
  filter(grepl("constant kristen p", name_lfm20)) %>%
  select(-year) %>% 
  tidyr::crossing(year = c(2018, 2019))

aff_crum <- 
  aff3 %>% 
  filter(year == 2014) %>% 
  filter(grepl("crum michael robert", name_lfm20)) %>%
  select(-year) %>% 
  tidyr::crossing(year = c(2015, 2016, 2017, 2018, 2019))


aff4 <- 
  aff3 %>% 
  bind_rows(aff_replace19) %>% 
  bind_rows(aff_smar) %>% 
  bind_rows(aff_constant) %>% 
  bind_rows(aff_crum)


#--fix other things?
# aff_na19 <- read_csv("data-raw/professors/aff19-nas.csv") %>%
#   pull(sal_lfm20)

# 
# setdiff(aff_na19, copythese)
# qs <- tibble(qs = setdiff(aff_na19, copythese)) %>% 
#   tidyr::separate(qs, into = c("last", "first", "mid"))
# qs
# 
data("affiliation")
affiliation %>%
  filter(grepl("bender", last_name)) %>%
  filter(grepl("holly", first_name))
# 
#sals <- read_csv("data-raw/professors/sals-profs.csv")

# sals %>%
#   filter(grepl("oneal", name_lfm20)) %>% 
#   select(year, name_lfm20)

#--arcand
#--not sure why strenberg isn't working

aff5 <- aff4


# 6 center reassignments ------------------------------------------------

#--only one prof in cds, google says this guy is in psychology
professors %>% 
  filter(grepl("lbrl art/sc cds", dept)) %>% 
  select(year, college, dept, gender, name, title, dept_chair, base_salary)


aff6  <- 
  aff5 %>% 
  mutate(
    dept_short_name = case_when(
      grepl("goggi alcira susana", name_lfm20) ~ "agronomy",
      grepl("kirschenmann frederi", name_lfm20) ~ "phil/relig st",
      grepl("johnson lawrence", name_lfm20) ~ "food sc/hn",
      grepl("niederhauser dale s", name_lfm20) ~ "school of ed",
      grepl("thompson elizabeth a", name_lfm20) ~ "school of ed",
      grepl("kedrowski karen m", name_lfm20) ~ "political sc",
      grepl("venkatagiri horabail", name_lfm20) ~ "psychology",
      TRUE ~ dept_short_name)
    ) %>% 
  mutate(
    org_short_name = case_when(
    grepl("kirschenmann frederi", name_lfm20) ~ "college of liberal arts & sciences",
    grepl("johnson lawrence", name_lfm20) ~ "college of human sciences",
    TRUE ~ org_short_name)
  ) 


  
# 7 manual clean up ---------------------------------------------------------
# elim david peterson in DEPT1 12170, prob a typo as it has no dept assigned
# li wang in eeob is a post-doc (why included in affs ?!)
# qian wang in accounting is prof, others are post-docs
# zhang hongwei is elec eng, agronomy one is post-doc
# Dept 7090 existed 2013-2015 and has no DEPT_SHORT_NAME
# The people associated w/7090 all transitioned to 7040 in 2016, which is ART/VISUAL CULT
# This assigns the name from 7040 to 7090
# we know prashant jha is agronomy, was hired 2019 but has no affil

aff7 <-
  aff6 %>% 
  mutate(dept_short_name = ifelse(dept1 == 7090, 
                                  "art/visual cult",
                                  dept_short_name),
         org_short_name = ifelse(dept1 == 7090, 
                                 "college of design",
                                 org_short_name)) %>% 
  filter(!(stringr::str_detect("peterson david", name_lfm20) & 
             dept1 == 12170)) %>% 
  filter(!(last_name == "johnson" & 
             first_name == "duane" & 
             org_short_name == "college of business")) %>% 
  filter(!(last_name == "wang" & 
             first_name == "li" & 
             dept_short_name == "eeob")) %>% 
  filter(!(last_name == "wang" & 
             first_name == "qian" & 
             dept_short_name == "physics/astron")) %>% 
  filter(!(last_name == "wang" & 
             first_name == "qian" & 
             dept_short_name == "food sc/hn-hsci")) %>% 
  filter(!(last_name == "zhang" & 
             first_name == "hongwei" & 
             dept_short_name == "agronomy")) %>% 
  dplyr::add_row(
    year = 2019, 
    dept1 = 1150,
    org_short_name = "college of agriculture & life sciences",
    dept_short_name = "agronomy",
    last_name = "jha",
    first_name = "prashant",
    mid_name = NA,
    name_lfm20 = paste(last_name, first_name))



# write it ----------------------------------------------------------------

write_csv(aff7, "data-raw/professors/munged-affs.csv")

