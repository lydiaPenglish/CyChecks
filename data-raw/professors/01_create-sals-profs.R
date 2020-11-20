# Keep just profs' salaries, >2011

library(dplyr) # for %>%


# Load the data ------------------------------------------------------------

data("salaries")

# 1. filter salary data to profs only ----------------------------------------

dontcare <- "emer|vstg|res|adj|affil|collab|clin"

# salary data filtered for professors positions only
s1 <- 
  salaries %>%
  filter(pay_period == "year") %>% 
  filter(grepl("prof", title)) %>% 
  filter(!grepl(dontcare, title)) %>% 
  filter(year > 2011) %>%  # only have dir data from 2012-2019
  select(-base_salary_date)


# 2. remove librarians ----------------------------------------------------

#--only identified ones from 2019 so far. can add to list as needed
libs <- read_csv("data-raw/professors/librarians.csv") %>% pull(value)

s2 <- 
  s1 %>% 
  filter(!(name_lfm20 %in% libs))


# 3. manual ---------------------------------------------------------------

#--crecelius must have been hired in 2019, salary is weird
#--same with lerman (both are in marketing)
#--holly bender is an admin thing at celt
#--gary taylor switched to outreach, which is not a college, in 2017
#--elena cotos is listed as being part of the grad college, google says she's english
s3 <- 
  s2 %>% 
  filter(!(grepl("crecelius", name_lfm20) & year == 2019)) %>% 
  filter(!(grepl("lerman", name_lfm20) & year == 2019)) %>% 
  filter(!(grepl("bender holly", name_lfm20))) %>% 
  filter(!(grepl("taylor gary", name_lfm20) & year > 2016)) %>% 
  filter(!(grepl("cotos elena", name_lfm20))) 

# write final -------------------------------------------------------------

sal_profs <- s3
write_csv(sal_profs, "data-raw/professors/sals-profs.csv")


# examine -----------------------------------------------------------------

#--who has a base salary of 0?!
sal_profs %>% 
  filter(base_salary == 0) 
#--a lot of people need to check that out later...
