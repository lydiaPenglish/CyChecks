########################
# author: Gina
# created: Feb 17 2020
# last modified:
#
# purpose: Calculate % of dept faculty that is m/f
# notes:
############################

library(tidyverse)

# ignore positions

cyd_gendept <- 
  cyd_salprofs %>% 
  group_by(fiscal_year, college, dept, gender) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = gender, 
              values_from = n) %>%
  mutate_if(is.numeric, replace_na, 0) %>% 
  mutate(tot = `F` + M,
         fracF = `F`/tot)

use_data(cyd_gendept, overwrite = T)

