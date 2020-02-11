########################
# purpose: Separate script to play around and run stats 
# 
# inputs: .rda datasets created in "02_make_combined_data.R"
#
# outputs: 
#
# notes: 
#


library(tidyverse)
library(CyChecks2)
library(lme4)
library(lmerTest)
data("cyd_salprofs")
data("cyd_saldept")

# i. ---- setting up some dataframes ----

# Getting rid of zero base salary ppl 
non_zero_sals <- cyd_salprofs %>% 
  mutate(college = replace_na(college, "college of combos")) %>%          # added this so we can include soc, eeob, etc. 
  filter(grepl("college", college)) %>% 
  # taking the log of base_salary
  mutate(lsal = log(base_salary)) %>% 
  filter(base_salary > 0) %>% 
  arrange(base_salary)

# Figuring out which departments have at least one male and female 
good_depts_profs <- 
  non_zero_sals %>% 
  # make it so each person only counts once per position
  group_by(college, dept, prof_simp, gender, id) %>% 
  summarise(base_salary = mean(base_salary)) %>% 
  # keep only depts w/m and f in both positions AT SOME POINT in the dataset
  group_by(college, dept, prof_simp, gender) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  # if NA get rid of that line
  filter(!is.na(`F`)) %>% 
  filter(!is.na(M)) %>% 
  select(college, dept, prof_simp)

# data from one year
ex_19 <- 
  good_depts_profs %>% 
  left_join(non_zero_sals) %>% 
  filter(fiscal_year == 2019)

# data from all years
ex_all <- 
  good_depts_profs %>% 
  left_join(non_zero_sals)

# ii. ---- trying out some models ----

# random slope 
m1a <- lmerTest::lmer(lsal ~ prof_simp * gender + (1 + gender|dept), data = ex_all, REML = F)
summary(m1a) # says difference in gender only for PROF positions

# Trying to add name or fiscal year - does not converge
m1e <- lmerTest::lmer(lsal ~ prof_simp * gender + (1 + gender|dept) + (1|id), data = ex_all, REML = F)

# iii ---- ratio of salaries, new model ----

# 2019 only
lrat_19 <- non_zero_sals %>% 
  group_by(fiscal_year, college, dept, prof_simp, gender) %>% 
  summarise(base_salary = mean(base_salary)) %>% 
  spread(gender, base_salary) %>% 
  mutate(rat = M/`F`,
         lrat = log(rat)) %>%
  filter(fiscal_year == 2019)

# all years
lrat_all <- non_zero_sals %>% 
  group_by(fiscal_year, college, dept, prof_simp, gender) %>% 
  summarise(base_salary = mean(base_salary)) %>% 
  spread(gender, base_salary) %>% 
  mutate(rat = M/`F`,
         lrat = log(rat))

# model 
m2a <- lmerTest::lmer(lrat ~ 1 + prof_simp + (1 | dept), data = lrat_all)
summary(m2a)
exp(fixef(m2a)[4])

# which depts have biggest effects on lrat? 
as_tibble(ranef(m2a)) %>% 
  ggplot(aes(grp, condval)) +
  geom_col() + 
  coord_flip()
fixef(m2a)

