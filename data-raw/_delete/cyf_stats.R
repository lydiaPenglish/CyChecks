rm(list = ls())

library(tidyverse)
library(lme4)
library(lmerTest)
library(CyChecks2)

# # messing with stats - I've reorganized this to be able to reference easily within presentation # # 

# use updated data
data("cyd_salprofs")
data("cyd_saldept")

# who gets paid 0? 
ex <- cyd_salprofs %>% 
  mutate(college = replace_na(college, "college of combos")) %>% # added this so we can include soc, eeob, etc. 
  filter(grepl("college", college)) %>% 
  mutate(lsal = log(base_salary)) %>% 
  filter(base_salary > 0) %>% 
  arrange(base_salary)

# not every dept has both m and f. get a list of things that have at least 1 of each
goodcmp <- 
  ex %>% 
  # make it so each person only counts once per position
  group_by(college, dept, prof_simp, gender, id) %>% 
  summarise(base_salary = mean(base_salary)) %>% 
  # keep only depts w/m and f in both positions AT SOME POINT in the dataset
  group_by(college, dept, prof_simp, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, value = n) %>% 
  # if NA get rid of that line
  filter(!is.na(`F`)) %>% 
  filter(!is.na(M)) %>% 
  select(college, dept, prof_simp)

ex_18 <- 
  goodcmp %>% 
  left_join(ex) %>% 
  filter(fiscal_year == 2018)

ex_all <- # including ALL years
  goodcmp %>% 
  left_join(ex)

# models

# This works
m1a <- lmerTest::lmer(lsal ~ prof_simp + gender + (1 + gender|dept), data = ex_all, REML = F)

# Trying to add name or fiscal year
m1e <- lmerTest::lmer(lsal ~ prof_simp * gender + (1 + gender|dept) + (1|id), data = ex_all, REML = F)

# This doesn't - why...? Fails to converge
m1b <- lmerTest::lmer(lsal ~ prof_simp + gender + (1 + gender|dept), data = ex_all, REML = F) 

# Without position
m1c <-lmerTest::lmer(lsal ~ gender + (1 + gender|dept), data = ex_all, REML = F)

# without random slope
m1d <- lmerTest::lmer(lsal ~ prof_simp * gender + (1|dept), data = ex_all, REML = F)

anova(m1d, m1a) # idk this suggests that we shouldn't do random slope

# male professors make more than females, everyone else is equal
exp(fixef(m1d)[8])

# ratio of salaries -------------------------------------------------------
# what if I take the ratio of M and F salaries

# 2018 only
lrat_18 <- ex %>% 
  group_by(fiscal_year, college, dept, prof_simp, gender) %>% 
  summarise(base_salary = mean(base_salary)) %>% 
  spread(gender, base_salary) %>% 
  mutate(rat = M/`F`,
         lrat = log(rat)) %>%
  filter(fiscal_year == 2018)

# all years
lrat_all <- ex %>% 
  group_by(fiscal_year, college, dept, prof_simp, gender) %>% 
  summarise(base_salary = mean(base_salary)) %>% 
  spread(gender, base_salary) %>% 
  mutate(rat = M/`F`,
         lrat = log(rat))

# models
m2a <- lmerTest::lmer(lrat ~ 1 + prof_simp + (1 | dept), data = lrat_all)
summary(m2a)
exp(fixef(m2a)[4])

# which depts have biggest effects on lrat? Show this...??!
as_tibble(ranef(m2a)) %>% 
  ggplot(aes(grp, condval)) +
  geom_col() + 
  coord_flip()
fixef(m2a)

as_tibble(ranef(m2a)) %>%
  mutate(avg_c = condval + )

ex_all %>% 
  filter(dept %in% c("agronomy", "statistics", "ag/biosys eng", "mechanical eng")) %>% 
  ggplot(aes(gender, base_salary)) + 
  geom_point(aes(color = prof_simp), alpha = 0.2) +
  facet_wrap(~dept)

lrat_all %>%
  filter(dept == "agronomy" | dept == "eeob" | dept == "ag education/st" | dept == "finance") %>%
  filter(!(is.na(lrat))) %>%
  ggplot(aes(prof_simp, lrat))+
  geom_boxplot(aes(group = prof_simp))+
  facet_wrap(~dept)+
  geom_hline(yintercept = 0, lty = 2)+
  theme_bw()




## Previous work....needs to be organized at some point...probably 

badgirl <- 
  ex %>% 
  # get rid of centers
  filter(!grepl('ctr', dept)) %>%
  # just look at 2018
  filter(fiscal_year == 2018) %>% 
  # make it so each person only counts once per position
  group_by(college, dept, prof_simp, gender, id) %>% 
  summarise(base_salary = mean(base_salary)) %>% 
  # keep only depts w/m and f in both positions AT SOME POINT in the dataset
  group_by(college, dept, prof_simp, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, value = n) %>% 
  # if NA get rid of that line
  filter(is.na(`F`)) %>% 
  select(college, dept, prof_simp)

combs <- 
  expand_grid(badgirl$dept, badgirl$prof_simp) %>% 
  distinct() %>% 
  rename("dept" = 1,
         "prof_simp" = 2)

combs %>% 
  left_join(badgirl) %>%
  mutate(id = ifelse(is.na(college), 1, 0)) %>% 
  fill(college) %>% 
  ggplot(aes(dept, prof_simp)) + 
  geom_tile(aes(fill = id)) + 
  coord_flip()

badboy <- 
  ex %>% 
  # get rid of centers
  filter(!grepl('ctr', dept)) %>% 
  # make it so each person only counts once per position
  group_by(college, dept, prof_simp, gender, id) %>% 
  summarise(base_salary = mean(base_salary)) %>% 
  # keep only depts w/m and f in both positions AT SOME POINT in the dataset
  group_by(college, dept, prof_simp, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, value = n) %>% 
  # if NA get rid of that line
  filter(is.na(`M`)) %>% 
  select(college, dept, prof_simp)


# gender by pos interaction? --------------------------------------------------------

# yes.   
m1a <- lmerTest::lmer(lsal ~ prof_simp * gender + (gender | dept), data = ex_all, REML = F)

m1b <- lmerTest::lmer(lsal ~ prof_simp*  gender + (1| dept), data = ex_18, REML = F) # this model works

m1c <-  lmerTest::lmer(lsal ~ prof_simp + (gender | dept), data = ex_18, REML = F)

m1 <- lmerTest::lmer(lsal ~ prof_simp * gender + (1 | dept) + (1 |id), data = ex_18, REML = F)

anova(m1a, m1)

summary(m1a)

# male professors make more than females, everyone else is equal
exp(fixef(m1)[8])



# ratio of salaries -------------------------------------------------------
# what if I take the ratio of M and F salaries

lrat <- ex %>% 
  group_by(fiscal_year, college, dept, prof_simp, gender) %>% 
  summarise(base_salary = mean(base_salary)) %>% 
  spread(gender, base_salary) %>% 
  mutate(rat = M/`F`,
         lrat = log(rat)) %>%
  filter(fiscal_year == 2018)
lrat_all <- ex %>% 
  group_by(fiscal_year, college, dept, prof_simp, gender) %>% 
  summarise(base_salary = mean(base_salary)) %>% 
  spread(gender, base_salary) %>% 
  mutate(rat = M/`F`,
         lrat = log(rat))

m2 <- lmerTest::lmer(lrat ~ 1 + prof_simp + (1 | dept), data = lrat_all)
summary(m2)
exp(fixef(m2)[4])

# which depts have biggest effects on lrat?
as_tibble(ranef(m2)) %>% 
  ggplot(aes(grp, condval)) +
  geom_col() + 
  coord_flip()

ex2 %>% 
  filter(dept %in% c("agronomy", "statistics", "ag/biosys eng", "mechanical eng")) %>% 
  ggplot(aes(gender, base_salary)) + 
  geom_point(aes(color = prof_simp)) +
  facet_wrap(~dept)

# what if I take the ratio of M and F salaries, but use TOTAL instead of base

lrat <- ex2 %>% 
  group_by(fiscal_year, college, dept, prof_simp, gender) %>% 
  summarise(total_salary_paid = mean(total_salary_paid)) %>% 
  spread(gender, total_salary_paid) %>% 
  mutate(rat = M/`F`,
         lrat = log(rat))

m3 <- lmerTest::lmer(lrat ~ 1 + prof_simp + (1 | dept), data = lrat)
summary(m3)
exp(fixef(m3)[4])

# which depts have biggest effects on lrat?
as_tibble(ranef(m3)) %>% 
  ggplot(aes(grp, condval)) +
  geom_col() + 
  coord_flip()

ex2 %>% 
  filter(dept %in% c("agronomy", "statistics", "ag/biosys eng", "mechanical eng")) %>% 
  ggplot(aes(gender, base_salary)) + 
  geom_point(aes(color = prof_simp)) +
  facet_wrap(~dept)


ex %>% 
  filter(dept %in% c("agronomy", "statistics", "ag/biosys eng", "mechanical eng")) %>% 
  ggplot(aes(gender, base_salary)) + 
  geom_point(aes(color = prof_simp)) +
  facet_wrap(~dept)



# should we have position and gender interaction?
m3a <- lmerTest::lmer(lsal ~ prof_simp + gender + (1 | dept) + (1|name), data = ex2, REML = F)
m3b <- lmerTest::lmer(lsal ~ prof_simp*gender + (1 | dept) + (1|name), data = ex2, REML = F)
anova(m3a, m3b) #--borderline...


m3 <- lmerTest::lmer(base_salary ~ position*gender + (1 | dept) + (1|name), data = ex3)
summary(m3)
exp(fixef(m2)[7])
exp(confint(m2)[7,])

# should we include name as a random factor (each name appears more than once probably)
m4a <- lmerTest::lmer(lsal ~ position + gender + (1 | dept) + (1|name), data = ex2, REML = F)
m4b <- lmerTest::lmer(lsal ~ position + gender + (1 | dept), data = ex2, REML = F)
anova(m4a, m4b) #--this says yes




# business, engineering adm, accounting, these are the highest SALARIES, have nothing to do with gender
a <- as_tibble(ranef(m2)) %>% 
  arrange(-condval)

a %>% 
  ggplot(aes(reorder(grp, condval), condval)) +
  geom_col() + 
  coord_flip()

ex2 %>% 
  filter(dept %in% c("business", "interior dsn")) %>% 
  ggplot(aes(gender, base_salary)) + 
  geom_point() +
  facet_wrap(~dept)


m2 <- lmerTest::lmer(lsal ~ position + gender + (1 | dept), data = ex2)
