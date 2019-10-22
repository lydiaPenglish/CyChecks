####################
# author: gina
# date created: Oct 16 2019
# last updated:
# purpose: explore calculating variances of ratios
# notes:
#
#########################

library(tidyverse)

#dat <- read_csv("data-raw/_tidy/cyd_salprofs.csv")

ex <-   tibble(position = c("asst prof", "asst prof", "asst prof", "prof", "prof", "prof", "prof", "prof"),
         gender = c("M", "M", "F", "M", "M", "F", "F", "F"),
         salary = c(100, 105, 100, 250, 260, 250, 240, 250), 
         dept = c("AGRON", "AGRON", "AGRON", "EEOB", "EEOB", "STAT", "STAT", "STAT"))
  
ex %>%
  group_by(position, gender) %>%
  summarise(msal = mean(salary)) %>%
  spread(gender, msal) %>%
  mutate(MFrat = M/`F`)

# How would you calculate the variation around that ratio?
# Katherine Goode recommended the delta method, which requires calculating covariance between M and F sals
# But the number of M and F points for a given position in a dept is never equal

m <- lm(log(salary) ~ position + gender + dept, data = ex)
m2 <- lmerTest::lmer(log(salary) ~ position + gender + (1 | dept), data = ex)
summary(m)
exp(coef(m)[3])
exp(confint(m2)[3,])
