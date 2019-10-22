####################
# author: gina
# date created: Oct 16 2019
# last updated: Oct 22 2019 (what to do with dept?)
# purpose: help w/stats model
# notes:
#
#########################

library(tidyverse)
library(lmerTest)

ex <-   tibble(dept = rep(c("eeob", "eng", "agron", "stats", "phil"), each = 6),
               position = rep(c("asst prof", "prof", "asst prof"), 10),
               gender = rep(c("M", "M", "F", "F", "M", "F"), 5),
               salary = runif(30, min = 95, max = 260))


# From Jarad
#
m <- lm(log(salary) ~ position + gender + dept, data = ex)
summary(m)
exp(coef(m)[3])
exp(confint(m)[3,])

# ok cool. But what would we do with dept?
# Could add it as a random factor. But what if we want to 'rank' depts by 'male effect'?
#
m2 <- lmerTest::lmer(log(salary) ~ position + gender + (1 | dept), data = ex)
m2
exp(fixef(m2)[3])
exp(confint(m2)[5,])
ranef(m2)
    