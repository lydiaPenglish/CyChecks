#--do stats on 'clean' data
#--11/20/20

library(dplyr)
library(tibble)
library(ggplot2)
library(scales)
library(stringr)

data("professors")

profs <- professors %>% as_tibble()

#--who makes $0-50000? Get rid of them for now.
#--log makes gender effect a ratio
bsal <- profs %>% 
  filter(dept_chair == "N") %>% 
  select(college, dept, gender, title_simp, base_salary, base50) %>% 
  filter(base50 == "Y") %>%
  mutate_if(is.character, str_to_title) %>% 
  mutate(lsal = log(base_salary),
         title_simp = factor(title_simp, levels = c("Asst Prof", "Assoc Prof", "Prof", "Awarded Prof")))


# prep work ---------------------------------------------------------------


#--look at it
bsal %>% 
  ggplot(aes(gender, base_salary, color = gender)) + 
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(alpha = 0.4) + 
  facet_grid(title_simp ~ .) +
  scale_y_continuous(labels = dollar_format()) + 
  guides(color = F) +
  coord_flip() + 
  labs(title = "2019 ISU Salaries",
       y = "Reported Base Salary",
       x = NULL) + 
  theme_bw()
  
#--dang who makes more than $150thou as an asst
#--oh. business.
professors %>% 
  filter(gender == "f",
         base_salary > 150000,
         title_simp == "asst prof") %>% 
  select(college, dept, gender, name, title)


# frequentist -------------------------------------------------------------

library(lme4)
library(lmerTest)
library(emmeans)
library(broom)


# This works but complains
fm1a <- lmerTest::lmer(lsal ~ title_simp * gender + (1 + gender|dept), data = bsal, REML = F)

# without random slope
fm1d <- lmerTest::lmer(lsal ~ title_simp * gender + (1|dept), data = bsal, REML = F)

anova(fm1d, fm1a) # idk this suggests that we don't need random slope

# male professors and awarded profs make more than females?
exp(fixef(fm1d)) %>% tidy()



# bayesian ----------------------------------------------------------------

library(brms)
library(tidybayes)
library(forcats)

bsal_brms <- 
  bsal %>% 
  select(-base50) %>% 
  mutate_if(is.character, as.factor)

options(mc.cores = parallel::detectCores())

#--what are default priors
get_prior(
  lsal ~  gender * title_simp + gender + (1 | dept),
  data = bsal_brms
  ) %>% 
  parse_dist(prior) %>% #--a tidybayes function
  ggplot(aes(y = prior, dist = .dist, args = .args, fill = class)) +
  stat_dist_halfeye() +
  theme(axis.text=element_text(size=15))

#--test new ones
p0 <- prior(student_t(3, 11.5, 2.5), class = "b",  coef = "default") + 
  prior(student_t(3, 11.5, 2), class = "b", coef = "tighter") + 
  prior(student_t(3, 11.5, 1), class = "b", coef = "even tighter") 

p0 %>% 
  parse_dist(prior) %>% #--a tidybayes function
  ggplot(aes(y = prior, dist = .dist, args = .args, fill = coef)) + #--never would've figured this out
  stat_dist_halfeye() +
  theme(axis.text=element_text(size=15))

exp(10)
exp(11)
exp(15)

#--I think the sd of 1 is reasonable

m1pr <- prior(student_t(3, 11.5, 1), class = "b")
  
m1 <- brm(
  lsal ~  gender * title_simp + gender + (1 | dept),
  data = bsal_brms,
  sample_prior = T
)

#--oof the intercept still had a hard time. I'll have to think about why that is
summary(m1)

bsal_brms %>% 
  ggplot(aes(lsal)) + 
  geom_density()

#--look at the intercept
#--should I restrict it to positive values? No one should make less than $1. 
prior_samples(m1) %>%
  as_tibble() %>% 
  tidyr::pivot_longer(cols = Intercept:sd_dept) %>% 
  ggplot(aes(y = name, x = value)) +
  stat_halfeye()


#--look at terms
#--b_Intercept is female-assistprof. 
#--the r_dept are offsets from the intercept for each dept. 
get_variables(m1)

#--since we only had random intercept effects, just get it by dept
# note: spread_draws does wide, gather_draws does long format
m1 %>%
  spread_draws(r_dept[dept,]) %>%
  head(10)

#--which departments have the highest salaries?
#--you can see the departments that don't have many people
m1 %>%
  spread_draws(b_Intercept, r_dept[dept,]) %>%
  median_qi(condition_mean = b_Intercept + r_dept) %>%
  ggplot(aes(y = reorder(dept, condition_mean), x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval()



# try with 0 intercept ----------------------------------------------------

#--mcelreath talks about how using the above assigns unequal uncertainty to m vs f

#--hmm they're all flat. I could probably constrain them. Try without defining them
get_prior(
  lsal ~  0 + gender * title_simp + gender + (1 | dept),
  data = bsal_brms
)

mypriors <- prior(student_t(3, 11.5, 2), class = b, coef = genderM) + 
  prior(student_t(3, 11.5, 2), class = b, coef = genderF)  

get_prior(
  lsal ~  0 + gender * title_simp + gender + (1 | dept),
  prior = mypriors,
  data = bsal_brms
)

# #--anabelle's for ref
# mypriors <-
#   prior(student_t(10, 0, 0.1), class = "sd", group = "exp_ID") +
#   prior(
#     student_t(10,-4.4, 0.1),
#     class = "b",
#     coef = "plot_scaleOFT",
#     dpar = "sigma"
#   ) +
#   prior(
#     student_t(5,-3.2, 0.5),
#     class = "b",
#     coef = "plot_scaleSPT",
#     dpar = "sigma"
#   ) +
#   prior(normal(0, 0.2), class = b, coef = plot_scaleOFT) +
#   prior(normal(0, 0.2), class = b, coef = plot_scaleSPT)


m2 <- brm(
  lsal ~  0 + gender * title_simp + gender + (1 | dept),
  prior = mypriors,
  data = bsal,
  sample_prior = T
)

#--man those intercept!
summary(m2)

#--look at terms
#--b_Intercept is female-assistprof. 
#--the r_dept are offsets from the intercept for each dept. 
get_variables(b1a)

#--since we only had random intercept effects, just get it by dept
# note: spread_draws does wide, gather_draws does long format
b1a %>%
  spread_draws(r_dept[dept,]) %>%
  head(10)

#--which departments have the highest salaries?
b1a %>%
  spread_draws(b_Intercept, r_dept[dept,]) %>%
  median_qi(condition_mean = b_Intercept + r_dept) %>%
  ggplot(aes(y = reorder(dept, condition_mean), x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval()




b1a %>%
  gather_draws(`b_.*`, regex = TRUE) %>%
  ungroup() %>%
  mutate(
    .variable = str_remove_all(.variable, "b_|gender|title_simp")) %>% 
  dplyr::filter(.variable != "Intercept",
                .variable != "Prof",
                .variable != "AssocProf",
                .variable != "AwardedProf") %>%
  mutate(
    .variable = ifelse(.variable == "M", "M:AsstProf", .variable),
    .variable = fct_reorder(.variable, .value)) %>% 
  ggplot(aes(x = .value, y = .variable)) +
  geom_vline(xintercept = 0, color = "gray50", size = 1.2, lty = 2, alpha = 0.5) +
  geom_halfeyeh(fill = "gray80") +
  stat_pointintervalh(.width = c(.66, .95)) +
  theme(legend.position = "none") 
