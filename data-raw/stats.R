#--do stats on 'clean' data
#--11/20/20

library(dplyr)
library(tibble)
library(ggplot2)
library(scales)
library(stringr)

data("professors")

profs <- professors %>% as_tibble()

#--who makes $0-50000? log makes gender effect a ratio
bsal <- profs %>% 
  filter(dept_chair == "N") %>% 
  select(college, dept, gender, title_simp, base_salary, base50) %>% 
  filter(base50 == "Y") %>%
  mutate_if(is.character, str_to_title) %>% 
  mutate(lsal = log(base_salary),
         title_simp = factor(title_simp, levels = c("Asst Prof", "Assoc Prof", "Prof", "Awarded Prof")))


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
  
professors %>% 
  filter(gender == "f",
         base_salary > 150000,
         title_simp == "asst prof")

library(lme4)
library(lmerTest)
library(emmeans)
library(broom)


# This works but complains
m1a <- lmerTest::lmer(lsal ~ title_simp * gender + (1 + gender|dept), data = bsal, REML = F)

# without random slope
m1d <- lmerTest::lmer(lsal ~ title_simp * gender + (1|dept), data = bsal, REML = F)

anova(m1d, m1a) # idk this suggests that we don't need random slope

# male professors and awarded profs make more than females
exp(fixef(m1d)) %>% tidy()


#--try using brms
library(brms)
library(tidybayes)
library(forcats)

options(mc.cores = parallel::detectCores())

#--default priors are probably fine. 

b1a <- brm(
  lsal ~  gender * title_simp + gender + (1 | dept),
  data = bsal
)

summary(b1a)

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
