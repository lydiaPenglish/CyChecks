####################
# author: gina
# date created: Dec 1 2019
# last updated: 
# purpose: create data for shiny app showing where females are missing
# notes:
#
#########################

library(tidyverse)
library(ggpubr)

exraw <- read_csv("data-raw/_tidy/cyd_salprofs.csv") %>% 
  #Just keep 'colleges'
  filter(grepl("college", college)) %>% 
  # who gets paid 0?
  filter(base_salary > 0) %>% 
  # get rid of centers
  filter(!grepl('ctr', dept)) %>%
  # keep fiscal year
  group_by(fiscal_year, college, dept, prof_simp, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, value = n) %>% 
  ungroup()

yrs <- exraw %>% pull(fiscal_year) %>% unique()
dept <- exraw %>% pull(dept) %>% unique()
prof_simp <- exraw %>% pull(prof_simp) %>% unique()

combs <- 
  expand_grid(yrs, dept, prof_simp) %>% 
  rename("fiscal_year" = 1)

coldept <- exraw %>% 
  select(fiscal_year, college, dept) %>% 
  distinct()


coldept2 <- exraw %>% 
  select(college, dept) %>% 
  distinct()

# assign ------------------------------------------------------------------

nomf <- 
  exraw %>% 
  mutate(id = ifelse(is.na(`F`), "No Females",
                     ifelse(is.na(M), "No Males", NA))) %>% 
  select(fiscal_year, dept, prof_simp, id) 



dat <- combs %>% 
  left_join(nomf) %>% 
  left_join(coldept) 


dat %>% 
  filter(fiscal_year == 2018) %>% 
  ggplot(aes(dept, prof_simp)) + 
  geom_tile(aes(fill = id)) + 
  coord_flip()

# write it ----------------------------------------------------------------

dat %>% write_csv("data-raw/_tidy/cyd_missing-genders.csv")


# trying something new ----------------------------------------------------

dat <- 
  # Get all combinations of prof/depts
  combs %>% 
  left_join(exraw) %>% 
  select(-college) %>% 
  # assign college
  left_join(coldept2) %>% 
  # assign gender verdict
  mutate(id = ifelse((is.na(`F`) & is.na(M)), "No One",
                     ifelse(is.na(`F`), "No Females",
                            ifelse(is.na(M), "No Males", "Both Genders Present")))) %>% 
  mutate(ndisplay = ifelse(id == "Both Genders Present", M + `F`,
                           ifelse( id == "No One", NA, 
                                   ifelse(id == "No Females", M, `F`))))


# write it ----------------------------------------------------------------

dat %>% write_csv("data-raw/_tidy/cyd_missing-genders.csv")

dat %>% 
  filter(grepl("ag", college)) %>% 
  filter(fiscal_year == 2018) %>% 
  ggplot(aes(x = dept, y = prof_simp)) +
  geom_tile(aes(fill = id)) +
  geom_point(aes(size = ndisplay)) +
  scale_fill_manual(values = c(`No Males` = "darkorchid1",
                               `No Females` = "goldenrod", 
                               `Both Genders Present` = "gray90",
                               `No One` = "gray10")) +
  coord_flip() +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL) +
  #theme_bw() +
  theme_pubclean() +
  facet_grid(~ college) + 
  
  theme(strip.text = element_text(size = rel(1.3), color = "white"),
        strip.background = element_rect(fill = "black"),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.3)),
        legend.position = "top",
        legend.background = element_rect(linetype = "solid", color = "black"))


library(ggpubr)
# practice shiny plot -----------------------------------------------------

dat %>% 
  filter(grepl("ag", college)) %>% 
  filter(fiscal_year == 2018) %>% 
  ggplot(aes(x = dept, y = prof_simp)) +
geom_tile(aes(fill = id)) +
  scale_fill_manual(values = c(`No Males` = "darkorchid1",
                               `No Females` = "goldenrod")) +
  coord_flip() +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL) +
  #theme_bw() +
  theme_pubclean() +
  facet_grid(~ college) + 
  
  theme(strip.text = element_text(size = rel(1.3), color = "white"),
        strip.background = element_rect(fill = "black"),
        axis.title = element_text(size = rel(1.3)),
        axis.text.x = element_text(size = rel(1.3)),
        legend.position = "top",
        legend.background = element_rect(linetype = "solid", color = "black"))
