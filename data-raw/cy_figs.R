########################
# author: Gina
# created: Sept 11 2019
#
# last updated: sept 14 (looking at gender ratios, who has improved?)
#               sept 16 (2018 ratio, agronomy is the worst)
#
# purpose: make a graph for kendall, or whoever
# 
# inputs: 
# outputs: 
#
# notes: 
#
########################


rm(list = ls())
library(tidyverse)
library(ggthemes)
library(janitor) #--cleans things nicely
library(readxl) #--reads excel spreadsheets
library(here) #--helps w/wd things


cyd_salprofs <- read_csv("data-raw/_tidy/cyd_salprofs.csv") %>%
  mutate(pos_simp = factor(pos_simp, levels = c("asst prof", "assoc prof", "prof")))



# look at who has improved the most in gender rep -------------------------

cyd_ratios <- 
  cyd_salprofs %>%
  group_by(fiscal_year, college, dept, gender) %>%
  summarise(n = n()) %>%
  spread(gender, value = n) %>%
  replace(is.na(.), 0) %>%
  mutate(ratio = `F`/`M`,
         total = `F` + M) %>%
  select(-M, -`F`) %>%
  ungroup() %>%
  filter(grepl("college of", college))


# just look at ratios -----------------------------------------------------


mycols = c("red", rep("black", 15))

cyd_ratios %>%
  filter(fiscal_year == 2018, college == "college of agriculture & life sciences") %>%
  mutate(verd = ifelse(ratio < 1, "mal", "good"),
         dept = str_to_title(dept),
         college = str_to_title(college)) %>%
  
  
  ggplot(aes(reorder(dept, ratio), ratio)) + 
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  #geom_hline(yintercept = 0, color = "red") +
  
  geom_point(aes(size = total, fill = verd), pch = 21, color = "black", stroke = 2) + 
  geom_segment(aes(x = dept, xend = dept, y = 0, yend = ratio), size = 1.1) + 
  geom_point(aes(size = total, fill = verd), pch = 21, color = "black", stroke = 2) + 
  guides(fill = F) +
  coord_flip() + 
  
  facet_wrap(~college, scales = "free") + 
  theme_base() +
  theme(axis.title.x = element_text(size = rel(1.3), face = "bold"), 
          axis.text.y = element_text(colour = mycols, size = rel(1.3), face = "bold"),
        strip.text = element_text(size = rel(1.5), face = "bold")) + 
  labs(y = "Ratio of Female:Male\nTenure-Track Faculty ",
       x = NULL,
       size = "Number of\nTenure-track\nFaculty") + 
  scale_fill_manual(values = c( "blue", "firebrick"))
  
ggsave("data-raw/fig_FM-ratio.png", width = 10, height = 6)

# look at who has improved the most in gender rep -------------------------


#--note, this didn't work. 
library(broom)

myf_lm <- function(mydata = data){
  
  myres <- 
    tidy(anova(lm(data = data, ratio ~ fiscal_year))) %>%
    filter(term == "fiscal_year") %>%
    pull(p.value)
  
  return(round(myres, 3))
}

cyd_slopes <- 
  cyd_ratios %>%
  nest(fiscal_year, ratio) %>%
  mutate(mdl = data %>% map(myf_lm)) %>%
  unnest(mdl) 

# ok no one has improved, using a linear model. 
cyd_slopes %>%
  select(-data) %>%
  arrange(mdl)

#--just take the difference btwn 2011 and 2018

cyd_ratios %>%
  filter(fiscal_year %in% c(2011, 2018)) %>%
  spread(fiscal_year, ratio) %>%
  mutate(diff = (`2018` - `2011`),
         pct = (`2018` - `2011`)/`2011`,
         
         ratrat = log(`2018` / `2011`),
         verd = ifelse(diff < 0, "dec", "inc")) %>%
  mutate(college = ifelse(is.na(verd), "Has a Gender Missing", college),
         diff = ifelse(is.na(verd), 0, diff)) %>%
ggplot(aes(reorder(dept, diff), diff)) + 
  geom_col(aes(fill = verd)) + 
  coord_flip() +
  guides(fill = F) +
  ggtitle("Change in F:M Ratio of Faculty 2011-2018") +
  labs(x = NULL,
       y = "diff",
       fill = NULL) + 
  theme_base() + 
  facet_wrap(~college, scales = "free")
  

#--diff from 1 for 2011 and 2018 (1 is perfect parity)


cyd_ratios %>%
  filter(fiscal_year %in% c(2011, 2018)) %>%
  spread(fiscal_year, ratio) %>%
  mutate(
    f1_11 = `2011`,
    f1_18 = `2018`,
    diff = f1_18 - f1_11,
    verd = ifelse(diff < 0, "dec", "inc")
  ) %>%
  mutate(
    college = ifelse(is.na(verd), "Has a Gender Missing", college),
    diff = ifelse(is.na(verd), 0, diff)
  ) %>%
  ggplot(aes(reorder(dept, diff), diff)) +
  geom_col(aes(fill = f1_11)) +
  coord_flip() +
  #guides(fill = F) +
  ggtitle("Change in F:M Ratio Faculty From 2011-2018") +
  labs(x = NULL,
       y = "diff",
       fill = "2011 Ratio") +
  theme_base() +
  facet_wrap( ~ college, scales = "free") +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                      midpoint = 1)





  #replace(is.na(.), 0) %>%
  filter(dept == "agronomy") %>%
  ggplot(aes(fiscal_year, ratio)) + 
  geom_jitter(aes(color = pos_simp))





# look at agroomy dept ----------------------------------------------------

cyd_salprofs %>%
  filter(dept == "agronomy") %>%
  
  group_by(fiscal_year, pos_simp, gender) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(fiscal_year, pos_simp, gender, fill = list(n = 0)) %>%
  
  
  ggplot(aes(fiscal_year, n, group = gender)) + 
  geom_point(aes(color = gender)) + 
  geom_line(aes(color = gender)) +
  facet_grid(~pos_simp)


cyd_salprofs %>%
  filter(dept == "agronomy") %>%
  
  ggplot(aes(fiscal_year, pos_simp)) + 
  geom_line(aes(group = anon, color = gender)) + 
  facet_grid(~gender)

library(ggalluvial)


cyd_salprofs %>%
  filter(dept == "agronomy") %>%
  
  group_by(pos_simp, gender) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(pos_simp, gender, fill = list(n = 0)) %>%
  
  
  ggplot(aes(y = n, 
             axis1 = gender, 
             axis2 = pos_simp)) +
  geom_alluvium(aes(fill = gender), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Dept of Agronomy Faculty")


cyd_salprofs %>%
  filter(dept == "agronomy") %>%
  mutate(year = paste0("Y", fiscal_year)) %>%
  
  group_by(year, pos_simp, gender) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  select(fiscal_year, anon, gender, pos_simp, n) %>%
  complete(pos_simp, gender, fill = list(n = 0)) %>%
  
  ggplot(aes(x = fiscal_year,
             stratum = gender,
             alluvium = anon,
             y = n,
             fill = gender)) +
  #scale_x_discrete(expand = c(.1, .1)) +
  #geom_lode() +
  geom_flow() #+ 
#geom_stratum() #+ 
#geom_text(stat = "stratum")


# mitch had an idea -------------------------------------------------------

library(ggthemes)

specprofs = c("distg prof", "prof & chair", "univ prof", "prof emeritus")


cyd_shinyalluv <- cyd_salprofs %>%
  #filter(dept == "agronomy") %>%
  
  #--make a special prof category
  mutate(
    pos_simp2 = as.character(pos_simp),
    pos_simp2 = ifelse(!prof_simp %in% specprofs, pos_simp2, "spec prof")
  ) %>%
  
  mutate(
    sal_cat = case_when(
      (base_salary < 50000) ~ "<50K",
      (base_salary > 50000 & base_salary < 100000) ~ "50-100K",
      (base_salary > 100000 & base_salary < 150000) ~ "100-150K",
      (base_salary > 150000) ~ ">150K",
      
    )
  ) %>%
  
  
  mutate(
    sal_cat = factor(sal_cat,
      levels = c("<50K",
                 "50-100K",
                 "100-150K",
                 ">150K")),
    gender = factor(gender, 
                    levels = c("F", "M")),
    pos_pretty = recode(pos_simp2,
                        `asst prof` = "Asst Prof",
                        `assoc prof` = "Assoc Prof",
                        `prof` = "Full Prof",
                        `spec prof` = "Named Prof"),
    pos_pretty = factor(pos_pretty, 
                    levels = c("Asst Prof", "Assoc Prof", "Full Prof", "Named Prof"))
  ) %>%
  
  group_by(dept, pos_pretty, gender, fiscal_year, sal_cat) %>%
  summarise(n = n())
  


write_csv(cyd_shinyalluv, "data-raw/_tidy/cyd_shinyalluv.csv")
  

#ungroup() %>%
  #complete(fiscal_year, pos_simp2, gender, fill = list(n = 0)) %>%

cyd_shinyalluv %>%
  filter(dept == "agronomy", fiscal_year == 2018) %>%
  ggplot(aes(y = n,
             axis1 = gender, 
             axis2 = pos_pretty,
             axis3 = sal_cat)) +
  
  geom_alluvium(aes(fill = gender), width = 1/6) +
  geom_stratum(width = 1/3, fill = "grey25", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  
  scale_x_discrete(limits = c("Gender", "Position", "Base Salary"), expand = c(.05, .05)) +
  scale_fill_manual(values = c("darkblue", "goldenrod"))+
  
  labs(y = "Employees", fill = "")+
  ggtitle("Dept of Agronomy Faculty")+
  
  theme_base()+
  theme(plot.background = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = rel(1.2), color = "white"),
        strip.background = element_rect(fill = "gray25")) + 
  facet_wrap(~fiscal_year)



cyd_salprofs %>%
filter(dept == "agronomy") %>%
mutate(year = as.character(fiscal_year),
salary = ifelse(total_salary_paid <50000, "<50k",
ifelse(total_salary_paid > 50000 & total_salary_paid <=100000, "50-100k",
ifelse(total_salary_paid >100000 & total_salary_paid <=150000, "100-150k",
ifelse(total_salary_paid>150000 & total_salary_paid<=200000, "150-200k",
ifelse( total_salary_paid>200000, ">200k",  total_salary_paid))))),
salary = factor(salary, levels = c("<50k","50-100k","100-150k","150-200k",">200k")),
gender = factor(gender, levels = c("M","F")))%>%
group_by(pos_simp, gender, year, salary) %>%
summarise(n = n()) %>%
ungroup() %>%
complete(year, pos_simp, gender, fill = list(n = 0)) %>%
ggplot(aes(y = n,
axis2 = salary,
axis1 = gender,
axis3 = pos_simp)) +
geom_alluvium(aes(fill = gender), width = 1/12) +
geom_stratum(width = 1/12, fill = "grey25", color = "grey") +
geom_label(stat = "stratum", label.strata = TRUE) +
scale_x_discrete(limits = c("Gender", "Annual Salary", "Position"), expand = c(.05, .05)) +
#scale_fill_brewer(type = "qual", palette = "Set1") +
scale_fill_manual(values = c("darkblue", "darkred"))+
labs(y = "Employees", fill = "")+
ggtitle("Dept of Agronomy Faculty")+
theme_base()+
theme(plot.background = element_blank(),
legend.position = "none")


cyd_salprofs %>%
  filter(gender == "F", pos_simp == "asst prof", fiscal_year == 2018, dept ==agronomy)



# eliminate anon? add a 'general public' delineation? ---------------------


dum <- tibble(
  year = c(rep(2009, 4), rep(2010, 5)),
  pos = c("prof", "prof", "asc", "st", "prof", "prof", "asc", "asc", "ast"),
  gender = c("m", "m", "f", "m", "m", "m", "f", "m", "m"),
  name = c(1, 2, 3, 4, 1, 2, 3, 4, 5)
)

dum %>%
  unite(name, gender, col = "name_gen") %>%
  complete(year, name_gen, fill = list(pos = "public")) %>%
  separate(name_gen, into = c("name", "gen"), remove = F) %>%
  
  group_by(year, pos, gen) %>%
  mutate(n = n()) %>%
  
  ggplot(aes(x =year,
             stratum = pos,
             alluvium = name_gen,
             y = n,
             fill = gen)) +
  #scale_x_discrete(expand = c(.1, .1)) +
  #geom_lode() +
  geom_flow() #+ 
geom_stratum(stat = "stratum") #+ 
geom_text(stat = "stratum")

data(vaccinations)
vaccinations



# Lydia's attempt with ggalluvial
cyd_salprofs %>%
  filter(dept == "agronomy") %>%
  group_by(fiscal_year, pos_simp, gender) %>%
  summarise(n = n()) %>%
  ggplot(aes(fiscal_year, n, alluvium = gender))+
  ggalluvial::geom_alluvium(aes(fill = gender, colour = gender))+
  theme_bw()+
  facet_wrap(~pos_simp)




