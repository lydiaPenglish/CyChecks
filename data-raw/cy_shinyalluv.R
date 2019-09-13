########################
# author: Gina
# created: Sept 13 2019
#
# last updated: 
#
# purpose: make a graph for kendall, maybe a shiny app
# 
# inputs: cyd_shinyalluv
# outputs: 
#
# notes: 
#
########################


rm(list = ls())
library(tidyverse)
library(janitor) #--cleans things nicely
library(readxl) #--reads excel spreadsheets
library(ggalluvial)
library(ggthemes)
library(here) #--helps w/wd things


cyd_aluv <- read_csv("data-raw/_tidy/cyd_shinyalluv.csv") %>%
  mutate(
    sal_cat = factor(sal_cat,
                     levels = c("<50K",
                                "50-100K",
                                "100-150K",
                                ">150K")),
    gender = factor(gender, 
                    levels = c("F", "M")),
    pos_pretty = factor(pos_pretty, 
                        levels = c("Asst Prof", "Assoc Prof", "Full Prof", "Named Prof"))
  )
  

cyd_aluv %>%
  filter(fiscal_year == 2018) %>%
  ggplot(aes(y = n,
             axis1 = gender, 
             axis2 = pos_pretty,
             axis3 = sal_cat)) +
  
  geom_alluvium(aes(fill = gender), width = 1/6) +
  geom_stratum(width = 1/3, fill = "grey25", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  
  scale_x_discrete(limits = c("Gender", "Position", "Base Salary"), expand = c(.05, .05)) +
  scale_fill_manual(values = c("darkblue", "goldenrod"))+
  
  labs(y = "Number", fill = "")+
  ggtitle("Dept of Agronomy Faculty")+
  
  theme_base()+
  theme(plot.background = element_blank(),
        legend.position = "none") + 
  facet_wrap(~fiscal_year)

