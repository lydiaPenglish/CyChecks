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
library(ggalluvial)
library(ggthemes)
library(here) #--helps w/wd things
library(shiny)


# create data -------------------------------------------------------------
cyd_aluv <- read_csv("data-raw/_tidy/cyd_shinyalluv.csv") %>%
  mutate(gender = recode(gender,
                         `F` = "Female",
                         M = "Male")) %>%
  mutate(
    sal_cat = factor(sal_cat,
                     levels = c("<50K",
                                "50-100K",
                                "100-150K",
                                ">150K")),
    gender = factor(gender, 
                    levels = c("Female", "Male")),
    pos_pretty = factor(pos_pretty, 
                        levels = c("Asst Prof", "Assoc Prof", "Full Prof", "Named Prof"))

      )



# drop down menus ---------------------------------------------------------

dept <- c(sort(unique(as.character(cyd_aluv$dept))))

fiscal_year <- c(sort(unique(as.character(cyd_aluv$fiscal_year))))


# user interface ----------------------------------------------------------

tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: 4; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 4;    /* Firefox */ 
                                   column-count: 4; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
                                 ))
  


ui <- fluidPage(
  hr(),
  
  # App Title
  titlePanel("CyChecks2"),
  h3(
    "Iowa State University\nBreakdown of Gender/Position/Base Salary"
  ),
  
  plotOutput("alluvplot"),
  
  tweaks,
  
  fluidRow(# selectize for dept
    column(
      6,
      selectizeInput(
        "dropdownGroup",
        label = h3("Department Selection"),
        choices = dept,
        selected = "statistics"
      )
    ),
    
    column(
      width = 6,
      h3("Fiscal Year Selection(s)"),
      tags$div(
        align = "left",
        class = "multicol",
        checkboxGroupInput(
          inputId = "checkGroup",
          choices = fiscal_year,
          label = NULL,
          selected = 2018
        )
      )
    ))
)



  


# server ------------------------------------------------------------------

server <- function(input, output) {
  
  liq_alluv <- reactive({
    
    cyd_aluv %>%
      filter(fiscal_year %in% input$checkGroup) %>%
      filter(dept == input$dropdownGroup)
  })
  
  output$alluvplot <- renderPlot({
    ggplot(data = liq_alluv(),
           aes(
             y = n,
             axis1 = gender,
             axis2 = pos_pretty,
             axis3 = sal_cat
           )) +
      
      geom_alluvium(aes(fill = gender), width = 1 / 6) +
      geom_stratum(width = 1 / 3,
                   fill = "grey25",
                   color = "grey") +
      geom_label(stat = "stratum", label.strata = TRUE) +
      
      scale_x_discrete(
        limits = c("Gender", "Position", "Base Salary"),
        expand = c(.05, .05)
      ) +
      scale_fill_manual(values = c("darkblue", "goldenrod")) +
      
      labs(y = "Number of People", fill = "") +
      
      theme_base() +
      theme(plot.background = element_blank(),
            legend.position = "none",
            strip.text = element_text(size = rel(1.2), color = "white"),
            strip.background = element_rect(fill = "gray25")) +
      facet_wrap( ~ fiscal_year)
  })
}

shinyApp(ui, server)

