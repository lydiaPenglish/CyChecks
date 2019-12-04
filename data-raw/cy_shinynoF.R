########################
# author: Gina
# created: Dec 1 2019
#
# last updated:
#
# purpose: make heatmap showing positions w/no females
#
# inputs: cyd_salprofs
# outputs:
#
# notes:
#
########################


rm(list = ls())
library(tidyverse)
library(shiny)
library(ggpubr)

cyd_base <- read_csv("data-raw/_tidy/cyd_missing-genders.csv") %>% 
  mutate_if(is.character, str_to_title) %>% 
  mutate(prof_simp = factor(prof_simp,
                             levels = c("Asst Prof", "Assoc Prof", "Prof", "Awarded Prof")))


# drop downs --------------------------------------------------------------

chcol <- c(sort(unique(as.character(cyd_base$college))))
chyear <- c(sort(unique(as.character(cyd_base$fiscal_year))))


# user interface ----------------------------------------------------------


ui <- fluidPage(
  # App Title
  titlePanel("CyChecks2"),
  
  # Sidebar drop-downs (department, fiscal_year)
  sidebarPanel(
    selectizeInput(
      "mycollege",
      label = ("College"),
      # - Based on gender
      choices = chcol,
      selected = "College Of Agriculture & Life Sciences"
    ),
    
    selectizeInput(
      "myyear",
      label = ("Year"),
      # - Based on gender
      choices = chyear,
      selected = "2018"
    )
  ),
  
  # prof salaries tab
  mainPanel(tabsetPanel(
    tabPanel("Positions Without Both Genders Present",
             plotOutput("plotgenrep")
             # fluidRow(
             #   splitLayout(cellWidths = c("50%", "50%"),
             #               plotOutput("plotsals"),
             #               plotOutput("plotns"))))
    )))
)

# server ------------------------------------------------------------------


server <- function(input, output) {
  
  #--Data--
  
  liq_gens <- reactive({
    cyd_base %>%
      filter(college == input$mycollege,
             fiscal_year == input$myyear)
  })
  
  #--plotgen--
  
  output$plotgenrep <- renderPlot({
    ggplot(data = liq_gens(),
           aes(x = dept,
               y = prof_simp)
           ) +
      
      geom_tile(aes(fill = id), color = "black", size = 1.5) +
      geom_point(aes(size = ndisplay, pch = isone)) +
      scale_fill_manual(values = c(`All Females` = "goldenrod",
                                   `All Males` = "deepskyblue3", 
                                   `Both Genders Present` = "gray90",
                                   `No One` = "gray10")) +
      scale_shape_manual(values = c(19, 1)) +
      
      coord_flip() +
      labs(
        x = NULL,
        y = NULL,
        fill = NULL, 
        size = "Number of Faculty",
        shape = NULL) +
      theme_pubclean() +
      facet_grid(~ college) + 
      
      theme(strip.text = element_text(size = rel(1.3), color = "white"),
            strip.background = element_rect(fill = "black"),
            axis.title = element_text(size = rel(1.3)),
            axis.text.x = element_text(size = rel(1.3)),
            legend.position = "right",
          legend.background = element_rect(linetype = "solid", color = "black"))
  })
  
}


shinyApp(ui, server)



