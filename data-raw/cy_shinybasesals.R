########################
# author: Gina
# created: Sept 29 2019
#
# last updated:
#
# purpose: update shiny from cychecks with base salaries
#
# inputs: cyd_salprofs
# outputs:
#
# notes:
#
########################


rm(list = ls())
library(tidyverse)
library(ggthemes)
library(here) #--helps w/wd things
library(shiny)

# create data -------------------------------------------------------------

cyd_base <- read_csv("_tidy/cyd_salprofs.csv") %>%
  mutate(gender = recode(gender,
                         `F` = "Female",
                         M = "Male"),
         gender = factor(gender,
                         levels = c("Female", "Male"))) %>%
  select(fiscal_year, college, dept, name, gender, base_salary, prof_simp) %>%
  
  mutate(college = str_to_title(college),
         dept = str_to_title(dept),
         prof_simp  = str_to_title(prof_simp),
         
         prof_simp = factor(prof_simp,
                             levels = c("Asst Prof", "Assoc Prof", "Prof", "Named Prof")))



# drop downs --------------------------------------------------------------

chdept <- c(sort(unique(as.character(cyd_base$dept))))
chyear <- c(sort(unique(as.character(cyd_base$fiscal_year))))


# user interface ----------------------------------------------------------



ui <- fluidPage(
  # App Title
  titlePanel("CyChecks2"),
  
  # Sidebar drop-downs (department, fiscal_year)
  sidebarPanel(
    selectizeInput(
      "mydept",
      label = ("Department"),
      # - Based on gender
      choices = chdept,
      selected = "Agronomy"
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
    tabPanel("Professor Salaries",
             plotOutput("plotsals")
             # fluidRow(
             #   splitLayout(cellWidths = c("50%", "50%"),
             #               plotOutput("plotsals"),
             #               plotOutput("plotns"))))
    )))
)

# server ------------------------------------------------------------------


server <- function(input, output) {
  
  
  #--Data--
  
  liq_sals <- reactive({
    cyd_base %>%
      filter(dept == input$mydept,
             fiscal_year == input$myyear)
  })
  
  liq_ns <- reactive({
    cyd_base %>%
      filter(dept == input$mydept) %>%
      group_by(fiscal_year, gender) %>%
      summarise(n = n())
  })
  
  
  #--plotsals--
  
  output$plotsals <- renderPlot({
    ggplot(
      data = liq_sals(),
      aes(x = gender,
          y = base_salary / 1000)) +
      
      geom_col(
        data = liq_sals() %>%
          group_by(prof_simp, gender) %>%
          summarise(base_salary = mean(base_salary)),
        aes(
          x = gender,
          y = base_salary / 1000,
          fill = gender
        )
      ) +
      geom_jitter(
        color = "gray80",
        size = 4,
        #pch = 21,
        fill = "black",
        width = 0.3,
        aes(shape = gender)
      ) +
      scale_fill_manual(values = c(Male = "darkblue",
                                   Female = "goldenrod")) +
      labs(
        x = NULL,
        y = "Base Salary (Thousands of $)",
        color = NULL) +
      theme_bw() +
      guides(color = F, fill = F, pch = F) +
      scale_shape_manual(values = c(22, 21)) +
      facet_grid(~ prof_simp) + 
      
      theme(strip.text = element_text(size = rel(1.3), color = "white"),
            strip.background = element_rect(fill = "black"),
            axis.title = element_text(size = rel(1.3)),
            axis.text.x = element_text(size = rel(1.3)))
    
    
    #+
    #theme(legend.position = "top",
    #      legend.background = element_rect(linetype = "solid", color = "black"))
  })
  
  #--plotns--
  
  output$plotns <- renderPlot({
    ggplot(data = liq_ns(),
           aes(
             x = fiscal_year,
             y = n,
             color = gender,
             group = gender
           )) +
      geom_line() +
      geom_point(size = 2) +
      theme_bw() +
      labs(
        x = NULL,
        y = "Number of Faculty",
        color = "Gender",
        title = "Faculty Departments Were Assigned Based on Information From HR"
      ) +
      scale_color_manual(values = c(M = "darkblue",
                                    `F` = "goldenrod")) +
      theme(
        legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(linetype = "solid", color = "black")
      )
  })
  
}


shinyApp(ui, server)


