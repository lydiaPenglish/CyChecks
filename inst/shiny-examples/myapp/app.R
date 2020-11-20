library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(shinythemes)
library(plotly)
library(scales) #--to get $ on y axis, so easy!
library(stringr)
library(CyChecks3)


# create data and dropdowns -----------------------------------------------------

#--salaries

sals <- 
  professors %>%
  filter(base_salary > 0) %>% 
  mutate_if(is.character, str_to_title) %>% 
  mutate(title_simp = factor(title_simp,
                            levels = c("Asst Prof", 
                                       "Assoc Prof", 
                                       "Prof", 
                                       "Awarded Prof"))) %>% 
  pivot_longer(cols = base_salary:total_salary_paid,
               names_repair = "minimal",
               names_to = "salary_type",
               values_to = "amount") %>% 
  mutate(salary_type_nice = str_replace_all(salary_type, "_", " ") %>% str_to_title(.),
         year = as.character(year))

malecolor <- "deepskyblue3"
femalecolor <- "goldenrod"

dd_college <- sals %>% arrange(college) %>% pull(college) %>% unique()
dd_dept <-  sals %>% arrange(dept) %>% pull(dept) %>% unique()
dd_year <-  sals %>% arrange(year) %>% pull(year) %>% unique() %>% as.character()


#--practice graph

sals %>%
  filter(college == dd_college[1]) %>%
  select(year, college, dept, gender, name, title, title_simp) %>%
  distinct() %>%
  group_by(year, college, dept, gender, title_simp) %>%
  summarise(n = n()) %>%
  filter(dept == "Agronomy") %>%
  ggplot(aes(dept, title_simp)) +
  geom_point(color = "white", size = 15, stroke = 2) +
  geom_point(aes(color = gender, size = n, pch = gender), stroke = 2) +
  guides(size = F) +
  scale_color_manual(values = c(`F` = femalecolor, 'M' = malecolor)) +
  scale_shape_manual(values = c(`F` = 16, 'M' = 21)) +
  scale_size(range = c(1, 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_line(size = 1.2, color = "black")) +
  labs(x = NULL,
       y = NULL)

  

# user interface ----------------------------------------------------------



ui <- fluidPage(theme = shinytheme("united"),
                
                # Application title
                navbarPage(
                  "CyChecks3 Professor Explorer",
                  
                  #--start tab
                  tabPanel("Salaries by Department",
                           fluidRow(
                             column(
                               width = 4,
                               #--input, select dept
                               selectInput(
                                   "sel_dept_sals",
                                   label = ("Department:"),
                                   choices = dd_dept,
                                   selected = dd_dept[6]),
                               #--input, select year
                               selectInput(
                                   "sel_year_sals",
                                   label = ("Year:"),
                                   choices = dd_year,
                                   selected = dd_year[1]),
                               #--download data
                               downloadButton("downloadData", "Download")
                              ),
                             column(width = 8,
                                    plotlyOutput("fig_sals", height = "800px", width = "1000px"))
                           )),
                  #--end sals tab-
                  
                  #--start gender rep tab-
                  tabPanel("Gender Makeup By Department",
                           fluidRow(
                             column(
                               width = 3,
                               #--input, select college
                               selectInput(
                                 "sel_college_gend",
                                 label = ("College:"),
                                 choices = dd_college,
                                 selected = dd_college[1]),
                               #--input, select dept
                               # selectInput(
                               #   "sel_dept_gend",
                               #   label = ("Department:"),
                               #   choices = dd_dept,
                               #   selected = dd_dept[6]),
                               #--input, select year
                               selectInput(
                                 "sel_year_gend",
                                 label = ("Year:"),
                                 choices = dd_year,
                                 selected = dd_year[1])
                               ),
                             column(width = 9,
                                    fluidRow(
                                    plotOutput("fig_gend",  height = "500px", width = '1200px')
                                    )#,
                                    #fluidRow(
                                    #  plotOutput("fig_gend_dept")
                           ))
                  )
                  #--end gender rep tab-
                  
                  
                )
                )




# server ------------------------------------------------------------------

server <- function(input, output) {
 
  
  #--salary tab-------------------
  
   liq_sals <- reactive({
    sals %>%
      filter(
      #  college == input$sel_college_sals,
         dept == input$sel_dept_sals,
         year == input$sel_year_sals)
  })
  
  output$fig_sals <- renderPlotly({
    
    p1 <-
      liq_sals() %>%
      ggplot(
        aes(gender, amount, group = 1,
        text = paste("Name:", name, "(", gender, ")",
          "<br>Salary: $", round(amount / 1000, digits = 0), "thou"))
        ) +
      stat_summary(fun = mean, geom = "bar", aes(fill = gender)) +
      geom_jitter(aes(color = dept_chair, pch = dept_chair), width = 0.15, size = 2) +
      guides(fill = F,
             color = F,
             pch = F) +
      scale_y_continuous(labels = label_dollar()) +
      scale_fill_manual(values = c(`F` = femalecolor, "M" = malecolor)) +
      scale_color_manual(values = c("N" = "black", "Y" = "red4")) +
      scale_shape_manual(values = c(16, 17)) +
      facet_grid(salary_type_nice ~ title_simp,
                 scales = "free_y",
                 switch = "y") +
      theme_pubclean() +
      theme(panel.border = element_rect(size = 1, fill = NA)) +
      labs(x = NULL, y = NULL)
    
    ggplotly(p1, tooltip = "text")
    
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ISUProfSals_", input$sel_dept_sals, "-", input$sel_year_sals, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(liq_sals(), file, row.names = FALSE)
    }
  )
  
  #--gender tab---------------
  
  liq_gend1 <- reactive({
    sals %>%
      filter(
        college == input$sel_college_gend,
        year == input$sel_year_gend) %>% 
      select(year, college, dept, gender, name, title, title_simp) %>% 
      distinct() %>% 
      group_by(year, college, dept, gender, title_simp) %>% 
      summarise(n = n())
      
  })
  
  output$fig_gend <- renderPlot({
    
    liq_gend1() %>% 
      ggplot(aes(dept, title_simp)) + 
      geom_point(color = "gray90", size = 15, stroke = 2) +
      geom_point(aes(color = gender, size = n, pch = gender), stroke = 2) + 
      guides(pch = F) +
      scale_color_manual(values = c(`F` = femalecolor, 'M' = malecolor)) +
      scale_shape_manual(values = c(`F` = 20, 'M' = 21)) +
      scale_size(range = c(1, 20)) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = rel(1.5)),
            panel.grid.major.x = element_line(size = 1, color = "black"),
            panel.background = element_rect(fill = "gray90", 
                                            colour = NA),
            legend.position = "right",
            #legend.direction = "horizontal",
            legend.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.5))) +
      labs(x = NULL,
           y = NULL,
           size = "Number of Faculty",
           color = "Gender")
  })
  
  

}

shinyApp(ui, server)
