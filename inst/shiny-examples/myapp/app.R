library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(shinythemes)
library(plotly)
library(scales) #--to get $ on y axis, so easy!
library(stringr)
library(forcats)
library(CyChecks3)


# create data and dropdowns -----------------------------------------------------

mytheme <- theme_bw() + 
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(size = rel(1.5)),
        strip.text.y = element_text(angle = 0),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.5)))


#--salaries


clean_punc <- function(x) {
  stringr::str_replace_all(x, "\\'", "") %>% 
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace_all(" ", "") %>%
    stringr::str_replace_all("[^[:alnum:]]", "") %>% 
    stringr::str_trim(.) %>% 
    stringr::str_squish(.)
}

#--problem - some depts are listed under multiple colleges

mul_col <- 
  professors %>% 
  select(college, dept) %>% 
  distinct() %>% 
  group_by(dept) %>% 
  mutate(n = n()) %>% 
  filter(n > 1) %>% 
  pull(dept)

sals <- 
  professors %>%
  filter(base50 == "Y") %>% 
  filter(base_salary > 0) %>% 
  mutate(college2 = ifelse(dept %in% mul_col, "Multiple", college)) %>% 
  mutate_if(is.character, str_to_title) %>% 
  mutate(
    gender = recode(gender, 
                    "F" = "Female",
                    "M" = "Male"),
    title_simp = factor(title_simp,
                            levels = c("Asst Prof", 
                                       "Assoc Prof", 
                                       "Prof", 
                                       "Awarded Prof"))) %>% 
  pivot_longer(cols = base_salary:total_salary_paid,
               names_repair = "minimal",
               names_to = "salary_type",
               values_to = "amount") %>% 
  mutate(salary_type_nice = str_replace_all(salary_type, "_", " ") %>% str_to_title(.),
         year = as.character(year),
         dept_clean = clean_punc(dept),
         dept_chair2 = ifelse(dept_chair == "Y", "Dept Chair", ""))

malecolor <- "deepskyblue3"
femalecolor <- "goldenrod"

dd_college <- sals %>% arrange(college2) %>% pull(college2) %>% unique()
dd_dept <-  sals %>% arrange(dept) %>% pull(dept) %>% unique()
dd_year <-  sals %>% arrange(year) %>% pull(year) %>% unique() %>% as.character()
dd_saltype <-  sals %>% arrange(salary_type_nice) %>% pull(salary_type_nice) %>% unique() %>% as.character()


#--practice graph
prac_gend <- 
  sals %>%
#  filter(college == dd_college[1]) %>%
  select(year, college, dept, gender, name, title, title_simp) %>% 
  distinct() %>% 
  group_by(year, college, dept, gender) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  mutate(
    Female = ifelse(is.na(Female), 0, Female),
    Male = ifelse(is.na(Male), 0, Male),
    tot = Female + Male,
    fracM = Male/tot,
    dept = fct_reorder(dept, fracM)) %>%
  arrange(dept) 

prac_gend %>% 
  arrange(college, fracM) %>% 
  mutate(dept = fct_inorder(dept)) %>% 
  ggplot(aes(dept, fracM,
         group = 1,
         text = paste("Total Faculty:", tot)
         )) +
  geom_segment(aes(x = dept, xend = dept, y = 0, yend = fracM)) +
  geom_point(aes(size = tot), color = "red4") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  facet_wrap(~college, scales = "free") +
  scale_y_continuous(labels = label_percent()) +
  coord_flip() +
  mytheme +
  theme(legend.position = "none") +
  labs(y = "Male Faculty (% of Total)", x = NULL)



plotbp <- 
  sals %>%
  filter(salary_type != "travel_subsistence") %>% 
  ggplot(aes(x = gender, y = amount, color = gender,
             group = 1,
             text = paste("Name:", name,
                          "<br>Dept:", dept, 
                          "<br> Dept Chair? ", dept_chair,
                          "<br>Salary: $", round(amount / 1000, digits = 0), "thou"))) +
  geom_boxplot(outlier.color = NA,
               outlier.size = 0,
               outlier.shape = NA,
               aes(color = gender)) +
  geom_jitter(alpha = 0.5, width = 0.2, size = 3) +
  guides(color = F) +
  scale_color_manual(values = c("Female" = femalecolor, "Male" = malecolor)) +
  facet_grid(title_simp ~ salary_type_nice) +
  scale_y_continuous(labels = dollar_format()) +
  coord_flip() +
  mytheme +
  theme(legend.position = "none",
        strip.background.y = element_rect(color = NA))+
  labs(x = NULL,
       y = NULL)

ggplotly(p1, tooltip = "text") %>% layout(margin = list(r = 160))


# user interface ----------------------------------------------------------



ui <- fluidPage(theme = shinytheme("united"),
                
                # Application title
                navbarPage(
                  "CyChecks3 Professor Explorer",
                  
                                #--start tab
                  tabPanel("University Salaries by Gender",
                           fluidRow(
                             column(
                               width = 3,
                              #--input, select year
                               selectInput(
                                 "sel_year_univ",
                                 label = ("Year:"),
                                 choices = dd_year,
                                 selected = dd_year[1]),
                              #--input, select salary type
                              selectInput(
                                "sel_saltype_univ",
                                label = ("Salary Type:"),
                                choices = dd_saltype,
                                selected = dd_saltype[1]),
                               #--download data
                               downloadButton("downloadData_univ", "Download")
                             ),
                             column(width = 9,
                                    plotlyOutput("fig_univ", 
                                               height = "800px", width = "1200px")))
                           ),
                  #--end univ tab-
                  
                  #--start tab
                  tabPanel("Salaries by Department",
                           fluidRow(
                             column(
                               width = 3,
                               #--input, select dept
                               selectInput(
                                 "sel_dept_sals",
                                 label = ("Department:"),
                                 choices = dd_dept,
                                 selected = dd_dept[6]),
                               #--input, select salary type
                               selectInput(
                                 "sel_saltype_sals",
                                 label = ("Salary Type:"),
                                 choices = dd_saltype,
                                 selected = dd_saltype[1]),
                               
                               #--input, select year
                               selectInput(
                                 "sel_year_sals",
                                 label = ("Year:"),
                                 choices = dd_year,
                                 selected = dd_year[1]),
                               #--download data
                               downloadButton("downloadData_sals", "Download")
                             ),
                             column(width = 9,
                                    plotlyOutput("fig_sals", height = "800px", width = "1200px"))
                           )),
                  #--end sals tab-
                  
                                    
                  #--start gender rep tab-
                  tabPanel("Gender Makeup By Department",
                           fluidRow(
                             column(
                               width = 2,
                               #--input, select college
                               selectInput(
                                 "sel_college_gend",
                                 label = ("College:"),
                                 choices = c("All", dd_college),
                                 selected = "All"),
                               #--input, select year
                               selectInput(
                                 "sel_year_gend",
                                 label = ("Year:"),
                                 choices = dd_year,
                                 selected = dd_year[1])
                               ),
                             column(width = 10,
                                    fluidRow(
                                    plotlyOutput("fig_gend",  height = "800px", width = '1300px')
                                    )
                           ))
                  )
                  #--end gender rep tab-
                  
                  
                )
                )




# server ------------------------------------------------------------------

server <- function(input, output) {
 
  #--university tab-------------------
  
  liq_univ <- reactive({
    sals %>%
      filter(
        year == input$sel_year_univ,
        salary_type_nice == input$sel_saltype_univ) 
    
  })
  
  
  output$fig_univ <- renderPlotly({
    
    
    puniv <- 
      liq_univ() %>%
      ggplot(aes(x = gender, y = amount, color = gender,
                 group = 1,
                 text = paste("Name:", name,
                              "<br>Dept:", dept, 
                              "<br>",   dept_chair2,
                              "<br>Salary: $", round(amount / 1000, digits = 0), "thou"))) +
      geom_boxplot(outlier.color = NA, aes(color = gender)) +
      geom_jitter(alpha = 0.5, width = 0.2, size = 2) +
      guides(color = F) +
      scale_color_manual(values = c("Female" = femalecolor, "Male" = malecolor)) +
      facet_grid(. ~title_simp) +
      scale_y_continuous(labels = dollar_format()) +
      # coord_flip() +
      mytheme +
      theme(legend.position = "none",
            strip.background.y = element_rect(color = NA)) +
      labs(x = NULL,
           y = NULL)
    
    ggplotly(puniv, tooltip = "text") %>% layout(margin = list(r = 160)) #--r = right
    
    
  })
  
  #--univ data download
  
  output$downloadData_univ <- downloadHandler(
    filename = function() {
      paste("ISUProfSals_AllDepts-", input$sel_year_univ, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(liq_univ(), file, row.names = FALSE)
    }
  )
  
  
  
  #--salary tab-------------------
  
   liq_sals <- reactive({
    sals %>%
      filter(
         dept == input$sel_dept_sals,
         salary_type_nice == input$sel_saltype_sals,
         year == input$sel_year_sals) 
  })
  
  output$fig_sals <- renderPlotly({
    
    p1 <-
      liq_sals() %>%
      ggplot(
        aes(gender, amount, group = 1,
        text = paste("Name:", name, "(", stringr::str_sub(gender, 1, 1), ")",
                     "<br>",   dept_chair2,
                     "<br>Salary: $", round(amount / 1000, digits = 0), "thou"))
        ) +
      stat_summary(fun = mean, geom = "bar", aes(fill = gender), width = 0.9) +
      geom_jitter(aes(color = dept_chair, pch = dept_chair), width = 0.2, size = 3, stroke = 1) +
      guides(fill = F,
             color = F,
             pch = F) +
      scale_y_continuous(labels = label_dollar()) +
      scale_fill_manual(values = c("Female" = femalecolor, "Male" = malecolor)) +
      scale_color_manual(values = c("N" = "black", "Y" = "red4")) +
      scale_shape_manual(values = c(21, 17)) +
      facet_grid(. ~ title_simp,
                 scales = "free_y",
                 switch = "y") +
      mytheme +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL)
    
    ggplotly(p1, tooltip = "text") %>% layout(margin = list(r = 200)) #--r = right
    
    
  })
  
  output$downloadData_sals <- downloadHandler(
    filename = function() {
      paste0("ISUProfSals_", 
           clean_punc(input$sel_dept_sals),
            "-", 
            input$sel_year_sals, 
            ".csv")
    },
    content = function(file) {
      write.csv(liq_sals(), file, row.names = FALSE)
    }
  )
 
  

   
  #--gender tab---------------
  
  liq_gend1 <- reactive({
    
    if(input$sel_college_gend == "All") {
    
    sals %>%
      filter(
        year == input$sel_year_gend) %>% 
      select(year, college2, dept, gender, name, title, title_simp) %>% 
      distinct() %>% 
      group_by(year, college2, dept, gender) %>% 
      summarise(n = n()) %>% 
      ungroup() %>% 
      pivot_wider(names_from = gender, values_from = n) %>% 
      mutate(
        Female = ifelse(is.na(Female), 0, Female),
        Male = ifelse(is.na(Male), 0, Male),
        tot = Female + Male,
        fracM = Male/tot,
        dept = fct_reorder(dept, fracM)) %>%
      arrange(dept) %>% 
      mutate(clr = ifelse(fracM > 0.5, "Male", "Female")) %>% 
        arrange(college2, fracM) %>% 
        mutate(dept = fct_inorder(dept),
               college2 = str_remove_all(college2, "College Of "))
      
    } else {
    
      sals %>%
        filter(
          college2 == input$sel_college_gend,
          year == input$sel_year_gend) %>% 
        select(year, college2, dept, gender, name, title, title_simp) %>% 
        distinct() %>% 
        group_by(year, college2, dept, gender) %>% 
        summarise(n = n()) %>% 
        ungroup() %>% 
        pivot_wider(names_from = gender, values_from = n) %>% 
        mutate(
          Female = ifelse(is.na(Female), 0, Female),
          Male = ifelse(is.na(Male), 0, Male),
          tot = Female + Male,
          fracM = Male/tot,
          dept = fct_reorder(dept, fracM)) %>%
        arrange(dept) %>% 
        mutate(clr = ifelse(fracM > 0.5, "Male", "Female")) 
      }
      
  })
  
  output$fig_gend <- renderPlotly({
    
    if(input$sel_college_gend == "All") {
      
    pgend <-
    liq_gend1() %>% 
      ggplot(aes(dept, fracM,
                 group = 1,
                 text = paste("Total Faculty:", tot,
                              "<br>Dept:", dept,
                              "<br>F:", Female,
                              "<br>M:", Male))
      ) +
      geom_hline(yintercept = 0.5, linetype = "dashed") +
      geom_segment(aes(x = dept, xend = dept, y = 0, yend = fracM)) +
      geom_point(aes(size = tot, color = clr)) +
      scale_color_manual(values = c("Female" = femalecolor, "Male" = malecolor)) +
      scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
      facet_wrap(~college2, scales = "free", nrow = 2) +
      coord_flip() +
      mytheme +
      theme(legend.position = "none",
            axis.text.y = element_blank()) +
      labs(y = "Male Faculty (% of Total)", x = NULL)
    
    ggplotly(pgend, tooltip = "text") %>% layout(margin = list(l = 160, b = 160))
    
    } else {
        pgend <-
          liq_gend1() %>% 
          ggplot(aes(dept, fracM,
                     group = 1,
                     text = paste("Total Faculty:", tot,
                                  "<br>F:", Female,
                                  "<br>M:", Male))
          ) +
          geom_hline(yintercept = 0.5, linetype = "dashed") +
          geom_segment(aes(x = dept, xend = dept, y = 0, yend = fracM)) +
          geom_point(aes(size = tot, color = clr)) +
          scale_color_manual(values = c("Female" = femalecolor, "Male" = malecolor)) +
          scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
          facet_grid(.~college2, scales = "free") +
          coord_flip() +
          mytheme +
          theme(legend.position = "none") +
          labs(y = "Male Faculty (% of Total)", x = NULL)
        
        ggplotly(pgend, tooltip = "text") %>% layout(margin = list(l = 160))
        
      }
    
  })
  
  

}

shinyApp(ui, server)
