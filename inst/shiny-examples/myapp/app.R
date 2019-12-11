library(tidyverse)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(CyChecks2)

# problem: eeob is not assigned a college
cyd_salprofs %>% 
  filter(dept == "eeob") %>% 
  select(dept, college)

# clean up basic data -----------------------------------------------------

cyd_base <- 
  cyd_salprofs %>% 
  # filter(fiscal_year > 2011) %>%  #--we don't have dept data before then; not true! I filled that in....(LE)
  # making NA in college into misc category
  mutate(college = replace_na(college, "college of combos")) %>%
  #Just keep 'colleges', not the weird things 
  filter(grepl("college", college)) %>% 
  # who gets paid 0? eliminate them
  filter(base_salary > 0) %>% 
  # get rid of centers and centers
  filter(!grepl('ctr', dept)) %>%
  filter(!grepl('center', dept)) %>% 
  mutate_if(is.character, str_to_title) %>% 
  mutate(prof_simp = factor(prof_simp,
                            levels = c("Asst Prof", "Assoc Prof", "Prof", "Awarded Prof"))) %>% 
  select(fiscal_year, college, dept, gender, prof_simp, total_salary_paid, base_salary)

malecolor <- "deepskyblue3"
femalecolor <- "goldenrod"

# create data for missing genders tab (cyd_mgd) -------------------------------------------------------

mgdraw <- 
  cyd_base %>% 
  # keep fiscal year
  group_by(fiscal_year, college, dept, prof_simp, gender) %>% 
  summarise(n = n()) %>% 
  spread(gender, value = n) %>% 
  ungroup()

#--need to get every combo so things are explicitly missing 
yrs <- mgdraw %>% pull(fiscal_year) %>% unique()
dept <- mgdraw %>% pull(dept) %>% unique()
prof_simp <- mgdraw %>% pull(prof_simp) %>% unique()
combs <- 
  expand_grid(yrs, dept, prof_simp) %>% 
  rename("fiscal_year" = 1)

coldept <- mgdraw %>% 
  select(fiscal_year, college, dept) %>% 
  distinct()

coldept2 <- mgdraw %>% 
  select(college, dept) %>% 
  distinct()

#--make data for shiny
cyd_mgd <- 
  # Get all combinations of prof/depts
  combs %>% 
  left_join(mgdraw) %>% 
  select(-college) %>% 
  # assign college
  left_join(coldept2) %>% 
  # assign gender verdict
  mutate(id = ifelse((is.na(`F`) & is.na(M)), "No One",
                     ifelse(is.na(`F`), "All Males",
                            ifelse(is.na(M), "All Females", "Both Genders Present")))) %>% 
  mutate(ndisplay = ifelse(id == "Both Genders Present", NA,
                           ifelse( id == "No One", NA, 
                                   ifelse(id == "All Males", M, `F`)))) %>% 
  mutate(isone = ifelse(ndisplay == 1, "1", ">1"),
         isone = ifelse(is.na(isone), ">1", isone)) 



# create data for ratios fig (cyd_rat) ------------------------------------

ratraw <- 
  cyd_base %>%
  group_by(fiscal_year, college, dept, gender, prof_simp) %>% 
  summarise(mean_base_salary = mean(base_salary)) %>%
  spread(gender, mean_base_salary) %>% 
  mutate(rat = log(M / `F`)) %>% 
  filter(!is.na(rat)) %>% 
  ungroup()
  


# Get all combinations of prof/depts
cyd_rat <- 
  combs %>% 
  left_join(ratraw) %>% 
  select(-college) %>% 
  # assign college
  left_join(coldept2) %>% 
  # assign colors
  mutate(id = ifelse(rat > 0, "Males Make More", "Females Make More"))

# make professor salary data ---------------------------------------------------


cyd_prof <- 
  combs %>% 
  left_join(cyd_base) %>% 
  select(-college) %>% 
  # assign college
  left_join(coldept2) 
  



# about text -------------------------------------------------------------
abouttext <- "This data is a combination of publicly-available data (PD; linked names to salaries)\n, and ISU-provided data (ISUD; linked names to departments). \nThe reliance on linking these data presented problems. \nSome people appeared in the PD but not in the ISUD, and are therefore omitted from the final dataset. \nOther faculty were linked to centers rather than their tenure-home departments. \nAs of Dec 6 2019 those faculty are also not included."

# make drop down menus ----------------------------------------------------

# drop-down menus
dd_dept <- c(sort(unique(as.character(cyd_base$dept))))
dd_col <- c(sort(unique(as.character(cyd_base$college))))
dd_year1 <- c(sort(unique(as.character(cyd_base$fiscal_year))))
dd_year2 <- c(sort(unique(as.character(cyd_base$fiscal_year))))

# user interface ----------------------------------------------------------

ui <- fluidPage(
  # App Title
  titlePanel("CyChecks2"),
  # Navigation panes for dept, organzation, etc...
  
  
  navbarPage("Iowa State Salary Data",
             
             tabPanel("Base Salaries",
                      mainPanel(
                        
                        ####
                        fluidRow(
                        column(
                          width = 6,
                          selectInput(
                            "mydept",
                            label = ("Department"),
                            # - Based on gender
                            choices = dd_dept,
                            selected = "Agronomy"
                          )
                        ),
                        column(
                          width = 6,
                          selectInput(
                            "myyear",
                            label = ("Year"),
                            # - Based on gender
                            choices = dd_year1,
                            selected = "2018"
                          )
                        )
                      ),
                      #######
                      

                      #######
                      fluidRow(
                        column(
                          width = 12,
                          align = "center",
                          #h3(em("Professors")),
                          fluidRow(
                            splitLayout(
                              cellWidths = c("50%", "50%"),
                              plotOutput("fig_bsden"),
                              plotOutput("fig_bsbar")
                            )
                            )
                          )
                        ),
                      ##########
                      br(),
                      #######
                      fluidRow(
                        column(
                          width = 12,
                          align = "center",
                          #h3(em("Professors")),
                          fluidRow(
                            splitLayout(
                              cellWidths = c("50%", "50%"),
                              plotOutput("fig_nline"),
                              plotOutput("fig_rat")
                            )
                          )
                        )
                      )
                      ##########
                      )),
             
             tabPanel("Gender Representation",
                      mainPanel(
                        fluidRow(column(
                          width = 6,
                          selectInput(
                            "mycollege",
                            label = ("College"),
                            # - Based on gender
                            choices = dd_col,
                            selected = "College Of Agriculture & Life Sciences"
                          )
                        ),
                        column(
                          width = 6,
                          selectInput(
                            "myyear",
                            label = ("Year"),
                            # - Based on gender
                            choices = dd_year2,
                            selected = "2018"
                          )
                        )),
                        
                        fluidRow(column(
                          width = 12,
                          align = "center",
                          #h3(em("Tenure-Track Faculty Positions")),
                          fluidRow(plotOutput("fig_mg"))
                        ))
                      )),
             tabPanel("About",
                      mainPanel(fluidRow(column(
                          width = 12,
                          align = "center",
                          h1(em(abouttext))
                        ))
                      ))
             
             
             )
)




# server ------------------------------------------------------------------

server <- function(input, output){
  
# gender rep --------------------------------------------------------------

#######---liq_mg---##############

liq_mg <- reactive({
    cyd_mgd %>%
    #filter(fiscal_year == 2018) %>% 
      filter(college == input$mycollege,
             fiscal_year == input$myyear
             )
  })
  

#######---fig_mg---##############

  output$fig_mg <- renderPlot({
    
    ggplot(data = liq_mg(),
           aes(x = dept,
               y = prof_simp)) +
      
      geom_tile(aes(fill = id), color = "black", size = 1) +
      geom_point(aes(size = ndisplay, pch = isone)) +
      scale_fill_manual(values = c(`All Females` = femalecolor,
                                   `All Males` = malecolor, 
                                   `Both Genders Present` = "gray90",
                                   `No One` = "gray10")) +
      scale_shape_manual(values = c(19, 0)) +
      coord_flip() +
      labs(
        x = NULL,
        y = NULL,
        fill = NULL, 
        size = "Number of Faculty",
        shape = NULL, 
        title = "Lack of gender representation in 2018\n(drop down doesn't work)") +
      theme_pubclean() +
      facet_grid(~ college) + 
      
      theme(strip.text = element_text(size = rel(1.3), color = "black"),
            strip.background = element_rect(fill = "white"),
            axis.title = element_text(size = rel(1.3)),
            axis.text.x = element_text(size = rel(1.3)),
            legend.position = "right",
            legend.direction = "vertical",
            legend.background = element_rect(linetype = "solid", color = "black")) + 
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  })
  
  
  #######---liq_pbase---##############
  
  liq_rat <- reactive({
    cyd_rat %>%
      filter(dept == input$mydept,
             fiscal_year == input$myyear)
  })
  
  liq_prof <- reactive({
    cyd_prof %>%
      filter(dept == input$mydept,
             fiscal_year == input$myyear)
  })
  
 liq_profns <- reactive({
   cyd_prof %>% 
   filter(dept == input$mydept) %>%
     group_by(fiscal_year, gender) %>%
     summarise(n = n())
   }) 
 
 
 #######---fig_rat---##############
 output$fig_rat <- renderPlot({
   
   ggplot(data = liq_rat(),
          aes(x = prof_simp,
              y = rat)) +
     geom_col(aes(fill = id), size = 5) +
     scale_fill_manual(values = c("Males Make More" = malecolor,
                                  "Females Make More" = femalecolor)) +
     labs(x = NULL,
          y = "Log (Male Salary / Female Salary",
          color = NULL,
          title = "Salary ratios by position in a given year") +
     coord_cartesian(y = c(-0.7, 0.7)) +
     geom_hline(yintercept = 0, color = "red") +
     
     geom_hline(yintercept = 0.095, color = malecolor, linetype = "dashed") +
     geom_hline(yintercept = -0.095, color = femalecolor, linetype = "dashed") +
     theme_bw() +
     guides(color = F, fill = F) +
     theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
 })
 
 #######---fig_bsbar---##############
output$fig_bsbar <- renderPlot({
  
     ggplot(data = liq_prof(),
           aes(x = gender,
               y = base_salary/1000)) +
      geom_col(data = liq_prof() %>%
                 group_by(prof_simp, gender) %>%
                 summarise(base_salary = mean(base_salary)),
               aes(x = gender,
                   y = base_salary/1000,
                   fill = gender)) +
       geom_point(color = "white", size = 2, pch = 21, fill = "black") +
       scale_fill_manual(values = c(M = malecolor,
                                    `F` = femalecolor)) +
      labs(x = NULL,
           y = "Base Salary Paid\nthousands of $",
           color = NULL,
           title = "Base salaries by position and gender") +
      theme_bw() +
      guides(color = F, fill = F) +
      facet_grid(~prof_simp) +
      theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
})
  
 
 #######---fig_nline---############## 
  
output$fig_nline <- renderPlot({
     
  ggplot(data = liq_profns(),
           aes(x = fiscal_year,
               y = n,
               color = gender,
               group = gender)) +
      geom_line() +
      geom_point(size = 2) +
      theme_bw() +
      labs(x = "Fiscal Year",
           y = "Number",
           color = "Gender",
           title = "Number of total tenure-track faculty\nby gender over time") +
      scale_color_manual(values = c(M = malecolor,
                                    `F` = femalecolor)) +
    geom_hline(yintercept = 0, color = "gray70") +
      theme(
        legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(linetype = "solid", color = "black"),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5))


  })
 
 #######---fig_bsden---##############
 
 output$fig_bsden <- renderPlot({
    ggplot(data = liq_prof(),
           aes(x = base_salary/1000,
               fill = gender)) +
      geom_density(alpha = 0.5, color = "black") +
      labs(x = "Salary (in thousands of $)", y = "Density", fill = "Gender",
           title = "Density plot of professor salaries by gender") +
      theme_bw() +
      scale_fill_manual(values = c(M = malecolor,
                                  `F` = femalecolor)) +
     theme(legend.position = c(0.99, 0.99),
           legend.justification = c(1, 1),
           legend.background = element_rect(linetype = "solid",
                                            color = "black"),
           plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

 })
 
 
 
 

 
  
  
}

shinyApp(ui, server)
  
