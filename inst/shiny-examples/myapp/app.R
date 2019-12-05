library(tidyverse)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(CyChecks2)


cyd_salprofs


# clean up basic data -----------------------------------------------------

cyd_base <- 
  cyd_salprofs %>% 
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
  mutate(ndisplay = ifelse(id == "Both Genders Present", M + `F`,
                           ifelse( id == "No One", NA, 
                                   ifelse(id == "All Males", M, `F`)))) %>% 
  mutate(isone = ifelse(ndisplay == 1, "1", ">1"),
         isone = ifelse(is.na(isone), ">1", isone)) 


# make professor salary data ---------------------------------------------------


cyd_prof <- 
  cyd_base %>% 
  ungroup()


# make drop down menus ----------------------------------------------------

# drop-down menus
dd_dept <- c(sort(unique(as.character(cyd_base$dept))))
dd_col <- c(sort(unique(as.character(cyd_base$college))))
dd_year <- c(sort(unique(as.character(cyd_base$fiscal_year))))


# user interface ----------------------------------------------------------

ui <- fluidPage(
  # App Title
  titlePanel("CyChecks2"),
  # Navigation panes for dept, organzation, etc...
  
  
  navbarPage("Iowa State Salary Data",
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
                            choices = dd_year,
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
                            choices = dd_year,
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
                              plotOutput("fig_basesalbar"),
                              plotOutput("fig_nline")
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
                              cellWidths = c("100%"),
                              plotOutput("fig_basesalden")
                              #plotOutput()
                            )
                          )
                        )
                      )
                      ##########
                      ))
             )
)




# server ------------------------------------------------------------------

server <- function(input, output){
  
# gender rep --------------------------------------------------------------

#######---liq_mg---##############

liq_mg <- reactive({
    cyd_mgd %>%
      filter(college == input$mycollege,
             fiscal_year == input$myyear)
  })
  

#######---fig_mg---##############

  output$fig_mg <- renderPlot({
    
    ggplot(data = liq_mg(),
           aes(x = dept,
               y = prof_simp)) +
      
      geom_tile(aes(fill = id), color = "black", size = 1.5) +
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
        shape = NULL) +
      theme_pubclean() +
      facet_grid(~ college) + 
      
      theme(strip.text = element_text(size = rel(1.3), color = "white"),
            strip.background = element_rect(fill = "black"),
            axis.title = element_text(size = rel(1.3)),
            axis.text.x = element_text(size = rel(1.3)),
            legend.position = "right",
            legend.direction = "vertical",
            legend.background = element_rect(linetype = "solid", color = "black"))
  })
  
  
  #######---liq_pbase---##############
  
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
 
 #######---fig_basesalbar---##############
output$fig_basesalbar <- renderPlot({
  
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
      facet_grid(~prof_simp)+
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
           y = "Number of Employees",
           color = "Gender",
           title = "Number of total professors\nby gender over time") +
      scale_color_manual(values = c(M = malecolor,
                                    `F` = femalecolor)) +
      theme(
        legend.position = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.background = element_rect(linetype = "solid", color = "black"),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5))


  })
 
 #######---fig_basesalden---##############
 
 output$fig_basesalden <- renderPlot({
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
  

  # 
  # # All tab----
  # liq_all <- reactive({
  #   # Show all departments and all years
  #   if (input$department == "All departments" & input$fiscal_year == 'All years'){
  #     sals_dept %>%
  #       select("total_salary_paid", "gender", "position", "fiscal_year")
  #   }
  #   # Show all departments but filter on years
  #   else if (input$department == "All departments"){
  #     sals_dept %>%
  #       filter(fiscal_year == input$fiscal_year) %>%
  #       select("total_salary_paid", "gender", "position", "fiscal_year")
  #   }
  #   # Show all years but filter on department
  #   else if (input$fiscal_year == "All years"){
  #     sals_dept %>%
  #       filter(department == input$department) %>%
  #       select("total_salary_paid", "gender", "position", "fiscal_year")
  #   }
  #   # Filter on department and year
  #   else {
  #     sals_dept %>%
  #       filter(department == input$department,
  #              fiscal_year == input$fiscal_year) %>%
  #       select("total_salary_paid", "gender", "position", "fiscal_year")
  #   }
  # 
  # })
  # 
  # org_all <- reactive({
  #   # Show all colleges and all years
  #   if (input$organization == "All organizations" & input$fiscal_year == 'All years'){
  #     sals_dept %>%
  #       select("total_salary_paid", "gender", "position", "fiscal_year")
  #   }
  #   # Show all college but filter on years
  #   else if (input$organization == "All organizations"){
  #     sals_dept %>%
  #       filter(fiscal_year == input$fiscal_year) %>%
  #       select("total_salary_paid", "gender", "position", "fiscal_year")
  #   }
  #   # Show all years but filter on college
  #   else if (input$fiscal_year == "All years"){
  #     sals_dept %>%
  #       filter(organization == input$organization) %>%
  #       select("total_salary_paid", "gender", "position", "fiscal_year")
  #   }
  #   # Filter on college and year
  #   else {
  #     sals_dept %>%
  #       filter(organization == input$organization,
  #              fiscal_year == input$fiscal_year) %>%
  #       select("total_salary_paid", "gender", "position", "fiscal_year")
  #   }
  # 
  # })
  # 
  # 
  # liq_all_ns <- reactive({
  #   # Show all departments
  #   if (input$department == "All departments") {
  #     sals_dept %>%
  #       group_by(fiscal_year, gender) %>%
  #       summarise(n = n())
  #   } else {
  #     sals_dept %>%
  #       filter(department == input$department) %>%
  #       group_by(fiscal_year, gender) %>%
  #       summarise(n = n())
  #   }
  # })
  # 
  # org_all_ns <- reactive({
  #   # Show all colleges
  #   if (input$organization == "All organizations") {
  #     sals_dept %>%
  #       group_by(fiscal_year, gender) %>%
  #       summarise(n = n())
  #   } else {
  #     sals_dept %>%
  #       filter(organization == input$organization) %>%
  #       group_by(fiscal_year, gender) %>%
  #       summarise(n = n())
  #   }
  # })
  # # All scatter -------------------------------------------------------------
  # 
  # output$allDat1 <- renderPlot({
  #   ggplot(data = filter(liq_all(), total_salary_paid < 500000),
  #          aes(x = total_salary_paid/1000,
  #              fill = gender)) +
  #     geom_density(alpha = 0.5, color = "black") +
  #     labs(x = "Salary (in thousands of $)", y = "Density", fill = "Gender",
  #          title = "Density plot of employee salaries, by gender") +
  #     theme_bw() +
  #     scale_fill_manual(values = c(M = "darkblue",
  #                                  `F` = "goldenrod")) +
  #     theme(legend.position = c(0.99, 0.99),
  #           legend.justification = c(1, 1),
  #           legend.background = element_rect(linetype = "solid",
  #                                            color = "black"),
  #           plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  # 
  # })
  # 
  # # All scatter for colleges
  # output$allDat3 <- renderPlot({
  #   ggplot(data = filter(org_all(), total_salary_paid < 500000),
  #          aes(x = total_salary_paid/1000,
  #              fill = gender)) +
  #     geom_density(alpha = 0.5, color = "black") +
  #     labs(x = "Salary (in thousands of $)", y = "Density", fill = "Gender",
  #          title = "Density plot of employee salaries, by gender") +
  #     theme_bw() +
  #     scale_fill_manual(values = c(M = "darkblue",
  #                                  `F` = "goldenrod")) +
  #     theme(legend.position = c(0.99, 0.99),
  #           legend.justification = c(1, 1),
  #           legend.background = element_rect(linetype = "solid",
  #                                            color = "black"),
  #           plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  # 
  # })
  # 
  # # All line graph chart -----------------------------------------------------------
  # 
  # output$allDat2 <- renderPlot({
  #   # Plot for all departments, all years
  #   ggplot(data = liq_all_ns(),
  #          aes(x = fiscal_year, y = n, color = gender)) +
  #     geom_line() +
  #     geom_point(size = 2) +
  #     theme_bw() +
  #     scale_color_manual(values = c(M = "darkblue",
  #                                   `F` = "goldenrod")) +
  #     labs(x = "Fiscal Year", y = "Number of Employees", color = "Gender",
  #          title = "The number of employees over time") +
  #     theme(legend.position = c(0.01,0.99),
  #           legend.justification = c(0,1),
  #           legend.background = element_rect(linetype = "solid", color = "black"),
  #           plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  # })
  # # separate graph for colleges
  # output$allDat4 <- renderPlot({
  #   # Plot for all departments, all years
  #   ggplot(data = org_all_ns(),
  #          aes(x = fiscal_year, y = n, color = gender)) +
  #     geom_line() +
  #     geom_point(size = 2) +
  #     theme_bw() +
  #     scale_color_manual(values = c(M = "darkblue",
  #                                   `F` = "goldenrod")) +
  #     labs(x = "Fiscal Year", y = "Number of Employees", color = "Gender",
  #          title = "The number of employees over time") +
  #     theme(legend.position = c(0.01,0.99),
  #           legend.justification = c(0,1),
  #           legend.background = element_rect(linetype = "solid", color = "black"),
  #           plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  # })
  # # liq_prof ----------------------------------------------------------------
  # liq_prof <- reactive({
  # 
  #   # Show all departments and all years
  #   if (input$department == "All departments" & input$fiscal_year == 'All years'){
  #     profs
  #   }
  #   # Show all departments but filter on years
  #   else if (input$department == "All departments"){
  #     profs %>%
  #       filter(fiscal_year == input$fiscal_year)
  #   }
  #   # Show all years but filter on department
  #   else if (input$fiscal_year == "All years"){
  #     profs %>%
  #       filter(department == input$department)
  #   }
  #   # Filter on department and year
  #   else {
  #     profs %>%
  #       filter(department == input$department,
  #              fiscal_year == input$fiscal_year)
  #   }
  # 
  # })
  # # profs for colleges
  # org_prof <- reactive({
  # 
  #   # Show all college and all years
  #   if (input$organization == "All organizations" & input$fiscal_year == 'All years'){
  #     profs
  #   }
  #   # Show all college but filter on years
  #   else if (input$organization == "All organizations"){
  #     profs %>%
  #       filter(fiscal_year == input$fiscal_year)
  #   }
  #   # Show all years but filter on college
  #   else if (input$fiscal_year == "All years"){
  #     profs %>%
  #       filter(organization == input$organization)
  #   }
  #   # Filter on college and year
  #   else {
  #     profs %>%
  #       filter(organization == input$organization,
  #              fiscal_year == input$fiscal_year)
  #   }
  # 
  # })
  # 
  # liq_prof_ns <- reactive({
  #   # Show all departments
  #   if (input$department == "All departments") {
  #     profs %>%
  #       group_by(fiscal_year, gender) %>%
  #       summarise(n = n())
  #   } else {
  #     profs %>%
  #       filter(department == input$department) %>%
  #       group_by(fiscal_year, gender) %>%
  #       summarise(n = n())
  #   }
  # })
  # 
  # org_prof_ns <- reactive({
  #   # Show all departments
  #   if (input$organization == "All organizations") {
  #     profs %>%
  #       group_by(fiscal_year, gender) %>%
  #       summarise(n = n())
  #   } else {
  #     profs %>%
  #       filter(organization == input$organization) %>%
  #       group_by(fiscal_year, gender) %>%
  #       summarise(n = n())
  #   }
  # })
  # 
  # # Prof scatter + bar ------------------------------------------------------------
  # output$prof1 <- renderPlot({
  # 
  #   ggplot(data = filter(liq_prof(), prof_simp != "OTHER"),
  #          aes(x = gender,
  #              y = total_salary_paid/1000)) +
  #     geom_col(data = liq_prof() %>%
  #                filter(prof_simp != "OTHER") %>%
  #                group_by(prof_simp, gender) %>%
  #                summarise(total_salary_paid = mean(total_salary_paid)),
  #              aes(x = gender,
  #                  y = total_salary_paid/1000,
  #                  fill = gender)) +
  #     geom_point(color = "white", size = 2, pch = 21, fill = "black") +
  #     scale_fill_manual(values = c(M = "darkblue",
  #                                  `F` = "goldenrod")) +
  #     labs(x = NULL,
  #          y = "Total Salary Paid\nthousands of $",
  #          color = NULL,
  #          title = "Salaries of three professor positions, \nin Thousands of $") +
  #     theme_bw() +
  #     guides(color = F, fill = F) +
  #     facet_wrap(~prof_simp)+
  #     theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))#+
  #   #theme(legend.position = "top",
  #   #      legend.background = element_rect(linetype = "solid", color = "black"))
  # })
  # 
  # # same graph for colleges
  # output$prof3 <- renderPlot({
  # 
  #   ggplot(data = filter(org_prof(), prof_simp != "OTHER"),
  #          aes(x = gender,
  #              y = total_salary_paid/1000)) +
  #     geom_col(data = org_prof() %>%
  #                filter(prof_simp != "OTHER") %>%
  #                group_by(prof_simp, gender) %>%
  #                summarise(total_salary_paid = mean(total_salary_paid)),
  #              aes(x = gender,
  #                  y = total_salary_paid/1000,
  #                  fill = gender)) +
  #     geom_point(color = "white", size = 2, pch = 21, fill = "black") +
  #     scale_fill_manual(values = c(M = "darkblue",
  #                                  `F` = "goldenrod")) +
  #     labs(x = NULL,
  #          y = "Total Salary Paid\nthousands of $",
  #          color = NULL,
  #          title = "Salaries of three professor positions, \nin Thousands of $") +
  #     theme_bw() +
  #     guides(color = F, fill = F) +
  #     facet_wrap(~prof_simp)+
  #     theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))#+
  #   #theme(legend.position = "top",
  #   #      legend.background = element_rect(linetype = "solid", color = "black"))
  # })
  # 
  # # Prof line graph ------------------------------------------------------------
  # output$prof2 <- renderPlot({
  #   ggplot(data = liq_prof_ns(),
  #          aes(x = fiscal_year,
  #              y = n,
  #              color = gender,
  #              group = gender)) +
  #     geom_line() +
  #     geom_point(size = 2) +
  #     theme_bw() +
  #     labs(x = "Fiscal Year",
  #          y = "Number of Employees",
  #          color = "Gender",
  #          title = "The number of employees with \n'professor' titles over time") +
  #     scale_color_manual(values = c(M = "darkblue",
  #                                   `F` = "goldenrod")) +
  #     theme(
  #       legend.position = c(0.01, 0.99),
  #       legend.justification = c(0, 1),
  #       legend.background = element_rect(linetype = "solid", color = "black"),
  #       plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  # 
  # 
  # })
  # output$prof4 <- renderPlot({
  #   ggplot(data = org_prof_ns(),
  #          aes(x = fiscal_year,
  #              y = n,
  #              color = gender,
  #              group = gender)) +
  #     geom_line() +
  #     geom_point(size = 2) +
  #     theme_bw() +
  #     labs(x = "Fiscal Year",
  #          y = "Number of Employees",
  #          color = "Gender",
  #          title = "The number of employees with \n'professor' titles over time") +
  #     scale_color_manual(values = c(M = "darkblue",
  #                                   `F` = "goldenrod")) +
  #     theme(
  #       legend.position = c(0.01, 0.99),
  #       legend.justification = c(0, 1),
  #       legend.background = element_rect(linetype = "solid", color = "black"),
  #       plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  # 
  # 
  # })


