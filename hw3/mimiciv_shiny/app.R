# Load packages ----
library(shiny)
library(tidyverse)
library(ggplot2)
library(lubridate)

cohort <- readRDS("./data/icu_cohort.rds")
getwd()

# User interface ----
ui <- fluidPage(
  titlePanel("MIMIC-IV Cohort EDA"),
  h3("Find summary statistics, visuals, and distributions for 
              our MIMIC cohort"),
  
  ##################
  # Cohort variables
  h4("Patient or stay characteristics"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select a cohort variable to examine."),
      
      selectInput("var_name", "Variable of interest", 
                  list(Stay = c(
                    "First care unit" = "first_careunit",
                    "Last care unit" = "last_careunit",
                    "Admission type" = "admission_type",
                    "Admission location" = "admission_location",
                    "Discharge location" = "discharge_location"
                  ),
                  Patient = c(
                    "Gender" = "gender",
                    "Age" = "anchor_age",
                    "Ethnicity" = "ethnicity",
                    "Martial status" = "marital_status",
                    "Language" = "language",
                    "Insurance" = "insurance"
                  )
                  )),
      
      checkboxInput("mortality_split_cohort", 
                    "Split bar chart by 30 day mortality status?", 
                    FALSE)
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Bar chart by group", 
                           plotOutput("cohort_plot")),
                  tabPanel("Count by group over time", 
                           plotOutput("line_plot")),
                  tabPanel("Mortality rate by group", 
                           tableOutput("cohort_summary"))
      )
    )
  ),
  
  ##################
  # Vitals/Lab measurements
  h4("Vital measurements or lab results"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select a lab test or vital measurement to examine."),
      
      selectInput("vital_lab_name", "Variable of interest", 
                  list(Labs = c(
                    "Bicarbonate" = "bicarbonate",
                    "Chloride" = "chloride",
                    "Creatinine" = "creatinine",
                    "Glucose" = "glucose",
                    "Potassium" = "potassium",
                    "Sodium" = "sodium",
                    "Hemocrit" = "hemocrit",
                    "White blood cell count" = "white_blood_cells"
                  ),
                  Vitals = c(
                    "Heart rate" = "heart_rate",
                    "Systolic blood pressure" = 
                      "non_invasive_blood_pressure_systolic",
                    "Average blood pressure" = 
                      "non_invasive_blood_pressure_mean",
                    "Respiratory rate" = "respiratory_rate",
                    "Body temperature in Fahrenheit" = "temperature_fahrenheit"
                  )
                  )),
      
      checkboxInput("mortality_split", 
                    "Split by 30 day mortality status?", 
                    FALSE)
      
      
    ),
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Histogram", 
                           plotOutput("hist_plot")),
                  tabPanel("Summary statistics", 
                           tableOutput("vital_summary"))
    )
  )
  )
  
)





# Server logic
server <- function(input, output) {
  #####################
  # Data
  cohort_data_input <- reactive({
    cohort %>% 
      select(input$var_name, thirty_day_mort) %>% 
      drop_na() 
    
  })
  
  
  count_data_input <- reactive({
    cohort %>% 
      select(admittime, input$var_name) %>% 
      mutate(admit_date = as_date(admittime)) %>% 
      group_by(admit_date, across(input$var_name)) %>%
      summarise(count = n())
  })
  
  
  vital_data_input <- reactive({
    if (input$mortality_split) {
      cohort %>% 
        select(input$vital_lab_name, thirty_day_mort) %>%
        drop_na() %>% 
        group_by(thirty_day_mort)
    } else {
      cohort %>% 
        select(input$vital_lab_name) %>% 
        drop_na()
    }
  })
  
  #####################
  # Cohort summary
  output$cohort_plot <- renderPlot({
    p <- cohort_data_input() %>% 
      ggplot(aes(y = get(input$var_name))) +
      labs(
        y = input$var_name,
        fill = "Thirty day mortality status",
        title = "Patient count by stay or patient variable group"
      )
    
    if (input$mortality_split_cohort) {
      p + geom_bar(aes(fill = as.factor(thirty_day_mort)))
    } else {
      p + geom_bar()
    }
    
    
  })
  
  output$cohort_summary <- renderTable({
    cohort_data_input() %>% 
      group_by_(input$var_name) %>% 
      summarise(count = n(),
                mortality_rate = mean(thirty_day_mort)
                )
  })
  
  #####################
  # Vital + lab summary
  output$hist_plot <- renderPlot({
    p <- vital_data_input() %>% 
      ggplot(aes_string(x = input$vital_lab_name)) +
      geom_histogram() + 
      labs(
        title = "Lab test or vital measurement distribution"
      )
    
    if (input$mortality_split) {
      p + facet_wrap(~ thirty_day_mort)
    } else {
      p
    }
  })
  
  output$vital_summary <- renderTable({
    vital_data_input() %>% 
      summarise(count = n(),
                average = mean(get(input$vital_lab_name)),
                median = median(get(input$vital_lab_name)),
                std = sd(get(input$vital_lab_name)),
                min = min(get(input$vital_lab_name)),
                max = max(get(input$vital_lab_name)),
                )
  })
  
  
  # output$test <- renderTable({count_data_input() %>%  head})
  
  
  #####################
  # Counts over time
  output$line_plot <- renderPlot({
    count_data_input() %>%
      ggplot(aes_string(x = "admit_date", color = input$var_name)) +
      geom_freqpoly() +
      labs(
        x = "Admission date",
        y = "Count",
        title = "Admssion count over time"
      )
  })
}

# Run the app
shinyApp(ui, server)
