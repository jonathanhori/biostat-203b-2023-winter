# Load packages ----
library(shiny)
library(tidyverse)
library(ggplot2)
library(quantmod)

# Source helpers ----
# source("helpers.R")


# ## To make
# 1. Histogram by variable (demographics, icu unit, etc)
# - split by other variables, split by thirty day mortality
# - grouped by another variable
# 2. Summary statistics of numerical variables
# 2. Distplots by mortality 
cohort <- readRDS("./data/icu_cohort.rds")
getwd()

# User interface ----
ui <- fluidPage(
  titlePanel("MIMIC-IV Cohort EDA"),
   
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
      # textInput("var_name", "Variable of interest", 'first_careunit'),
      
      
    ),
    
    
    
    mainPanel(
      plotOutput("plot"),
      tableOutput("summary")
      )
  ),
  
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
                    "Systolic blood pressure" = "non_invasive_blood_pressure_systolic",
                    "Average blood pressure" = "non_invasive_blood_pressure_mean",
                    "Respiratory rate" = "respiratory_rate",
                    "Body temperature in Fahrenheit" = "temperature_fahrenhe"
                  )
                  )),
      # textInput("var_name", "Variable of interest", 'first_careunit'),
      
      
    ),
    
    
    mainPanel(
      # plotOutput("plot"),
      # tableOutput("summary")
    )
  )
  
)





# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({
    cohort %>% 
      select(input$var_name)
  })
  
  # output$plot <- renderPlot({
  #   cohort %>% 
  #     ggplot() +
  #     geom_bar(aes(x = !!input$var_name))
  #   
  # })
  
  output$plot <- renderPlot({
    dataInput() %>% 
      ggplot(aes_string(x = input$var_name)) +
      geom_bar()
    
  })
  
  output$summary <- renderTable({
    dataInput() %>% 
      # group_by(input$var_name) %>% 
      summarise(count = n(),
                average = mean(!!input$var_name, na.rm = TRUE))
  })
  
}

# Run the app
shinyApp(ui, server)
