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
      
      # varSelectInput("var_name", "Variable of interest", cohort),
      textInput("var_name_s", "Variable of interest", 'first_careunit'),
      
      
    ),
    
    mainPanel(
      plotOutput("plot")
      )
  )
)

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({
    cohort %>% 
      select(input$var_name_s)
  })
  
  # output$plot <- renderPlot({
  #   cohort %>% 
  #     ggplot() +
  #     geom_bar(aes(x = !!input$var_name))
  #   
  # })
  
  output$plot <- renderPlot({
    dataInput() %>% 
      ggplot(aes_string(x = input$var_name_s)) +
      geom_bar()
    
  })
  
  output$summary <- renderTable({
    dataInput() %>% 
      group_by(input$var_name_s)
  })
  
}

# Run the app
shinyApp(ui, server)
