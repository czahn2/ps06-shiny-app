#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)


data <- read_delim("UAH-lower-troposphere-long.csv.bz2")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("PS06-shiny"),
    
    
      mainPanel(
        tabsetPanel(
          tabPanel("About", "This page includes a", strong("Plot"), "panel and", em("Table"), "panel
                   for the global temperature data."),
          tabPanel("Plot", 
            sidebarLayout(
              sidebarPanel(
                checkboxGroupInput("regions", label = "Choose regions",
                                   choices = c("globe", "globe_land", "globe_ocean"),
                                   selected = c("globe", "globe_land", "globe_ocean")),
                radioButtons("color", "Select a color:",
                           choices =   c("red","blue","pink"),
                           selected = "red"),
                textOutput("color_text"),
              ),
              mainPanel(plotOutput("plot"))
            )),     
                   
          tabPanel("Table",
            sidebarLayout(
              sidebarPanel(
                sliderInput("year_range", label = "Choose the year range",
                          min = min(data$year),
                          max = max(data$year),
                          value = c(1978, 2023)),
                textOutput("year_text"),
            ),
            mainPanel(
              dataTableOutput("dataTable")
            )
      ))
    )))
    

# Define server logic required to draw a histogram.
server <- function(input, output) {

  output$dataTable <- renderDataTable({
    data %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2])
  })
  
  
  output$plot <- renderPlot({
    color <- switch(input$color, 
                    red = "red",
                    blue = "blue",
                    pink = "pink")
    data %>%
      filter(region %in% input$regions) %>%
      group_by(region) %>%
      ggplot(aes(year, temp, group = region, col = factor(region))) +
      ggtitle("Global Temperature Data")+
      scale_fill_manual(values = color) +
      geom_line(color = input$color) +
      geom_point(color = input$color)
  })
  output$color_text <- renderText({
    paste("You chose: ", input$color)
  })
  output$year_text <- renderText({
    paste("You chose:", input$year_range[1],"-", input$year_range[2])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


## to do

## make year selector actually work..........................

## publish to shiny, put link in readme

