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
                                   selected = c("globe", "globe_land", "globe_ocean"))
              ),
              mainPanel(plotOutput("plot"))
            )),     
                   
          tabPanel("Table",
            sidebarLayout(
              sidebarPanel(
                sliderInput("year_range", label = "Choose the year range",
                          min = min(data$year),
                          max = max(data$year),
                          value = c(1978, 2023))
            ),
            mainPanel(
              dataTableOutput("dataTable")
            )
      ))
    )))
    

# Define server logic required to draw a histogram.
server <- function(input, output) {

  output$dataTable <- renderDataTable({
    data
  })
  
  output$plot <- renderPlot({
    data %>%
      filter(region %in% input$regions) %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2]) %>%
      group_by(region) %>%
      ggplot(aes(year, temp, group = region, color = factor(region))) +
      geom_line() +
      geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


## to do

## make year selector actually work..........................

##Your github repo should include a readme file that contains a brief "user documentation" of
##your project: a brief description of the data, and explanation what are the widgets and panels
##doing. This file should contain a link to your project on the https://www.shinyapps.io
##server.

##When you start the app, it should show explanatory text about your dataset and the app on
##the main panel. It may be very brief. No separate sidebars and no interactivity is needed
##here.
##6. The explanatory text uses at least two html text formatting markers, such as strong(), em()
##or similar (raw html is OK too).
##See https://faculty.washington.edu/otoomet/info201-book/shiny.html#shiny-ui-text.

##Both plot and table page must also include some textual output that reacts to a widget, such
##as the number of non-missing observations, or the overall average value. For instance “Selected
##subset contains 1234 observations” is such a sentence, given 1234 is derived from the actual
##(subset of) data.

## The plot also includes a widget that allows one to change the visuals only, leaving data
##untouched.
##See https://faculty.washington.edu/otoomet/info201-book/shiny.html#shiny-server-reactive