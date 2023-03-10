library(shiny)
library(tidyverse)
library(ggplot2)


data <- read_delim("nces330_20.csv")

# ui 
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
                   checkboxGroupInput("Expense", label = "Choose expense",
                                      choices = c("Fees/Tuition", "Room/Board"),
                                      selected = c("Fees/Tuition", "Room/Board")),
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
                               value = c(2013, 2021)),
                   textOutput("year_text"),
                 ),
                 mainPanel(
                   dataTableOutput("dataTable")
                 )
               ))
    )))


# server logic!
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
      filter(Expense %in% input$Expense) %>%
      group_by(Expense) %>%
      ggplot(aes(Year, Value, group = Expense, col = factor(Expense))) +
      ggtitle("Average Cost of Undergrad College")+
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
