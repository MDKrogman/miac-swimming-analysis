library(shiny)
library(tidyverse)
library(plotly)
library(ggthemes)
library(showtext)

# Data for rq 2
data <- read_csv('../data/full_miac_points.csv') 

data <- data %>%
  filter(season != "2024-2025pm")

data$team <- recode(data$team,
                    'SJU/CSB' = "Saint John's/Saint Ben's",
                    "Saint. Kate's" = "Saint Kate's",
                    "St. Kate's" = "Saint Kate's",
                    'St. Olaf' = 'Saint Olaf')

data$class_yr <- fct_relevel(data$class_yr, 'FR', 'SO', 'JR', 'SR', '5Y')

miac_swim <- c('Carleton' = "#2F5392",
               'Concordia-Moorhead' = "#630E37",
               'Gustavus' = "#F0C100", 
               'Hamline' = "#8A8D8C",
               'Macalester' = "#E14826",
               "Saint John's/Saint Ben's" = '#BD0F34', 
               "Saint Kate's" = '#491A6A', 
               'Saint Olaf' = '#231F20')

# UI
iu <- fluidPage(
  titlePanel("Impact of Fifth-Year Swimmers on Team Standings"),
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Select a Season:",
                  choices = unique(data$season),
                  selected = "2024-2025")
    ),
    mainPanel(
      plotlyOutput("fifthyrPlot")
    )
  )
)

# Server
server <- function(input, output) {
  output$fifthyrPlot <- renderPlotly({
    filtered_data <- data %>% filter(season == input$season)
    
    plot <- ggplot(filtered_data, aes(x = class_yr, y = points, color = team)) +
      geom_jitter(width = 0.35, height = 0.35, alpha = 0.75) +
      scale_color_manual(values = miac_swim) +
      theme_clean() +
      labs(x = 'Class Year', y = 'Power Points', color = 'Team')
    
    ggplotly(plot, tooltip = c('points', 'team'))
  })
}

# Run the application
shinyApp(ui = iu, server = server)
