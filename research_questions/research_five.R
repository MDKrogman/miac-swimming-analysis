library(tidyverse)
library(patchwork)
library(ggthemes)
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(scales)
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)


full_miac_points <- read_csv('../data/full_miac_points.csv') %>%
  filter(season != "2024-2025pm") %>%
  mutate(points = as.numeric(ifelse(points == "–", NA, points)))

miac_points <- read_csv('../data/miac_chpoints.csv') %>%
  mutate(points = as.numeric(ifelse(points == "–", NA, points)))


team_name_corrections <- c(
  'SJU/CSB' = "Saint John's/Saint Ben's",
  "Saint. Kate's" = "Saint Kate's",
  "St. Kate's" = "Saint Kate's",
  'St. Olaf' = 'Saint Olaf'
)

full_miac_points$team <- recode(full_miac_points$team, !!!team_name_corrections)
miac_points$team <- recode(miac_points$team, !!!team_name_corrections)


# Bootstrapping only once to add more variability
bootstrap_team <- function(obs) {
  boots <- 1
  stats <- numeric(boots)
  for (i in 1:boots) {
    team <- sample(obs, size = 15, replace = TRUE)
    stats[i] <- mean(team, na.rm = TRUE)
  }
  stats
}


ui <- fluidPage(
  titlePanel("MIAC Random Team Bootstrap"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_season", "Select Season", choices = unique(full_miac_points$season)),
      selectInput("selected_gender", "Select Gender", choices = c("Male", "Female")),  
      textInput("custom_team_name", "Enter Your Team Name", "Ethan's Excellent Team"),
      actionButton("generate", "Generate Team")
    ),
    
    mainPanel(
      h3("MIAC Standings(With Bootstrap Team)"),
      plotOutput("team_comparison")
    )
  )
)


server <- function(input, output) {
  
  # Reactive function to filter swimmers by season & gender
  selected_swimmers <- reactive({
    full_miac_points %>%
      filter(season == input$selected_season, gender == input$selected_gender) %>%
      pull(points) %>%
      na.omit()
  })
  
  # Bootstrap
  bootstrap_results <- reactive({
    req(input$generate)
    bootstrap_team(selected_swimmers())
  })
  
  # Avg points for bootstrap team
  team_avg_points <- reactive({
    mean(bootstrap_results(), na.rm = TRUE)
  })
  
  # Plot
  output$team_comparison <- renderPlot({
    req(input$generate)  
    
    miac_season_data <- miac_points %>%
      filter(season == input$selected_season, gender == input$selected_gender) %>%
      select(team, points)
    
    random_avg <- team_avg_points()
    
    custom_team_data <- data.frame(
      team = input$custom_team_name,
      points = random_avg
    )
    
    full_data <- bind_rows(miac_season_data, custom_team_data) %>%
      arrange(desc(points))  
    
    ggplot(full_data, aes(x = reorder(team, -points), y = points, fill = team == input$custom_team_name)) +
      geom_bar(stat = "identity", show.legend = FALSE, color = "black") +
      scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "green")) +  
      labs(title = paste("MIAC Team Comparison -", input$selected_season),
           x = "Teams",
           y = "Average Points") +
      theme_clean() +
      coord_flip()
  })
}


shinyApp(ui = ui, server = server)


