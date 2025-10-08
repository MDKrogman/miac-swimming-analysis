library(shiny)
library(tidyverse)
library(readr)
library(ggthemes)
library(plotly)
library(stringr)

# Load data
data1 <- read_csv('../data/miac_chpoints.csv')

data2 <- data1 %>%
  filter(team != 'Concordia-Moorhead' |  # Removing schools with only women's teams
           team != "Saint Kate's") %>% 
  arrange(team, season) %>%
  group_by(team, gender) %>%
  mutate(change_points = points - lag(points)) %>%
  ungroup()

mens_changes <- data2 %>% filter(gender == 'Male')
womens_changes <- data2 %>% filter(gender == 'Female')

data2 <- mens_changes %>%
  select(season, team, change_points) %>%
  rename(men_change = change_points) %>%
  left_join(womens_changes %>%
              select(season, team, change_points) %>%
              rename(women_change = change_points), 
            by = c("season", "team")) %>%
  mutate(change_difference = men_change - women_change) %>% 
  filter(!is.na(men_change))   # Only need filter by one variable

data2$season <- factor(data2$season, levels = c('2018-2019', '2019-2020',
                                                '2021-2022', '2022-2023',
                                                '2023-2024', '2024-2025'))

miac_swim <- c(
  'Carleton' = "#2F5392",
  'Concordia-Moorhead' = "#630E37",
  'Gustavus' = "#F0C100", 
  'Hamline' = "#8A8D8C",
  'Macalester' = "#E14826",
  "Saint John's/Saint Ben's" = '#BD0F34', 
  "Saint Kate's" = '#491A6A', 
  'Saint Olaf' = '#231F20'
)

ui <- fluidPage(
  titlePanel("MIAC Swimming Power Points Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender_powerpoints", "Select Gender:", choices = unique(data1$gender), selected = "Male"),
      checkboxGroupInput("team_powerpoints", "Select Teams:", choices = unique(data1$team), selected = unique(data1$team))
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("Power Points Analysis", plotlyOutput("teamPointsPlot")),
      tabPanel("Men's/Women's Relative Power Point Change", plotlyOutput("power_points_change"))
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_teams <- reactive({
    req(input$gender_powerpoints)
    teams <- unique(data1$team)
    if (input$gender_powerpoints == "Male") {
      teams <- teams[!teams %in% c("Concordia-Moorhead", "Saint Kate's")]
    }
    return(teams)
  })
  
  observeEvent(filtered_teams(), {
    updateCheckboxGroupInput(session, "team_powerpoints", choices = filtered_teams(), selected = filtered_teams())
  })
  
  filtered_data1 <- reactive({
    req(input$team_powerpoints)
    data1 %>% filter(gender == input$gender_powerpoints, team %in% input$team_powerpoints)
  })
  
  output$teamPointsPlot <- renderPlotly({
    p <- ggplot(filtered_data1(), aes(x = season, y = points, color = team)) + 
      stat_summary(group = 1, color = 'darkgreen', fun = 'mean', geom = 'line', linewidth = .5) +
      stat_summary(group = 1, color = 'darkgreen', fun = 'mean', geom = 'point', size = 1.75) +
      geom_point() +
      geom_line(aes(group = team)) +
      scale_y_continuous(breaks = seq(100, 650, by = 100)) +
      scale_color_manual(values = miac_swim) +
      theme_clean() +
      theme(
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 7),
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        strip.text = element_text(face = "bold", size = 12)) +
      labs(x = 'Season', y = 'Power Points', color = 'Team')
    ggplotly(p, tooltip = c('team', 'points'))
  })
  

  filtered_data2 <- reactive({
    req(input$team_powerpoints)
    data2 %>% filter(team %in% input$team_powerpoints)
  })
  
  output$power_points_change <- renderPlotly({
    data_filtered2 <- filtered_data2()
    p2 <- ggplot(data_filtered2, aes(x = season, y = change_difference, color = team)) +
      geom_point() +
      geom_line(aes(group = team)) + 
      geom_hline(yintercept = 0, linetype = 'dashed') +
      geom_vline(xintercept = 2.5, linetype = 'dashed') + # Pandemic timeskip
      scale_color_manual(values = miac_swim) +
      theme_clean() + 
      labs(
        x = 'Season', y = "Relative Power Points Change Between Men's/Women's Teams",
        color = 'Team'
      )
    
    ggplotly(p2, tooltip = c('team', 'change_difference'))
  })
}

shinyApp(ui, server)
