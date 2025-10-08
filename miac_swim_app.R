# Load packages --------------------------------------------------------------
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggthemes)
library(lubridate)
library(readr)
library(scales)
library(plotly)
# Load and prep data ---------------------------------------------------------

# Standardize team names
team_name_corrections <- c(
  'SJU/CSB' = "Saint John's/Saint Ben's",
  "Saint. Kate's" = "Saint Kate's",
  "St. Kate's" = "Saint Kate's",
  'St. Olaf' = 'Saint Olaf'
)

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

# Load taper data
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

# Data for rq1
pre <- read_csv('data/miac_points_2425PREMIAC.csv') %>%
  mutate(points = as.double(ifelse(points == "–", NA, points)))
pre$team <- recode(pre$team, !!!team_name_corrections)

post <- read_csv('data/miac_points_2425_2.csv') %>%
  mutate(points = as.double(ifelse(points == "–", NA, points)))
post$team <- recode(post$team, !!!team_name_corrections)

combined <- left_join(pre, post, by = c("swimmer", "team", "gender", "class_yr", "season")) %>% 
  filter(!is.na(points.x)) %>%
  mutate(points_diff = points.y - points.x) %>%
  filter(points_diff != 0)

# Data for rq2
data <- read_csv('data/full_miac_points.csv') 

data <- data %>%
  filter(!season %in% c("2017-2018", "2018-2019", "2019-2020", "2024-2025pm"))

data$team <- recode(data$team, !!!team_name_corrections)

data$class_yr <- fct_relevel(data$class_yr, 'FR', 'SO', 'JR', 'SR', '5Y')

# Data for rq3
pre_covid <- read_csv('data/miac_points_1920.csv') %>%
  mutate(points = as.double(ifelse(points == "–", NA, points))) %>%
  mutate(season = "Pre-COVID") %>%
  mutate(team = recode(team, !!!team_name_corrections))

post_covid <- read_csv('data/miac_points_2122.csv') %>%
  mutate(points = as.double(ifelse(points == "–", NA, points))) %>%
  mutate(season = "Post-COVID") %>%
  mutate(team = recode(team, !!!team_name_corrections))

covid_swim <- left_join(pre_covid, post_covid, by = c("swimmer", "team", "gender")) %>%
  filter(!is.na(points.x)) %>%
  filter(!is.na(points.y))

total <- bind_rows(pre_covid, post_covid) %>% semi_join(covid_swim, by = "swimmer")


# Data for rq4
data1 <- read_csv('data/miac_chpoints.csv')

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

# Data for rq5
full_miac_points <- read_csv('data/full_miac_points.csv') %>%
  filter(season != "2024-2025pm") %>%
  mutate(points = as.numeric(ifelse(points == "–", NA, points)))

miac_points <- read_csv('data/miac_chpoints.csv') %>%
  mutate(points = as.numeric(ifelse(points == "–", NA, points)))

full_miac_points$team <- recode(full_miac_points$team, !!!team_name_corrections)
miac_points$team <- recode(miac_points$team, !!!team_name_corrections)

# Data for rq6

miac_timedrops <- read_csv("data/miac_timedrops.csv", 
                           col_types = cols(events = readr::col_factor(levels = c("100 Fly", 
                                                                           "200 Fly", "100 Back", "200 Back", 
                                                                           "100 Breast", "200 Breast", "50 Free", 
                                                                           "100 Free", "200 Free", "500 Free", 
                                                                           "1650 Free", "200 IM", "400 IM"))))

convert_to_seconds <- function(time) {
  if (str_detect(time, ':')) {
    mmsshh <- strsplit(time, split = '[:.]')[[1]]
    minutes <- as.numeric(mmsshh[1])  
    seconds <- as.numeric(mmsshh[2])  
    hundredths <- as.numeric(mmsshh[3])  
    total_seconds <- minutes * 60 + seconds + hundredths / 100
  } else {
    mmsshh <- strsplit(time, split = '\\.')[[1]]  
    seconds <- as.numeric(mmsshh[1])  
    hundredths <- as.numeric(mmsshh[2])  
    total_seconds <- seconds + hundredths / 100
  }
}

miac_timedrops$times <- map(miac_timedrops$times, convert_to_seconds)
miac_timedrops$times2 <- map(miac_timedrops$times2, convert_to_seconds)

miac_timedrops <- miac_timedrops %>% 
  mutate(time_drop = as.numeric(times2) - as.numeric(times))

miac_timedrops$gender <- recode(miac_timedrops$gender, 'female' = 'Female')

# Define UI ------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("MIAC Swimming Analysis - Matthew Krogman & Ethan Chan"),
  
  # Tab for rq1
  tabsetPanel(
    tabPanel("Top 10 Tapers",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("selected_teams", "Select Teams", 
                                    choices = unique(combined$team),
                                    selected = unique(combined$team)),
                 selectInput("taper_type", "Select Taper Type", 
                             choices = c("Top 10 Tapers" = "top", "Bottom 10 Tapers" = "worst"),
                             selected = "top")
               ),
               mainPanel(
                 plotOutput("taper_plot"),
                 textOutput("rq1_text")
               )
             )
    ), 
    # Tab for rq2
    tabPanel("Impact of Fifth-Year Swimmers on Team Standings",
             sidebarLayout(
               sidebarPanel(
                 selectInput("season", "Select a Season:",
                             choices = unique(data$season),
                             selected = "2024-2025")
               ),
               mainPanel(
                 plotlyOutput("fifthyrPlot"),
                 textOutput("rq2_text")
               )
             )
    ),
    # Tab for rq3
    tabPanel("Impact of COVID on Swimmer Performance",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("covid_teams", "Select Teams", 
                                    choices = unique(total$team),
                                    selected = unique(total$team))
               ),
               mainPanel(
                 plotOutput("covid_boxplot"),
                 textOutput("rq3_text")
               )
             )
    ),
    # Tab for rq4
    tabPanel("Team Power Points Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("gender", "Select Gender:", choices = unique(data1$gender), selected = "Male"),
                 checkboxGroupInput("team_powerpoints", "Select Teams:", choices = unique(data1$team), selected = unique(data1$team))
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Power Points Analysis", plotlyOutput("teamPointsPlot")),
                   tabPanel("Men's/Women's Relative Power Point Change", plotlyOutput("power_points_change"))
                 ),
                 textOutput("rq4_text")
               )
             )
    ),
    # Tab for rq5
    tabPanel("Random Team Bootstrap",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_season", "Select Season", choices = unique(full_miac_points$season)),
                 selectInput("selected_gender", "Select Gender", choices = c("Male", "Female")),  
                 textInput("custom_team_name", "Enter Your Team Name", "Ethan's Excellent Team"),
                 actionButton("generate", "Generate Random Team")
               ),
               mainPanel(
                 h3("Team Performance Comparison"),
                 plotOutput("team_comparison"),
                 textOutput("rq5_text")
               )
             )
    ),
    # Tab for rq6
    tabPanel("Time Drops Analysis",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("teams", "Select Teams:", 
                                    choices = unique(miac_timedrops$team), 
                                    selected = unique(miac_timedrops$team)),
                 selectInput("event", "Select Event:", 
                             choices = unique(miac_timedrops$events), 
                             selected = unique(miac_timedrops$events)[1])
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Event Specialization", plotlyOutput("event_plot")),
                   tabPanel("Time Drops", plotlyOutput("timedrops_plot"))
                 ),
                 textOutput("rq6_text")
               )
             )
    )
  )
)

# Define Server -------------------------------------------------------------
server <- function(input, output) {
  
  # RQ1 Plot
  output$taper_plot <- renderPlot({
    filtered_data <- combined %>% filter(team %in% input$selected_teams)
    
    if (input$taper_type == "top") {
      taper_data <- filtered_data %>% 
        group_by(team) %>% 
        slice_max(order_by = points_diff, n = 10) %>% 
        ungroup()
      plot_title <- "Top 10 Tapers in the MIAC by Team"
    } else {
      taper_data <- filtered_data %>% 
        group_by(team) %>% 
        slice_min(order_by = points_diff, n = 10) %>% 
        ungroup()
      plot_title <- "Bottom 10 Tapers in the MIAC by Team"
    }
    
    ggplot(taper_data, aes(y = reorder(swimmer, points_diff), x = points_diff, fill = team)) +
      geom_col() +  
      facet_wrap(~team, scales = "free_y") +  
      labs(title = plot_title,
           x = "Points Post-Taper",
           y = "Swimmer") +
      scale_fill_manual(values = miac_swim) +
      theme_clean() +
      theme(legend.position = "none",
            axis.title = element_text(face = "bold"),
            axis.title.x = element_text(face = "bold", size = 14),
            axis.title.y = element_text(face = "bold", size = 14),
            axis.text.y = element_text(face = "bold", size = 8),
            panel.grid = element_blank(),
            axis.line.x = element_line(color = "black"),
            strip.text = element_text(face = "bold", size = 12))
  })
  
  # RQ2 Plot
  output$fifthyrPlot <- renderPlotly({
    filtered_data <- data %>% filter(season == input$season)
    
    plot <- ggplot(filtered_data, aes(x = class_yr, y = points, color = team)) +
      geom_jitter(width = 0.35, height = 0.35, alpha = 0.75) +
      scale_color_manual(values = miac_swim) +
      theme_clean() +
      theme(axis.title = element_text(face = "bold"),
            axis.title.x = element_text(face = "bold", size = 14),
            axis.title.y = element_text(face = "bold", size = 14),
            axis.text.y = element_text(face = "bold", size = 8),
            panel.grid = element_blank(),
            axis.line.x = element_line(color = "black"),
            strip.text = element_text(face = "bold", size = 12))+
      labs(x = 'Class Year', y = 'Power Points', color = 'Team')
    
    ggplotly(plot, tooltip = c('points', 'team'))
  })
  
  # RQ3 Plot
  output$covid_boxplot <- renderPlot({
    covid_filtered <- total %>% filter(team %in% input$covid_teams)
    
    ggplot(covid_filtered, aes(x = points, y = season, fill = season)) +
      geom_boxplot() +
      facet_wrap(~team, scales = "free") +
      labs(title = "Impact of COVID on Team Power Points",
           y = "",
           x = "Power Points") +
      theme_clean() +
      theme(legend.position = "none",
            axis.title = element_text(face = "bold"),
            axis.title.x = element_text(face = "bold", size = 14),
            axis.title.y = element_text(face = "bold", size = 14),
            axis.text.y = element_text(face = "bold", size = 8),
            panel.grid = element_blank(),
            axis.line.x = element_line(color = "black"),
            strip.text = element_text(face = "bold", size = 12))
  })
  
  # RQ 4
  filtered_data1 <- reactive({
    data1 %>% filter(gender == input$gender, team %in% input$team_powerpoints)
  })
  
  output$teamPointsPlot <- renderPlotly({
    data_filtered <- filtered_data1()
    p <- ggplot(data_filtered, aes(x = season, y = points, color = team)) + 
      stat_summary(group = 1, color = 'darkgreen', fun = 'mean', geom = 'line', linewidth = .5) +
      stat_summary(group = 1, color = 'darkgreen', fun = 'mean', geom = 'point', size = 1.75) +
      geom_point() +
      geom_line(aes(group = team)) +
      scale_y_continuous(breaks = seq(100, 650, by = 100)) +
      scale_color_manual(values = miac_swim) +
      theme_clean() +
      theme(legend.position = "none",
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
      theme(legend.position = "none",
            axis.title = element_text(face = "bold"),
            axis.title.x = element_text(face = "bold", size = 12),
            axis.title.y = element_text(face = "bold", size = 10),
            axis.text.x = element_text(face = "bold", size = 7),
            panel.grid = element_blank(),
            axis.line.x = element_line(color = "black"),
            strip.text = element_text(face = "bold", size = 12))+
      labs(
        x = 'Season', y = "Relative Power Points Change Between Men's/Women's Teams",
        color = 'Team'
      )
    
    ggplotly(p2, tooltip = c('team', 'change_difference'))
  })
  
  # RQ 5 Plot
  selected_swimmers <- reactive({
    full_miac_points %>%
      filter(season == input$selected_season, gender == input$selected_gender) %>%
      pull(points) %>%
      na.omit()
  })
  
  bootstrap_results <- reactive({
    req(input$generate)
    bootstrap_team(selected_swimmers())
  })
  
  team_avg_points <- reactive({
    mean(bootstrap_results(), na.rm = TRUE)
  })
  
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
  
  # RQ6 Plot
  filtered_data <- reactive({
    miac_timedrops %>% filter(team %in% input$teams)
  })
  
  output$event_plot <- renderPlotly({
    event_col <- filtered_data() %>% 
      group_by(team, events) %>% 
      summarize(count = n(), .groups = 'drop') %>% 
      ggplot(aes(x = events, y = count, fill = team)) + 
      geom_col(color = 'black') +
      scale_fill_manual(values = miac_swim) + 
      theme_clean() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 7)  # Adjusted x-axis text size
      ) +
      labs(x = 'Event', y = 'Frequency', fill = 'Team')
    ggplotly(event_col, tooltip = c('team', 'count'))
  })
  
  output$timedrops_plot <- renderPlotly({
    timedrops_sp <- filtered_data() %>% filter(events == input$event) %>%
      ggplot(aes(x = team, y = time_drop, color = team)) + 
      geom_jitter(width = .3) +
      geom_hline(yintercept = 0, linetype = 'dashed') +
      scale_y_reverse() +
      scale_color_manual(values = miac_swim) +
      theme_clean() +
      theme(legend.position = "none",
            axis.title = element_text(face = "bold"),
            axis.title.x = element_text(face = "bold", size = 14),
            axis.title.y = element_text(face = "bold", size = 14),
            axis.text.x = element_text(face = "bold", size = 6),
            panel.grid = element_blank(),
            axis.line.x = element_line(color = "black"),
            strip.text = element_text(face = "bold", size = 12))+
      labs(x = 'Team', y = 'Time Drop at MIACs (seconds)')
    ggplotly(timedrops_sp, tooltip = 'time_drop')
  })
  
  output$rq1_text <- renderText({
    "This plot gives you the top 10 and bottom 10 tapers on each team in the MIAC for the MIAC championships swim meet. A taper is a reduction in training volume and intensity prior to important an important swim meets to optimize swimming speed prior to an important event. Good tapers can shave seconds off a swimmer's average race times"
  })
  
  output$rq2_text <- renderText({
    "This tab displays a faceted scattterplot to look at the impact of 5th year's on swim rosters in seasons where there were 5th years. After COVID-19, many swimmers were given the opportunity to swim a 5th year and this plot aims to look at the impact of swimming an extra season had on an individuals scoring."
  })
  
  output$rq3_text <- renderText({
    "This tab displays faceted boxplots to show the impact of COVID-19 on swimmers. We took swimmers who swam at least one season prior to the 2019-2020 COVID season and swam at least one season after the 2019-2020 COVID season. We have displayed the difference between the average points scored by each team before and after COVID."
  })
  
  output$rq4_text <- renderText({
    "This tab displays a team's power points over time for both men's and women's teams. We made this plot to look at which schools have been the most dominant in the MIAC over time. And also to look at whether or not men's and women's team often improve together. Extra Note: The dark green line on Power Points Analysis represents the average power points in each season."
  })
  
  output$rq5_text <- renderText({
    "This tab allows you to generate a random team of 15 swimmers who are pulled randomly from all the swimmers in the MIAC in the season you specify. It shows where your team would have placed within the MIAC. We only bootstrap a single time to allow for more variation(Hopefully you can beat Gustavus at least once!)"
  })
  output$rq6_text <- renderText({
    "In this tab we use temporal data to display which events the best swimmers on each team swim, and how their time drops compare to each other in their shared events."
  })
}

shinyApp(ui = ui, server = server)