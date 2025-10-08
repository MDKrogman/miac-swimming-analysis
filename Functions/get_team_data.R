# get_team_data.R
# Function that gets data from a men's and women's miac team and dets it into a data frame for you

library(readr)
library(rvest)
library(dplyr)

# url: chr vector of the team's roster page on Swimcloud.
#      This works for both men's and women's teams, but if a school has both a men's and women's team,
#      use the men's team page. 
#      (There are schools without a men's team but no schools without a women's team,
#      the exception being Saint John's/Saint Ben's, but that's why they have their own function)

# season_id: chr vector that chooses the season, i.e. 28 = 2024-25, 26 = 2022-2023

# season_title: chr vector of just the two years that the season spans.
#               i.e. this year's season is 2024-2025

# team_name: chr vector that selects what team we want, i.e. 'Carleton'.
#            This affects the 'team' column in the output

get_team_data <- function(url, season_id, season_title, team_name){
  if (str_detect(url, 'gender=M') == TRUE){
  team_url <- str_replace(url, '28', season_id)     # Setting the team url for the season
  
  team_urlM <- read_html(team_url)            
  
  nameM <- team_urlM %>%
    html_elements(".u-text-semi") %>%
    html_text() %>% 
    str_squish()
  
  class_yrM <- team_urlM %>%
    html_elements(".u-text-truncate+ .c-table-clean__col-fit") %>%
    html_text() %>% 
    str_squish()
  
  pointsM <- team_urlM %>%
    html_elements(".u-text-end") %>%
    html_text() %>% 
    str_squish()
  
  pointsM <- tibble(
    swimmer = nameM,
    class_yr = class_yrM,
    points = pointsM
  ) %>% 
    mutate(
      team = team_name,
      gender = 'Male',
      .before = 2
    ) %>% 
    mutate(
      season = season_title,
    )
  
  team_urlF <- gsub('gender=M', 'gender=F', team_url)
  
  team_urlF <- read_html(team_urlF)
  
  nameF <- team_urlF %>%
    html_elements(".u-text-semi") %>%
    html_text() %>% 
    str_squish()
  
  class_yrF <- team_urlF %>%
    html_elements(".u-text-truncate+ .c-table-clean__col-fit") %>%
    html_text() %>% 
    str_squish()
  
  pointsF <- team_urlF %>%
    html_elements(".u-text-end") %>%
    html_text() %>% 
    str_squish()
  
  pointsF <- tibble(
    swimmer = nameF,
    class_yr = class_yrF,
    points = pointsF
  ) %>% 
    mutate(
      team = team_name,
      gender = 'Female',
      .before = 2
    ) %>% 
    mutate(
      season = season_title
    )
  
  team_points <- pointsM %>%                         # Name your object `team`_points`season` and that should do it
    bind_rows(pointsF)
  
  return(team_points)
  
  } else{                                            # Case for if school only has a women's team
    team_urlF <- str_replace(url, '28', season_id)
    
    team_urlF <- read_html(team_urlF)
    
    nameF <- team_urlF %>%
      html_elements(".u-text-semi") %>%
      html_text() %>% 
      str_squish()
    
    class_yrF <- team_urlF %>%
      html_elements(".u-text-truncate+ .c-table-clean__col-fit") %>%
      html_text() %>% 
      str_squish()
    
    pointsF <- team_urlF %>%
      html_elements(".u-text-end") %>%
      html_text() %>% 
      str_squish()
    
    pointsF <- tibble(
      swimmer = nameF,
      class_yr = class_yrF,
      points = pointsF
    ) %>% 
      mutate(
        team = team_name,
        gender = 'Female',
        .before = 2
      ) %>% 
      mutate(
        season = season_title
      )
    
    return(pointsF)
  }
}


# Example call: get the full Carleton team roster from the 2022-2023 season
carleton_points2223 <- get_team_data(
  'https://www.swimcloud.com/team/197/roster/?page=1&gender=M&season_id=28&sort=name',
  '26',
  '2022-2023',
  'Carleton'
  )

# Example call: get the full St. Kate's roster from the 2021-2022 season
stk_points2122 <- get_team_data(
  'https://www.swimcloud.com/team/458/roster/?page=1&gender=F&season_id=28&sort=perf',
  '25',
  '2021-2022',
  "St. Kate's"
)

