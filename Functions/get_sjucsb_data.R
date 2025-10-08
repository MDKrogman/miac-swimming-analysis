# get_sjucsb_data.R
# Function that gets data from a men's and women's miac team and dets it into a data frame for you
# It's the same as get_team_data.R, but we do need to make a different function because of diff. team Ids.
# They just had to be unique ig ¯\_(ツ)_/¯ 

library(dplyr)
library(readr)
library(rvest)

# season_id: chr vector that chooses the season, i.e. '28' = 2024-25, '26' = 2022-2023
# season_title: chr vector of just the two years that the season spans.
#               i.e. this year's season is 2024-2025

get_sjucsb_data <- function(season_id, season_title){
  sju_url <- str_replace('https://www.swimcloud.com/team/461/roster/?page=1&gender=M&season_id=28&sort=name',
              '28',
              season_id)
  
  sju_url <- read_html(sju_url)
  
  sju_name <- sju_url %>%
    html_elements(".u-text-semi") %>%
    html_text() %>% 
    str_squish()
  
  sju_class_yr <- sju_url %>%
    html_elements(".u-text-truncate+ .c-table-clean__col-fit") %>%
    html_text() %>% 
    str_squish()
  
  sju_points <- sju_url %>%
    html_elements(".u-text-end") %>%
    html_text() %>% 
    str_squish()
  
  sju_points <- tibble(
    swimmer = sju_name,
    class_yr = sju_class_yr,
    points = sju_points
  ) %>% 
    mutate(
      team = 'SJU/CSB',
      gender = 'Male',
      .before = 2
    ) %>% 
    mutate(
      season = season_title,
    )
 
  csb_url <- str_replace('https://www.swimcloud.com/team/456/roster/?page=1&gender=F&season_id=28&sort=name',
                         '28',
                         season_id)  
  
  csb_url <- read_html(csb_url)
  
  csb_name <- csb_url %>%
    html_elements(".u-text-semi") %>%
    html_text() %>% 
    str_squish()
  
  csb_class_yr <- csb_url %>%
    html_elements(".u-text-truncate+ .c-table-clean__col-fit") %>%
    html_text() %>% 
    str_squish()
  
  csb_points <- csb_url %>%
    html_elements(".u-text-end") %>%
    html_text() %>% 
    str_squish()
  
  csb_points <- tibble(
    swimmer = csb_name,
    class_yr = csb_class_yr,
    points = csb_points
  ) %>% 
    mutate(
      team = 'SJU/CSB',
      gender = 'Female',
      .before = 2
    ) %>% 
    mutate(
      season = season_title
    )
  
  sju_points%>%               # Name your object `team`_points`season` and that should do it
    bind_rows(csb_points)
}

# Example call: get the full SJU/CSB team rosters from 2023-2024:
sjucsb_points2324 <- get_sjucsb_data('27', '2023-2024')



#bals