# get_time_drops.R
# We're going into the top 3 swimmers of each gender of each MIAC swim team
# Then, we're taking their (individual) events and times from MIAC, 
# getting it in mm:ss:hh format and calculating differences from their best times in those events from before
# event1, event2, event3

# NOTE: I'm storing everybody in functions, but they don't actually do anything.
# I'm just doing that so the document is more navigable

# Boy I hope there was not an easier/less intensive way to do this that we learned in class!
# - Matthew 

library(tidyverse)
library(readr)
library(rvest)
library(dplyr)

# Carleton
ethantun <- function(){
  tun <- read_html('https://www.swimcloud.com/results/310345/swimmer/1332068/')
  
  tunevents <- tun %>% 
    html_elements('.o-list-block__item:nth-child(11) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(9) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  tuntimes <- tun %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  tuntime2_13 <- read_html('https://www.swimcloud.com/results/324658/swimmer/1332068/')
  
  tuntimes2_1 <- tuntime2_13 %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  tuntime2_2 <- read_html('https://www.swimcloud.com/results/320490/swimmer/1332068/')
  
  tuntimes2_2 <- tuntime2_2 %>% 
    html_elements('.o-list-block__item:nth-child(5) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  tuntimes2_3 <- tuntime2_13 %>% 
    html_elements('.o-list-block__item:nth-child(15) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  tuntimes2 <- c(tuntimes2_1, tuntimes2_2, tuntimes2_3)
  
  ethantun <- tibble(
    name = 'Tun, Ethan',
    team = 'Carleton',
    gender = 'Male',
    events = tunevents,
    times = tuntimes,
    times2 = tuntimes2
  )
}

jacksonrankin <- function(){
  rankin <- read_html('https://www.swimcloud.com/results/310345/swimmer/1632455/')
  
  rankinevents <- rankin %>% 
    html_elements('.o-list-block__item:nth-child(9) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  rankintimes <- rankin %>% 
    html_elements('.o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  rankintimes2_123 <- read_html('http://swimcloud.com/results/324658/swimmer/1632455/')
  
  rankintimes2_123 <- rankintimes2_123 %>% 
    html_elements('.o-list-block__item:nth-child(13) .u-mr- , .o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  jacksonrankin <- tibble(
    name = 'Rankin, Jackson',
    team = 'Carleton',
    gender = 'Male',
    events = rankinevents,
    times = rankintimes,
    times2 = rankintimes2_123
  )
}

matthewkrogman <- function(){
  krogman <- read_html('https://www.swimcloud.com/results/310345/swimmer/1216477/')
  
  krogmanevents <- krogman %>% 
    html_elements('.o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(4) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(2) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  krogmantimes <- krogman %>% 
    html_elements('.o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(4) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  krogmantimes2_123 <- read_html('https://www.swimcloud.com/results/324658/swimmer/1216477/')
  
  krogmantimes2_123 <- krogmantimes2_123 %>% 
    html_elements('.o-list-block__item:nth-child(13) .u-mr- , .o-list-block__item:nth-child(6) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  matthewkrogman <- tibble(
    name = 'Krogman, Matthew',
    team = 'Carleton',
    gender = 'Male',
    events = krogmanevents,
    times = krogmantimes,
    times2 = krogmantimes2_123
  )
}

gracehou <- function(){
  hou <- read_html('https://www.swimcloud.com/results/310345/swimmer/1331026/')
  
  houevents <- hou %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(3) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish() 
    
  houtimes <- hou %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(6) .u-mr- , .o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
    
  houtimes2_12 <- read_html('https://www.swimcloud.com/results/324658/swimmer/1331026/')
  
  houtimes2_12 <- houtimes2_12 %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  houtimes2_3 <- read_html('https://www.swimcloud.com/results/335981/swimmer/1331026/')
  
  houtimes2_3 <- houtimes2_3 %>% 
    html_elements('.o-list-block__item+ .o-list-block__item .u-mr-') %>% 
    html_text() %>% 
    str_squish() 
  
  houtimes2 <- c(houtimes2_12, houtimes2_3)
  
  gracehou <- tibble(
    name = 'Hou, Grace',
    team = 'Carleton',
    gender = 'Female',
    events = houevents,
    times = houtimes,
    times2 = houtimes2
  )
}

chelseaandersen <- function(){
  andersen <- read_html('https://www.swimcloud.com/results/310345/swimmer/2216148/')
  
  andersenevents <- andersen %>% 
    html_elements('.o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(5) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  andersentimes <- andersen %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(5) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  andersentimes2_123 <- read_html('https://www.swimcloud.com/results/324658/swimmer/2216148/')
  
  andersentimes2 <- andersentimes2_123 %>% 
    html_elements('.o-list-block__item:nth-child(12) .u-mr- , .o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  chelseaandersen <- tibble(
    name = 'Andersen, Chelsea',
    team = 'Carleton',
    gender = 'Female',
    events = andersenevents,
    times = andersentimes,
    times2 = andersentimes2
  )
}

stephaniebaranov <- function(){
  baranov <- read_html('http://swimcloud.com/results/310345/swimmer/1510659/') 
  
  baranovevents <- baranov %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(4) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  baranovtimes <- baranov %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  baranovtimes2 <- read_html('https://www.swimcloud.com/results/324658/swimmer/1510659/')
  
  baranovtimes2 <- baranovtimes2 %>% 
    html_elements('.o-list-block__item:nth-child(12) .u-mr- , .o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(8) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  stephaniebaranov <- tibble(
    name = 'Baranov, Stephanie',
    team = 'Carleton',
    gender = 'Female',
    events = baranovevents,
    times = baranovtimes,
    times2 = baranovtimes2
  )
}

# Concordia-Moorhead
calliemetsala <- function(){
  metsala <- read_html('https://www.swimcloud.com/results/310345/swimmer/1627620/')
  
  metsalaevents <- metsala %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(3) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  metsalatimes <- metsala %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  metsala2 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1627620/')
  
  metsalatimes2 <- metsala2 %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(6) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish() 
  
  calliemetsala <- tibble(
    name = 'Metsala, Callie',
    team = 'Concordia-Moorhead',
    gender = 'Female',
    events = metsalaevents,
    times = metsalatimes,
    times2 = metsalatimes2
  )
}

haileyjaeger <- function(){
  jaeger <- read_html('https://www.swimcloud.com/results/310345/swimmer/805088/')
  
  jaegerevents <- jaeger %>% 
    html_elements('.o-list-block__item:nth-child(12) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(5) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  jaegertimes <- jaeger %>% 
    html_elements('.o-list-block__item:nth-child(12) .u-mr- , .o-list-block__item:nth-child(5) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
    
  jaeger2_13 <- read_html('https://www.swimcloud.com/results/324567/swimmer/805088/')
  
  jaeger2_2 <- read_html('https://www.swimcloud.com/results/328388/swimmer/805088/')
  
  jaegertimes2_1 <- jaeger2_13 %>% 
    html_elements('.o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  jaegertimes2_2 <- jaeger2_2 %>% 
    html_elements('.o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  jaegertimes2_3 <- jaeger2_13 %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  jaegertimes2 <- c(jaegertimes2_1, jaegertimes2_2, jaegertimes2_3)
  
  haileyjaeger <- tibble(
    name = 'Jaeger, Hailey',
    team = 'Concordia-Moorhead',
    gender = 'Female',
    events = jaegerevents,
    times = jaegertimes,
    times2 = jaegertimes2
  )
}

leahenedy <- function(){
  enedy <- read_html('https://www.swimcloud.com/results/310345/swimmer/2910653/')
  
  enedyevents <- enedy %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(9) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish() 
  
  enedytimes <- enedy %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  enedy2_1 <- read_html('https://www.swimcloud.com/results/315618/swimmer/2910653/')
  
  enedy2_23 <- read_html('https://www.swimcloud.com/results/324567/swimmer/2910653/') 
  
  enedytimes2_1 <- enedy2_1 %>% 
    html_elements('.o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  enedytimes2_23 <- enedy2_23 %>% 
    html_elements('.o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(5) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  enedytimes2 <- c(enedytimes2_1, enedytimes2_23)
  
  leahenedy <- tibble(
    name = 'Enedy, Leah',
    team = 'Concordia-Moorhead',
    gender = 'Female',
    events = enedyevents,
    times = enedytimes,
    times2 = enedytimes2
  )
}

# Gustavus
bradenripken <- function(){
  ripken <- read_html('https://www.swimcloud.com/results/310345/swimmer/1433910/')
  
  ripkenevents <- ripken %>% 
    html_elements('.o-list-block__item:nth-child(13) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(5) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(2) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  ripkentimes <- ripken %>%
    html_elements('.o-list-block__item:nth-child(14) .u-mr- , .o-list-block__item:nth-child(4) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  ripken2_1 <- read_html('https://www.swimcloud.com/results/335981/swimmer/1433910/')
  
  ripken2_23 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1433910/')
  
  ripkentimes2_1 <- ripken2_1 %>% 
    html_elements('.o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  ripkentimes2_23 <- ripken2_23 %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  ripkentimes2 <- c(ripkentimes2_1, ripkentimes2_23)
  
  bradenripken <- tibble(
    name = 'Ripken, Braden',
    team = 'Gustavus',
    gender = 'Male',
    events = ripkenevents,
    times = ripkentimes,
    times2 = ripkentimes2
  )
}

peytonrichardson <- function(){
  richardson <- read_html('https://www.swimcloud.com/results/310345/swimmer/888901/')
  
  richardsonevents <- richardson %>% 
    html_elements('.o-list-block__item:nth-child(12) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  richardsontimes <- richardson %>% 
    html_elements('.o-list-block__item:nth-child(13) .u-mr- , .o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  richardson2 <- read_html('https://www.swimcloud.com/results/324567/swimmer/888901/')

  richardsontimes2 <- richardson2 %>% 
    html_elements('.o-list-block__item:nth-child(12) .u-mr- , .o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(9) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  peytonrichardson <- tibble(
    name = 'Richardson, Peyton',
    team = 'Gustavus',
    gender = 'Male',
    events = richardsonevents,
    times = richardsontimes,
    times2 = richardsontimes2
  )
}

nydenhill <- function(){
  hill <- read_html('http://swimcloud.com/results/310345/swimmer/977363/')
  
  hillevents <- hill %>% 
    html_elements('.o-list-block__item:nth-child(9) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(4) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  hilltimes <- hill %>% 
    html_elements('.o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  hill2_13 <- read_html('https://www.swimcloud.com/results/324567/swimmer/977363/') 
  hill2_2 <- read_html('https://www.swimcloud.com/results/335981/swimmer/977363/') 
  
  hilltimes2_1 <- hill2_13 %>% 
    html_elements('.o-list-block__item:nth-child(5) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  hilltimes2_2 <- hill2_2 %>% 
    html_elements('.o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  hilltimes2_3 <- hill2_13 %>% 
    html_elements('.o-list-block__item:nth-child(8) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  hilltimes2 <- c(hilltimes2_1, hilltimes2_2, hilltimes2_3)
  
  nydenhill <- tibble(
    name = 'Hill, Nyden',
    team = 'Gustavus',
    gender = 'Male',
    events = hillevents,
    times = hilltimes,
    times2 = hilltimes2
  )
}

genevafackler <- function(){
  fackler <- read_html('https://www.swimcloud.com/results/310345/swimmer/2277213/')
  
  facklerevents <- fackler %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(4) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  facklertimes <- fackler %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(5) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  fackler2_12 <- read_html('https://www.swimcloud.com/results/324567/swimmer/2277213/') 
  fackler2_3 <- read_html('https://www.swimcloud.com/results/335981/swimmer/2277213/')
  
  facklertimes2_12 <- fackler2_12 %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  facklertimes2_3 <- fackler2_3 %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  facklertimes2 <- c(facklertimes2_12, facklertimes2_3)
  
  genevafackler <- tibble(
    name = 'Fackler, Geneva',
    team = 'Gustavus',
    gender = 'Female',
    events = facklerevents,
    times = facklertimes,
    times2 = facklertimes2
  )
}

maritisaacson <- function(){
  
  isaacson <- read_html('https://www.swimcloud.com/results/310345/swimmer/599656/')
  
  isaacsonevents <- isaacson %>% 
    html_elements('.o-list-block__item:nth-child(15) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(13) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(5) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  isaacsontimes <- isaacson %>% 
    html_elements('.o-list-block__item:nth-child(16) .u-mr- , .o-list-block__item:nth-child(14) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  isaacson2_13 <- read_html('https://www.swimcloud.com/results/335981/swimmer/599656/')
  isaacson2_2 <- read_html('https://www.swimcloud.com/results/324567/swimmer/599656/') 
  
  isaacsontimes2_1 <- isaacson2_13 %>% 
    html_elements('.o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  isaacsontimes2_2 <- isaacson2_2 %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  isaacsontimes2_3 <- isaacson2_13 %>% 
    html_elements('.o-list-block__item:nth-child(9) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  isaacsontimes2 <- c(isaacsontimes2_1, isaacsontimes2_2, isaacsontimes2_3)
  
  maritisaacson <- tibble(
    name = 'Isaacson, Marit',
    team = 'Gustavus',
    gender = 'Female',
    events = isaacsonevents,
    times = isaacsontimes,
    times2 = isaacsontimes2
  )
}

katelynsiers <- function(){
  siers <- read_html('https://www.swimcloud.com/results/310345/swimmer/2164369/')
  
  siersevents <- siers %>% 
    html_elements('.o-list-block__item:nth-child(9) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(4) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(2) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  sierstimes <- siers %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(5) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  siers2 <- read_html('https://www.swimcloud.com/results/335981/swimmer/2164369/')
  
  sierstimes2 <- siers2 %>% 
    html_elements('.u-link-text.u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  katelynsiers <- tibble(
    name = 'Siers, Katelyn',
    team = 'Gustavus',
    gender = 'Female',
    events = siersevents,
    times = sierstimes,
    times2 = sierstimes2
  )
}

# Hamline
dylanpurrington <- function(){
  purrington <- read_html('https://www.swimcloud.com/results/310345/swimmer/599876/')
  
  purringtonevents <- purrington %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(9) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  purringtontimes <- purrington %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>%
    str_squish()
  
  purrington2_1 <- read_html('https://www.swimcloud.com/results/320424/swimmer/599876/')
  purrington2_23 <- read_html('https://www.swimcloud.com/results/331248/swimmer/599876/')
    
  purringtontimes2_1 <- purrington2_1 %>% 
    html_elements('.o-list-block__item~ .o-list-block__item+ .o-list-block__item .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  purringtontimes2_23 <- purrington2_23 %>% 
    html_elements('.o-list-block__item+ .o-list-block__item .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  purringtontimes2 <- c(purringtontimes2_1, purringtontimes2_23)
  
  dylanpurrington <- tibble(
    name = 'Purrington, Dylan',
    team = 'Hamline',
    gender = 'Male',
    events = purringtonevents,
    times = purringtontimes,
    times2 = purringtontimes2
  )
}

gusshaffer <- function(){
  shaffer <- read_html('https://www.swimcloud.com/results/310345/swimmer/935320/')
  
  shafferevents <- shaffer %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  shaffertimes <- shaffer %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  shaffer2_12 <- read_html('https://www.swimcloud.com/results/335321/swimmer/935320/')
  shaffer2_3 <- read_html('https://www.swimcloud.com/results/333711/swimmer/935320/')
  
  shaffertimes2_12 <- shaffer2_12 %>% 
    html_elements('.o-list-block__item:nth-child(3) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  shaffertimes2_3 <- shaffer2_3 %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
    
  shaffertimes2 <- c(shaffertimes2_12, shaffertimes2_3)
  
  gusshaffer <- tibble(
    name = 'Shaffer, Gus',
    team = 'Hamline',
    gender = 'Male',
    events = shafferevents,
    times = shaffertimes,
    times2 = shaffertimes
  )
}

mannyposcher <- function(){
  poscher <- read_html('https://www.swimcloud.com/results/310345/swimmer/2939405/')
  
  poscherevents <- poscher %>% 
    html_elements('.o-list-block__item:nth-child(4) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  poschertimes <- poscher %>% 
    html_elements('.o-list-block__item:nth-child(4) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  poschertimes2 <- read_html('https://www.swimcloud.com/results/333711/swimmer/2939405/') %>% 
    html_elements('.o-list-block__item:nth-child(5) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  mannyposcher <- tibble(
    name = 'Poscher, Manny',
    team = 'Hamline',
    gender = 'Male',
    events = poscherevents,
    times = poschertimes,
    times2 = poschertimes2
  )
}

silayuttras <- function(){
  yuttras <- read_html('https://www.swimcloud.com/results/310345/swimmer/745107/')
  
  yuttrasevents <- yuttras %>% 
    html_elements('.o-list-block__item:nth-child(11) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  yuttrastimes <- yuttras %>% 
    html_elements('.o-list-block__item:nth-child(12) .u-mr- , .o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  yuttrastimes2_12 <- read_html('https://www.swimcloud.com/results/335321/swimmer/745107/') %>% 
    html_elements('.o-list-block__item:nth-child(4) .u-mr- , .o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  yuttrastimes2_3 <- read_html('https://www.swimcloud.com/results/333711/swimmer/745107/') %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  yuttrastimes2 <- c(yuttrastimes2_12, yuttrastimes2_3)

  silayuttras <- tibble(
    name = 'Yuttras, Sila',
    team = 'Hamline',
    gender = 'Female',
    events = yuttrasevents,
    times = yuttrastimes,
    times2 = yuttrastimes2
  )
}

ellsasw <- function(){
  sw <- read_html('https://www.swimcloud.com/results/310345/swimmer/728067/') 
  
  swevents <- sw %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  swtimes <- sw %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish() 
  
  swtimes2_1 <- read_html('https://www.swimcloud.com/results/324567/swimmer/728067/') %>% 
    html_elements('.o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  swtimes2_23 <- read_html('https://www.swimcloud.com/results/335321/swimmer/728067/') %>% 
    html_elements('.o-list-block__item~ .o-list-block__item+ .o-list-block__item .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  swtimes2 <- c(swtimes2_1, swtimes2_23)
  
  ellsasw <- tibble(
    name = 'Sorenson-Wagner, Ellsa',
    team = 'Hamline',
    gender = 'Female',
    events = swevents,
    times = swtimes,
    times2 = swtimes2
  )
}

nicolephillips <- function(){
  phillips <- read_html('https://www.swimcloud.com/results/310345/swimmer/1975678/')
  
  phillipsevents <- phillips %>% 
    html_elements('.o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(3) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  phillipstimes <- phillips %>% 
    html_elements('.o-list-block__item:nth-child(6) .u-mr- , .o-list-block__item:nth-child(3) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  phillipstimes2_1 <- read_html('https://www.swimcloud.com/results/335321/swimmer/1975678/') %>% 
    html_elements('.u-link-text.u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  phillipstimes2_23 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1975678/') %>% 
    html_elements('.o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  phillipstimes2 <- c(phillipstimes2_1, phillipstimes2_23)
  
  nicolephillips <- tibble(
    name = 'Phillips, Nicole',
    team = 'Hamline',
    gender = 'Female',
    events = phillipsevents,
    times = phillipstimes,
    times2 = phillipstimes2
  )
}

# Macalester (boo hiss)
charlesbatsaikahn <- function(){
  bat <- read_html('https://www.swimcloud.com/results/310345/swimmer/1711819/')
  
  batevents <- bat %>% 
    html_elements('.o-list-block__item:nth-child(11) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(9) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish() 
  
  battimes <- bat %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  battimes2_12 <- read_html('https://www.swimcloud.com/results/320948/swimmer/1711819/') %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
    
  battimes2_3 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1711819/') %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  battimes2 <- c(battimes2_12, battimes2_3)
  
  charlesbatsaikahn <- tibble(
    name = 'Batsaikahn, Charles',
    team = 'Macalester',
    gender = 'Male',
    events = batevents,
    times = battimes,
    times2 = battimes2
  )
}

tommymoore <- function(){
  moore <- read_html('https://www.swimcloud.com/results/310345/swimmer/1760429/')
  
  mooreevents <- moore %>% 
    html_elements('.o-list-block__item:nth-child(11) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(2) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  mooretimes <- moore %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  mooretimes2_1 <- read_html('https://www.swimcloud.com/results/320948/swimmer/1760429/') %>% 
    html_elements('.o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  mooretimes2_2 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1760429/') %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  mooretimes2_3 <- read_html('https://www.swimcloud.com/results/306127/swimmer/1760429/') %>% 
    html_elements('.o-list-block__item:nth-child(5) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  mooretimes2 <- c(mooretimes2_1, mooretimes2_2, mooretimes2_3)
  
  tommymoore <- tibble(
    name = 'Moore, Tommy',
    team = 'Macalester',
    gender = 'Male',
    events = mooreevents,
    times = mooretimes,
    times2 = mooretimes2
  )
}

willsj <- function(){
  sj <- read_html('https://www.swimcloud.com/results/310345/swimmer/1337800/') 
  
  sjevents <- sj %>% 
    html_elements('.o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(5) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(3) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish() 
  
  sjtimes <- sj %>% 
    html_elements('.o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(6) .u-mr- , .o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  sjtimes2 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1337800/') %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(6) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  willsj <- tibble(
    name = 'St. John, Will',
    team = 'Macalester',
    gender = 'Male',
    events = sjevents,
    times = sjtimes,
    times2 = sjtimes2
  )
}

veritywr <- function(){
  wr <- read_html('https://www.swimcloud.com/results/310345/swimmer/599618/')
  
  wrevents <- wr %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  wrtimes <- wr %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  wrtimes2_12 <- read_html('https://www.swimcloud.com/results/324567/swimmer/599618/') %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  wrtimes2_3 <- read_html('https://www.swimcloud.com/results/333485/swimmer/599618/') %>% 
    html_elements('.o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  wrtimes2 <- c(wrtimes2_12, wrtimes2_3)
  
  veritywr <- tibble(
    name = 'Wray-Raabolle, Verity',
    team = 'Macalester',
    gender = 'Female',
    events = wrevents,
    times = wrtimes,
    times2 = wrtimes2
  )
}

izzyut <- function(){
  ut <- read_html('https://www.swimcloud.com/results/310345/swimmer/1105012/')
  
  utevents <- ut %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(5) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  uttimes <- ut %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  uttimes2_12 <- read_html('https://www.swimcloud.com/results/335981/swimmer/1105012/') %>% 
    html_elements('.o-list-block__item:nth-child(3) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  uttimes2_3 <- read_html('https://www.swimcloud.com/results/335321/swimmer/1105012/') %>% 
    html_elements('.o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  uttimes2 <- c(uttimes2_12, uttimes2_3)
  
  izzyut <- tibble(
    name = 'Uhlhonr-Thornton, Izzy',
    team = 'Macalester',
    gender = 'female',
    events = utevents,
    times = uttimes,
    times2 = uttimes2
  )
}

carolinechapon <- function(){
  chapon <- read_html('https://www.swimcloud.com/results/310345/swimmer/1161529/')
  
  chaponevents <- chapon %>% 
    html_elements('.o-list-block__item:nth-child(11) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish() 
  
  chapontimes <- chapon %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  chapontimes2_1 <- read_html('https://www.swimcloud.com/results/320948/swimmer/1161529/') %>% 
    html_elements('.o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  chapontimes2_2 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1161529/') %>% 
    html_elements('.o-list-block__item:nth-child(5) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  chapontimes2_3 <- read_html('https://www.swimcloud.com/results/335981/swimmer/1161529/') %>% 
    html_elements('.o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  chapontimes2 <- c(chapontimes2_1, chapontimes2_2, chapontimes2_3)
  
  carolinechapon <- tibble(
    name = 'Chapon, Caroline', 
    team = 'Macalester',
    gender = 'Female',
    events = chaponevents,
    times = chapontimes,
    times2 = chapontimes2
  )
}

# Saint Kate's
emmasvendsen <- function(){
  svendsen <- read_html('https://www.swimcloud.com/results/310345/swimmer/479780/')
  
  svendsenevents <- svendsen %>% 
    html_elements('.o-list-block__item:nth-child(11) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(3) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  svendsentimes <- svendsen %>% 
    html_elements('.o-list-block__item:nth-child(12) .u-mr- , .o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  svendsentimes2_1 <- read_html('https://www.swimcloud.com/results/321884/swimmer/479780/') %>% 
    html_elements('.o-list-block__item:nth-child(12) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  svendsentimes2_2 <- read_html('https://www.swimcloud.com/results/331498/swimmer/479780/') %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  svendsentimes2_3 <- read_html('https://www.swimcloud.com/results/321884/swimmer/479780/') %>% 
    html_elements('.o-list-block__item:nth-child(16) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
    
  svendsentimes2 <- c(svendsentimes2_1, svendsentimes2_2, svendsentimes2_3)
  
  emmasvendsen <- tibble(
    name = 'Svendsen, Emma',
    team = "Saint Kate's",
    gender = 'Female',
    events = svendsenevents,
    times = svendsentimes,
    times2 = svendsentimes2
  )
}

ellahochstetler <- function(){
  hoch <- read_html('https://www.swimcloud.com/results/310345/swimmer/2500798/')
  
  hochevents <- hoch %>% 
    html_elements('.o-list-block__item:nth-child(11) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(3) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  hochtimes <- hoch %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(3) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  hochtimes2 <- read_html('https://www.swimcloud.com/results/321884/swimmer/2500798/') %>% 
    html_elements('.o-list-block__item:nth-child(13) .u-mr- , .o-list-block__item:nth-child(4) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish() 
    
  ellahochstetler <- tibble(
    name = 'Hochstetler, Ella', 
    team = "Saint Kate's",
    gender = 'Female',
    events = hochevents,
    times = hochtimes,
    times2 = hochtimes2
  )
}

oliviaweil <- function(){
  weil <- read_html('https://www.swimcloud.com/results/310345/swimmer/1149763/')
  
  weilevents <- weil %>% 
    html_elements('.o-list-block__item:nth-child(11) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(3) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  weiltimes <- weil %>% 
    html_elements('.o-list-block__item:nth-child(12) .u-mr- , .o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  weiltimes2 <- read_html('https://www.swimcloud.com/results/321884/swimmer/1149763/') %>% 
    html_elements('.o-list-block__item:nth-child(15) .u-mr- , .o-list-block__item:nth-child(12) .u-mr- , .o-list-block__item:nth-child(5) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  oliviaweil <- tibble(
    name = 'Weil, Olivia',
    team = "Saint Kate's",
    gender = 'Female',
    events = weilevents,
    times = weiltimes,
    times2 = weiltimes2
  )
}

# SJU/CSB
braydenslavik <- function(){
  slavik <- read_html('https://www.swimcloud.com/results/310345/swimmer/1726516/') 
  
  slavikevents <- slavik %>% 
    html_elements('.o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(5) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(2) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  slaviktimes <- slavik %>% 
    html_elements('.o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(5) .u-mr- , .o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  slaviktimes2 <- read_html('https://www.swimcloud.com/results/324658/swimmer/1726516/') %>% 
    html_elements('.o-list-block__item:nth-child(13) .u-mr- , .o-list-block__item:nth-child(6) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  braydenslavik <- tibble(
    name = 'Slavik, Brayden',
    team = "Saint John's/Saint Ben's",
    gender = 'Male',
    events = slavikevents,
    times = slaviktimes,
    times2 = slaviktimes2
  )
}

carterlarson <- function(){
  larson <- read_html('https://www.swimcloud.com/results/310345/swimmer/1782505/')
  
  larsonevents <- larson %>% 
    html_elements('.o-list-block__item:nth-child(9) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(2) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  larsontimes <- larson %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  larsontimes2 <- read_html('https://www.swimcloud.com/results/324658/swimmer/1782505/') %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  carterlarson <- tibble(
    name = 'Larson, Carter',
    team = "Saint John's/Saint Ben's",
    gender = 'Male',
    events = larsonevents,
    times = larsontimes,
    times2 = larsontimes2
  )
}

carstenreuter <- function(){
  reuter <- read_html('https://www.swimcloud.com/results/310345/swimmer/2451752/')
  
  reuterevents <- reuter %>% 
    html_elements('.o-list-block__item:nth-child(9) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(5) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  reutertimes <- reuter %>% 
    html_elements('.o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(6) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  reutertimes2_1 <- read_html('https://www.swimcloud.com/results/331386/swimmer/2451752/') %>% 
    html_elements('.o-list-block__item:nth-child(1) .u-mr-') %>%
    html_text() %>% 
    str_squish()
  
  reutertimes2_2 <- read_html('https://www.swimcloud.com/results/324658/swimmer/2451752/') %>% 
    html_elements('.o-list-block__item:nth-child(5) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  reutertimes2_3 <- read_html('https://www.swimcloud.com/results/335981/swimmer/2451752/') %>% 
    html_elements('.o-list-block__item+ .o-list-block__item .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  reutertimes2 <- c(reutertimes2_1, reutertimes2_2, reutertimes2_3)
  
  carstenreuter <- tibble(
    name = 'Reuter, Carsten',
    team = "Saint John's/Saint Ben's",
    gender = 'Male',
    events = reuterevents,
    times = reutertimes,
    times2 = reutertimes2
  )
}

marymorris <- function(){
  morris <- read_html('https://www.swimcloud.com/results/310345/swimmer/1188248/')
  
  morrisevents <- morris %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(3) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  morristimes <- morris %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  morristimes2 <- read_html('https://www.swimcloud.com/results/324658/swimmer/1188248/') %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(5) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  marymorris <- tibble(
    name = 'Morris, Mary',
    team = "Saint John's/Saint Ben's",
    gender = 'Male',
    events = morrisevents,
    times = morristimes,
    times2 = morristimes2
  )
}

rachelschlueter <- function(){
  schlueter <- read_html('https://www.swimcloud.com/results/310345/swimmer/1218163/') 
  
  schlueterevents <- schlueter %>% 
    html_elements('.o-list-block__item:nth-child(11) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  schluetertimes <- schlueter %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(6) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  schluetertimes2 <- read_html('https://www.swimcloud.com/results/324658/swimmer/1218163/') %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(4) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish
    
  rachelschlueter <- tibble(
    name = 'Schlueter, Rachel',
    team = "Saint John's/Saint Ben's",
    gender = 'Female',
    events = schlueterevents,
    times = schluetertimes,
    times2 = schluetertimes
  )
}

clairecanfield <- function(){
  canfield <- read_html('https://www.swimcloud.com/results/310345/swimmer/633802/')
  
  canfieldevents <- canfield %>% 
    html_elements('.o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(2) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  canfieldtimes <- canfield %>% 
    html_elements('.o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  canfieldtimes2 <- read_html('https://www.swimcloud.com/results/324658/swimmer/633802/') %>% 
    html_elements('.o-list-block__item:nth-child(13) .u-mr- , .o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  clairecanfield <- tibble(
    name = 'Canfield, Claire',
    team = "Saint John's/Saint Ben's",
    gender = 'Female',
    events = canfieldevents,
    times = canfieldtimes,
    times2 = canfieldtimes2
  )
}

# Saint Olaf
nickstarcevich <- function(){
  star <- read_html('https://www.swimcloud.com/results/310345/swimmer/1739702/')
  
  starevents <- star %>% 
    html_elements('.o-list-block__item:nth-child(11) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(4) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  startimes <- star %>% 
    html_elements('.o-list-block__item:nth-child(12) .u-mr- , .o-list-block__item:nth-child(5) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  startimes2 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1739702/') %>% 
    html_elements('.o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(5) .u-mr- , .o-list-block__item:nth-child(1) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  nickstarcevich <- tibble(
    name = 'Starcevich, Nick',
    team = 'Saint Olaf',
    gender = 'Male',
    events = starevents,
    times = startimes,
    times2 = startimes2
  )
}

traviselling <- function(){
  elling <- read_html('https://www.swimcloud.com/results/310345/swimmer/2999034/')
  
  ellingevents <- elling %>% 
    html_elements('.o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(4) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(1) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  ellingtimes <- elling %>% 
    html_elements('.o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(5) .u-mr- , .o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  ellingtimes2_1 <- read_html('https://www.swimcloud.com/results/321391/swimmer/2999034/') %>% 
    html_elements('.o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  ellingtimes2_23 <- read_html('https://www.swimcloud.com/results/324567/swimmer/2999034/') %>% 
    html_elements('.o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  ellingtimes2 <- c(ellingtimes2_1, ellingtimes2_23)
  
  traviselling <- tibble(
    name = 'Elling, Travis',
    team = 'Saint Olaf',
    gender = 'Male',
    events = ellingevents,
    times = ellingtimes,
    times2 = ellingtimes2
  )
}

aidenyung <- function(){
  yung <- read_html('https://www.swimcloud.com/results/310345/swimmer/1653951/')
  
  yungevents <- yung %>% 
    html_elements('.o-list-block__item:nth-child(7) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(5) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(3) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  yungtimes <- yung %>% 
    html_elements('.o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(6) .u-mr- , .o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  yungtimes2 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1653951/') %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(8) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  aidenyung <- tibble(
    name = 'Yung, Aiden',
    team = 'Saint Olaf',
    gender = 'Male',
    events = yungevents,
    times = yungtimes,
    times2 = yungtimes2
  )
}

katiehomme <- function(){
  homme <- read_html('https://www.swimcloud.com/results/310345/swimmer/1454170/') 
  
  hommeevents <- homme %>% 
    html_elements('.o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(5) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(2) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  hommetimes <- homme %>% 
    html_elements('.o-list-block__item:nth-child(10) .u-mr- , .o-list-block__item:nth-child(6) .u-mr- , .o-list-block__item:nth-child(2) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  hommetimes2_13 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1454170/') 
  
  hommetimes2_1 <- hommetimes2_13 %>% 
    html_elements('.o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  hommetimes2_2 <- read_html('https://www.swimcloud.com/results/321391/swimmer/1454170/') %>% 
    html_elements('.o-list-block__item:nth-child(3) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  hommetimes2_3 <- hommetimes2_13 %>% 
    html_elements('.o-list-block__item:nth-child(8) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  hommetimes2 <- c(hommetimes2_1, hommetimes2_2, hommetimes2_3)
  
  katiehomme <- tibble(
    name = 'Homee, Katie',
    team = 'Saint Olaf',
    gender = 'Female',
    events = hommeevents,
    times = hommetimes,
    times2 = hommetimes2
  )
}

paigesteenblock <- function(){
  steen <- read_html('https://www.swimcloud.com/results/310345/swimmer/1471559/') 
  
  steenevents <- steen %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(8) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  steentimes <- steen %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  steentimes2 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1471559/') %>% 
    html_elements('.o-list-block__item:nth-child(8) .u-mr- , .o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  paigesteenblock <- tibble(
    name = 'Steenblock, Paige',
    team = 'Saint Olaf',
    gender = 'Female',
    events = steenevents,
    times = steentimes,
    times2 = steentimes2
  )
}

oliviasmall <- function(){
  small <- read_html('https://www.swimcloud.com/results/310345/swimmer/1683413/') 
  
  smallevents <- small %>% 
    html_elements('.o-list-block__item:nth-child(10) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(6) .c-swimmer-times__value.u-link-text , .o-list-block__item:nth-child(3) .c-swimmer-times__value.u-link-text') %>% 
    html_text() %>% 
    str_squish()
  
  smalltimes <- small %>% 
    html_elements('.o-list-block__item:nth-child(11) .u-mr- , .o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(4) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  smalltimes2 <- read_html('https://www.swimcloud.com/results/324567/swimmer/1683413/') %>% 
    html_elements('.o-list-block__item:nth-child(9) .u-mr- , .o-list-block__item:nth-child(7) .u-mr- , .o-list-block__item:nth-child(6) .u-mr-') %>% 
    html_text() %>% 
    str_squish()
  
  oliviasmall <- tibble(
    name = 'Small, Olivia',
    team = 'Saint Olaf',
    gender = 'Female',
    events = smallevents,
    times = smalltimes,
    times2 = smalltimes2
  )
}

miac_timedrops <- ethantun %>% 
  bind_rows(jacksonrankin) %>% 
  bind_rows(matthewkrogman) %>% 
  bind_rows(gracehou) %>% 
  bind_rows(chelseaandersen) %>% 
  bind_rows(stephaniebaranov) %>% 
  bind_rows(calliemetsala) %>% 
  bind_rows(haileyjaeger) %>% 
  bind_rows(leahenedy) %>% 
  bind_rows(bradenripken) %>% 
  bind_rows(peytonrichardson) %>% 
  bind_rows(nydenhill) %>% 
  bind_rows(genevafackler) %>% 
  bind_rows(maritisaacson) %>% 
  bind_rows(katelynsiers) %>% 
  bind_rows(dylanpurrington) %>% 
  bind_rows(gusshaffer) %>% 
  bind_rows(mannyposcher) %>% 
  bind_rows(silayuttras) %>% 
  bind_rows(ellsasw) %>% 
  bind_rows(nicolephillips) %>% 
  bind_rows(charlesbatsaikahn) %>% 
  bind_rows(tommymoore) %>% 
  bind_rows(willsj) %>% 
  bind_rows(veritywr) %>% 
  bind_rows(izzyut) %>% 
  bind_rows(carolinechapon) %>% 
  bind_rows(emmasvendsen) %>% 
  bind_rows(ellahochstetler) %>% 
  bind_rows(oliviaweil) %>% 
  bind_rows(braydenslavik) %>% 
  bind_rows(carterlarson) %>% 
  bind_rows(carstenreuter) %>% 
  bind_rows(marymorris) %>% 
  bind_rows(rachelschlueter) %>% 
  bind_rows(clairecanfield) %>% 
  bind_rows(nickstarcevich) %>% 
  bind_rows(traviselling) %>% 
  bind_rows(aidenyung) %>% 
  bind_rows(katiehomme) %>% 
  bind_rows(paigesteenblock) %>% 
  bind_rows(oliviasmall)

write_csv(miac_timedrops, 'miac_timedrops.csv')