# MIAC Swim Analysis by Mathew Krogman and Ethan Chan

Link to published app: https://m-krogman.shinyapps.io/final-miac-swim/

# Technical analysis of project:

Our project uses data from Swimcloud in order to answer a number of research questions about swimming in the Minnesota Intercollegiate Athletic Conference (MIAC). Data was scraped extensively using `rvest`, with visualizations made using `tidyverse` tools.

## Description of Variables (across all data sets):

- Swimmer: Every individual in the conference
- Team: The school that each swimmer attends
- Gender: What gender each swimmer belongs to (for our sake only Male and Female)
- Points: The power points ranking for each swimmer/team
- Events: The events swum by each of the top 3 swimmers on each team at the conference meet
- Times: The time swum in each of the events from the above variable at the conference meet
- Times2: The best time in each of the events from `Events` for each swimmer from BEFORE the conference meet

## Research Questions:

### Question 1:

To figure out which swimmers improved the most because of their taper, we acquired power points for all swimmers from before and after the conference meet in the 2024-2025 season. We created a new variable that compares the pre- and post-conference points, and placed them together on a column chart. You can add or remove teams to get a better idea of each individual's improvement.

### Question 2:

We separated all swimmers from 2021-2025 into their respective seasons and class years, so that they could be placed into a scatter plot which shows how fast the fifth-years were relative to other class years. This required the class year variable to be modified, as fifth years were stored as en-dashes in the original source.

### Question 3:

This question required the use of joins to figure out which swimmers swam in in the 2019-2020, 2020-2021, and 2021-2022 seasons, to track their points through the COVID-19 Pandemic. Once that was done, they could be filtered by team and put into boxplots.

### Question 4:

We created a lagging variable to track the relative change in points between men's and women's teams from the same schools in the conference across the years. These are present in the second time series plot. The first time series involved the creation of summary objects to get an average points value for the entire conference. When removing teams, the impact of each team can be seen.

### Question 5:

This question creates a bootsrap using `sample`, to make a random team using individuals from the selected year.

### Question 6:

To figure out the time drops each of the top 3 individuals, we transformed character data into numeric data, with new variables containing the time drops. We took count of the number of swimmers in each event, which is depicted in the column chart. The scatterplot shows individual time drops at the conference meet, and you can select the event!

## Shiny App Integration

We answered every research question within our shiny app. Each research questions was answered on a different tab of the shiny app. On many of the tabs, we added selectInput and checkboxInput options to allow the user to look at different season, gender, and also different teams. We also added a renderText description below every plot in our Shiny app addressing what question we were trying to answer.

## Final Project Rubric

# Successful

All boxes should be checked

- [x] Acquire data from at least 2 sources
- [x] Consider the who, what, when, why, and how of your datasets
- [x] Work with a type of data that was not accessible to you as a Stat120, 230, or 250 student (text, spatial, network, date, etc)
- [x] Demonstrate proficiency with joining data
- [x] Demonstrate proficiency with tidying data
- [x] Demonstrate proficiency using non-numeric data types (text, spatial, date time, factor)
- [x] Create high-quality, customized graphics using R or ggplot
- [x] Graphs contain interactive components (plotly, leaflet, etc)
- [x] Product is published online as an rpubs or shinyapps website and is pitched towards a public audience
- [x] Product meets high submission quality standards:
  - [x] No grammatical mistakes, spelling mistakes, or typos
  - [x] All graphs are readable with appropriate labels and titles
  - [x] Rendered document does not contain any unnecessary content (package loading messages, warnings, etc.)
  - [x] Graphs have been customized and are appropriate and readable with the theme of the document
- [x] All group members have a commit history on github
- [x] Code is well-document and clean
- [x] Project repo is organized
- [x] README contains a link to your published project, a technical summary of what you did, and a self-evaluation using this rubric

# Excellent

## Must be checked

- [x] Meets very high submission quality standards: Final product is polished, professional, and customized


## Two of three must be checked

- [x] Acquire data using one of the advanced techniques discussed in class (scraping, API, database, iterating over files in a folder, etc.)
- [x] Significant portion of the project uses non-numeric data types (e.g. maps that use lat/long location; text analysis; etc) **OR** Project relied on a significant amount of joining or combining data
- [x] Product is an interactive shiny app
