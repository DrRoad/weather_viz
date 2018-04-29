# weather_viz

**Data:** Severe weather events recorded in the [NOAA Storm Events Database](https://www.ncdc.noaa.gov/stormevents/) from 1950-present. 

Thank you to Jonathan Mackrory from Portland Data Science Group for providing this trimmed version (1996-2017, with some cleanup and broader groupings of event types).

**Goal:** Explore different ways to visualize this dataset. 

I first generated a static choropleth showing all weather events in 2016 by state using ggplot with the [fiftystater](https://cran.r-project.org/web/packages/fiftystater/vignettes/fiftystater.html) package.

I next used plotly to generate an interactive choropleth showing all weather events in 2016 by state, with injuries, deaths, and damage in the hover text.

My next goal is to create a Shiny app with more interactive features.

**Results:**

![ggplot](https://github.com/lopierra/weather_viz/blob/master/all_weather_fiftystater.png)

plotly widget can be downloaded [here](https://github.com/lopierra/weather_viz/blob/master/plotly_2016_weather.html)
