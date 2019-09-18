library(tidyverse)
library(vroom)
library(RSQLite)
library(lubridate)
library(ggplot2)
library(sf)
library(leaflet)
library(ggplot2)
library(ggthemes)

memory.size()

# SQLite (locally hosted) -------------------------------------------------
print("Init")
system.time({
  # Initialize Time
  # Load Shapefile
  louisville_shapefile <- readRDS("jeff_co_zip_shp.rds")
  louisville_shapefile <- sf::st_transform(louisville_shapefile,4326)
  zip_filter <- louisville_shapefile %>%
    select(zip_code = ZCTA5CE10) %>%
    pull(zip_code)
  
  db <- dbConnect(RSQLite::SQLite(), "big_app.sqlite")
  crime_tbl <- tbl(db,'louisvilleCrimeData')
  weather_tbl <- tbl(db,'louisvilleWeatherData')
  # comment_tbl <- tbl(db,'comments')
  
  # sqlite_numeric_to_date <- function(number.in) {
  #   as_datetime(as.numeric(number.in),
  #               origin = "1970-01-01",
  #               tz = "GMT") # Fix because the data was incorrectly stored in db as GMT instead of EST
  # }
  # 
  # date_to_sqlite_numeric <- function(date.in) {
  #   as.numeric(date.in)
  # }
})
memory.size()

print("Filter Crime Data")
system.time({
  # Filter Crime Data Time
  filteredCrimeData <- crime_tbl %>%
    inner_join(tibble(crime_type = "DUI"),copy = TRUE) %>%
    select(date_occured,year,month,day,hour,crime_type,zip_code) %>%
    inner_join(tibble(zip_code = zip_filter),copy = TRUE) %>%
    compute()
})
memory.size()

print("Plot Map")
system.time({
  # Plot Map Time
  shapesToPlot <- louisville_shapefile %>%
    left_join(
      filteredCrimeData %>%
        select(zip_code) %>%
        group_by(zip_code) %>%
        summarise(crimes = n()) %>%
        collect(),
      by = c("ZCTA5CE10" = "zip_code")
    )
  pal <- colorNumeric("OrRd",
                      domain = shapesToPlot$crimes,
                      reverse = FALSE)
  leaflet::leaflet(shapesToPlot) %>% 
    # addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(fillOpacity = 0.75,
                color = ~pal(crimes), 
                weight = 1)
})
memory.size()

print("Join Weather Data")
system.time({
  # Join to Weather Data Time
  crimeDataWithTemp <- filteredCrimeData %>%
    left_join(
      weather_tbl %>%
        select(year,month,day,hour,air_temp_c),
      by = c("year" = "year",
             "month" = "month",
             "day" = "day",
             "hour" = "hour")
    ) %>%
    mutate(air_temp_c = floor(air_temp_c)) %>%
    group_by(crime_type,air_temp_c) %>%
    summarise(crimes = n()) %>%
    ungroup() %>%
    collect()
})
memory.size()

print("Plot Lines")
system.time({
  # Plot Lines Time
  crimeDataWithTemp %>%
    ggplot(aes(x = air_temp_c,y = crimes)) +
    # geom_ridgeline()
    geom_line(aes(color = crime_type)) + 
    theme_minimal() +
    ggthemes::theme_fivethirtyeight()
})
memory.size()


# readr -------------------------------------------------------------------
print("Init")
system.time({
  # Initialize Time
  # Load Shapefile
  louisville_shapefile <- readRDS("jeff_co_zip_shp.rds")
  louisville_shapefile <- sf::st_transform(louisville_shapefile,4326)
  zip_filter <- louisville_shapefile %>%
    select(zip_code = ZCTA5CE10) %>%
    pull(zip_code)
  
  crime_tbl <- read_csv("./Louisville_crime_data/Louisville_crime_data.csv",
                        col_types = cols(.default = "c"))
  weather_tbl <- read_csv("./Louisville_hourly_weather_data/Bowman_Field_Weather.csv",
                          col_types = cols(.default = "c"))
  
  colnames(crime_tbl) <- snakecase::to_snake_case(colnames(crime_tbl))
  colnames(weather_tbl) <- snakecase::to_snake_case(colnames(weather_tbl))
  # comment_tbl <- tbl(db,'comments')
})
memory.size()

print("Filter Crime Data")
system.time({
  # Filter Crime Data Time
  filteredCrimeData <- crime_tbl %>%
    inner_join(tibble(crime_type = "DUI")) %>%
    select(date_occured,crime_type,zip_code) %>%
    mutate(
      year = year(date_occured) %>% as.character(),
      month = month(date_occured) %>% as.character(),
      day = day(date_occured) %>% as.character(),
      hour = hour(date_occured) %>% as.character()
    ) %>%
    inner_join(tibble(zip_code = zip_filter)) %>%
    compute()
})
memory.size()

print("Plot Map")
system.time({
  # Plot Map Time
  shapesToPlot <- louisville_shapefile %>%
    left_join(
      filteredCrimeData %>%
        select(zip_code) %>%
        group_by(zip_code) %>%
        summarise(crimes = n()) %>%
        collect(),
      by = c("ZCTA5CE10" = "zip_code")
    )
  pal <- colorNumeric("OrRd",
                      domain = shapesToPlot$crimes,
                      reverse = FALSE)
  leaflet::leaflet(shapesToPlot) %>% 
    # addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(fillOpacity = 0.75,
                color = ~pal(crimes), 
                weight = 1)
})
memory.size()

print("Join Weather Data")
system.time({
  # Join to Weather Data Time
  crimeDataWithTemp <- filteredCrimeData %>%
    left_join(
      weather_tbl %>%
        select(year,month,day,hour,air_temp_c),
      by = c("year" = "year",
             "month" = "month",
             "day" = "day",
             "hour" = "hour")
    ) %>%
    mutate(air_temp_c = floor(as.numeric(air_temp_c))) %>%
    group_by(crime_type,air_temp_c) %>%
    summarise(crimes = n()) %>%
    ungroup() %>%
    collect()
})
memory.size()

print("Plot Lines")
system.time({
  # Plot Lines Time
  crimeDataWithTemp %>%
    ggplot(aes(x = air_temp_c,y = crimes)) +
    # geom_ridgeline()
    geom_line(aes(color = crime_type)) + 
    theme_minimal() +
    ggthemes::theme_fivethirtyeight()
})
memory.size()

# vroom -------------------------------------------------------------------
print("Init")
system.time({
  # Initialize Time
  # Load Shapefile
  louisville_shapefile <- readRDS("jeff_co_zip_shp.rds")
  louisville_shapefile <- sf::st_transform(louisville_shapefile,4326)
  zip_filter <- louisville_shapefile %>%
    select(zip_code = ZCTA5CE10) %>%
    pull(zip_code)
  
  crime_tbl <- vroom("./Louisville_crime_data/Louisville_crime_data.csv",
                        col_types = cols(.default = "c"))
  weather_tbl <- vroom("./Louisville_hourly_weather_data/Bowman_Field_Weather.csv",
                          col_types = cols(.default = "c"))
  
  colnames(crime_tbl) <- snakecase::to_snake_case(colnames(crime_tbl))
  colnames(weather_tbl) <- snakecase::to_snake_case(colnames(weather_tbl))
  # comment_tbl <- tbl(db,'comments')
})
memory.size()

print("Filter Crime Data")
system.time({
  # Filter Crime Data Time
  filteredCrimeData <- crime_tbl %>%
    inner_join(tibble(crime_type = "DUI")) %>%
    select(date_occured,crime_type,zip_code) %>%
    mutate(
      year = year(date_occured) %>% as.character(),
      month = month(date_occured) %>% as.character(),
      day = day(date_occured) %>% as.character(),
      hour = hour(date_occured) %>% as.character()
    ) %>%
    inner_join(tibble(zip_code = zip_filter)) %>%
    compute()
})
memory.size()

print("Plot Map")
system.time({
  # Plot Map Time
  shapesToPlot <- louisville_shapefile %>%
    left_join(
      filteredCrimeData %>%
        select(zip_code) %>%
        group_by(zip_code) %>%
        summarise(crimes = n()) %>%
        collect(),
      by = c("ZCTA5CE10" = "zip_code")
    )
  pal <- colorNumeric("OrRd",
                      domain = shapesToPlot$crimes,
                      reverse = FALSE)
  leaflet::leaflet(shapesToPlot) %>% 
    # addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(fillOpacity = 0.75,
                color = ~pal(crimes), 
                weight = 1)
})
memory.size()

print("Join Weather Data")
system.time({
  # Join to Weather Data Time
  crimeDataWithTemp <- filteredCrimeData %>%
    left_join(
      weather_tbl %>%
        select(year,month,day,hour,air_temp_c),
      by = c("year" = "year",
             "month" = "month",
             "day" = "day",
             "hour" = "hour")
    ) %>%
    mutate(air_temp_c = floor(as.numeric(air_temp_c))) %>%
    group_by(crime_type,air_temp_c) %>%
    summarise(crimes = n()) %>%
    ungroup() %>%
    collect()
})
memory.size()

print("Plot Lines")
system.time({
  # Plot Lines Time
  crimeDataWithTemp %>%
    ggplot(aes(x = air_temp_c,y = crimes)) +
    # geom_ridgeline()
    geom_line(aes(color = crime_type)) + 
    theme_minimal() +
    ggthemes::theme_fivethirtyeight()
})
memory.size()
