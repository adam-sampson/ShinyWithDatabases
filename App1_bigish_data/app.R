#******************************************************************************#
# Bigish Data App
# Author: Adam Sampson
# Date: 2019-08-30
# Description:
#   This is an example Shiny app that is used to demonstrate how and why to 
#   use databases as the back-end data repository for Shiny apps
# Prerequisites:
#   In order for the app to work the data sources must first be extracted to the
#   app directory. These are not included in the git repository because of their
#   size. 
# Data Options (not yet decided):
#   https://www.kaggle.com/worldbank/world-development-indicators
#   https://www.kaggle.com/noaa/noaa-global-surface-summary-of-the-day
#   https://www.eia.gov/totalenergy/data/monthly/
#   https://fred.stlouisfed.org/
#   https://www.ncei.noaa.gov/access/search/data-search/global-hourly?bbox=38.486,-85.998,38.024,-85.536&name=Louisville%252C%2520KY%252C%2520USA&sdate=2000-01-01&edate=2019-09-01
#   https://data.louisvilleky.gov/dataset/crime-reports
#******************************************************************************#

# Options and Settings ----------------------------------------------------
# Select method for using data from: database, read_csv, data.table, or vroom
data_method <- "database"


# Global libraries for all servers ----------------------------------------
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)
library(magrittr)
library(sf)
library(leaflet)
library(ggplot2)
# library(ggridges)
library(ggthemes)
# library(readr)
# library(vroom)
# library(data.table)
# library(dplyr)


# Load Shapefile which is slow and hard to store in db --------------------
louisville_shapefile <- readRDS("jeff_co_zip_shp.rds")
louisville_shapefile <- sf::st_transform(louisville_shapefile,4326)
zip_filter <- louisville_shapefile %>%
  select(zip_code = ZCTA5CE10) %>%
  pull(zip_code)

# Shiny Server ------------------------------------------------------------
server <- function(input, output, session) {
  if(data_method == "database") {
    database_server(input,output, session)
  } else if(data_method == "read_csv") {
    readcsv_server(input,output, session)
  } else if(data_method == "data.table") {
    datatable_server(input,output, session)
  } else if(data_method == "vroom") {
    vroom_server(input,output, session)
  }
}

database_server <- function(input, output, session) {

# SQLite specific code and data -------------------------------------------
  library(RSQLite)
  library(dbplyr)
  library(dplyr)
  
  db <- dbConnect(RSQLite::SQLite(), "big_app.sqlite")
  crime_tbl <- tbl(db,'louisvilleCrimeData')
  weather_tbl <- tbl(db,'louisvilleWeatherData')
  comment_tbl <- tbl(db,'comments')
  
  sqlite_numeric_to_date <- function(number.in) {
    as_datetime(as.numeric(number.in),
               origin = "1970-01-01",
               tz = "GMT") # Fix because the data was incorrectly stored in db as GMT instead of EST
  }
  
  date_to_sqlite_numeric <- function(date.in) {
    as.numeric(date.in)
  }
  
  

# Inital settings ---------------------------------------------------------
  # because this is the 2003:2019 dataset
  min_date <- ymd_hms("2003-01-01 00:00:00",tz = "GMT") %>%
    as_date()
  
  # find the max date of data available in both datasets
  max_date <- min(
    crime_tbl %>% 
      select(date_occured) %>% 
      summarize(max = max(date_occured,na.rm = TRUE)) %>% 
      collect() %>%
      sqlite_numeric_to_date(),
    weather_tbl %>%
      select(date_time) %>%
      summarise(max = max(date_time,na.rm = TRUE)) %>%
      collect() %>%
      sqlite_numeric_to_date()
  ) %>%
    as_date()
  
  uniqueCrimeList <- crime_tbl %>%
    select(crime_type) %>%
    distinct() %>%
    collect() %>%
    pull(crime_type)
  

# Render UI elements dynamically ------------------------------------------
  output$dynamicControls <- renderUI({
    tagList(
      dateRangeInput(
        "selectedDateRange",
        "Select Date Range:",
        start = min_date,
        end = max_date,
        min = min_date,
        max = max_date,
        format = "M dd, yyyy"
      ),
      selectInput(
        "crimeSelection",
        "Select Crime[s]:",
        # choices = c("All",uniqueCrimeList),
        choices = uniqueCrimeList,
        selected = c(NULL),
        multiple = TRUE
      ),
      div(style="text-align:center","Note: empty selection == 'All'")
    )
  })
  
  output$commentEntryControls <- renderUI({
    fluidRow(
      column(
        width = 3,
        selectInput("commentCrimeType",
                    "Crime Type",
                    choices = uniqueCrimeList,
                    selected = NULL)
      ),
      column(
        width = 2,
        textInput("commentUserName",
                  "User Name")
      ),
      column(
        width = 3,
        textAreaInput("commentText",
                  "Comment")
      ),
      column(
        width = 2,
        radioGroupButtons("readyToSubmit",
                      "Ready to Submit",
                      choices = c("No","Yes"),
                      status = "primary")
      )
    )
  })

# Filter and Calculate data -----------------------------------------------
  selectedStartDate <- reactive({
    req(input$selectedDateRange)
    if(is.null(input$selectedDateRange[[1]])){return(as_datetime(min_date))}
    if(is.na(input$selectedDateRange[[1]])){return(as_datetime(min_date))}
    return(as_datetime(input$selectedDateRange[[1]]))
  })
  
  selectedEndDate <- reactive({
    if(is.null(input$selectedDateRange[[2]])){return(as_datetime(max_date))}
    if(is.na(input$selectedDateRange[[2]])){return(as_datetime(max_date))}
    return(as_datetime(input$selectedDateRange[[2]]))
  })
  
  selectedCrimeTypes <- eventReactive(input$crimeSelection,{
    # if(is.null(input$crimeSelection)){ return("All") } # warning if multiple...
    if(length(input$crimeSelection)==0){ return("All") }
    return(input$crimeSelection)
  },
  ignoreNULL = FALSE)
  
  filteredCrimeData <- eventReactive(list(selectedCrimeTypes(),selectedStartDate(),selectedEndDate()),{
    # declare local variables to avoid syntax issues with variable() in dbplyr
    crime_filter <- selectedCrimeTypes() 
    start_date <- date_to_sqlite_numeric(selectedStartDate())
    end_date <- date_to_sqlite_numeric(selectedEndDate())
    
    # Iff a crime filter is required prepare it.
    if(crime_filter[[1]] != "All"){
      out <- crime_tbl %>%
        filter(crime_type %in% crime_filter)
    } else {
      out <- crime_tbl # No filter needed if "All"
    }
    
    # Finish preparing filters and then compute
    out <- out %>%
      select(date_occured,year,month,day,hour,crime_type,zip_code) %>%
      filter(zip_code %in% zip_filter) %>%
      filter(date_occured >= start_date) %>%
      filter(date_occured <= end_date) %>%
      compute()
    return(out)
  })
  
  filteredWeatherData <- reactive({
    # start_date <- date_to_sqlite_numeric(selectedStartDate())
    # end_date <- date_to_sqlite_numeric(selectedEndDate())
    # 
    # out <- weather_tbl %>%
    #   filter(date_occured >= start_date) %>%
    #   filter(date_occured <= end_date) %>%
    #   compute()
    # return(out)
    weather_tbl
  })
  
  crimeDataWithTemp <- eventReactive(filteredCrimeData(),{
    out <- filteredCrimeData() %>%
      left_join(
        weather_tbl %>%
          select(year,month,day,hour,air_temp_c),
        by = c("year" = "year",
               "month" = "month",
               "day" = "day",
               "hour" = "hour")
      )
    return(out)
  })
  

# Perform map plotting ----------------------------------------------------
  output$louisvilleMap <- renderLeaflet({
    shapesToPlot <- louisville_shapefile %>%
      left_join(
        filteredCrimeData() %>%
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


# Perform Line Plotting ---------------------------------------------------
  output$crimeTempLineplot <- renderPlot({
    data <- crimeDataWithTemp() %>%
      mutate(air_temp_c = floor(air_temp_c)) %>%
      group_by(crime_type,air_temp_c) %>%
      summarise(crimes = n()) %>%
      ungroup() %>%
      collect()
    # str(data)
    
    outplot <- data %>%
      ggplot(aes(x = air_temp_c,y = crimes)) +
      # geom_ridgeline()
      geom_line(aes(color = crime_type)) + 
      theme_minimal() +
      ggthemes::theme_fivethirtyeight()
    return(outplot)
  })
  

# Perform Comment Charting ------------------------------------------------
  # n_comments <- reactive({
  #   if(length(comment_data()) == 0){ return(nrow(comment_tbl)) }
  #   return(nrow(comment_data))
  # })
  
  comment_data <- reactivePoll(
    5000,
    session,
    checkFunc = function(){
      comment_tbl %>% tally() %>% pull()
    },
    valueFunc = function(){
      comment_tbl %>% collect()
    }
  )
  
  output$commentTable <- renderDataTable({
   comment_data() %>%
   # comment_tbl %>%
   #   collect() %>%
     rename(`Submit Time` = datetime,
            `Crime Type` = CrimeType,
            `Username` = UserName,
            `Comment` = comment) %>%
     datatable()
 })
  
# Perform addition of new comments ----------------------------------------
  validateComment <- function(crimeTypeIn,userNameIn,commentIn,readyIn){
    if(readyIn != "Yes"){
      return("Not Ready")
    }
    if(str_length(userNameIn) < 4 | str_detect(userNameIn,"[^A-Za-z0-9]")){
      return("Invalid User Name")
    }
    if(str_length(commentIn) < 4 | str_detect(commentIn,"[^A-Za-z0-9 \\!\\,\\-\\.]")){
      return("Invalid Comment")
    }
    return("Ready")
  }
  
  observeEvent(list(input$commentCrimeType,
                    input$commentUserName,
                    input$commentText,
                    input$readyToSubmit),{
    req(input$commentCrimeType,
        input$commentUserName,
        input$commentText,
        input$readyToSubmit)
    check_comment <- validateComment(input$commentCrimeType,
                                     input$commentUserName,
                                     input$commentText,
                                     input$readyToSubmit)
    if(check_comment == "Invalid User Name"){
      showModal(
        modalDialog(
          title = "Invalid User Name",
          "Comment must by > 4 characters long, and contain only numbers and letters.",
          easyClose = TRUE
        )
      )
    } else if(check_comment == "Invalid Comment"){
      showModal(
        modalDialog(
          title = "Invalid Comment",
          "User name must by > 4 characters long, and contain only numbers, letters, spaces, hyphens, periods, commas, and explanation points.",
          easyClose = TRUE
        )
      )
    } else if(check_comment == "Ready"){
      toSave <- tibble(
        datetime = Sys.time(),
        CrimeType = input$commentCrimeType,
        UserName = input$commentUserName,
        comment = input$commentText
      )
      dbWriteTable(db,'comments',toSave,append = TRUE)
      showModal(
        modalDialog(
          title = "Comment Uploaded",
          "Your comment has been uploaded. Thanks!",
          easyClose = TRUE
        )
      )
    }
  })
}

readcsv_server <- function(input, output) {
  library(dplyr)
  library(readr)
  
  
}

datatable_server <- function(input, output) {
  library(data.table)
  
}

vroom_server <- function(input, output) {
  library(dplyr)
  library(vroom)
  
}


# Shiny UI ----------------------------------------------------------------
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = 'Shiny w/ Databases'),
    dashboardSidebar(
      uiOutput("dynamicControls"),
      br(),
      div(
          # style="display:inline-block;text-align: center;",
          submitButton(text = "Apply Changes",width = '100%')
      )
    ),
    dashboardBody(
      tabsetPanel(
        tabPanel(
          "Map",
          leafletOutput('louisvilleMap') %>% withSpinner(type = 8)
        ),
        tabPanel(
          "Plots",
          br(),
          plotOutput('crimeTempLineplot') %>% withSpinner(type = 8)
        ),
        tabPanel(
          "Comments",
          br(),
          h3("Previous comments"),
          dataTableOutput("commentTable") %>% withSpinner(type = 8),
          br(),
          h3("Enter your own comment:"),
          uiOutput("commentEntryControls")
        )
      )
    )
  )
)

shinyApp(ui = ui, server = server)