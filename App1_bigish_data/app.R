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
library(shinycssloaders)
library(DT)
library(ggplot2)
library(magrittr)
# library(readr)
# library(vroom)
# library(data.table)
# library(dplyr)

# Shiny Server ------------------------------------------------------------
server <- function(input, output) {
  if(data_method == "database") {
    database_server(input,output)
  } else if(data_method == "read_csv") {
    readcsv_server(input,output)
  } else if(data_method == "data.table") {
    datatable_server(input,output)
  } else if(data_method == "vroom") {
    vroom_server(input,output)
  }
}

database_server <- function(input, output) {
  library(RSQLite)
  library(dbplyr)
  library(dplyr)
  
  df <- mtcars
  
  output$mainTable <- renderDT(df,options = list())
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
  titlePanel("Example App: Big[ish] Data Using Databases"),
  fluidRow(
    column(
      width = 5,
      wellPanel(
        DTOutput('mainTable') %>% withSpinner(type = 8)
      )
    ),
    column(
      width = 7,
      h3("Placeholder for Daily Temperature Plot"),
      h3("Placeholder for Weekly Temperature Plot"),
      h3("Placeholder for Monthly Temperature Plot"),
      h3("Placeholder for Decomposition Temperature Plot")
    )
  )
)

shinyApp(ui = ui, server = server)