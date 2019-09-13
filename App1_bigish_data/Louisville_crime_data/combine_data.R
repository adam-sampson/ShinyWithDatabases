# Combine Data obtained from https://data.louisvilleky.gov/dataset/crime-reports

library(vroom)
library(tidyverse)

data_dir <- "./App1_bigish_data/Louisville_crime_data/"
file_list <- list.files(data_dir,pattern = "Crime_Data_\\d{4}.+csv",full.names = TRUE)

data <- vroom(file_list,col_types =  cols(.default = "c"),delim = ",")

vroom_write(data,
            path = "./App1_bigish_data/Louisville_crime_data/Louisville_crime_data.csv",
            delim = ",",
            quote = "all")
