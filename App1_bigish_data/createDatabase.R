# Data for the apps is received in csv format. In order to use
# the data as a database it first needs to be uploaded to the
# db one time.

# Requirements:
# ./App1_bigish_data/Louisville_crime_data/Louisville_crime_data.csv

# Assuming SQLite will be used. Change this section if you wish
# to use a db on a SQL server instead.

library(RSQLite)
library(vroom)
library(tidyverse)
library(lubridate)

# Create an SQLite database -----------------------------------------------
big_app_db <- dbConnect(RSQLite::SQLite(), "./big_app.sqlite")

# Load data into R --------------------------------------------------------
crime_data <- vroom("./Louisville_crime_data/Louisville_crime_data.csv",
                    .name_repair = snakecase::to_snake_case,
                    col_types = cols(.default = "c"))

# Get weather data from ftp server ----------------------------------------
# ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/2000/
# 72423513810 - bowman field airport
file_list <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/",
                    2000:2019,
                    "/",
                    "724235-13810-",
                    2000:2019,
                    ".gz")
# 72423599999 - bowman fld
file_list <- c(file_list,
               paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/",
                    2000:2019,
                    "/",
                    "724235-99999-",
                    2000:2019,
                    ".gz")
)

for(file in file_list){
  try(download.file(file,str_replace(file,"ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/\\d{4}/","./Louisville_hourly_weather_data/")))
}

file_list <- list.files("./Louisville_hourly_weather_data/",
                        ".gz",
                        full.names = TRUE)

weather_data <- NULL
for(file in file_list){
  zz=gzfile(file,'rt')  
  dat=read.table(zz,header=F,stringsAsFactors = FALSE)
  weather_data <- bind_rows(
    weather_data,
    dat
  )
}
colnames(weather_data) <- c(
  "year",
  "month",
  "day",
  "hour",
  "air_temp_c",
  "dew_point_temp_c",
  "sea_level_pressure_hpasc",
  "wind_dir_deg",
  "wind_speed_mps",
  "sky_coverage_code",
  "precip_one_hour_mm",
  "precip_six_hour_mm"
)

weather_data <- weather_data %>%
  mutate(
    air_temp_c = ifelse(air_temp_c=="-9999",NA,air_temp_c/10),
    dew_point_temp_c = ifelse(dew_point_temp_c=="-9999",NA,dew_point_temp_c/10),
    sea_level_pressure_hpasc = ifelse(sea_level_pressure_hpasc=="-9999",NA,sea_level_pressure_hpasc/10),
    wind_dir_deg = ifelse(wind_dir_deg=="-9999",NA,wind_dir_deg/1),
    wind_speed_mps = ifelse(wind_speed_mps=="-9999",NA,wind_speed_mps/10),
    sky_coverage_code = ifelse(sky_coverage_code=="-9999",NA,sky_coverage_code),
    precip_one_hour_mm = ifelse(precip_one_hour_mm=="-9999",NA,precip_one_hour_mm/10),
    precip_six_hour_mm = ifelse(precip_six_hour_mm=="-9999",NA,precip_six_hour_mm/10)
  )

write_csv(weather_data,"./Louisville_hourly_weather_data/Bowman_Field_Weather.csv")

weather_data <- read_csv("./Louisville_hourly_weather_data/Bowman_Field_Weather.csv")
weather_data <- weather_data %>%
  mutate(date_time = ymd_hms(paste(year,month,day,hour,"0","0",sep = "-"))) %>%
  select(date_time,everything())
  
# Load data into database tables ------------------------------------------
# NOTE: dbWriteTable performs a bulk load and is faster over a network than
#    copy_to as of January 2019. This may change in updates to copy_to.
crime_data <- crime_data %>%
  mutate(
    year = year(date_occured),
    month = month(date_occured),
    day = day(date_occured),
    hour = hour(date_occured)
  ) %>%
  select(incident_number:date_occured,year,month,day,hour,everything())

crime_data <- crime_data %>%
  mutate(
    date_reported = ymd_hms(date_reported),
    date_occured = ymd_hms(date_occured)
  )

# big_app_db %>% dbExecute("DROP TABLE louisvilleCrimeData;")
big_app_db %>%
  dbWriteTable('louisvilleCrimeData',crime_data)
big_app_db %>%
  dbExecute("CREATE INDEX crime_inc_index ON louisvilleCrimeData(incident_number);")
big_app_db %>%
  dbExecute("CREATE INDEX crime_date_index ON louisvilleCrimeData(date_occured);")
# big_app_db %>%
#   dbExecute("CREATE INDEX crime_date_index ON louisvilleCrimeData(date_occured);")
big_app_db %>%
  dbExecute("CREATE INDEX crime_type_index ON louisvilleCrimeData(crime_type);")
big_app_db %>%
  dbExecute("CREATE INDEX crime_zip_index ON louisvilleCrimeData(zip_code);")

# big_app_db %>% dbExecute("DROP TABLE louisvilleWeatherData;")
# big_app_db %>%
#   copy_to(weather_data,'louisvilleWeatherData')
# weather_col_names <- c("DATETIME","INT","INT","INT","INT","NUM","NUM","NUM","NUM","NUM","INT","NUM","NUM")
# names(weather_col_names) <- colnames(weather_data)
# big_app_db %>%
#   dbCreateTable(
#     'louisvilleWeatherData',
#     weather_col_names
#   )
big_app_db %>%
  dbWriteTable('louisvilleWeatherData',weather_data)
big_app_db %>%
  dbExecute("CREATE INDEX date_time_index ON louisvilleWeatherData(date_time);")

comments <- tibble(
  datetime = NA,
  CrimeType = NA,
  UserName = NA,
  comment = NA
)
big_app_db %>%
  dbWriteTable('comments',comments)

# Verify data is uploaded -------------------------------------------------
tbl(big_app_db,'louisvilleCrimeData')
tbl(big_app_db,'louisvilleWeatherData')
tbl(big_app_db,'comments')

# Close database connections ----------------------------------------------
dbDisconnect(big_app_db)
unlink("./App1_bigish_data/big_app.sqlite")
