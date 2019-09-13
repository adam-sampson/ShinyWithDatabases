## App 1: Bigish Data

This README details some of the information you will need to get the app running. Due to file sizes the data is not stored in the GIT repository with the code for the app. The data will need to be collected from the source and converted to appropriate files.

## Data Sources

### Louisville Crime Data

Get the Louisville crime data from `https://data.louisvilleky.gov/dataset/crime-reports`. From this page download all of the .csv files to the Louisville_crime_data directory (2003 to 2019). 

Once you have all the .csv files in the folder run the combine_data.R script to combine the data into a single .csv file. After that the app files need the `Louisville_crime_data.csv` file and the database file (depending on which app file you run).

### NOAA Hourly Weather Data

NOAA hourly weather data will be downloaded by the database creation step. You will need the make sure the `Louisville_hourly_weather_data` folder exists in the proper location so that folder can store data appropriately. 

### SQLite database

In order to create a database for this project you will need the most recent version of RSQLite. You will need to install RSQlite, then reboot your computer, then verify that you can load the library, then verify that the library version is new.

Next, we will need to run `createDatabase.R` to add the raw data to the SQLite database file. 

NOTE: This database file should be run off of the local machine (not over a network drive) if at all possible. SQLite works best locally. If you need to run over a network it is recommended to set up a SQL server such as MySQL, SQL Server, or PostgreSQL. In order to use a different database simply change the `dbConnect()` call to point to the appropriate database.

