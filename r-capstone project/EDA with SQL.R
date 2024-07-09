library("RSQLite")
library(dplyr)
library(readr)

conn= dbConnect(RSQLite::SQLite(),"FinalDB_Lab4.sqlite")

df1 <- dbExecute(conn, 
                 "CREATE TABLE SEOUL_BIKE_SHARING (
                                      DATE DATE NOT NULL,
                                      RENTED_BIKE_COUNT INTEGER NOT NULL,
                                      HOUR INTEGER NOT NULL,
                                      TEMPERATURE INTEGER NOT NULL, 
                                      HUMIDITY INTEGER NOT NULL,
                                      WIND_SPEED INTEGER NOT NULL,
                                      VISIBILITY INTEGER NOT NULL,
                                      DEW_POINT_TEMPERATURE INTEGER NOT NULL,
                                      SOLAR_RADIATION INTEGER NOT NULL,
                                      RAINFALL INTEGER NOT NULL,
                                      SNOWFALL INTEGER NOT NULL,
                                      SEASONS  VARCHAR(15) NOT NULL,
                                      HOLIDAY  VARCHAR(20) NOT NULL,
                                      FUNCTIONING_DAY VARCHAR(10) NOT NULL
                                  
                                      )", 
                 errors=FALSE
)

if (df1 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
}



df2 <- dbExecute(conn, 
                 "CREATE TABLE CITIES_WEATHER_FORECAST (
                                      CITY CHAR NOT NULL,
                                      WEATHER CHAR NOT NULL,
                                      VISIBILITY INTEGER NOT NULL,
                                      TEMP INTEGER NOT NULL, 
                                      TEMP_MIN INTEGER NOT NULL,
                                      TEMP_MAX INTEGER NOT NULL,
                                      PRESSURE INTEGER NOT NULL,
                                      HUMIDITY INTEGER NOT NULL,
                                      WIND_SPEED INTEGER NOT NULL,
                                      WIND_DEG INTEGER NOT NULL,
                                      SEASON VARCHAR NOT NULL,
                                      FORECAST_DATETIME DATE NOT NULL
                                      
                                  
                                      )", 
                 errors=FALSE
)

if (df2 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
}




df3 <- dbExecute(conn, 
                 "CREATE TABLE BIKE_SHARING_SYSTEMS (
                                      COUNTRY VARCHAR NOT NULL,
                                      CITY CHAR NOT NULL,
                                      SYSTEM VARCHAR NOT NULL,
                                      BICYCLES INTEGER NOT NULL
                                      
                                     
                                  
                                      )", 
                 errors=FALSE
)

if (df3 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
}



df4 <- dbExecute(conn, 
                 "CREATE TABLE WORLD_CITIES (
                                      CITY_ASCII VARCHAR NOT NULL,
                                      LAT INTEGER NOT NULL,
                                      LNG INTEGER NOT NULL,
                                      COUNTRY VARCHAR NOT NULL,
                                      ISO2 CHAR NOT NULL,
                                      ISO3 CHAR NOT NULL,
                                      ADMIN_NAME VARCHAR NOT NULL,
                                      CAPITAL VARCHAR NOT NULL,
                                      POPULATION INTEGER NOT NULL,
                                      ID INTEGER NOT NULL
                                      
                                
                                  
                                      )", 
                 errors=FALSE
)

if (df4 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
}


seoul_bike_sharing_df = read_csv("D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\secondSet\\seoul_bike_sharing.csv")

cities_weather_forecast_df = read_csv("D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\secondSet\\cities_weather_forecast.csv")

bike_sharing_systems_df = read_csv("D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\secondSet\\bike_sharing_systems.csv")

world_cities_df = read_csv("D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\secondSet\\world_cities.csv")



#Write our dataframes into the created SQL tables
dbWriteTable(conn, "SEOUL_BIKE_SHARING", seoul_bike_sharing_df, overwrite=TRUE, header = TRUE)

dbWriteTable(conn, "CITIES_WEATHER_FORECAST", cities_weather_forecast_df, overwrite=TRUE, header = TRUE)

dbWriteTable(conn, "BIKE_SHARING_SYSTEMS", bike_sharing_systems_df, overwrite=TRUE, header = TRUE)

dbWriteTable(conn, "WORLD_CITIES", world_cities_df, overwrite=TRUE, header = TRUE)



#Task 1 - Record Count
#Determine how many records are in the seoul_bike_sharing dataset.

dbGetQuery(conn, "select count(DATE) from SEOUL_BIKE_SHARING")


#Task 2 - Operational Hours
#Determine how many hours had non-zero rented bike count
dbGetQuery(conn, "SELECT COUNT(DISTINCT strftime('%H',DATE )) AS non_zero_rented_hours
                  FROM SEOUL_BIKE_SHARING
                  WHERE RENTED_BIKE_COUNT > 0;")


#Task 3 - Weather Outlook
#Query the the weather forecast for Seoul over the next 3 hours.
#Recall that the records in the CITIES_WEATHER_FORECAST dataset are 3 hours apart, so we just need the first record from the query.


dbGetQuery(conn, "SELECT * FROM CITIES_WEATHER_FORECAST LIMIT 1")



#Task 4 - Seasons
#Find which seasons are included in the seoul bike sharing dataset.

dbGetQuery(conn, "SELECT DISTINCT (SEASONS) FROM SEOUL_BIKE_SHARING")

#Task 5 - Date Range
#Find the first and last dates in the Seoul Bike Sharing dataset.

dbGetQuery(conn, 'SELECT min(DATE) FIRST_DATE, max(DATE) LAST_DATE FROM SEOUL_BIKE_SHARING')



#Task 6 - Subquery - 'all-time high'
#determine which date and hour had the most bike rentals.

dbGetQuery(conn, 'SELECT DATE, HOUR, RENTED_BIKE_COUNT
FROM SEOUL_BIKE_SHARING
WHERE (DATE, HOUR) IN (
    SELECT DATE, HOUR
    FROM (
        SELECT DATE, HOUR, MAX(RENTED_BIKE_COUNT) AS total_rentals
        FROM SEOUL_BIKE_SHARING
        GROUP BY DATE
        ORDER BY total_rentals DESC
        LIMIT 2
    ) top_date
)
 ')


#Task 7 - Hourly popularity and temperature by season
#Determine the average hourly temperature and the average number of bike rentals per hour over each season. 
#List the top ten results by average bike count.



dbGetQuery(conn, "SELECT 
     HOUR as rental_hour,SEASONS,
    AVG(br.TEMPERATURE) as avg_temperature,
    AVG(br.RENTED_BIKE_COUNT) as avg_bike_count
FROM 
    SEOUL_BIKE_SHARING br
     
                   
GROUP BY br.SEASONS,rental_hour
    
ORDER BY 
    avg_bike_count DESC
LIMIT 10
")

#Task 8 - Rental Seasonality
#Find the average hourly bike count during each season.
#Also include the minimum, maximum, and standard deviation of the hourly bike count for each season.

#Hint : Use the SQRT(AVG(col*col) - AVG(col)*AVG(col) ) function where col refers to your column name for finding the standard deviation

dbGetQuery(conn, "SELECT 
    br.SEASONS as season,
    br.HOUR as rental_hour,
    AVG(br.RENTED_BIKE_COUNT) as avg_bike_count,
    MIN(br.RENTED_BIKE_COUNT) as min_bike_count,
    MAX(br.RENTED_BIKE_COUNT) as max_bike_count,
    SQRT(AVG(br.RENTED_BIKE_COUNT * br.RENTED_BIKE_COUNT) - AVG(br.RENTED_BIKE_COUNT) * AVG(br.RENTED_BIKE_COUNT)) as std_dev_bike_count
FROM 
    SEOUL_BIKE_SHARING br
GROUP BY 
    br.SEASONS, br.HOUR
ORDER BY 
    season, rental_hour
")



#Task 9 - Weather Seasonality
#Consider the weather over each season. On average, what were the TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, DEW_POINT_TEMPERATURE, SOLAR_RADIATION, RAINFALL, and SNOWFALL per season?
 # Include the average bike count as well , and rank the results by average bike count so you can see if it is correlated with the weather at all.
dbGetQuery(conn, "SELECT 
    br.SEASONS as season,
    AVG(br.TEMPERATURE) as avg_temperature,
    AVG(br.HUMIDITY) as avg_humidity,
    AVG(br.WIND_SPEED) as avg_wind_speed,
    AVG(br.VISIBILITY) as avg_visibility,
    AVG(br.DEW_POINT_TEMPERATURE) as avg_dew_point_temperature,
    AVG(br.SOLAR_RADIATION) as avg_solar_radiation,
    AVG(br.RAINFALL) as avg_rainfall,
    AVG(br.SNOWFALL) as avg_snowfall,
    AVG(br.rented_bike_count) as avg_bike_count
FROM 
    SEOUL_BIKE_SHARING br

GROUP BY SEASONS
ORDER BY 
    avg_bike_count DESC;
")



#Task 10 - Total Bike Count and City Info for Seoul
#Use an implicit join across the WORLD_CITIES and the BIKE_SHARING_SYSTEMS tables to determine the total number of bikes available in Seoul, 
#plus the following city information about Seoul: CITY, COUNTRY, LAT, LON, POPULATION, in a single view.
#Notice that in this case, the CITY column will work for the WORLD_CITIES table, but in general you would have to use the CITY_ASCII column.

dbGetQuery(conn, "SELECT 
    wc.CITY,
    wc.COUNTRY,
    wc.LAT,
    wc.LNG,
    wc.POPULATION,
    SUM(bss.BICYCLES) as total_bikes_in_seoul
FROM 
    WORLD_CITIES wc,
    BIKE_SHARING_SYSTEMS bss
WHERE 
    wc.CITY = 'Seoul' AND
    wc.CITY = bss.CITY
GROUP BY 
    wc.CITY, wc.COUNTRY, wc.LAT, wc.LNG, wc.POPULATION;
")



#Task 11 - Find all city names and coordinates with comparable bike scale to Seoul's bike sharing system
#Find all cities with total bike counts between 15000 and 20000. Return the city and country names, plus the coordinates (LAT, LNG),
#population, and number of bicycles for each city.

dbGetQuery(conn, "SELECT 
    wc.CITY,
    wc.COUNTRY,
    wc.LAT,
    wc.LNG,
    wc.POPULATION,
    bss.BICYCLES
FROM 
    WORLD_CITIES wc
JOIN 
    BIKE_SHARING_SYSTEMS bss ON wc.CITY = bss.CITY
WHERE 
    bss.BICYCLES BETWEEN 15000 AND 20000
")



