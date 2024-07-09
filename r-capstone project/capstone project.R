library(rvest)
library(httr)

# Define the URL
url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"

# Read the HTML content of the page
page <- read_html(url)


# Extract the table
table <- page %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE)

# Convert the table to a dataframe
df <- as.data.frame(table)

#summary of the bike sharing system data frame
summary(df)

#exporting the dataframe as a CSV file

write.csv(df, "D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_bike_sharing_systems.csv", row.names = FALSE)





#Coding Practice: Get the current weather data for a city using OpenWeather API
#First import httr library

# URL for Current Weather API
current_weather_url <- 'https://api.openweathermap.org/data/2.5/weather'


#Next, let's create a list to hold URL parameters for current weather API

# need to be replaced by your real API key
your_api_key <- "786492856e74afacdd517464a9bae978"
# Input `q` is the city name
# Input `appid` is your API KEY, 
# Input `units` are preferred units such as Metric or Imperial

current_query <- list(q = "Seoul", appid = your_api_key, units="metric")


#Now we can make a HTTP request to the current weather API

response <- GET(current_weather_url, query=current_query)

#If we check the response type, we can see it is in JSON format
http_type(response)



#To read the JSON HTTP response, you can use the content() function to parse it as a named list in R.

json_result <- content(response, as="parsed")

#If you use the class() function, you can see it is a R List object

class(json_result)
#Now let's print the JSON result.

json_result

# Create some empty vectors to hold data temporarily
weather <- c()
visibility <- c()
temp <- c()
temp_min <- c()
temp_max <- c()
pressure <- c()
humidity <- c()
wind_speed <- c()
wind_deg <- c()


# $weather is also a list with one element, its $main element indicates the weather status such as clear or rain
weather <- c(weather, json_result$weather[[1]]$main)
# Get Visibility
visibility <- c(visibility, json_result$visibility)
# Get current temperature 
temp <- c(temp, json_result$main$temp)
# Get min temperature 
temp_min <- c(temp_min, json_result$main$temp_min)
# Get max temperature 
temp_max <- c(temp_max, json_result$main$temp_max)
# Get pressure
pressure <- c(pressure, json_result$main$pressure)
# Get humidity
humidity <- c(humidity, json_result$main$humidity)
# Get wind speed
wind_speed <- c(wind_speed, json_result$wind$speed)
# Get wind direction
wind_deg <- c(wind_deg, json_result$wind$deg)


# Combine all vectors
weather_data_frame <- data.frame(weather=weather, 
                                 visibility=visibility, 
                                 temp=temp, 
                                 temp_min=temp_min, 
                                 temp_max=temp_max, 
                                 pressure=pressure, 
                                 humidity=humidity, 
                                 wind_speed=wind_speed, 
                                 wind_deg=wind_deg)


# Check the generated data frame
print(weather_data_frame)


#TASK: Get 5-day weather forecasts for a list of cities using the OpenWeather API
#TODO: Write a function to return a data frame containing 5-day weather forecasts for a list of cities

# Create some empty vectors to hold data temporarily

# City name column
city <- c()
# Weather column, rainy or cloudy, etc
weather <- c()
# Sky visibility column
visibility <- c()
# Current temperature column
temp <- c()
# Max temperature column
temp_min <- c()
# Min temperature column
temp_max <- c()
# Pressure column
pressure <- c()
# Humidity column
humidity <- c()
# Wind speed column
wind_speed <- c()
# Wind direction column
wind_deg <- c()
# Forecast timestamp
forecast_datetime <- c()
# Season column
# Note that for season, you can hard code a season value from levels Spring, Summer, Autumn, and Winter based on your current month.
season <- c()






# Get forecast data for a given city list
get_weather_forecaset_by_cities <- function(city_names){
  df <- data.frame()
  for (city_name in city_names){
    # Forecast API URL
    forecast_url <- 'https://api.openweathermap.org/data/2.5/forecast'
    # Create query parameters
    #your_api_key <- "786492856e74afacdd517464a9bae978"
    forecast_query <- list(q = city_name, appid = "786492856e74afacdd517464a9bae978", units="metric")
    # Make HTTP GET call for the given city
    
    forecast_response <- GET(forecast_url, query=forecast_query)
    # Note that the 5-day forecast JSON result is a list of lists. You can print the reponse to check the results
    #results <- json_list$list
    
    #To read the JSON HTTP response, you can use the content() function to parse it as a named list in R.
    
    json_results <- content(forecast_response, "parsed")
    #responses[[city]] <- weather_data
    
    # Loop the json result
    for(result in json_results) {
      city <- c(city, city_name)
      weather <- c(weather,weather[[1]]$main)
      # Get Visibility
      visibility <- c(visibility,visibility)
      # Get current temperature 
      temp <- c(temp,main$temp)
      # Get min temperature 
      temp_min <- c(temp_min, main$temp_min)
      # Get max temperature 
      temp_max <- c(temp_max, main$temp_max)
      # Get pressure
      pressure <- c(pressure, main$pressure)
      # Get humidity
      humidity <- c(humidity, main$humidity)
      # Get wind speed
      wind_speed <- c(wind_speed, wind$speed)
      # Get wind direction
      wind_deg <- c(wind_deg, wind$deg)
    }
    
    # Add the R Lists into a data frame
    # Combine all vectors
    allcities_weather_data_frame <- data.frame(
                                      
                                      weather=weather, 
                                      
                                     visibility=visibility, 
                                     temp=temp, 
                                     temp_min=temp_min, 
                                     temp_max=temp_max, 
                                     pressure=pressure, 
                                     humidity=humidity, 
                                     wind_speed=wind_speed, 
                                     wind_deg=wind_deg)
    
    
    
    
  }
  
  # Return a data frame
  return(allcities_weather_data_frame)
  
}
cities <- c("Seoul", "Washington, D.C.", "Paris", "Suzhou")
cities_weather_df <- get_weather_forecaset_by_cities(cities)




