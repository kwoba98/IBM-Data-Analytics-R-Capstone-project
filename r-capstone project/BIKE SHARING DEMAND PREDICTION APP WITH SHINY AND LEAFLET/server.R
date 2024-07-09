# Install and import required libraries

# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("D:\\data analysis\\data Viz with r\\r-capstone project\\BIKE SHARING DEMAND PREDICTION APP WITH SHINY AND LEAFLET\\model_prediction.R")

# select drop down list to select city
list_cities=  c("All", "Seoul", "Suzhou", "London", "New York", "Paris")
test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}





# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()

  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  
  
  #city_weather_bike_df$FORECASTDATETIME <- as.Date(city_weather_bike_df$FORECASTDATETIME)
  # Assuming 'city_weather_bike_df' has a DATETIME column
  # Convert DATETIME to a proper date-time format (if it's not already)
  city_weather_bike_df$FORECASTDATETIME <- as.POSIXct(city_weather_bike_df$FORECASTDATETIME)
  
  # Add a new column 'hourly'
  city_weather_bike_df$hourly <- format(city_weather_bike_df$FORECASTDATETIME, "%H")
  
  
  print(city_weather_bike_df)
  # Group by City and find the maximum Bike_Prediction for each city
  cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII   ) %>%
    summarise(Max_Bike_Prediction = max(BIKE_PREDICTION),
              LNG = first(LNG),  # Assuming these columns exist in your data
              LAT = first(LAT) ,    # Assuming these columns exist in your data
              BIKE_PREDICTION_LEVEL = BIKE_PREDICTION_LEVEL,
              LABEL= LABEL,
              hourly=hourly,
              
               )
  
  
  # Print the aggregated data frame to verify
  print(cities_max_bike)
  
  
  
  colors <- c(small = "green", medium = "yellow", large = "red")
  sizes <- c(small = 6, medium = 10, large = 12)
  
  observeEvent(input$city_dropdown, {
    cities_max_bike <- city_weather_bike_df %>%
      group_by(CITY_ASCII   ) %>%
      summarise(Max_Bike_Prediction = max(BIKE_PREDICTION),
                LNG = first(LNG),  # Assuming these columns exist in your data
                LAT = first(LAT) ,    # Assuming these columns exist in your data
                BIKE_PREDICTION_LEVEL = BIKE_PREDICTION_LEVEL,
                DETAILED_LABEL = DETAILED_LABEL,
                FORECASTDATETIME=FORECASTDATETIME,
                TEMPERATURE=TEMPERATURE,
                hourly= hourly,
                BIKE_PREDICTION=BIKE_PREDICTION,
                HUMIDITY = HUMIDITY ,
                LABEL= LABEL
                
                )
    
    if(input$city_dropdown != 'All') {
      
      colors <- c(small = "green", medium = "yellow", large = "red")
      sizes <- c(small = 6, medium = 10, large = 12)
      
      
      # Specific city selected
      selected_city <- input$city_dropdown
      selected_city_data <- cities_max_bike %>%
        filter(CITY_ASCII == selected_city)
    
      
      print(selected_city_data)
      output$city_bike_map <- renderLeaflet({
        leaflet(selected_city_data) %>%
          addCircleMarkers(
            lng = ~LNG,  # Assuming these columns exist in your data
            lat = ~LAT,  # Assuming these columns exist in your data
            #radius = ~sizes['selected_city_data$BIKE_PREDICTION_LEVEL'],
            color = "yellow",
            popup = ~DETAILED_LABEL 
          ) %>%
          addProviderTiles("CartoDB.Positron")
      })
      
      #city_weather_bike_df$hourly <- as.numeric(city_weather_bike_df$hourly)
      output$temp_line = renderPlot({
        # Assuming 'city_weather_bike_df' has a 'hourly' column and 'TEMPERATURE' column
        
        # Convert 'hourly' to numeric (if it's not already)
        selected_city_data$hourly <- as.numeric(selected_city_data$hourly)
        
        # Create the temperature trend plot
        ggplot(selected_city_data, aes(x = hourly, y = TEMPERATURE)) +
          geom_line(color = "blue") +  # Add a smooth line
          geom_point(color = "red") +  # Add temperature points
          geom_text(aes(label = round(TEMPERATURE, 2)), vjust = -0.5, color = "green") +  # Add temperature labels
          labs(title = "Temperature Trend Plot",
               x = "Time",
               y = "Temperature")
      })
      
      output$bike_line = renderPlot({
     
        # Create the temperature trend plot
        ggplot(selected_city_data, aes(x = FORECASTDATETIME, y = BIKE_PREDICTION)) +
          geom_line(color = "blue") +  # Add a smooth line
          geom_point(color = "red") +  # Add temperature points
          geom_text(aes(label = round(BIKE_PREDICTION, 2)), vjust = -0.5, color = "yellow") +  # Add temperature labels
          labs(title = "bike predition Trend Plot",
               x = "date",
               y = "bike_prediction")
        
      })
      
      output$bike_date_output <- renderText({
        if(!is.null(input$plot_click)){
          clicked_datetime <- as.POSIXct(input$plot_click$x, origin = "1970-01-01", tz = "UTC")
          paste("Clicked Datetime Value:", format(clicked_datetime, "%Y-%m-%d %H:%M:%S"), "\n",
                "Clicked Y Value:", input$plot_click$y)
        }
      })
      
      
      
      output$humidity_pred_chart <- renderPlot({
        ggplot(selected_city_data, aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
          geom_point(color = "blue") +  # Add scatter plot points
          geom_smooth(method = "lm", formula = y ~ poly(x, 4), color = "orange") +  # Add polynomial regression line
          labs(title = "Scatter Plot with Polynomial Regression",
               x = "Humidity",
               y = "Bike Prediction")
      })
      
      
    } else {
      # All cities selected
      output$city_bike_map <- renderLeaflet({
        leaflet(cities_max_bike) %>%
          addCircleMarkers(
            lng = ~LNG,  # Assuming these columns exist in your data
            lat = ~LAT,  # Assuming these columns exist in your data
            #radius = ~sizes['cities_max_bike$BIKE_PREDICTION_LEVEL'],
            #color = ~colors['cities_max_bike$BIKE_PREDICTION_LEVEL'],
            radius = 6,
            color= "yellow",
            popup = ~LABEL ,
            options = markerOptions(autoPopup = FALSE)
          ) %>%
          addProviderTiles("CartoDB.Positron")
      })
    }
  })
  })














  
  # prediction for the city
  
  # Observe drop-down event
  
  # Then render output plots with an id defined in ui.R
  
  # If All was selected from dropdown, then render a leaflet map with circle markers
  # and popup weather LABEL for all five cities
  
  # If just one specific city was selected, then render a leaflet map with one marker
  # on the map and a popup with DETAILED_LABEL displayed
  


























#observeEvent(input$city_dropdown, {
  # Execute code when users make selections on the dropdown
 # output$city_bike_map <- renderLeaflet({
    # Complete this function to render a leaflet map
    # Assuming cities_max_bike is the aggregated data frame
    #cities_max_bike <- ... # Replace with the actual aggregated data frame
    
    # Define color and size based on BIKE_PREDICTION_LEVEL
  #  colors <- c(small = "green", medium = "yellow", large = "red")
   # sizes <- c(small = 6, medium = 10, large = 12)
    
    # Create leaflet map
    #leaflet(city_weather_bike_df) %>%
      #addCircleMarkers(
        #lng = ~city_weather_bike_df$LNG, # Assuming you have longitude information
        #lat = ~city_weather_bike_df$LAT,  # Assuming you have latitude information
        #radius = ~sizes[cities_max_bike$Max_Bike_Prediction],
       # color = ~colors[cities_max_bike$Max_Bike_Prediction],
      #  popup = ~city_weather_bike_df$LABEL
     # ) %>%
    #  addProviderTiles("CartoDB.Positron")
  #})
#})

