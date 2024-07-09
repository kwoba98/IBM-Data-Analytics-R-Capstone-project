# Load required libraries
require(leaflet)

# select drop down list to select city
list_cities=  c("All", "Seoul", "Suzhou", "London", "New York", "Paris")
# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
  titlePanel("Bike-sharing demand prediction app"), 
  # Create a side-bar layout
  sidebarLayout(
    # Create a main panel to show cities on a leaflet map
    mainPanel(
      # leaflet output with id = 'city_bike_map', height = 1000
      leafletOutput('city_bike_map', height = '700px')
    ),
    # Create a side bar to show detailed plots for a city
    sidebarPanel(
   
      selectInput(inputId="city_dropdown",
                  label =  "select a city", 
                  choices= list_cities, 
                  selected = ""
                  ),
      plotOutput(outputId= "temp_line" , height = "200px", width = "400px"
                 ),
      
      plotOutput("bike_line", click = "plot_click", height = "200px", width = "400px"),
      
      verbatimTextOutput("bike_date_output"),
      
    
    plotOutput(outputId= "humidity_pred_chart" , height = "200px", width = "400px"
    )
     
       
    )
))

)