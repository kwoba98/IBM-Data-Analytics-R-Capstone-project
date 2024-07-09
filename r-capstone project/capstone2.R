# Load required library
library(httr)
library(tidyr)

# Define API endpoint and parameters

get_weather_forecaset_by_cities <- function(city_names){
url <- 'https://api.openweathermap.org/data/2.5/forecast'
api_key <- '786492856e74afacdd517464a9bae978'  # Replace with your actual API key

# Create an empty list to store the responses
responses <- list()

# Loop through the list of cities
for (city in city_names) {
  # Make the API request
  response <- GET(url, query = list(q = city, appid = api_key))
  
  # Check if the request was successful (status code 200)
  if (http_type(response) == "application/json") {
    # Parse the JSON response
    weather_data <- content(response, "parsed")
    responses[[city]] <- weather_data
  } else {
    print(paste('Error for city:', city))
  }
}

# Now, responses will contain weather data for each city

# Assuming 'responses' contains the weather data

# Create an empty list to store all data
all_data <- list()

# Loop through the responses
for (city in city_names) {
  weather_data <- responses[[city]]
  
  # Extract the specific information you're interested in (e.g., temperature, humidity)
  extracted_data <- data.frame(
    city = rep(city, length(weather_data$list)),
    date_time = sapply(weather_data$list, function(x) x$dt_txt),
    temperature = sapply(weather_data$list, function(x) x$main$temp),
    humidity = sapply(weather_data$list, function(x) x$main$humidity),
    temp_min= sapply(weather_data$list, function(x) x$main$temp_min),
    temp_max= sapply(weather_data$list, function(x) x$main$temp_max),
    pressure= sapply(weather_data$list, function(x) x$main$pressure),
    wind_speed= sapply(weather_data$list, function(x) x$wind$speed),
    wind_deg= sapply(weather_data$list, function(x) x$wind$deg)
    # Add more variables as needed
  )
  # Append the extracted data to the list
  all_data <- append(all_data, list(extracted_data))
}
# Combine the list of extracted data into a single dataframe
weather_df <- do.call(rbind, all_data)
# Print the dataframe
return(weather_df)
}
cities <- c("Seoul", "Washington, D.C.", "Paris", "Suzhou")
get_weather_forecaset_by_cities(cities)
cities_weather_df <- get_weather_forecaset_by_cities(cities)
# Write cities_weather_df to `cities_weather_forecast.csv`
write.csv(cities_weather_df, "D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\cities_weather_forecast.csv", row.names=FALSE)


# Download several datasets

# Download some general city information such as name and locations
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
# download the file
download.file(url, destfile = "D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_worldcities.csv")

# Download a specific hourly Seoul bike sharing demand dataset
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
# download the file
download.file(url, destfile = "D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_seoul_bike_sharing.csv")



#WEEK TWO : DATA WRANGLING
# Check whether you need to install the `tidyverse` library
require("tidyverse")
library(tidyverse)

dataset_list <- c("D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_bike_sharing_systems.csv",
                  "D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_seoul_bike_sharing.csv", 
                  "D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_cities_weather_forecast.csv",
                  "D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_worldcities.csv"
                  )
#TODO: Write a for loop to iterate over the above datasets and convert their column names

# Assuming 'dataset_list' contains a list of dataset file names (e.g., "dataset1.csv", "dataset2.csv", ...)
for (dataset_name in dataset_list){
  # Read dataset
  dataset <- read.csv(dataset_name)
  
  # Standardize column names
  colnames(dataset) <- gsub("\\s", "_", toupper(colnames(dataset)))
  
  # Save the dataset 
  write.csv(dataset, file = dataset_name, row.names=FALSE)
}


#TODO: Read the resulting datasets back and check whether their column names follow the naming convention


# Assuming 'dataset_list' contains a list of dataset file names (e.g., "dataset1.csv", "dataset2.csv", ...)
for (dataset_name in dataset_list){
  # Read the modified dataset
  modified_dataset <- read.csv(dataset_name)
  
  # Print a summary
  head(modified_dataset)
  cat("Summary for", dataset_name, ":\n")
  print(summary(modified_dataset))
  
  cat("\n")
}


# First load the dataset
bike_sharing_df <- read_csv("D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_bike_sharing_systems.csv")
head(bike_sharing_df)


# Select the four columns
sub_bike_sharing_df <- bike_sharing_df %>% select(COUNTRY, CITY, SYSTEM, BICYCLES)


sub_bike_sharing_df %>% 
  summarize_all(class) %>%
  gather(variable, class)

#They are all interpreted as character columns, but we expect the BICYCLES column to be of numeric type. Let's see why it wasn't loaded as a numeric column - possibly some entries contain characters. Let's create a simple function called find_character to check that
# grepl searches a string for non-digital characters, and returns TRUE or FALSE
# if it finds any non-digital characters, then the bicyle column is not purely numeric
find_character <- function(strings) grepl("[^0-9]", strings)


#Let's try to find any elements in the Bicycles column containing non-numeric characters.

sub_bike_sharing_df %>% 
    select(BICYCLES) %>% 
    filter(find_character(BICYCLES)) %>%
    slice(0:10)






#Next, let's take a look at the other columns, namely COUNTRY, CITY, and SYSTEM, to see if they contain any undesired reference links, such as in Melbourne[12].

# Define a 'reference link' character class, 
# `[A-z0-9]` means at least one character 
# `\\[` and `\\]` means the character is wrapped by [], such as for [12] or [abc]


ref_pattern <- "\\[[A-z0-9]+\\]"
find_reference_pattern <- function(strings) grepl(ref_pattern, strings)
# Check whether the COUNTRY column has any reference links


sub_bike_sharing_df %>% 
    select(COUNTRY) %>% 
    filter(find_reference_pattern(COUNTRY)) %>%
    slice(0:10)


#Ok, looks like the COUNTRY column is clean. Let's check the CITY column.

# Check whether the CITY column has any reference links
sub_bike_sharing_df %>% 
    select(CITY) %>% 
    filter(find_reference_pattern(CITY)) %>%
    slice(0:10)

#Hmm, looks like the CITY column has some reference links to be removed. Next, let's check the SYSTEM column.

# Check whether the System column has any reference links
sub_bike_sharing_df %>% 
    select(SYSTEM) %>% 
    filter(find_reference_pattern(SYSTEM)) %>%
    slice(0:10)





#TASK: Remove undesired reference links using regular expressions
#TODO: Write a custom function using stringr::str_replace_all to replace all reference links with an empty character for columns CITY and SYSTEM
# Load required library
library(stringr)
library(dplyr)
# Custom function to remove reference links
remove_ref <- function(strings) {
  # Define a pattern matching a reference link
  ref_pattern <- "\\[\\d+\\]"  # This pattern matches references like [1], [2], etc.
  
  # Replace all matched substrings with an empty character using str_replace_all()
  result <- str_replace_all(strings, pattern = ref_pattern, replacement = "")
  
  return(result)
}
#TODO: Use the dplyr::mutate() function to apply the remove_ref function to the CITY and SYSTEM columns
sub_bike_sharing_df1=sub_bike_sharing_df %>% mutate(CITY=remove_ref(CITY), SYSTEM=remove_ref(SYSTEM) )
head(sub_bike_sharing_df1)



#TODO: Use the following code to check whether all reference links are removed:
#TASK: Extract the numeric value using regular expressions
#TODO: Write a custom function using stringr::str_extract to extract the first digital substring match and convert it into numeric type For example, extract the value '32' from 32 (including 6 rollers) [162].

# Extract the first number
# Custom function to extract the first numeric substring
extract_num <- function(columns){
  # Define a pattern matching a digital substring
  digitals_pattern <- "\\d+"
  
  # Find the first match using str_extract
  result <- str_extract(columns, pattern = digitals_pattern)
  
  # Convert the result to numeric
  numeric_result <- as.numeric(result)
  
  return(numeric_result)
}


sub_bike_sharing_df2= sub_bike_sharing_df1 %>% mutate(BICYCLES=extract_num(BICYCLES))

summary(sub_bike_sharing_df2$BICYCLES)


write.csv(sub_bike_sharing_df2,"D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_bike_sharing_systems.csv")
sample= read.csv("D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_bike_sharing_systems.csv")




#Data Wrangling with dplyr
#In this lab, you will focus on wrangling the Seoul bike-sharing demand historical dataset.
#For this dataset, you will be asked to use tidyverse to perform the following data wrangling tasks:
  
#TASK: Detect and handle missing values
#TASK: Create indicator (dummy) variables for categorical variables
#TASK: Normalize data



bike_sharing_df <- read_csv("D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_seoul_bike_sharing.csv")
summary(bike_sharing_df)
dim(bike_sharing_df)

 #url_1 <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"

#url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
#download.file(url_1, destfile="D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_seoul_bike_sharing1.csv")
#sample2=read.csv("D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\raw_seoul_bike_sharing1.csv")
#summary(sample2)


#TASK: Detect and handle missing values
#TODO: Drop rows with missing values in the RENTED_BIKE_COUNT column
# Drop rows with `RENTED_BIKE_COUNT` column == NA
# Print the dataset dimension again after those rows are dropped



bike_sharing_df= bike_sharing_df %>%drop_na(RENTED_BIKE_COUNT)

bike_sharing_df %>% filter(is.na(TEMPERATURE))

#TODO: Impute missing values for the TEMPERATURE column using its mean value.
# Calculate the summer average temperature
temp_avg= mean(bike_sharing_df$TEMPERATURE,na.rm = TRUE)

# Impute missing values for TEMPERATURE column with summer average temperature
bike_sharing_df= bike_sharing_df %>% mutate(TEMPERATURE= replace_na(TEMPERATURE , temp_avg))

# Save the dataset as `seoul_bike_sharing.csv`
print(bike_sharing_df)

write.csv(bike_sharing_df,"D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\seoul_bike_sharing.csv")

# Using mutate() function to convert HOUR column into character type

bike_sharing_df= bike_sharing_df %>% mutate(HOUR= as.character(HOUR))





#TODO: Convert SEASONS, HOLIDAY, FUNCTIONING_DAY, and HOUR columns into indicator columns.

#Note that if FUNCTIONING_DAY only contains one categorical value after missing values removal, then you don't need to convert it to an indicator column.

# Convert SEASONS, HOLIDAY, FUNCTIONING_DAY, and HOUR columns into indicator columns.
# Create indicator columns using spread()
# Step 1: Spread SEASONS
bike_sharing_df<- bike_sharing_df %>%
  mutate(dummy = 1)%>%
  spread(key = SEASONS, value = dummy, fill = 0)
# Step 2: Spread HOLIDAY
bike_sharing_df<- bike_sharing_df %>%
  mutate(dummy = 1)%>%spread(key = HOLIDAY, value = dummy, fill = 0)
# Step 3: Spread FUNCTIONING_DAY
bike_sharing_df<- bike_sharing_df %>%
  mutate(dummy = 1)%>%spread(key = FUNCTIONING_DAY, value = dummy, fill = 0)
# Step 4: Spread HOUR
bike_sharing_df<- bike_sharing_df %>%
  mutate(dummy = 1)%>%spread(key = HOUR, value = dummy, fill = 0)
# Remove the 'dummy' column
#bike_sharing_df <- select(bike_sharing_df, -dummy)
print(bike_sharing_df)

# Print the dataset summary again to make sure the indicator columns are created properly
head(bike_sharing_df)
summary(bike_sharing_df)
print(bike_sharing_df)

# Save the dataset as `seoul_bike_sharing_converted.csv`

write_csv(bike_sharing_df, "D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\seoul_bike_sharing_converted.csv")
#bike_sharing_df=read_csv("D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\seoul_bike_sharing_converted.csv")
#head(bike_sharing_df)
#TASK: Normalize data

#TODO: Apply min-max normalization on RENTED_BIKE_COUNT, TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, DEW_POINT_TEMPERATURE, SOLAR_RADIATION, RAINFALL, SNOWFALL

# Use the `mutate()` function to apply min-max normalization on columns

# Define the columns to be normalized
columns_to_normalize <- c(
  "RENTED_BIKE_COUNT",
  "TEMPERATURE",
  "HUMIDITY",
  "WIND_SPEED",
  "VISIBILITY",
  "DEW_POINT_TEMPERATURE",
  "SOLAR_RADIATION",
  "RAINFALL",
  "SNOWFALL"
)

# Apply min-max normalization
bike_sharing_df <- bike_sharing_df %>%
  mutate(across(all_of(columns_to_normalize), ~ (.-min(.))/(max(.)-min(.))))

# Print the first few rows to verify the changes
head(bike_sharing_df)





# `RENTED_BIKE_COUNT`, `TEMPERATURE`, `HUMIDITY`, `WIND_SPEED`, `VISIBILITY`, `DEW_POINT_TEMPERATURE`, `SOLAR_RADIATION`, `RAINFALL`, `SNOWFALL`
# Print the summary of the dataset again to make sure the numeric columns range between 0 and 1
summary(bike_sharing_df)
# Save the dataset as `seoul_bike_sharing_converted_normalized.csv`

 write_csv(bike_sharing_df, "D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\seoul_bike_sharing_converted_normalized.csv")

 
 
 
 
 
 
 
 #Standardize the column names again for the new datasets
 #Since you have added many new indicator variables, you need to standardize their column names again by using the following code:
   
   # Dataset list\
   dataset_list <- c("D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\seoul_bike_sharing.csv", 
                     "D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\seoul_bike_sharing_converted.csv",
                     "D:\\data analysis\\data Viz with r\\r-capstone project\\data used\\seoul_bike_sharing_converted_normalized.csv"
                     )
 
 for (dataset_name in dataset_list){
   # Read dataset
   dataset <- read_csv(dataset_name)
   # Standardized its columns:
   # Convert all columns names to uppercase
   names(dataset) <- toupper(names(dataset))
   # Replace any white space separators by underscore, using str_replace_all function
   names(dataset) <- str_replace_all(names(dataset), " ", "_")
   # Save the dataset back
   write.csv(dataset, dataset_name, row.names=FALSE)
 }
 
 
 
 
 
 
 
 
 
 
 
 






