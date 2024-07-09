library(tidyverse)
library(ggplot2)
library(dplyr)


#Task 1 - Load the dataset

url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing.csv"  
download.file(url,destfile="D:\\data analysis\\data Viz with r\\r-capstone project\\EDA with ggplot2 and tidyverse\\data used\\seoul_bike_sharing.csv")

seoul_bike_sharing= read.csv("D:\\data analysis\\data Viz with r\\r-capstone project\\EDA with ggplot2 and tidyverse\\data used\\seoul_bike_sharing.csv")


#Task 2 - Recast DATE as a date
#Use the format of the data, namely "%d/%m/%Y".
seoul_bike_sharing <- seoul_bike_sharing %>%
  mutate(DATE = as.Date(DATE, format = "%d/%m/%Y"))






#Task 3 - Cast HOURS as a categorical variable
#Also, coerce its levels to be an ordered sequence. This will ensure your visualizations correctly utilize HOURS as a 
#discrete variable with the expected ordering.
#Now, you should have both the original HOUR column and the new categorical version HOUR_cat.

seoul_bike_sharing <- seoul_bike_sharing %>%
  mutate(HOUR_cat = factor(HOUR, levels = unique(HOUR), ordered = TRUE)) %>%
  mutate(dummy = 1) %>%
  spread(key = HOUR_cat, value = dummy, fill = 0)


str(seoul_bike_sharing)

sum(is.na(seoul_bike_sharing))






#Descriptive Statistics
#Now you are all set to take a look at some high level statistics of the seoul_bike_sharing dataset.

#Task 4 - Dataset Summary
#Use the base R sumamry() function to describe the seoul_bike_sharing dataset

summary(seoul_bike_sharing)


#Task 5 - Based on the above stats, calculate how many Holidays there are.
#Solution 5:

num_holidays <- sum(seoul_bike_sharing$HOLIDAY == "Holiday")
num_NOholidays <- sum(seoul_bike_sharing$HOLIDAY == "No Holiday")

# Print the number of holidays
print(num_holidays)
print(num_NOholidays)


#Task 6 - Calculate the percentage of records that fall on a holiday.
#Solution 6

num_records <- nrow(seoul_bike_sharing)  # Get the total number of records


# Calculate the percentage
percentage_on_holiday <- (num_holidays / num_records) * 100

# Print the result
print(percentage_on_holiday)


#Task 7 - Given there is exactly a full year of data, determine how many records we expect to have.
#Solution 7
expected_records <- 365  # Assuming a full year has 365 days

# Print the expected number of records
print(expected_records)


#Task 8 - Given the observations for the 'FUNCTIONING_DAY' how many records must there be?
#  Solution 8


#Task 9 - Load the dplyr package, group the data by SEASONS, and use the summarize() function to calculate the seasonal total rainfall and snowfall.
#Solution 9

seasonal_totals <- seoul_bike_sharing %>%
  group_by(SEASONS) %>%
  summarize(total_rainfall = sum(RAINFALL, na.rm = TRUE),
            total_snowfall = sum(SNOWFALL, na.rm = TRUE))

# Print the result
print(seasonal_totals)


# Create the scatter plot
scatter_plot <- ggplot(seoul_bike_sharing, aes(x = DATE, y = RENTED_BIKE_COUNT)) +
  geom_point(alpha = 0.5) +  # Adjust opacity with alpha parameter
  labs(title="Bike rental vs. Date",x = "Date", y = "Rented Bike Count")  # Add labels for x and y axes

# Print the scatter plot
print(scatter_plot)


#Ungraded Task: We can see some patterns emerging here.
#Describe them and keep your findings for your presentation in the final project.




#Task 11 - Create the same plot of the RENTED_BIKE_COUNT time series, but now add HOURS as the colour.
# Create the time series plot with color encoding by HOURS
time_series_plot <- ggplot(seoul_bike_sharing, aes(x = DATE, y = RENTED_BIKE_COUNT, color = as.factor(HOUR))) +
  geom_point(alpha = 0.5) +  # Adjust opacity with alpha parameter
  labs( title ="Bike rental vs Datetime" ,x = "Date", y = "Rented Bike Count") +  # Add labels for x and y axes
  scale_color_discrete(name = "Hours")  # Add legend for hours

# Print the time series plot
print(time_series_plot)






#Task 12 - Create a histogram overlaid with a kernel density curve¶

# Create the histogram with kernel density curve
histogram_density_plot <- ggplot(seoul_bike_sharing, aes(x = RENTED_BIKE_COUNT, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "white", color = "black") +
  geom_density(color = "blue", alpha = 0.3) +
  labs(title="Bike rental histogram",x = "RENTED_BIKE_COUNT", y = "Density")

# Print the histogram with kernel density plot
print(histogram_density_plot)



#Task 13 - Use a scatter plot to visualize the correlation between RENTED_BIKE_COUNT and TEMPERATURE by SEASONS.
#Start with RENTED_BIKE_COUNT vs. TEMPERATURE, then generate four plots corresponding to the SEASONS by adding a facet_wrap() layer. Also, make use of colour and opacity to emphasize any patterns that emerge. Use HOUR as the color.

# Create the scatter plot
scatter_plot13 <- ggplot(seoul_bike_sharing, aes(x = TEMPERATURE, y = RENTED_BIKE_COUNT, color = as.factor(HOUR))) +
  geom_point(alpha = 0.6) +
  facet_wrap(~SEASONS, ncol = 2) +
  labs(x = "Temperature", y = "Rented Bike Count") +
  scale_color_discrete(name = "Hour")

# Print the scatter plot
print(scatter_plot13)






#Task 14 - Create a display of four boxplots of RENTED_BIKE_COUNT vs. HOUR grouped by SEASONS.
#Use facet_wrap to generate four plots corresponding to the seasons.

# Create the boxplots
boxplot_display <- ggplot(seoul_bike_sharing, aes(x = HOUR, y = RENTED_BIKE_COUNT, fill = as.factor(HOUR))) +
  geom_boxplot() +
  facet_wrap(~SEASONS, ncol = 2) +
  labs(title="RENTED_BIKE_COUNT vs. HOUR grouped by SEASONS",x = "Hour", y = "Rented Bike Count") +
  scale_fill_discrete(name = "Hour")

# Print the boxplot display
print(boxplot_display)


#Task 15 - Group the data by DATE, and use the summarize() function to calculate the daily total rainfall and snowfall.

# Group the data by DATE and calculate the daily total rainfall and snowfall
daily_totals <- seoul_bike_sharing %>%
  group_by(DATE) %>%
  summarize(total_rainfall = sum(RAINFALL, na.rm = TRUE),
            total_snowfall = sum(SNOWFALL, na.rm = TRUE))

# Print the resulting dataset with daily totals
print(daily_totals)


#Task 16 - Determine how many days had snowfall.
days_with_snowfall <- sum(seoul_bike_sharing$total_snowfall > 0)

# Print the number of days with snowfall
print(days_with_snowfall)


























