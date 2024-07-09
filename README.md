# IBM-Data-Analytics-R-Capstone-project

# Project Title: Bike Sharing Demand Prediction in Urban Centers Based on Weather and Time

## Overview
This project aims to predict the demand for bike sharing in urban centers using weather and time variables. By leveraging comprehensive weather data, historical bike usage, and advanced predictive modeling techniques, the project provides insights to enhance urban mobility planning and user experience.

## Project Structure

### 1. Executive Summary
- **Objective**: Predict bike sharing demand based on weather conditions.
- **Methodology**: Data collection, variable engineering, and ensemble techniques for demand forecasting.
- **Key Findings**: 
  - High bike sharing demand in summer, followed by autumn, spring, and winter.
  - Peak demand in the evening, particularly at 18:00.
- **Implications**: 
  - Supports infrastructure development and resource allocation.
  - Enhances bike availability and service quality.
- **Conclusion**: Demonstrates the potential for data-driven insights to improve urban bike-sharing services.

### 2. Introduction
- **Objective**: Analyze the impact of weather on bike-sharing demand.
- **Approach**: Collect and process weather and demand data, perform exploratory data analysis (EDA), and build predictive models.
- **Outcome**: Real-time interactive dashboard showing weather and bike demand.

### 3. Methodology
#### Data Collection
- **Sources**:
  - `cities_weather_forecast.csv` from OpenWeather API.
  - `raw_bike_sharing_systems.csv` from Wikipedia table.
  - `raw_seoul_bike_sharing.csv` and `raw_worldcities.csv` from cloud storage.
- **Data Details**:
  - Weather forecast for every 3 hours over the next 5 days.
  - Bike-sharing systems around the world.
  - Weather and bike rental data in Seoul.

#### Data Wrangling
- Handling missing values, inconsistent data, and incorrect formats.
- Creating dummy variables for categorical data.
- Normalizing data.

#### Exploratory Data Analysis (EDA)
- Using SQL queries to uncover trends, patterns, and anomalies.
- Visualizations: Histograms, scatter plots, and boxplots.

#### Predictive Analysis
- Built five linear regression models using weather and time variables as predictors.
- Improved models using polynomial and interaction terms, and regularization techniques.
- Evaluated models based on R-squared and RMSE values.

### 4. Results
#### EDA Results
- Insights on busiest bike rental times, hourly popularity, and seasonal patterns.
- Summer has the highest bike count, with significant correlations between temperature and bike rentals.

#### Predictive Analysis Results
- Identified key coefficients and evaluated model performance.
- Best performing model: RENTED_BIKE_COUNT ~ TEMPERATURE_poly + interaction_term4 * DEW_POINT_TEMPERATURE + SOLAR_RADIATION * AUTUMN + SNOWFALL.

### 5. Dashboard
- **Features**:
  - Interactive map showing predicted bike-sharing demand.
  - Static and interactive plots for temperature trends, demand prediction, and correlations.
- **Cities Analysis**:
  - Predictions for bike demand in cities like Paris and Suzhou.

### 6. Conclusion
- Summer has the least bike count rentals.
- Temperature positively correlates with bike rentals.
- Seoul has the highest bike usage.
- Peak bike rentals occur at 18:00.

### 7. Appendix
- Includes R-code snippets, SQL queries, and notebook outputs.

## Usage
1. **Data Collection**: Collect weather and bike-sharing data from the specified sources.
2. **Data Wrangling**: Process the collected data to handle missing values and create necessary variables.
3. **EDA**: Perform exploratory data analysis using SQL and visualizations to identify patterns and trends.
4. **Predictive Modeling**: Build and evaluate regression models to forecast bike-sharing demand.
5. **Dashboard**: Use R Shiny to build an interactive dashboard for stakeholders.

## Installation
1. Clone the repository: `git clone <repository-url>`
2. Install dependencies: `pip install -r requirements.txt`
3. Run the data processing scripts: `python data_processing.py`
4. Launch the dashboard: `R -e "shiny::runApp('dashboard.R')"`

## Contact
For any questions or feedback, please contact:
- Name: Kwoba Fredrick
- Email: kwobafredrick98@gmail.com
- LinkedIn: www.linkedin.com/in/kwoba-fredrick-23960417a

---

