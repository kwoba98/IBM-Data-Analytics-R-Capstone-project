library("tidymodels")
library("tidyverse")
library("stringr")
library("readr")
library("glmnet")



# Create an empty data frame to store results
results_df <- data.frame(Model = character(), RMSE = numeric(), Rsq = numeric(), stringsAsFactors = FALSE)


# Function to calculate RMSE and Rsq
calculate_metrics <- function(predictions, truth) {
  RMSE <- rmse(predictions, truth)
 # Rsq <- rsq(predictions, truth)
  return(c(RMSE))
}

# Dataset URL
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
download.file(dataset_url, dest="D:\\data analysis\\data Viz with r\\r-capstone project\\REFINE THE BASELINE REGRESSION MODEL\\seoul_bike_sharing_converted_normalized.csv")
#"D:\\data analysis\\data Viz with r\\r-capstone project\\REFINE THE BASELINE REGRESSION MODEL\\seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv("D:\\data analysis\\data Viz with r\\r-capstone project\\REFINE THE BASELINE REGRESSION MODEL\\seoul_bike_sharing_converted_normalized.csv")


bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)


#Define a linear regression model specification.

lm_spec <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression")


#Split the data into training and testing datasets.

set.seed(1234)
data_split <- initial_split(bike_sharing_df, prop = 4/5)
train_data <- training(data_split)
test_data <- testing(data_split)

#TASK: Add polynomial terms
ggplot(data = train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point()


# Plot the higher order polynomial fits
ggplot(data=train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color="red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color="yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color="green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color="blue")


#TODO: Fit a linear regression model lm_poly with higher order polynomial terms on the important variables (larger coefficients) found in the baseline model

lm_poly <- lm(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6, raw= TRUE) + poly(HUMIDITY, 4,raw= TRUE), data= train_data) 


# Print model summary

summary(lm_poly$fit)


# Make predictions using lm_model_all
predictions_poly <- predict(lm_poly, new_data = test_data)

# Calculate R-squared and RMSE for the new model to see if performance has improved

# Create a data frame to store the results for lm_poly
poly_result <- data.frame(truth = train_data$RENTED_BIKE_COUNT, predicted = predictions_poly)


poly_result[poly_result<0] <- 0

#calculating  rmse and rsq
rmse(poly_result, truth= truth, estimate=predicted)
rsq(poly_result, truth= truth, estimate=predicted)

test_results[test_results<0] <- 0


#TODO: Try adding some interaction terms to the previous polynomial models.
# Add interaction terms to the poly regression built in previous step

# HINT: You could use `*` operator to create interaction terms such as HUMIDITY*TEMPERATURE and make the formula look like:
# RENTED_BIKE_COUNT ~ RAINFALL*HUMIDITY ...


lm_poly_int <- lm(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6, raw= TRUE) + RAINFALL*HUMIDITY  , data= train_data) 

# Print model summary
summary(lm_poly_int)

predictions_poly_int <- predict(lm_poly_int, new_data = test_data)
poly_int_result <- data.frame(truth = train_data$RENTED_BIKE_COUNT, predicted = predictions_poly_int)

# Calculate R-squared and RMSE for the new model to see if performance has improved
rmse(poly_int_result, truth= truth, estimate=predicted)
rsq(poly_int_result, truth= truth, estimate=predicted)

#TASK: Add regularization
#TODO: Define a linear regression model specification glmnet_spec using glmnet engine

# Fit the glmnet model
#lm_glmnet <- fit(glmnet_spec, data = train_data, formula = RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6, raw= TRUE) + RAINFALL*HUMIDITY  )



# Define the formula
formula <- RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6, raw= TRUE) + RAINFALL*HUMIDITY

# Define the linear regression model specification
glmnet_spec <- linear_reg(engine = "glmnet", 
                          penalty = 0.5, 
                          mixture = 0.6)

# Fit the glmnet model
lm_glmnet <- fit(glmnet_spec, data = train_data, formula = formula)

# Calculate predictions
predictions <- predict(lm_glmnet, new_data = test_data)

lm_glmnet_result <- data.frame(truth = train_data$RENTED_BIKE_COUNT, predicted = predictions)

rmse(lm_glmnet_result, truth= truth, estimate=.pred)
rsq(lm_glmnet_result, truth= truth, estimate=.pred)



#TASK: Experiment to search for improved models
#TODO: Experiment by building and testing at least five different models. For each of your experiments, include polynomial terms, interaction terms, and one of the three regularizations we introduced.

#MODEL 1
# Create polynomial terms
train_data <- within(train_data, {TEMPERATURE_poly <- poly(TEMPERATURE, 7, raw=TRUE)})
# Create interaction term
train_data$interaction_term1 <- train_data$RAINFALL * train_data$HUMIDITY
# Combine the predictors into a matrix
X <- model.matrix(RENTED_BIKE_COUNT ~ TEMPERATURE_poly + interaction_term1 * DEW_POINT_TEMPERATURE + SOLAR_RADIATION*AUTUMN , data = train_data)
# Define the response variable
y <- train_data$RENTED_BIKE_COUNT
# To make predictions on new data, use:
test_data <- within(test_data, {TEMPERATURE_poly <- poly(TEMPERATURE, 7, raw=TRUE)})
# Create interaction term
test_data$interaction_term1 <- test_data$RAINFALL * test_data$HUMIDITY * test_data$SOLAR_RADIATION
X_new <- model.matrix(RENTED_BIKE_COUNT ~ TEMPERATURE_poly + interaction_term1* DEW_POINT_TEMPERATURE+ SOLAR_RADIATION *AUTUMN  , data = test_data)
lambda_values <- seq(0.01, 1, by = 0.01)
set.seed(123)  # Set a seed for reproducibility
cv_model <- cv.glmnet(X, y, alpha = 0, lambda = lambda_values, nfolds = 10)
best_lambda <- cv_model$lambda.min
best_model <- glmnet(X, y, alpha = 0, lambda = best_lambda)
predictions_1 <- predict(best_model, s = best_lambda, newx = X_new)
predictions_1_result <- data.frame(truth = test_data$RENTED_BIKE_COUNT, predicted = predictions_1)
metrics_1 <- c(rmse(predictions_1_result,truth, s1), 
               rsq(predictions_1_result,truth, s1))

#MODEL 2
# Create polynomial terms
train_data <- within(train_data, {RAINFALL_poly <- poly(RAINFALL, 7, raw=TRUE)})
# Create interaction term
train_data$interaction_term2 <- train_data$TEMPERATURE * train_data$HUMIDITY
# Combine the predictors into a matrix
X <- model.matrix(RENTED_BIKE_COUNT ~ RAINFALL_poly + interaction_term2* DEW_POINT_TEMPERATURE+ SOLAR_RADIATION *AUTUMN + SNOWFALL, data = train_data)
# Define the response variable
y <- train_data$RENTED_BIKE_COUNT
# Fit Ridge regression model
lambda <- 0.1  # Adjust lambda as needed (higher values give stronger regularization)
model_2 <- glmnet(X, y, alpha = 0, lambda = lambda)
# Calculate predictions
# To make predictions on new data, use:
test_data <- within(test_data, {RAINFALL_poly <- poly(RAINFALL, 7, raw=TRUE)})
# Create interaction term
test_data$interaction_term2 <- test_data$TEMPERATURE * test_data$HUMIDITY
X_new <- model.matrix(RENTED_BIKE_COUNT ~ RAINFALL_poly + interaction_term2* DEW_POINT_TEMPERATURE+ SOLAR_RADIATION *AUTUMN +SNOWFALL, data = test_data)
predictions_2 <- predict(model_2, s = lambda, newx = X_new)
predictions_2_result <- data.frame(truth = test_data$RENTED_BIKE_COUNT, predicted = predictions_2)
#RMSE AND RSQ
metrics_2 <- c(rmse(predictions_2_result,truth, s1), 
               rsq(predictions_2_result,truth, s1))

#MODEL 3
# Create polynomial terms
train_data <- within(train_data, {HUMIDITY_poly <- poly(HUMIDITY, 10, raw=TRUE)})
# Create interaction term
train_data$interaction_term3 <- train_data$TEMPERATURE * train_data$RAINFALL
# Combine the predictors into a matrix
X <- model.matrix(RENTED_BIKE_COUNT ~ HUMIDITY_poly + interaction_term3 * DEW_POINT_TEMPERATURE+ SOLAR_RADIATION *AUTUMN +SNOWFALL, data = train_data)
# Define the response variable
y <- train_data$RENTED_BIKE_COUNT
# Fit Ridge regression model
lambda <- 0.1  # Adjust lambda as needed (higher values give stronger regularization)
model_3 <- glmnet(X, y, alpha = 0.5, lambda = lambda)
# Print Ridge regression coefficients
coef(model_3)
# Calculate predictions
test_data <- within(test_data, {HUMIDITY_poly <- poly(HUMIDITY, 10, raw=TRUE)})
# Create interaction term
test_data$interaction_term3 <- test_data$TEMPERATURE * test_data$RAINFALL
X_new <- model.matrix(RENTED_BIKE_COUNT ~ HUMIDITY_poly + interaction_term3* DEW_POINT_TEMPERATURE+ SOLAR_RADIATION *AUTUMN +SNOWFALL, data = test_data)
predictions_3 <- predict(model_3, s = lambda, newx = X_new)
predictions_3_result <- data.frame(truth = test_data$RENTED_BIKE_COUNT, predicted = predictions_3)
#RMSE AND RSQ
metrics_3 <- c(rmse(predictions_3_result,truth, s1), 
               rsq(predictions_3_result,truth, s1))

#MODEL 4
# Create polynomial terms
train_data <- within(train_data, {TEMPERATURE_poly <- poly(TEMPERATURE, 10, raw=TRUE)})
# Create interaction term
train_data$interaction_term4 <- train_data$RAINFALL * train_data$HUMIDITY
# Combine the predictors into a matrix
X <- model.matrix(RENTED_BIKE_COUNT ~ TEMPERATURE_poly + interaction_term4 * DEW_POINT_TEMPERATURE+ SOLAR_RADIATION *AUTUMN +SNOWFALL, data = train_data)
# Define the response variable
y <- train_data$RENTED_BIKE_COUNT

# Fit Ridge regression model
lambda_values <- seq(0.01, 1, by = 0.01)
set.seed(123)  # Set a seed for reproducibility
model_4 <-cv.glmnet(X, y, alpha = 0.7, lambda = lambda_values, nfolds = 5)
best_lambda <- model_4$lambda.min
best_model <- glmnet(X, y, alpha = 0.7, lambda = best_lambda)

# Calculate predictions
# To make predictions on new data, use:
test_data <- within(test_data, {TEMPERATURE_poly <- poly(TEMPERATURE, 10, raw=TRUE)})
# Create interaction term
test_data$interaction_term4 <- test_data$RAINFALL * test_data$HUMIDITY
X_new <- model.matrix(RENTED_BIKE_COUNT ~ TEMPERATURE_poly + interaction_term4 * DEW_POINT_TEMPERATURE+ SOLAR_RADIATION *AUTUMN +SNOWFALL, data = test_data)
predictions_best <- predict(model_4, s = best_lambda, newx = X_new)
predictions_4_result <- data.frame(truth = test_data$RENTED_BIKE_COUNT, predicted = predictions_best)
#RMSE AND RSQ
metrics_4 <- c(rmse(predictions_4_result,truth, s1), 
               rsq(predictions_4_result,truth, s1))


#MODEL 5
# Create polynomial terms
train_data <- within(train_data, {TEMPERATURE_poly <- poly(18, 4, raw=TRUE)})
# Create interaction term
train_data$interaction_term4 <- train_data$RAINFALL * train_data$HUMIDITY
# Combine the predictors into a matrix
X <- model.matrix(RENTED_BIKE_COUNT ~ TEMPERATURE_poly + interaction_term4, data = train_data)
# Define the response variable
y <- train_data$RENTED_BIKE_COUNT
# Fit Ridge regression model
lambda <- 0.1  # Adjust lambda as needed (higher values give stronger regularization)
model_5 <- glmnet(X, y, alpha = 1, lambda = lambda)

# Calculate predictions
# To make predictions on new data, use:
test_data <- within(test_data, {TEMPERATURE_poly <- poly(18, 4, raw=TRUE)})
# Create interaction term
test_data$interaction_term5 <- test_data$RAINFALL * test_data$HUMIDITY
X_new <- model.matrix(RENTED_BIKE_COUNT ~ TEMPERATURE_poly + interaction_term5, data = test_data)
predictions_5 <- predict(model_5, s = lambda, newx = X_new)
predictions_5_result <- data.frame(truth = test_data$RENTED_BIKE_COUNT, predicted = predictions_5)
#RMSE AND RSQ
#rmse(predictions_5_result, truth= truth, estimate=s1)
#rsq(predictions_5_result, truth= truth, estimate=s1)
#metrics_5 <- calculate_metrics(predictions_5_result$truth, predictions_5_result$S1)
#results_df <- rbind(results_df, c("Model 5", metrics_5[1], metrics_5[2]))
metrics_5 <- c(rmse(predictions_5_result,truth, s1), 
               rsq(predictions_5_result,truth, s1))

# Combine all metrics into a data frame
results_RMSE <- data.frame(
 # Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  RMSE = c(metrics_1[3], metrics_2[3], metrics_3[3], metrics_4[3], metrics_5[3])

                        )

results_RSQ <- data.frame(
  # Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  
  RSQ = c(metrics_1[6], metrics_2[6], metrics_3[6], metrics_4[6], metrics_5[6])
)

results_RMSE_mod= t(results_RMSE)
results_RSQ_mod= t(results_RSQ)
# Print the results data frame
print(results_RMSE_mod[,1] )
print(results_RSQ_mod[,1] )
Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")
final_results_dataframe= data.frame(Model,results_RMSE_mod[,1],results_RSQ_mod[,1])


# Assuming `results_RMSE_mod[,1]` and `results_RSQ_mod[,1]` are meant to represent categories
final_results_dataframe$RMSE_Category <- factor(results_RMSE_mod[,1])
final_results_dataframe$RSQ_Category <- factor(results_RSQ_mod[,1])
library(tidyr)
results_long <- gather(final_results_dataframe, variable, value, -Model)
# Create the grouped bar chart
ggplot(results_long, aes(x = Model, y = value, fill = variable)
       ) +
  geom_bar(stat = "identity", position = "dodge") +
  labs( title="Refined Models’ RMSE and R-squared ",x = "Model", y = "Value", fill = "Variable") +
  scale_fill_manual(values = c("RMSE_Category" = "blue", "RSQ_Category" = "yellow")) 





# Assuming you have a data frame or vectors named `test_predictions` and `true_values`

# Load the necessary libraries
library(ggplot2)

# Create a Q-Q plot
ggplot() +
  stat_qq(aes(sample = predictions_4_result$truth), color = 'green') +
  stat_qq(aes(sample = predictions_4_result$s1), color = 'red') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title="Q-Q plot of the best model’s test results vs the truths ",x = "Test results", y = "Truths") +
  theme_minimal()

















# Load necessary libraries
library(ggplot2)

# Create a hypothetical dataset (replace this with your actual data)
results <- data.frame(
  Model = c("Model A", "Model B", "Model C"),
  RMSE = c(0.1, 0.2, 0.15),
  R_squared = c(0.8, 0.85, 0.9)
)

# Reshape the data for plotting
library(tidyr)
results_long <- gather(results, variable, value, -Model)

# Create the grouped bar chart
ggplot(results_long, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Model", y = "Value", fill = "Variable") +
  scale_fill_manual(values = c("RMSE" = "blue", "R-squared" = "yellow")) +
  theme_minimal()






























