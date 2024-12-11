
# Load dplyr package
library(dplyr)
library(randomForest)
library(rpart)
library(ggplot2)
library(caTools)
library(gridExtra)
library(tidyr)


# Read and clean data
# sdata <- read.csv("housingData2.csv", header = TRUE, sep = ",")
# 
# colnames(sdata)
# 
# head(sdata, 10)
# 
# 
# library(tidyr)
# 
# # Convert to long format
# sdata_long <- sdata %>%
#   pivot_longer(
#     cols = starts_with("X"),  # Select all columns that start with "X" (the date columns)
#     names_to = "date",        # New column name for dates
#     values_to = "price"       # New column name for prices
#   ) %>%
#   # Clean up the date column by removing the "X" and converting to proper date format
#   mutate(date = as.Date(gsub("X", "", date), format = "%Y.%m.%d"))
# 
# 
# sdata_long <- sdata_long[sdata_long$StateName == "FL", ]
# 
# 
# unique(sdata_long$RegionName)
# 
# 
# write.csv(sdata_long, "housing_data_long.csv", row.names = FALSE)


##########################################################################
### BEGIN DATA READING ###
##########################################################################

# Read cleaned zillow data
zdata <- read.csv("housing_data_long.csv", header = TRUE, sep = ",")

# Read and clean housing data set 1
sdata <- read.csv("housing_data1_cleaned.csv", header = TRUE, sep = ",")

#population data by city
pdata <- read.csv("us-cities-table.csv", header = TRUE, sep = ",")

##########################################################################
### BEGIN POPULATION DATA TRANSFORMATION ###
##########################################################################
# pdata[is.na(pdata)] <- 0
# pdata$population <- NULL
# 
# 
# colnames(pdata)
# 
# # Consolidate population columns into one "date" column
# pdata_long <- pdata %>%
#   pivot_longer(cols = starts_with("X"), 
#                names_to = "date", 
#                values_to = "population")
# 
# # View the resulting data
# head(pdata_long$date, 10)


##########################################################################
### BEGIN DATA TRANSFORMATION ###
##########################################################################
# Remove ", FL" from zdata$RegionName
zdata$RegionName <- gsub(", FL", "", zdata$RegionName)

#convert sdata city to region
sdata$region <- case_when(
  # Southeast Florida (Miami-Dade, Broward, Palm Beach area)
  sdata$city %in% c("Miami", "Fort Lauderdale", "Hollywood", "Boca Raton", "Boynton Beach", "Delray Beach", 
                    "West Palm Beach", "Pompano Beach", "Deerfield Beach", "Coral Springs", "Plantation", 
                    "Sunrise", "Doral", "Hialeah", "Miami Beach", "Aventura", "Jupiter", "Palm Beach Gardens",
                    "Wellington", "Lake Worth", "Royal Palm Beach") ~ "Southeast_Florida",
  
  # Central Florida (Orlando area and surroundings)
  sdata$city %in% c("Orlando", "Kissimmee", "Winter Park", "Sanford", "Altamonte Springs", "Oviedo",
                    "Winter Springs", "Lake Mary", "Casselberry", "Ocoee", "Winter Garden", "Clermont",
                    "St. Cloud", "Apopka", "Maitland", "Windermere", "DeLand", "Celebration") ~ "Central_Florida",
  
  # Tampa Bay Area
  sdata$city %in% c("Tampa", "St. Petersburg", "Clearwater", "Largo", "Palm Harbor", "Dunedin",
                    "Safety Harbor", "Temple Terrace", "Plant City", "Brandon", "Riverview", "Wesley Chapel",
                    "Land O Lakes", "Spring Hill", "New Port Richey", "Port Richey", "Tarpon Springs") ~ "Tampa_Bay",
  
  # Southwest Florida (Fort Myers, Naples area)
  sdata$city %in% c("Fort Myers", "Naples", "Cape Coral", "Bonita Springs", "Marco Island", "Estero",
                    "Fort Myers Beach", "Sanibel", "Captiva", "North Fort Myers", "Port Charlotte",
                    "Punta Gorda") ~ "Southwest_Florida",
  
  # Northeast Florida (Jacksonville area)
  sdata$city %in% c("Jacksonville", "Jacksonville Beach", "Neptune Beach", "Atlantic Beach",
                    "Ponte Vedra Beach", "St. Augustine", "Fernandina Beach", "Orange Park",
                    "Green Cove Springs", "Middleburg") ~ "Northeast_Florida",
  
  # Space Coast (Brevard County area)
  sdata$city %in% c("Melbourne", "Palm Bay", "Titusville", "Cocoa", "Cocoa Beach", "Merritt Island",
                    "Satellite Beach", "Rockledge", "Cape Canaveral", "Viera") ~ "Space_Coast",
  
  # Treasure Coast
  sdata$city %in% c("Port St. Lucie", "Stuart", "Fort Pierce", "Vero Beach", "Sebastian",
                    "Jensen Beach") ~ "Treasure_Coast",
  
  # Sarasota-Bradenton Area
  sdata$city %in% c("Sarasota", "Bradenton", "Venice", "North Port", "Longboat Key", "Lakewood Ranch",
                    "Palmetto", "Holmes Beach", "Anna Maria", "Siesta Key") ~ "Sarasota_Bradenton",
  
  # Florida Panhandle
  sdata$city %in% c("Pensacola", "Tallahassee", "Panama City", "Fort Walton Beach", "Destin",
                    "Milton", "Niceville", "Panama City Beach", "Crestview", "Gulf Breeze") ~ "Panhandle",
  
  # Florida Keys
  sdata$city %in% c("Key West", "Key Largo", "Marathon", "Islamorada", "Big Pine Key", 
                    "Sugarloaf Key", "Tavernier", "Cudjoe Key", "Summerland Key") ~ "Florida_Keys",
  
  # Gainesville Area
  sdata$city %in% c("Gainesville", "Alachua", "High Springs", "Newberry", "Archer",
                    "Hawthorne", "Micanopy") ~ "North_Central_Florida",
  
  # Default for any cities not specifically categorized
  TRUE ~ "Other_Florida"
)


#convert zdata regionType to region
zdata$region <- case_when(
  # Southeast Florida (Miami-Dade, Broward, Palm Beach area)
  zdata$RegionName %in% c("Miami", "Fort Lauderdale", "Hollywood", "Boca Raton", "Boynton Beach", "Delray Beach", 
                    "West Palm Beach", "Pompano Beach", "Deerfield Beach", "Coral Springs", "Plantation", 
                    "Sunrise", "Doral", "Hialeah", "Miami Beach", "Aventura", "Jupiter", "Palm Beach Gardens",
                    "Wellington", "Lake Worth", "Royal Palm Beach", "Okeechobee", "Clewiston") ~ "Southeast_Florida",
  
  # Central Florida (Orlando area and surroundings)
  zdata$RegionName %in% c("Orlando", "Kissimmee", "Winter Park", "Sanford", "Altamonte Springs", "Oviedo",
                    "Winter Springs", "Lake Mary", "Casselberry", "Ocoee", "Winter Garden", "Clermont",
                    "St. Cloud", "Apopka", "Maitland", "Windermere", "DeLand", "Celebration", "Homosassa Springs",
                    "The Villages", "Sebring", "Ocala", "Lakeland", "Deltona") ~ "Central_Florida",
  
  # Tampa Bay Area
  zdata$RegionName %in% c("Tampa", "St. Petersburg", "Clearwater", "Largo", "Palm Harbor", "Dunedin",
                    "Safety Harbor", "Temple Terrace", "Plant City", "Brandon", "Riverview", "Wesley Chapel",
                    "Land O Lakes", "Spring Hill", "New Port Richey", "Port Richey", "Tarpon Springs") ~ "Tampa_Bay",
  
  # Southwest Florida (Fort Myers, Naples area)
  zdata$RegionName %in% c("Fort Myers", "Naples", "Cape Coral", "Bonita Springs", "Marco Island", "Estero",
                    "Fort Myers Beach", "Sanibel", "Captiva", "North Fort Myers", "Port Charlotte",
                    "Punta Gorda", "Arcadia", "Wauchula") ~ "Southwest_Florida",
  
  # Northeast Florida (Jacksonville area)
  zdata$RegionName %in% c("Jacksonville", "Jacksonville Beach", "Neptune Beach", "Atlantic Beach",
                    "Ponte Vedra Beach", "St. Augustine", "Fernandina Beach", "Orange Park",
                    "Green Cove Springs", "Middleburg", "Palatka") ~ "Northeast_Florida",
  
  # Space Coast (Brevard County area)
  zdata$RegionName %in% c("Melbourne", "Palm Bay", "Titusville", "Cocoa", "Cocoa Beach", "Merritt Island",
                    "Satellite Beach", "Rockledge", "Cape Canaveral", "Viera") ~ "Space_Coast",
  
  # Treasure Coast
  zdata$RegionName %in% c("Port St. Lucie", "Stuart", "Fort Pierce", "Vero Beach", "Sebastian",
                    "Jensen Beach") ~ "Treasure_Coast",
  
  # Sarasota-Bradenton Area
  zdata$RegionName %in% c("Sarasota", "Bradenton", "Venice", "North Port", "Longboat Key", "Lakewood Ranch",
                    "Palmetto", "Holmes Beach", "Anna Maria", "Siesta Key") ~ "Sarasota_Bradenton",
  
  # Florida Panhandle
  zdata$RegionName %in% c("Pensacola", "Tallahassee", "Panama City", "Fort Walton Beach", "Destin",
                    "Milton", "Niceville", "Panama City Beach", "Crestview", "Gulf Breeze") ~ "Panhandle",
  
  # Florida Keys
  zdata$RegionName %in% c("Key West", "Key Largo", "Marathon", "Islamorada", "Big Pine Key", 
                    "Sugarloaf Key", "Tavernier", "Cudjoe Key", "Summerland Key") ~ "Florida_Keys",
  
  # Gainesville Area
  zdata$RegionName %in% c("Gainesville", "Alachua", "High Springs", "Newberry", "Archer",
                    "Hawthorne", "Micanopy", "Lake City") ~ "North_Central_Florida",
  
  # Default for any cities not specifically categorized
  TRUE ~ "Other_Florida"
)


# Group by region and calculate the average and standard deviation for each relevant column
sdata <- sdata %>%
  group_by(region) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    # sd_price = sd(price, na.rm = TRUE),
    avg_bed = mean(bed, na.rm = TRUE),
    # sd_bed = sd(bed, na.rm = TRUE),
    avg_bath = mean(bath, na.rm = TRUE),
    # sd_bath = sd(bath, na.rm = TRUE),
    avg_acre_lot = mean(acre_lot, na.rm = TRUE),
    # sd_acre_lot = sd(acre_lot, na.rm = TRUE),
    avg_house_size = mean(house_size, na.rm = TRUE),
    # sd_house_size = sd(house_size, na.rm = TRUE),
    avg_Property_has_house = mean(Property_has_house, na.rm = TRUE),
    avg_condo_apartment = mean(condo_apartment, na.rm = TRUE),
    avg_statusfor_sale = mean(statusfor_sale, na.rm = TRUE),
    avg_statusready_to_build = mean(statusready_to_build, na.rm = TRUE),
    avg_statussold = mean(statussold, na.rm = TRUE)
  )

# Join sdata and zdata on the "region" column
merged_data <- inner_join(sdata, zdata, by = "region")

library(lubridate)

print(merged_data$date)

# Convert date to Date using the correct format
merged_data$date <- ymd(merged_data$date)  # Use ymd() or mdy() based on your format

# Assuming 'date' is in Date format
merged_data <- merged_data %>%
  mutate(
    year = year(date),
    month = month(date)
  )


#drop unimportant columns
merged_data$RegionName <- NULL
merged_data$SizeRank <- NULL
# merged_data$RegionID <- NULL
merged_data$RegionType <- NULL
merged_data$StateName <- NULL
merged_data$date <- NULL

# Remove rows where 'price' is NA
merged_data <- merged_data[!is.na(merged_data$price), ]

# write final
write.csv(merged_data, "DDDMFinalProjectDataset.csv", row.names = FALSE)

# Convert year and month to factors before creating dummies
merged_data$year <- as.factor(merged_data$year)
merged_data$month <- as.factor(merged_data$month)
merged_data$region <- as.factor(merged_data$region)
merged_data$RegionID <- as.factor(merged_data$RegionID)

# Create dummy variables
region_dummies <- model.matrix(~region - 1, data=merged_data)
year_dummies <- model.matrix(~year - 1, data=merged_data)
month_dummies <- model.matrix(~month - 1, data=merged_data)
regionName_dummies <- model.matrix(~RegionID - 1, data=merged_data)


# Add dummies to original dataframe
merged_data <- cbind(merged_data, region_dummies)
merged_data <- cbind(merged_data, year_dummies)
merged_data <- cbind(merged_data, month_dummies)
merged_data <- cbind(merged_data, regionName_dummies)


# drop original city/region column
merged_data$region <- NULL
merged_data$year <- NULL
merged_data$month <- NULL
merged_data$year2024 <- NULL
merged_data$month12 <- NULL
merged_data$RegionID <- NULL


# View the result
colnames(merged_data)


##########################################################################
### FIX MULTICOLINEARITY ###
##########################################################################

#drop colinear columns
merged_data$avg_statussold <- NULL
# merged_data$avg_statusready_to_build <- NULL
# merged_data$regionNorth_Central_Florida <- NULL
# merged_data$regionNortheast_Florida <- NULL
# merged_data$regionPanhandle <- NULL
# merged_data$regionTreasure_Coast <- NULL
# merged_data$regionTampa_Bay <- NULL
# merged_data$regionCentral_Florida <- NULL
# merged_data$regionFlorida_Keys <- NULL
# merged_data$RegionID394957 <- NULL
# merged_data$RegionID753906 <- NULL
# merged_data$RegionID395148 <- NULL

# Load necessary libraries
library(ggplot2)
library(corrplot)

# Calculate the correlation matrix (only for numeric variables)
numeric_data <- merged_data %>%
  select_if(is.numeric)  # Select only numeric columns

cor_matrix <- cor(numeric_data, use = "complete.obs")  # Compute correlation matrix

options(repr.plot.width = 300, repr.plot.height = 240)

# Plot the heatmap
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, # Text color and rotation
         title = "Correlation Heatmap", 
         mar = c(0, 0, 1, 0))  # Adjust margin for title

print(cor_matrix)

model <- lm(price ~ ., data = merged_data)
# summary(model)

# here I realized the data must be bugged because despite no perfect correlations I am still getting NA so I am dropping them
# Get coefficients including NAs
coeffs <- coef(model)

# Find all NA coefficients 
na_coeffs <- names(coeffs)[is.na(coeffs)]

# Print which variables we're removing
print("Combining the following variables with NA coefficients into 'special_variable':")
print(na_coeffs)

# Remove all NA coefficient variables from the dataset
merged_data <- merged_data[, !colnames(merged_data) %in% na_coeffs]


model <- lm(price ~ ., data = merged_data)
summary(model)

# head(sdata)



##########################################################################
### FEATURE REDUCTION ###
##########################################################################
# Function for forward stepwise selection 
forward_stepwise <- function(data, response_var, significance_level = 0.05) {
  remaining_features <- setdiff(names(data), response_var)
  selected_features <- c()
  current_model <- NULL
  
  while(length(remaining_features) > 0) {
    p_values <- sapply(remaining_features, function(feature) {
      test_features <- c(selected_features, feature)
      formula <- as.formula(paste(response_var, "~", paste(test_features, collapse = " + ")))
      model <- lm(formula, data = data)  
      summary(model)$coefficients[length(test_features) + 1, "Pr(>|t|)"]  # t-statistic p-value
    })
    
    best_feature <- names(which.min(p_values))
    if(p_values[best_feature] <= significance_level) {
      selected_features <- c(selected_features, best_feature)
      remaining_features <- setdiff(remaining_features, best_feature)
      formula <- as.formula(paste(response_var, "~", paste(selected_features, collapse = " + ")))
      current_model <- lm(formula, data = data)  
    } else {
      break
    }
  }
  
  return(list(features = selected_features, model = current_model))
}

# Function for backward stepwise selection 
backward_stepwise <- function(data, response_var, significance_level = 0.05) {
  current_features <- setdiff(names(data), response_var)
  
  while(length(current_features) > 0) {
    formula <- as.formula(paste(response_var, "~", paste(current_features, collapse = " + ")))
    model <- lm(formula, data = data)  
    p_values <- summary(model)$coefficients[-1, "Pr(>|t|)"]  # t-statistic p-values
    
    max_p_value <- max(p_values)
    if(max_p_value > significance_level) {
      feature_to_remove <- current_features[which.max(p_values)]
      current_features <- setdiff(current_features, feature_to_remove)
    } else {
      break
    }
  }
  
  formula <- as.formula(paste(response_var, "~", paste(current_features, collapse = " + ")))
  final_model <- lm(formula, data = data)  
  return(list(features = current_features, model = final_model))
}



# Apply forward stepwise selection
forward_selection <- forward_stepwise(merged_data, "price")
forward_features <- c(forward_selection$features, "price")
print(paste(length(forward_features), "Columns Forward:"))
print(forward_features)

# Apply backward stepwise selection
backward_selection <- backward_stepwise(merged_data, "price")
backward_features <- c(backward_selection$features, "price")
print(paste(length(backward_features), "Columns Backward:"))
print(backward_features)


##########################################################################
### BEGIN MODEL BUILDING ###
##########################################################################
# Load necessary libraries
library(randomForest)
# install.packages("Metrics")
library(Metrics)

# Split data into training and testing (80-20 split)
train_index <- sample(1:nrow(merged_data), 0.8 * nrow(merged_data))
train_data <- merged_data[train_index, ]
test_data <- merged_data[-train_index, ]

head(train_data)

# Train a linear regression model using forward-selected features
model_forward <- lm(price ~ ., data = train_data[, forward_features, drop = FALSE])

# Train a linear regression model using backward-selected features
model_backward <- lm(price ~ ., data = train_data[, backward_features, drop = FALSE])

# Train Random Forest model using forward selection for features
forward_rf <- randomForest(price ~ ., data = train_data[, forward_features, drop = FALSE], ntree = 50, mtry = sqrt(ncol(train_data)), importance = TRUE)

# Train Random Forest model using backward selection for features
backward_rf <- randomForest(price ~ ., data = train_data[, backward_features, drop = FALSE], ntree = 50, mtry = sqrt(ncol(train_data)), importance = TRUE)

# Train a Random Forest model using all features
model_rf_full <- randomForest(price ~ ., data = train_data, ntree = 50, mtry = sqrt(ncol(train_data)), importance = TRUE)

# Predict on the test set using forward-selected model
predictions_forward <- predict(model_forward, test_data[, forward_features, drop = FALSE])

# Predict on the test set using backward-selected model
predictions_backward <- predict(model_backward, test_data[, backward_features, drop = FALSE])

# Predict on the test set using forward-selected Random Forest model
predictions_rf_forward <- predict(forward_rf, test_data[, forward_features, drop = FALSE])

# Predict on the test set using backward-selected Random Forest model
predictions_rf_backward <- predict(backward_rf, test_data[, backward_features, drop = FALSE])

# Predict on the test set using the full Random Forest model
predictions_rf_full <- predict(model_rf_full, test_data)

# Calculate RMSE for all models
rmse_forward <- sqrt(mean((predictions_forward - test_data$price)^2))
rmse_backward <- sqrt(mean((predictions_backward - test_data$price)^2))
rmse_rf_forward <- sqrt(mean((predictions_rf_forward - test_data$price)^2))
rmse_rf_backward <- sqrt(mean((predictions_rf_backward - test_data$price)^2))
rmse_rf_full <- sqrt(mean((predictions_rf_full - test_data$price)^2))

# Print RMSE values
print(paste("RMSE (Forward - Linear Regression):", rmse_forward))
print(paste("RMSE (Backward - Linear Regression):", rmse_backward))
print(paste("RMSE (Forward - Random Forest):", rmse_rf_forward))
print(paste("RMSE (Backward - Random Forest):", rmse_rf_backward))
print(paste("RMSE (Full Random Forest):", rmse_rf_full))

# Calculate Percent Error for all models
percent_error_forward <- mean(abs((predictions_forward - test_data$price) / test_data$price)) * 100
percent_error_backward <- mean(abs((predictions_backward - test_data$price) / test_data$price)) * 100
percent_error_rf_forward <- mean(abs((predictions_rf_forward - test_data$price) / test_data$price)) * 100
percent_error_rf_backward <- mean(abs((predictions_rf_backward - test_data$price) / test_data$price)) * 100
percent_error_rf_full <- mean(abs((predictions_rf_full - test_data$price) / test_data$price)) * 100

# Print Percent Error values
print(paste("Percent Error (Forward - Linear Regression):", percent_error_forward))
print(paste("Percent Error (Backward - Linear Regression):", percent_error_backward))
print(paste("Percent Error (Forward - Random Forest):", percent_error_rf_forward))
print(paste("Percent Error (Backward - Random Forest):", percent_error_rf_backward))
print(paste("Percent Error (Full Random Forest):", percent_error_rf_full))

# Variable importance from Random Forest models
# print("Variable Importance (Forward Random Forest):")
# print(importance(forward_rf))
# 
# print("Variable Importance (Backward Random Forest):")
# print(importance(backward_rf))
# 
# print("Variable Importance (Full Random Forest):")
# print(importance(model_rf_full))



##########################################################################
### BEGIN EXTENSIVE MODEL TESTING ###
##########################################################################

# Set the number of iterations
iterations <- 100

# Store RMSE and Percent Error for each iteration for each model
rmse_values_forward_lm <- numeric(iterations)
rmse_values_backward_lm <- numeric(iterations)

percent_error_values_forward_lm <- numeric(iterations)
percent_error_values_backward_lm <- numeric(iterations)

# Loop through the iterations for splitting and evaluating
for (i in 1:iterations) {
  # Split data into training and testing (80-20 split)
  set.seed(i)  # To ensure different splits each time
  train_index <- sample(1:nrow(merged_data), 0.8 * nrow(merged_data))
  train_data <- merged_data[train_index, ]
  test_data <- merged_data[-train_index, ]
  
  # Train a linear regression model using forward-selected features
  model_forward_lm <- lm(price ~ ., data = train_data[, forward_features, drop = FALSE])
  
  # Train a linear regression model using backward-selected features
  model_backward_lm <- lm(price ~ ., data = train_data[, backward_features, drop = FALSE])
  
  # Predict on the test set using forward-selected linear regression model
  predictions_forward_lm <- predict(model_forward_lm, test_data[, forward_features, drop = FALSE])
  
  # Predict on the test set using backward-selected linear regression model
  predictions_backward_lm <- predict(model_backward_lm, test_data[, backward_features, drop = FALSE])
  
  # Calculate RMSE for forward and backward LM models
  rmse_values_forward_lm[i] <- sqrt(mean((predictions_forward_lm - test_data$price)^2))
  rmse_values_backward_lm[i] <- sqrt(mean((predictions_backward_lm - test_data$price)^2))
  
  # Calculate Percent Error for forward and backward LM models
  percent_error_values_forward_lm[i] <- mean(abs((predictions_forward_lm - test_data$price) / test_data$price)) * 100
  percent_error_values_backward_lm[i] <- mean(abs((predictions_backward_lm - test_data$price) / test_data$price)) * 100
}

# Create data frames to store RMSE and Percent Error for plotting
results_rmse <- data.frame(
  Model = rep(c("Forward LM", "Backward LM"), each = iterations),
  RMSE = c(rmse_values_forward_lm, rmse_values_backward_lm)
)

results_percent_error <- data.frame(
  Model = rep(c("Forward LM", "Backward LM"), each = iterations),
  PercentError = c(percent_error_values_forward_lm, percent_error_values_backward_lm)
)

# Create a grid plot with Violin Plots for RMSE and Percent Error
# Plot for RMSE
p_rmse <- ggplot(results_rmse, aes(x = Model, y = RMSE, fill = Model)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("lightgreen", "lightblue")) +
  theme_minimal() +
  labs(title = "RMSE Distribution", y = "RMSE", x = "") +
  theme(legend.position = "none")

# Plot for Percent Error
p_percent_error <- ggplot(results_percent_error, aes(x = Model, y = PercentError, fill = Model)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("lightgreen", "lightblue")) +
  theme_minimal() +
  labs(title = "Percent Error Distribution", y = "Percent Error", x = "") +
  theme(legend.position = "none")

# Combine the plots into a grid
grid.arrange(p_rmse, p_percent_error, ncol = 2)





