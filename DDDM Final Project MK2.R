




# Read and clean data
sdata <- read.csv("realtor-data.zip.csv", header = TRUE, sep = ",")


##########################################################################
### BEGIN DATA TRANSFORMATION ###
##########################################################################

# Filter for Florida data
sdata <- sdata[sdata$state == "Florida", ]


# Add variable (Property_has_house 0 or 1) 1 when bed, bath, and house_size are not NA 0 when all three are NA
sdata$Property_has_house <- ifelse(
  !is.na(sdata$bed) & !is.na(sdata$bath) & !is.na(sdata$house_size) & !is.na(sdata$acre_lot), 
  1, 
  0
)


# set NA values to 0 where conditions for house are met
all_na_rows <- is.na(sdata$bed) & is.na(sdata$bath) & is.na(sdata$house_size) & !is.na(sdata$acre_lot)


# Replace NA values with 0 only in rows where conditions for house are met
sdata$bed[all_na_rows] <- 0
sdata$bath[all_na_rows] <- 0
sdata$house_size[all_na_rows] <- 0


# Add variable (condo_apartment 0 or 1) 1 when bed, bath, and house_size are not NA but acre_lot is else 0
sdata$condo_apartment <- ifelse(
  # Has house features (bed, bath, house_size not NA)
  (!is.na(sdata$bed) & !is.na(sdata$bath) & !is.na(sdata$house_size)) &
    # But no acre_lot info
    is.na(sdata$acre_lot),
  1,  # It's a condo/apartment
  0   # It's not a condo/apartment
)

# set NA values to 0 where conditions for condo/apartment are met
all_na_rows <-   (!is.na(sdata$bed) & !is.na(sdata$bath) & !is.na(sdata$house_size)) &
  # But no acre_lot info
  is.na(sdata$acre_lot)

# Replace NA values with 0 only in rows where conditions for condo/apartment are met
sdata$acre_lot[all_na_rows] <- 0


# replace column "prev_sold_date" 1 if previous sold date and 0 if blank ('')
sdata$prev_sold_date <- ifelse(
  sdata$prev_sold_date == "" | is.na(sdata$prev_sold_date),
  0,
  1
)

# Remove rows where city is "Out of State" or empty
sdata <- sdata[!(sdata$city == "Out of State" | sdata$city == ""), ]


# Create dummy variables for status
status_dummies <- model.matrix(~status - 1, data=sdata)


# Add dummies to original dataframe
sdata <- cbind(sdata, status_dummies)

# Drop original status column
sdata$status <- NULL

write.csv(sdata, "housing_data1_cleaned.csv", row.names = FALSE)


# Feature reduce Cities
library(dplyr)
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

# Create dummy variables for region
region_dummies <- model.matrix(~region - 1, data=sdata)


# Add dummies to original dataframe
sdata <- cbind(sdata, region_dummies)


# drop original city/region column
sdata$city <- NULL
sdata$region <- NULL


# Drop Un-Needed Columns
sdata$state <- NULL


# Remove rows with missing values
sdata <- na.omit(sdata)


# remove outliers
# Compute average price and standard deviation
price_mean <- mean(sdata$price, na.rm = TRUE)
price_sd <- sd(sdata$price, na.rm = TRUE)

# Remove rows where prices fall outside 2 standard deviations
sdata <- sdata[sdata$price >= (price_mean - 2 * price_sd) & sdata$price <= (price_mean + 2 * price_sd), ]


##########################################################################
### BEGIN FIXING MULTICOLINEARITY ### 
##########################################################################
# Ensure necessary libraries are installed
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}

# Check the cleaned data and refit the final model if needed
model <- lm(price ~ ., data = sdata)
summary(model)

library(corrplot)

# Select only numeric columns for correlation analysis
numeric_cols <- sdata[sapply(sdata, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_cols, use = "complete.obs")

# Plot the heatmap
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black", # Add correlation coefficients
  diag = FALSE
)

# Drop MULTICOLINEAR Columns
sdata$statusready_to_build <- NULL


# Run Heat map Again
numeric_cols <- sdata[sapply(sdata, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_cols, use = "complete.obs")

# Plot the heatmap
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black", # Add correlation coefficients
  diag = FALSE
)

# Drop MULTICOLINEAR Columns
sdata$statussold <- NULL

# Run Heat map Again
numeric_cols <- sdata[sapply(sdata, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_cols, use = "complete.obs")

# Plot the heatmap
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black", # Add correlation coefficients
  diag = FALSE
)

# Check the cleaned data and refit the final model if needed
model <- lm(price ~ ., data = sdata)
summary(model)

# here I realized the data must be bugged because despite no perfect correlations I am still getting NA so I am dropping them
# Get coefficients including NAs
coeffs <- coef(model)

# Find all NA coefficients 
na_coeffs <- names(coeffs)[is.na(coeffs)]

# Print which variables we're removing
print("Removing the following variables with NA coefficients:")
print(na_coeffs)

# head(sdata)

# Remove all NA coefficient variables at once
sdata <- sdata[, !colnames(sdata) %in% na_coeffs]

# head(sdata)

# Refit model without NA variables
model <- lm(price ~ ., data=sdata)

# Show final model
summary(model)

##########################################################################
### BEGIN FEATURE REDUCTION ### 
##########################################################################

# the variables when multipled become statistically
sdata$brokered_by_street <- sdata$brokered_by * sdata$street


# drop highest P value features
sdata$street <- NULL
sdata$regionNortheast_Florida <- NULL  
sdata$regionNorth_Central_Florida <- NULL
sdata$brokered_by <- NULL
sdata$regionPanhandle <- NULL


model <- lm(price ~ ., data = sdata)
summary(model)


##########################################################################
### BEGIN EXTENSIVE MODEL TESTING ###
##########################################################################

# Ensure necessary libraries are installed
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}
if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("caTools", quietly = TRUE)) {
  install.packages("caTools")
}

library(randomForest)
library(rpart)
library(ggplot2)
library(caTools)


# Set the number of iterations
iterations <- 100

# Store RMSE and Percent Error for each iteration
rmse_values <- numeric(iterations)
percent_error_values <- numeric(iterations)

# Loop through the iterations
for (i in 1:iterations) {
  # Split data into training and testing (80-20 split)
  set.seed(i)  # To ensure different splits each time
  train_index <- sample(1:nrow(sdata_clean), 0.8 * nrow(sdata_clean))
  train_data <- sdata_clean[train_index, ]
  test_data <- sdata_clean[-train_index, ]
  
  # Train a linear regression model
  model <- lm(price ~ ., data = train_data)
  
  # Predict on the test set
  predictions <- predict(model, test_data)
  
  # Calculate RMSE
  rmse_values[i] <- sqrt(mean((predictions - test_data$price)^2))
  
  # Calculate Percent Error
  percent_error_values[i] <- mean(abs((predictions - test_data$price) / test_data$price)) * 100
}

# Create a data frame to store RMSE and Percent Error for plotting
results <- data.frame(
  Metric = rep(c("RMSE", "Percent Error"), each = iterations),
  Value = c(rmse_values, percent_error_values)
)

print(results)

# Create a grid plot with Violin Plots for RMSE and Percent Error
library(ggplot2)
library(gridExtra)

# Plot for RMSE
p_rmse <- ggplot(results[results$Metric == "RMSE",], aes(x = Metric, y = Value, fill = Metric)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("lightgreen")) +
  theme_minimal() +
  labs(title = "RMSE Distribution", y = "RMSE", x = "") +
  theme(legend.position = "none")

# Plot for Percent Error
p_percent_error <- ggplot(results[results$Metric == "Percent Error",], aes(x = Metric, y = Value, fill = Metric)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("lightblue")) +
  theme_minimal() +
  labs(title = "Percent Error Distribution", y = "Percent Error", x = "") +
  theme(legend.position = "none")

# Combine the plots into a grid
grid.arrange(p_rmse, p_percent_error, ncol = 2)




write.csv(sdata, "housing_data1_cleaned.csv", row.names = FALSE)

