  library(caret)
  library(ggplot2)
  
  
  sdata <- read.csv("realtor-data.zip.csv", header = TRUE, sep = ",")
  
  
  
  # Filter the rows where the "state" column is "Florida"
  sdata <- sdata[sdata$state == "Florida", ]
  
  # Remove columns 'prev_sold_date', 'status', and 'brokered_by' from sdata_cleaned
  sdata_cleaned <- sdata[, !(names(sdata) %in% c("prev_sold_date", "status", "brokered_by", "state", "street", "city"))]
  
  # # Sort the dataset by 'price' in descending order and remove the top 10 rows
  sdata_cleaned <- sdata_cleaned[order(sdata_cleaned$price, decreasing = TRUE), ]
  
  # Remove the top 10 rows with the highest 'price' values
  sdata_cleaned <- sdata_cleaned[-(1:20000), ]
  
  # Convert 'acre_lot' to numeric
  sdata_cleaned$acre_lot <- as.numeric(sdata_cleaned$acre_lot)
  
  # Check for NA values introduced during the conversion
  sum(is.na(sdata_cleaned$acre_lot))  # Prints the number of NAs
  
  # Handle potential NAs (optional)
  # For example, you could remove rows with NA in 'acre_lot'
  sdata_cleaned <- sdata_cleaned[!is.na(sdata_cleaned$acre_lot), ]
  
  # Create bins for 'acre_lot'
  sdata_cleaned$acre_lot_bins <- cut(sdata_cleaned$acre_lot, 
                                     breaks = c(0, 0.5, 1, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80, 90, 100, Inf), 
                                     labels = c("0-0.5", "0.5-1", "1-5", "5-10", "10-15", "15-20", "20-25", 
                                                "25-30", "30-35", "35-40", "40-50", "50-60", "60-70", "70-80", 
                                                "80-90", "90-100", "100+"),
                                     right = FALSE)
  
  # Box plot of 'price' grouped by 'acre_lot_bins'
  ggplot(sdata_cleaned, aes(x = acre_lot_bins, y = price)) + 
    geom_boxplot(fill = "lightgreen", color = "darkgreen") + 
    labs(title = "Box Plot of Price by Acre Lot Size", 
         x = "Acre Lot Size (Bins)", 
         y = "Price") + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  
 
  
  
  # Create dummy variables for the 'city' column using model.matrix
  # dummies <- model.matrix(~ city - 1, data = sdata_cleaned)
  
  # Convert the matrix into a data frame and add it back to the dataset
  # dummies_df <- as.data.frame(dummies)
  
  # Replace the original 'city' column with the dummies in the dataset
  # sdata_cleaned <- cbind(sdata_cleaned, dummies_df)
  
  # Remove the original 'city' column
  # sdata_cleaned$city <- NULL
  
  
  # Alternatively, you can use complete.cases to keep only rows without NAs
  sdata_cleaned <- sdata_cleaned[complete.cases(sdata), ]
  
  # Remove columns that contain only empty strings
  sdata_cleaned <- sdata_cleaned[, apply(sdata_cleaned, 2, function(x) !all(x == ""))]
  
  colnames(sdata_cleaned)
  
  # Fit a linear model
  model <- lm(price ~ ., data = sdata_cleaned)
  
  # Check VIF
  # install.packages("car")
  # library(car)
  # vif(model)
  # 
  # # Check correlations between the dummy variables (excluding the intercept)
  # cor_matrix <- cor(sdata_cleaned[, sapply(sdata_cleaned, is.numeric)])
  # View(cor_matrix)
  
  
  # Summarize the model to see the coefficients, p-values, and other statistics
  summary(model)
  
  ### REMOVE OUTLIERS ###
  # Remove rows where 'bed' > 15 using subset()
  sdata_cleaned <- subset(sdata_cleaned, bed <= 25)
  
  


  # Remove the top 10 rows with the highest 'price' values
  sdata_scatter <- sdata_cleaned[-(1:6000), ]
  # 
  # # Sort the dataset by 'acre_lot' in descending order and remove the top 10 rows
  # sdata_scatter <- sdata_scatter[order(sdata_scatter$acre_lot, decreasing = TRUE), ]
  # 
  # # Remove the top 10 rows with the highest 'acre_lot' values
  # sdata_scatter <- sdata_scatter[-(1:2000), ]


  
  # Box plot of 'price' grouped by 'bed'
  ggplot(sdata_scatter, aes(x = factor(bed), y = price)) + 
    geom_boxplot(fill = "lightblue", color = "darkblue") + 
    labs(title = "Box Plot of Price by Bed Count", 
         x = "Number of Bedrooms (Bed)", 
         y = "Price") + 
    theme_minimal()
  
  
  # Linear Regression Model
  linear_model <- lm(price ~ ., data = sdata_scatter)
  
  # Summary of the Linear Regression Model
  summary(linear_model)
  
  
  sdata_scatter <- sdata_scatter[complete.cases(sdata_scatter), ]
  
  # Install and load the randomForest package
  if (!require(randomForest)) install.packages("randomForest")
  library(randomForest)
  
  # Random Forest Model
  set.seed(42)  # For reproducibility
  rf_model <- randomForest(price ~ ., data = sdata_scatter, ntree = 500, mtry = 3, importance = TRUE)
  
  # Print Random Forest Model
  print(rf_model)
  
  # Variable importance plot
  varImpPlot(rf_model)
  

  
  # Install and load the nnet package
  if (!require(nnet)) install.packages("nnet")
  library(nnet)
  
  # Normalize the data for the Neural Network (scaled between 0 and 1)
  normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  
  # Normalize predictors and target variable
  sdata_normalized <- as.data.frame(lapply(sdata_scatter, normalize))
  
  # Neural Network Model
  nn_model <- nnet(price ~ ., data = sdata_normalized, size = 10, maxit = 500, linout = TRUE, decay = 0.01)
  
  # Print Neural Network Model
  print(nn_model)
  
  
  # Predictions
  linear_pred <- predict(linear_model, newdata = sdata_scatter)
  rf_pred <- predict(rf_model, newdata = sdata_scatter)
  nn_pred <- predict(nn_model, newdata = sdata_normalized)
  
  # Calculate MSE for each model
  mse_linear <- mean((linear_pred - sdata_scatter$price)^2)
  mse_rf <- mean((rf_pred - sdata_scatter$price)^2)
  mse_nn <- mean((nn_pred - sdata_normalized$price)^2)
  
  # Print MSEs
  cat("Linear Model MSE:", mse_linear, "\n")
  cat("Random Forest MSE:", mse_rf, "\n")
  cat("Neural Network MSE:", mse_nn, "\n")
  
  

  
