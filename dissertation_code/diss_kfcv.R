
## CART, no ethnic ------------------------------------------------------------

## packages
library (caret)

df = final_input_data_imp_cart

# Set the number of folds for cross-validation
k <- 24

# Create a vector of the unique years in dataset
years <- as.numeric (unique (df$year))

# Initialize an empty vector to store the fold indices
fold_indices <- vector("list", k)

# Loop over each fold and create the indices for the training and test data
for (i in 1:k) {
  # Determine the years to use for training and testing for this fold
  train_years <- min(years):(max(years) - (k - i))
  test_years <- max (train_years) + 1
  
  # Create the indices for the training and test data for this fold
  train_index <- which(df$year %in% train_years)
  test_index <- which(df$year == test_years)
  
  # Store the fold indices in the fold_indices vector
  fold_indices[[i]] <- list(train = train_index, test = test_index)
}

# Initialize a vector to store the performance metrics for each fold
metrics <- matrix(NA, k, 2, dimnames = list(NULL, c("RMSE", "R^2")))

# Now you can use the fold indices to perform k-fold cross-validation
for (i in 1:k) {
  # Get the indices for the training and test data for this fold
  train_index <- fold_indices[[i]]$train
  test_index <- fold_indices[[i]]$test
  
  # Subset the data into the training and test sets
  train_data <- df[train_index, ]
  test_data <- df[test_index, ]
  
  # Fit gravity model on the training data
  model <- lm (totalrefugees ~ dist + 
                 ref_lag + 
                 contig + 
                 gdp_d +
                 gdp_d_lag + 
                 pop_o + 
                 pop_d + 
                 gdpcap_d +
                 political_terror_o +
                 state_fragility_o + 
                 state_fragility_o_lag + 
                 conflict_intensity_o + 
                 as.factor (orig) + 
                 as.factor (dest),
               data = train_data)
  
  # Predict the test data using the fitted model
  test_pred <- predict(model, newdata = test_data)
  
  # Calculate the RMSE and R^2 for the test data
  metrics[i, "RMSE"] <- sqrt(mean((test_data$totalrefugees - test_pred)^2, na.rm = T))
  metrics[i, "R^2"] <- summary(model)$adj.r.squared
}

# Print the average performance metrics across all folds
cat("Average RMSE:", mean(metrics[, "RMSE"], na.rm = T), "\n")
cat("Average R^2:", mean(metrics[, "R^2"]), "\n")

## Average RMSE: 1.664438 
## Average R^2: 0.7461517  

## CART, ethnic ----------------------------------------------------------------

df = final_input_data_imp_cart

# Set the number of folds for cross-validation
k <- 24

# Create a vector of the unique years in your dataset
years <- as.numeric(unique (df$year))

# Initialize an empty vector to store the fold indices
fold_indices <- vector("list", k)

# Loop over each fold and create the indices for the training and test data
for (i in 1:k) {
  # Determine the years to use for training and testing for this fold
  train_years <- min(years):(max(years) - (k - i))
  test_years <- max (train_years) + 1
  
  # Create the indices for the training and test data for this fold
  train_index <- which(df$year %in% train_years)
  test_index <- which(df$year == test_years)
  
  # Store the fold indices in the fold_indices vector
  fold_indices[[i]] <- list(train = train_index, test = test_index)
}

# Initialize a vector to store the performance metrics for each fold
metrics <- matrix(NA, k, 2, dimnames = list(NULL, c("RMSE", "R^2")))

# Now you can use the fold indices to perform k-fold cross-validation
for (i in 1:k) {
  # Get the indices for the training and test data for this fold
  train_index <- fold_indices[[i]]$train
  test_index <- fold_indices[[i]]$test
  
  # Subset the data into the training and test sets
  train_data <- df[train_index, ]
  test_data <- df[test_index, ]
  
  # Fit a general linear model on the training data
  model <- lm (totalrefugees ~ dist + 
                 ref_lag + 
                 contig + 
                 gdp_d +
                 gdp_d_lag + 
                 pop_o + 
                 pop_d + 
                 gdpcap_d +
                 political_terror_o +
                 state_fragility_o + 
                 state_fragility_o_lag + 
                 conflict_intensity_o +  
                 as.factor (ethnic_link) + 
                 as.factor (comlang_off) + 
                 as.factor (comlang_ethno) + 
                 as.factor (comcol) + 
                 comrelig +
                 as.factor (orig) + 
                 as.factor (dest),
               data = train_data)
  
  # Predict the test data using the fitted model
  test_pred <- predict(model, newdata = test_data)
  
  # Calculate the RMSE and R^2 for the test data
  metrics[i, "RMSE"] <- sqrt(mean((test_data$totalrefugees - test_pred)^2, na.rm = T))
  metrics[i, "R^2"] <- summary(model)$adj.r.squared
}

# Print the average performance metrics across all folds
cat("Average RMSE:", mean(metrics[, "RMSE"], na.rm = T), "\n")
cat("Average R^2:", mean(metrics[, "R^2"]), "\n")

## Average RMSE: 1.663257
## Average R^2: 0.751582  

## mean, no ethnic -------------------------------------------------------------

df = final_input_data_imp_mean

# Set the number of folds for cross-validation
k <- 24

# Create a vector of the unique years in dataset
years <- as.numeric (unique (df$year))

# Initialize an empty vector to store the fold indices
fold_indices <- vector("list", k)

# Loop over each fold and create the indices for the training and test data
for (i in 1:k) {
  # Determine the years to use for training and testing for this fold
  train_years <- min(years):(max(years) - (k - i))
  test_years <- max (train_years) + 1
  
  # Create the indices for the training and test data for this fold
  train_index <- which(df$year %in% train_years)
  test_index <- which(df$year == test_years)
  
  # Store the fold indices in the fold_indices vector
  fold_indices[[i]] <- list(train = train_index, test = test_index)
}

# Initialize a vector to store the performance metrics for each fold
metrics <- matrix(NA, k, 2, dimnames = list(NULL, c("RMSE", "R^2")))

# Now you can use the fold indices to perform k-fold cross-validation
for (i in 1:k) {
  # Get the indices for the training and test data for this fold
  train_index <- fold_indices[[i]]$train
  test_index <- fold_indices[[i]]$test
  
  # Subset the data into the training and test sets
  train_data <- df[train_index, ]
  test_data <- df[test_index, ]
  
  # Fit gravity model on the training data
  model <- lm (totalrefugees ~ dist + 
                 ref_lag + 
                 contig + 
                 gdp_d +
                 gdp_d_lag + 
                 pop_o + 
                 pop_d + 
                 gdpcap_d +
                 political_terror_o +
                 state_fragility_o + 
                 state_fragility_o_lag + 
                 conflict_intensity_o + 
                 as.factor (orig) + 
                 as.factor (dest),
               data = train_data)
  
  # Predict the test data using the fitted model
  test_pred <- predict(model, newdata = test_data)
  
  # Calculate the RMSE and R^2 for the test data
  metrics[i, "RMSE"] <- sqrt(mean((test_data$totalrefugees - test_pred)^2, na.rm = T))
  metrics[i, "R^2"] <- summary(model)$adj.r.squared
}

# Print the average performance metrics across all folds
cat("Average RMSE:", mean(metrics[, "RMSE"], na.rm = T), "\n")
cat("Average R^2:", mean(metrics[, "R^2"]), "\n")

## Average RMSE: 1.551051 
## Average R^2: 0.8817866 

## mean, ethnic ----------------------------------------------------------------

df = final_input_data_imp_mean

# Set the number of folds for cross-validation
k <- 24

# Create a vector of the unique years in your dataset
years <- as.numeric(unique (df$year))

# Initialize an empty vector to store the fold indices
fold_indices <- vector("list", k)

# Loop over each fold and create the indices for the training and test data
for (i in 1:k) {
  # Determine the years to use for training and testing for this fold
  train_years <- min(years):(max(years) - (k - i))
  test_years <- max (train_years) + 1
  
  # Create the indices for the training and test data for this fold
  train_index <- which(df$year %in% train_years)
  test_index <- which(df$year == test_years)
  
  # Store the fold indices in the fold_indices vector
  fold_indices[[i]] <- list(train = train_index, test = test_index)
}

# Initialize a vector to store the performance metrics for each fold
metrics <- matrix(NA, k, 2, dimnames = list(NULL, c("RMSE", "R^2")))

# Now you can use the fold indices to perform k-fold cross-validation
for (i in 1:k) {
  # Get the indices for the training and test data for this fold
  train_index <- fold_indices[[i]]$train
  test_index <- fold_indices[[i]]$test
  
  # Subset the data into the training and test sets
  train_data <- df[train_index, ]
  test_data <- df[test_index, ]
  
  # Fit a general linear model on the training data
  model <- lm (totalrefugees ~ dist + 
                 ref_lag + 
                 contig + 
                 gdp_d +
                 gdp_d_lag + 
                 pop_o + 
                 pop_d + 
                 gdpcap_d +
                 political_terror_o +
                 state_fragility_o + 
                 state_fragility_o_lag + 
                 conflict_intensity_o +  
                 as.factor (ethnic_link) + 
                 as.factor (comlang_off) + 
                 as.factor (comlang_ethno) + 
                 as.factor (comcol) + 
                 comrelig +
                 as.factor (orig) + 
                 as.factor (dest),
               data = train_data)
  
  # Predict the test data using the fitted model
  test_pred <- predict(model, newdata = test_data)
  
  # Calculate the RMSE and R^2 for the test data
  metrics[i, "RMSE"] <- sqrt(mean((test_data$totalrefugees - test_pred)^2, na.rm = T))
  metrics[i, "R^2"] <- summary(model)$adj.r.squared
}

# Print the average performance metrics across all folds
cat("Average RMSE:", mean(metrics[, "RMSE"], na.rm = T), "\n")
cat("Average R^2:", mean(metrics[, "R^2"]), "\n")

## Average RMSE: 1.578868 
## Average R^2: 0.8824293








