

rf_basic <- ranger::ranger( 
  y = df_train[, target],     # target variable
  x = df_train[, predictors_all], # Predictor variables
  seed = 42,                    # Specify the seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1) # Use all but one CPU core for quick model training

# Print a summary of fitted model


