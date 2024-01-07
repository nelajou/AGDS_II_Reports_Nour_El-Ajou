

# The same model formulation is in the previous chapter
pred_plus <- paste(predictors_selected,collapse  = " + ")
pp <- recipes::recipe(as.formula(paste0("waterlog.100 ~", pred_plus)), 
                      data = df_train_bor) 


## --------------------------------------------------------------------------------------
# Load required libraries
library(caret)
library(ranger)

# Set seed for reproducibility
set.seed(199)

# Define the hyperparameter grid
mtry_values <- seq(20,38,2)
min.node.size_values <- seq(10,30,2)
# Create a data frame to store results
results <- data.frame()

# Perform hyperparameter tuning with 5-fold cross-validation

# Train the model using caret::train for classification
mod <- caret::train(
  x = df_train_bor[, predictors_selected],
  y = df_train_bor[, target],
  method = "ranger",
  metric = "Accuracy",
  trControl = trainControl(
    method = "cv",
    number = 5,
    savePredictions = "final"
  ),
  tuneGrid = expand.grid(
    .mtry = mtry_values,
    .min.node.size = min.node.size_values,
    .splitrule = "gini"  # or "extratrees" or "entropy", depending on your preference
  ),
  replace = FALSE,
  sample.fraction = 0.5,
  num.trees = 100,
  seed = 1982
)


## --------------------------------------------------------------------------------------
opt_row <- mod$results[which.max(mod$results$Accuracy),]
opt_mtry          <- opt_row$mtry
opt_min_node_size <- opt_row$min.node.size

# opt_row
cat("the optimal value vor mtry is", opt_mtry)


## --------------------------------------------------------------------------------------
cat("the optimal value vor the minimal node size is", opt_min_node_size)



## --------------------------------------------------------------------------------------
hyper.param.table <- 
  as.data.frame(
    rbind(
      c(opt_mtry,opt_min_node_size),
      c(rf_bor$mtry, rf_bor$min.node.size)
    )
  )
rownames(hyper.param.table) <- c("tuned boruta RF" ,"boruta RF")
colnames(hyper.param.table) <- c("mtry", "minimum node size")
hyper.param.table


## --------------------------------------------------------------------------------------
# re-train Random Forest model
rf_bor_tuned <- ranger::ranger(
  y = df_train[, target],              # target variable
  x = df_train[, predictors_selected], # Predictor variables
  seed = 42,                           # Specify the seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1
  , mtry = opt_mtry, min.node.size = opt_min_node_size) # Use all but one CPU core for quick model training

# quick report and performance of trained model object
rf_bor_tuned



## --------------------------------------------------------------------------------------

# Need to load {ranger} because ranger-object is used in predict()
library(ranger) 

# Make predictions for validation sites
prediction_bor_tuned <- predict(
  rf_bor_tuned,           # RF model
  data = df_test_bor,   # Predictor data
  num.threads = parallel::detectCores() - 1
)

# Save predictions to validation df
df_test_bor$pred_tuned <- prediction_bor_tuned$predictions

## --------------------------------------------------------------------------------------




## --------------------------------------------------------------------------------------
# Make classification predictions
y_bor <- as.factor(df_test_bor$waterlog.100)
x_bor <- df_test_bor$pred_tuned # Use 0.5 as threshold

# Change class names
levels(y_bor) <- levels(x_bor) <- c("no", "yes")

# plot confusion matrix
conf_matrix_bor_tuned <- caret::confusionMatrix(data = x_bor, reference = y_bor)
conf_matrix_bor_tuned$table

tp_bor_tuned <- conf_matrix_bor_tuned$table[2,2]
fp_bor_tuned <- conf_matrix_bor_tuned$table[2,1]

tn_bor_tuned <- conf_matrix_bor_tuned$table[1,1]
fn_bor_tuned <- conf_matrix_bor_tuned$table[1,2]


## --------------------------------------------------------------------------------------


acc_bor_tuned <- na.omit( (tp_bor_tuned + tn_bor_tuned )/ sum(conf_matrix_bor_tuned$table) )
cat("the accuracy for the tuned Boruta model is ",acc_bor_tuned)


## --------------------------------------------------------------------------------------

prec_bor_tuned <- na.omit(tp_bor_tuned / (tp_bor_tuned + fp_bor_tuned) )
cat("the Precision for the tuned Boruta model is ",prec_bor_tuned)


## --------------------------------------------------------------------------------------

f1_bor_tuned <-  na.omit(2*tp_bor_tuned / (2*tp_bor_tuned + fp_bor_tuned +fn_bor_tuned) )

cat("the F1 for the tuned Boruta model is ",f1_bor_tuned)


## --------------------------------------------------------------------------------------
obb_bor_tuned <- sqrt(rf_bor_tuned$prediction.error)

cat("the F1 for the tuned Boruta model is ",obb_bor_tuned)



## --------------------------------------------------------------------------------------
diag_table_3 <- rbind( c(round(obb_basic,3), round(acc_basic,3),                                              round(prec_basic,3),round(f1_basic,3) ),
                       diag_bor <-  c(round(obb_bor,3), round(acc_bor,3),  
                                      round(prec_bor,3), round(f1_bor,3) ),
                       diag_bor_tuned <- c(round(obb_bor_tuned,3),  round(acc_bor_tuned,3),                                      round(prec_bor_tuned,3), round(f1_bor_tuned,3) )
)
diag_table_3 <- as.data.frame(diag_table_3)
rownames(diag_table_3) <- c("Basic RF", "Boruta RF","Boruta tuned")

colnames(diag_table_3) <- c("OBB", "Accuracy", "Precision", "F1")

best_model <- as.vector(
  c( rownames(diag_table_3)[which.min(diag_table_3$OBB)],
     rownames(diag_table_3 ) [which.max(diag_table_3$Accuracy)],
     rownames(diag_table_3) [which.max(diag_table_3$Precision)],
     rownames(diag_table_3  )[which.max(diag_table_3$F1) ]  ) 
)


diag_table_3 <- rbind(diag_table_3, best_model)

rownames(diag_table_3) <- c("Basic RF", "Boruta RF","Boruta tuned", "Best Model")


diag_table_3


