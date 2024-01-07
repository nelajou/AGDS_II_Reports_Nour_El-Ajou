


## --------------------------------------------------------------------------------------
# Let's run the basic model again but with recording the variable importance
rf_basic <- ranger::ranger( 
  y = df_train[, target],     # target variable
  x = df_train[, predictors_all],   # Predictor variables
  importance   = "permutation", # Pick permutation to calculate variable importance
  seed = 42,                    # Specify seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1) # Use all but one CPU core for quick model training

# Extract the variable importance and create a long tibble
vi_rf_basic <- rf_basic$variable.importance |>
  dplyr::bind_rows() |> 
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable")

# Plot variable importance, ordered by decreasing value
gg <- vi_rf_basic |> 
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable, value), y = value)) +
  ggplot2::geom_bar(stat = "identity", fill = "grey50", width = 0.75) + 
  ggplot2::labs(
    y = "Change in OOB MSE after permutation", 
    x = "",
    title = "Variable importance based on OOB") +
  ggplot2::theme_classic() +
  ggplot2::coord_flip() + ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 45, hjust = 1) )

vi_rf_basic
# Display plot
# gg


## --------------------------------------------------------------------------------------

set.seed(42)

# run the algorithm
bor <- Boruta::Boruta(
  y = df_train[, target],
  x = df_train[, predictors_all],
  maxRuns = 50, # Number of iterations. Set to 30 or lower if it takes too long
  num.threads = parallel::detectCores()-1)

# obtain results: a data frame with all variables, ordered by their importance
df_bor <- Boruta::attStats(bor) |>
  tibble::rownames_to_column() |>
  dplyr::arrange(dplyr::desc(meanImp))

# plot the importance result
feature_importance_plot <- 
  ggplot2::ggplot(ggplot2::aes(x = reorder(rowname, meanImp),
                               y = meanImp,
                               fill = decision),
                  data = df_bor) +
  ggplot2::geom_bar(stat = "identity", width = 0.75) +
  ggplot2::scale_fill_manual(values = c("grey30", "tomato", "grey70")) +
  ggplot2::labs(
    y = "Variable importance",
    x = "",
    title = "Variable importance based on Boruta") +
  ggplot2::theme_classic() +
  ggplot2::coord_flip()

ggsave("../plots/ex_soil/feat_imp_plot.png", plot = feature_importance_plot,
       width = 8, height = 6)


## --------------------------------------------------------------------------------------


# get retained important variables
predictors_selected <- df_bor |>
  dplyr::filter(decision == "Confirmed") |>
  dplyr::pull(rowname)

length(predictors_selected)


# re-train Random Forest model
rf_bor <- ranger::ranger(
  y = df_train[, target],              # target variable
  x = df_train[, predictors_selected], # Predictor variables
  seed = 42,                           # Specify the seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1) # Use all but one CPU core for quick model training

# quick report and performance of trained model object
rf_bor


## --------------------------------------------------------------------------------------
# Save relevant data for model testing in the next chapter.

saveRDS(rf_bor,                   
        here::here("data/ex_soil/rf_bor_wat.10.rds"))

saveRDS(df_train[, c(target, predictors_selected)],
        here::here("data/ex_soil/cal_bor_wat.10.rds"))

saveRDS(df_test[, c(target, predictors_selected)],
        here::here("data/ex_soil/val_bor_wat.10.rds"))



saveRDS(rf_basic,                   
        here::here("data/ex_soil/rf_wat.10.rds"))

saveRDS(df_train[, c(target, predictors_all)],
        here::here("data/ex_soil/cal_wat.10.rds"))

saveRDS(df_test[, c(target, predictors_all)],
        here::here("data/ex_soil/val_wat.10.rds"))



## --------------------------------------------------------------------------------------
# Load random forest model
rf_bor   <- readRDS(here::here("data/ex_soil/rf_bor_wat.10.rds"))
df_train_bor <- readRDS(here::here("data/ex_soil/cal_bor_wat.10.rds"))
df_test_bor  <- readRDS(here::here("data/ex_soil/val_bor_wat.10.rds"))


rf_basic <- readRDS(here::here("data/ex_soil/rf_wat.10.rds"))
df_train <- readRDS(here::here("data/ex_soil/cal_wat.10.rds"))
df_test  <- readRDS(here::here("data/ex_soil/val_wat.10.rds"))



## --------------------------------------------------------------------------------------
# Filter that list only for the variables used in the RF
preds_selected_bor <- names(df_train[ ,predictors_selected])
files_selected_bor <- files_covariates[apply(sapply(X = preds_selected_bor,
                                                    FUN = grepl,
                                                    files_covariates),
                                             MARGIN =  1,
                                             FUN = any)]

# Load all rasters as a stack
raster_covariates_bor <- terra::rast(files_selected_bor)


## --------------------------------------------------------------------------------------
# Get coordinates for which we want data
df_locations <- df_mask |> 
  dplyr::select(x, y)

# Extract data from covariate raster stack for all gridcells in the raster
df_predict_bor <- terra::extract(
  raster_covariates_bor,   # The raster we want to extract from
  df_locations,        # A matrix of x and y values to extract for
  ID = FALSE           # To not add a default ID column to the output
)

df_predict_bor <- cbind(df_locations, df_predict_bor) |> 
  tidyr::drop_na()  # Se_TWI2m has a small number of missing data


## --------------------------------------------------------------------------------------
# Need to load {ranger} because ranger-object is used in predict()
library(ranger) 

# Make predictions for validation sites
prediction_bor <- predict(
  rf_bor,           # RF model
  data = df_test_bor,   # Predictor data
  num.threads = parallel::detectCores() - 1
)

# Save predictions to validation df
df_test_bor$pred <- prediction_bor$predictions


## --------------------------------------------------------------------------------------
# Make predictions using the RF model
prediction_bor <- predict(
  rf_bor,              # RF model
  data = df_predict_bor,   
  num.threads = parallel::detectCores() - 1
)


# Attach predictions to dataframe and round them
df_predict_bor$prediction <- prediction_bor$predictions


## --------------------------------------------------------------------------------------
# Extract dataframe with coordinates and predictions
df_map_bor <- df_predict_bor|>
  dplyr::select(x, y, prediction)

# Turn dataframe into a raster
raster_pred_bor <- terra::rast(
  df_map_bor,                  # Table to be transformed
  crs = "+init=epsg:2056", # Swiss coordinate system
  extent = terra::ext(raster_covariates_bor) # Prescribe same extent as predictor ras
)



## --------------------------------------------------------------------------------------
# Let's have a look at our predictions!
# To have some more flexibility, we can plot this in the ggplot-style as such:
raster_pred_bor_plot <- terra::as.factor(raster_pred_bor)
raster_pred_bor_plot_df <- terra::as.data.frame(raster_pred_bor_plot)


rf_boruta_map <- ggplot2::ggplot(raster_pred_bor_plot_df) +
  tidyterra::geom_spatraster(data = raster_pred_bor_plot) +
  ggplot2::scale_fill_viridis_d(
    na.value = NA,
    option = "viridis",
    name = "Waterlog.100", 
    labels = c("False", "True")
  ) +
  ggplot2::theme_classic() +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::labs(title = "Predicted waterlog at 100 cm depth, binary variable")

ggsave("../plots/ex_soil/rf_boruta_map.png", plot = rf_boruta_map, width = 8, height = 6)


## --------------------------------------------------------------------------------------
terra::writeRaster(
  raster_pred_bor,
  "../data/ex_soil/ra_predicted_bor_ph0-10.tif",
  datatype = "FLT4S",  # FLT4S for floats, INT1U for integers (smaller file)
  filetype = "GTiff",  # GeoTiff format
  overwrite = TRUE     # Overwrite existing file
)


## --------------------------------------------------------------------------------------

# Make classification predictions
y_bor <- as.factor(df_test_bor$waterlog.100)
x_bor <- df_test_bor$pred # Use 0.5 as threshold

# Change class names
levels(y_bor) <- levels(x_bor) <- c("no", "yes")

# plot confusion matrix
conf_matrix_bor <- caret::confusionMatrix(data = x_bor, reference = y_bor)
conf_matrix_bor$table

## --------------------------------------------------------------------------------------
tp_bor <- conf_matrix_bor$table[2,2]
fp_bor <- conf_matrix_bor$table[2,1]

tn_bor <- conf_matrix_bor$table[1,1]
fn_bor <- conf_matrix_bor$table[1,2]


## --------------------------------------------------------------------------------------
acc_bor <- na.omit( (tp_bor + tn_bor )/ sum(conf_matrix_bor$table) )
# cat("the accuracy for the boruta model is ",acc_bor)


## --------------------------------------------------------------------------------------
prec_bor <- na.omit(tp_bor / (tp_bor + fp_bor) )
# cat("the precision for the boruta model is ", prec_bor)


## --------------------------------------------------------------------------------------

f1_bor <-  na.omit(2*tp_bor / (2*tp_bor + fp_bor +fn_bor) )
# cat("the f1 for the boruta model is ", f1_bor)



## --------------------------------------------------------------------------------------
# OOB prediction error of the final model
obb_basic <- sqrt(rf_basic$prediction.error)
obb_bor <- sqrt(rf_bor$prediction.error)


## --------------------------------------------------------------------------------------
# cat("Looking at the prediction error, the Boruta Model would be preferred over the
# basic case since its Prediction error with", sqrt(rf_bor$prediction.error), "smaller than the one of the basic model"
# ) 



## --------------------------------------------------------------------------------------
diag_table <- rbind(diag_basic <- c(round(obb_basic,3), round(acc_basic,3),                                              round(prec_basic,3),round(f1_basic,3) ),
                    diag_bor <-  c(round(obb_bor,3), round(acc_bor,3),                                              round(prec_bor,3),round(f1_bor,3) )
)
diag_table <- as.data.frame(diag_table, row.names = c("Basic RF", "Boruta RF") )
colnames(diag_table) <- c("OBB", "Accuracy", "Precision", "F1")

preferred_model <- as.vector(
  c( rownames(diag_table)[which.min(diag_table$OBB)],
     rownames(diag_table ) [which.max(diag_table$Accuracy)],
     rownames(diag_table) [which.max(diag_table$Precision)],
     rownames(diag_table  )[which.max(diag_table$F1) ]  )  )


diag_table <- rbind(diag_table, preferred_model)

rownames(diag_table) <- c("Basic RF", "Boruta RF", "preferred Model")
diag_table




## --------------------------------------------------------------------------------------
summary(df_predict$prediction == df_predict_bor$prediction)



