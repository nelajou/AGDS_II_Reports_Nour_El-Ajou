
## --------------------------------------------------------------------------------------
# ranger() crashes when using tibbles, so we are using the
# base R notation to enter the data
rf_basic <- ranger::ranger( 
  y = df_train[, target],     # target variable
  x = df_train[, predictors_all], # Predictor variables
  seed = 42,                    # Specify the seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1) # Use all but one CPU core for quick model training

# Print a summary of fitted model
# print(rf_basic)

## --------------------------------------------------------------------------------------
# Load area to be predicted
raster_mask <- terra::rast(here::here("data/ex_soil/area_to_be_mapped.tif"))


# Turn target raster into a dataframe, 1 px = 1 cell
df_mask <- as.data.frame(raster_mask, xy = TRUE)

# Filter only for area of interest
df_mask <- df_mask |> 
  dplyr::filter(area_to_be_mapped == 1)

# Display df
head(df_mask) |> 
  knitr::kable()


## --------------------------------------------------------------------------------------
files_covariates <- list.files(
  path = here::here("data/ex_soil/covariates/"), 
  pattern = ".tif$",
  recursive = TRUE, 
  full.names = TRUE
)



## --------------------------------------------------------------------------------------
random_files <- sample(files_covariates, 2)
terra::rast(random_files[1])


## --------------------------------------------------------------------------------------
# Filter that list only for the variables used in the RF
preds_selected <- names(df_train[ ,predictors_all])
files_selected <- files_covariates[apply(sapply(X = preds_selected,
                                                FUN = grepl,
                                                files_covariates),
                                         MARGIN =  1,
                                         FUN = any)]

# Load all rasters as a stack
raster_covariates <- terra::rast(files_selected)



## --------------------------------------------------------------------------------------
# Get coordinates for which we want data
df_locations <- df_mask |> 
  dplyr::select(x, y)

# Extract data from covariate raster stack for all gridcells in the raster
df_predict <- terra::extract(
  raster_covariates,   # The raster we want to extract from
  df_locations,        # A matrix of x and y values to extract for
  ID = FALSE           # To not add a default ID column to the output
)

df_predict <- cbind(df_locations, df_predict) |> 
  tidyr::drop_na()  # Se_TWI2m has a small number of missing data


## --------------------------------------------------------------------------------------
# Need to load {ranger} because ranger-object is used in predict()
library(ranger) 

# Make predictions for validation sites
prediction <- predict(
  rf_basic,           # RF model
  data = df_test,   # Predictor data
  num.threads = parallel::detectCores() - 1
)

# Save predictions to validation df
df_test$pred <- prediction$predictions



## --------------------------------------------------------------------------------------
# Make predictions using the RF model
prediction <- predict(
  rf_basic,              # RF model
  data = df_predict,   
  num.threads = parallel::detectCores() - 1
)

# Attach predictions to dataframe and round them
df_predict$prediction <- prediction$predictions


## --------------------------------------------------------------------------------------
# Extract dataframe with coordinates and predictions
df_map <- df_predict |>
  dplyr::select(x, y, prediction)

# Turn dataframe into a raster
raster_pred <- terra::rast(
  df_map,                  # Table to be transformed
  crs = "+init=epsg:2056", # Swiss coordinate system
  extent = terra::ext(raster_covariates) # Prescribe same extent as predictor rasters
)


## --------------------------------------------------------------------------------------
raster_pred_plot <- terra::as.factor(raster_pred)
raster_pred_plot_df <- terra::as.data.frame(raster_pred_plot)

# Let's have a look at our predictions!
# To have some more flexibility, we can plot this in the ggplot-style as such:
rf_basic_map <- ggplot2::ggplot(raster_pred_plot_df) +
  tidyterra::geom_spatraster(data = raster_pred_plot) +
  ggplot2::scale_fill_viridis_d(
    na.value = NA,
    option = "viridis",
    name = "Waterlog",
    labels = c("False", "True")
  ) +
  ggplot2::theme_classic() +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::labs(title = "Predicted waterlog at 100 cm depth, binary variable")

ggsave("../plots/ex_soil/rf_basic_map.png", plot = rf_basic_map, width = 8, height = 6)


## --------------------------------------------------------------------------------------
# Save raster as .tif file
terra::writeRaster(
  raster_pred,
  "../data/ex_soil/ra_predicted_ph0-10.tif",
  datatype = "FLT4S",  # FLT4S for floats, INT1U for integers (smaller file)
  filetype = "GTiff",  # GeoTiff format
  overwrite = TRUE     # Overwrite existing file
)

## --------------------------------------------------------------------------------------
# Make classification predictions
y <- as.factor(df_test$waterlog.100)
x <- df_test$pred # Use 0.5 as threshold

# Change class names
levels(y) <- levels(x) <- c("no", "yes")

# plot confusion matrix
conf_matrix_basic <- caret::confusionMatrix(data = x, reference = y)
conf_matrix_basic


## --------------------------------------------------------------------------------------
tp_basic <- conf_matrix_basic$table[2,2]
fp_basic <- conf_matrix_basic$table[2,1]

tn_basic <- conf_matrix_basic$table[1,1]
fn_basic <- conf_matrix_basic$table[1,2]


## --------------------------------------------------------------------------------------
acc_basic <- na.omit( (tp_basic + tn_basic )/ sum(conf_matrix_basic$table) )
# cat("the accuracy for the basic model is ",acc_basic)


## --------------------------------------------------------------------------------------
prec_basic <- na.omit(tp_basic / (tp_basic + fp_basic) )
# cat("the precision for the basic model is ",prec_basic)


## --------------------------------------------------------------------------------------
f1_basic <-  na.omit(2*tp_basic / (2*tp_basic + fp_basic +fn_basic) )
# cat("the f1 for the basic model is ",f1_basic)



## --------------------------------------------------------------------------------------
# balance

# summary(df_train$waterlog.100 == 1)

share_of_postives_train <-  as.numeric(summary(df_train$waterlog.100 == 1)[3])/  ( (as.numeric(summary(df_train$waterlog.100 == 1)[2]) ) + 
                                                                                     (as.numeric(summary(df_train$waterlog.100 == 1)[3]) ) )




