

## ----setup, include=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
rm(list = ls())


## ----echo = FALSE----------------------------------------------------------------------
list.of.packages <- c("ggplot2", "tidyverse", "caret", "OneR", "dplyr", "kableExtra", "visdat", "lubridate", "ranger", "stringr", "utils", "lattice", "tibble", "stats", "datasets", "purrr", "graphics", "methods", "OneR", "forcats", "grDevices")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only=TRUE)


## --------------------------------------------------------------------------------------
library(tidyverse)

# sessionInfo()

# Load soil data from sampling locations
df_obs <- readr::read_csv(
  here::here("data/ex_soil/berne_soil_sampling_locations.csv")
)

# Display data
head(df_obs) |> 
  knitr::kable()



## --------------------------------------------------------------------------------------
# Get a list with the path to all raster files
list_raster <- list.files(
  here::here("data/ex_soil/covariates"),
  full.names = TRUE
)


## --------------------------------------------------------------------------------------
# Display data (lapply to clean names)
lapply(
  list_raster, 
  function(x) sub(".*/(.*)", "\\1", x)
) |> 
  unlist() |> 
  head(5) |> 
  print()


## --------------------------------------------------------------------------------------
# Load a raster file as example: Picking the slope profile at 2 m resolution
raster_example <- terra::rast(
  here::here("data/ex_soil/covariates/Se_slope2m.tif")
)
# raster_example


## --------------------------------------------------------------------------------------
# Load all files as one batch
all_rasters <- terra::rast(list_raster)
all_rasters





## --------------------------------------------------------------------------------------
# Extract coordinates from sampling locations
sampling_xy <- df_obs |> 
  dplyr::select(x, y)

# From all rasters, extract values for sampling coordinates
df_covars <- terra::extract(
  all_rasters,  # The raster we want to extract from
  sampling_xy,  # A matrix of x and y values to extract for
  ID = FALSE    # To not add a default ID column to the output
)

df_full <- cbind(df_obs, df_covars)

head(df_full) |> 
  knitr::kable() 


## --------------------------------------------------------------------------------------
vars_categorical <- df_covars |> 
  
  # Get number of distinct values per variable
  dplyr::summarise(dplyr::across(dplyr::everything(), ~dplyr::n_distinct(.))) |> 
  
  # Turn df into long format for easy filtering
  tidyr::pivot_longer(
    dplyr::everything(), 
    names_to = "variable", 
    values_to = "n"
  ) |> 
  
  # Filter out variables with 10 or less distinct values
  dplyr::filter(n <= 10) |>
  
  # Extract the names of these variables
  dplyr::pull('variable')

# cat("Variables with less than 10 distinct values:", 
#     ifelse(length(vars_categorical) == 0, "none", vars_categorical))


## --------------------------------------------------------------------------------------
df_full <- df_full |> 
  dplyr::mutate(dplyr::across(all_of(vars_categorical), ~as.factor(.)))


## --------------------------------------------------------------------------------------

# Get number of rows to calculate percentages
n_rows <- nrow(df_full)

# Get number of distinct values per variable
df_full |> 
  dplyr::summarise(dplyr::across(dplyr::everything(), 
                                 ~ length(.) - sum(is.na(.)))) |> 
  tidyr::pivot_longer(dplyr::everything(), 
                      names_to = "variable", 
                      values_to = "n") |>
  dplyr::mutate(perc_available = round(n / n_rows * 100)) |> 
  dplyr::arrange(perc_available) |> 
  head(10) |> 
  knitr::kable()


## --------------------------------------------------------------------------------------
na_dist_plot <-  df_full |> 
  dplyr::select(1:20) |>   # reduce data for readability of the plot
  visdat::vis_miss()

ggsave("../plots/ex_soil/na_dist_plot.png", plot = na_dist_plot,
       width = 8, height = 6)

## --------------------------------------------------------------------------------------
# if (!dir.exists(here::here("data"))) system(paste0("mkdir ", here::here("data")))
# saveRDS(df_full, 
# here::here("data/ex_soil/df_full.rds"))


## --------------------------------------------------------------------------------------
# df_full <- readRDS(here::here("data/ex_soil/df_full.rds"))

head(df_full) |> 
  knitr::kable()


## --------------------------------------------------------------------------------------
# Specify target: The pH in the top 10cm
target <- "waterlog.100"
df_full <- subset(df_full, select = - c(vszone) )

# Specify predictors_all: Remove soil sampling and observational data
predictors_all <- names(df_full)[14:ncol(df_full)]


# cat("The target is:", target,
#     "\nThe predictors_all are:", paste0(predictors_all[1:8], sep = ", "), "...")


## --------------------------------------------------------------------------------------
df_full$waterlog.100 <- as.factor(df_full$waterlog.100)

# Split dataset into training and testing sets
df_train <- df_full |> dplyr::filter(dataset == "calibration")
df_test  <- df_full |> dplyr::filter(dataset == "validation")

# Filter out any NA to avoid error when running a Random Forest
df_train <- df_train |> tidyr::drop_na()
df_test <- df_test   |> tidyr::drop_na()

# A little bit of verbose output:
n_tot <- nrow(df_train) + nrow(df_test)

perc_cal <- (nrow(df_train) / n_tot) |> round(2) * 100
perc_val <- (nrow(df_test)  / n_tot) |> round(2) * 100
# 
# cat("For model training, we have a calibration / validation split of: ",
#     perc_cal, "/", perc_val, "%")


## --------------------------------------------------------------------------------------
