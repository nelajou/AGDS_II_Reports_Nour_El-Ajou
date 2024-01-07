

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

sessionInfo()

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
raster_example


## --------------------------------------------------------------------------------------
# Load all files as one batch
all_rasters <- terra::rast(list_raster)
all_rasters

