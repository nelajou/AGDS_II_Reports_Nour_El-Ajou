

## ----echo = FALSE---------------------------------------------------------------
library(tidyverse)

list.of.packages <- c("ggplot2","skimr", "tidyverse", "caret", "OneR", "dplyr", "kableExtra", "visdat", "h2o")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only=TRUE)


## -------------------------------------------------------------------------------
common_species <- df |> 
  dplyr::group_by(Species) |> 
  dplyr::summarise(count = n()) |> 
  dplyr::arrange(desc(count)) |> 
  dplyr::slice(1:50) |> 
  dplyr::pull(Species)

dfs <- df |> 
  dplyr::select(leafN, lon, lat, elv, mat, map, ndep, mai, Species) |> 
  filter(Species %in% common_species)



## ----echo = FALSE---------------------------------------------------------------
# Data splitting
split <- rsample::initial_split(dfs, prop = 0.7, strata = "Species")
df_train <- rsample::training(split)


## ----echo = FALSE---------------------------------------------------------------
# show missing data
visdat::vis_miss(dfs)
dfs$Species <- dfs$Species |> as.factor()



## -------------------------------------------------------------------------------
# The same model formulation is in the previous chapter
pp <- recipes::recipe(leafN ~ elv + mat + map + ndep + mai + Species, 
                      data = df_train) |> 
  recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
  recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())


## -------------------------------------------------------------------------------
# Load required libraries
library(caret)
library(ranger)

# Set seed for reproducibility
set.seed(199)

# Create a data frame to store results
results <- data.frame()

rf_1000_5 <- caret::train(
  pp, 
  data = df_train |> 
    drop_na(), 
  method = "ranger",
  metric = "RMSE",
  trControl = trainControl(
    method = "cv",
    number = 5,
    savePredictions = "final"
  ),
  tuneGrid = expand.grid(
    .mtry = 3,        # default p/3
    .min.node.size = 12,          
    .splitrule = "variance"      # default "variance"
  ),
  # arguments specific to "ranger" method
  num.trees = 1000
)


## -------------------------------------------------------------------------------
print(rf_1000_5)


## -------------------------------------------------------------------------------
# RMSE of 5-fold cross-validation
rmse.bas <- rf_1000_5$results$RMSE

cat("the RMSE of the basic model is", round(rmse.bas, 3))



## -------------------------------------------------------------------------------
# Rsquared of 5-fold cross-validation
r2.bas <- rf_1000_5$results$Rsquared
cat("the R2 of the basic model is", r2.bas)



## -------------------------------------------------------------------------------
