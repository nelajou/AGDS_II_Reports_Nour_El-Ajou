# -------------------------------------------------------------------------------
# DISPLAYING SPATIAL DISTRIBUTION
list.of.packages <- c("sf", "rnaturalearth", "rnaturalearthdata", "cowplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only=TRUE)


## -------------------------------------------------------------------------------
# get coast outline
coast <- rnaturalearth::ne_coastline(scale = 110, returnclass = "sf")

ggplot() +
  
  # plot coastline
  geom_sf(data = coast,
          colour = 'black',
          size = 0.2) +
  
  # set extent in longitude and latitude
  coord_sf(
    ylim = c(-60, 80),
    expand = FALSE) +  # to draw map strictly bounded by the specified extent
  
  # plot points on map
  geom_point(data = dfs, aes(x = lon, y = lat), color = "red", size = 0.2) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")


## ----echo = FALSE---------------------------------------------------------------
# K-Means algorithm
clusters <- kmeans(
  dfs |> dplyr::select(lon, lat),
  centers = 5
)

#add to dataset
dfs <- cbind(dfs, clusters[["cluster"]]) |> rename(cluster = 10)


## ----echo = FALSE---------------------------------------------------------------
ggplot() +
  
  # plot coastline
  geom_sf(data = coast,
          colour = 'black',
          size = 0.3) +
  
  # set extent in longitude and latitude
  coord_sf(
    ylim = c(-60, 80),
    expand = FALSE) +  # to draw map strictly bounded by the specified extent
  
  # plot points on map
  geom_point(data = dfs, aes(x = lon, y = lat, col = as.factor(cluster)),
             size = 0.5) +
  labs(x = "", y = "", col = "Cluster", title = "Spatial Clusters based on Geographical distance") +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=5)))


## ----echo = FALSE---------------------------------------------------------------
# Distribution curve plot
curve_plot <- ggplot(dfs, aes(x = leafN, color = as.factor(cluster))) +
  geom_density(alpha = 0.7) +
  labs(x = "Leaf N content (%)", y = "Density", col = "Cluster") +
  theme_minimal()

# Boxplot plot
boxplot_plot <- ggplot(dfs, aes(x = cluster, y = leafN, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(x = "Cluster", y = "Leaf N content (%)", fill = "Cluster") +
  theme_minimal()

# Combine both plots using cowplot package
combined_plot <- plot_grid(curve_plot, boxplot_plot, ncol = 2, labels = c("A", "B"))

print(combined_plot)


## -------------------------------------------------------------------------------
# create folds based on clusters
group_folds_train <- purrr::map(
  seq(length(unique(dfs$cluster))),
  ~ {
    dfs |> 
      select(cluster) |> 
      mutate(idx = 1:n()) |> 
      filter(cluster != .) |> 
      pull(idx)
  }
)

group_folds_test <- purrr::map(
  seq(length(unique(dfs$cluster))),
  ~ {
    dfs |> 
      select(cluster) |> 
      mutate(idx = 1:n()) |> 
      filter(cluster == .) |> 
      pull(idx)
  }
)


## -------------------------------------------------------------------------------
# create a function that trains a random forest model on a given set of rows and 
# predicts on a disjunct set of rows
train_test_by_fold <- function(train_idx, val_idx) {
  
  # Train the model
  mod <- ranger(
    formula = leafN ~ elv + mat + map + ndep + mai + Species, 
    data = dfs[train_idx, ],
    mtry = 3,
    min.node.size = 12,
    num.trees = 500
  )
  
  
  # Predict on the validation set
  pred <- predict(mod, data = dfs[val_idx, ])
  
  # Calculate R-squared on the validation set
  rsq <- cor(pred$predictions, dfs$leafN[val_idx])^2
  
  # Calculate RMSE on the validation set
  rmse <- sqrt(mean((pred$predictions - dfs$leafN[val_idx])^2))
  
  return(tibble(rsq = rsq, rmse = rmse))
}

# apply function on each custom fold and collect validation results in a nice
# data frame
output_geo <- purrr::map2_dfr(
  group_folds_train,
  group_folds_test,
  ~train_test_by_fold(.x, .y)
) |> 
  mutate(test_fold = 1:5)

print(output_geo)


## -------------------------------------------------------------------------------
rmse.geo <- (mean(output_geo$rmse))
cat("the mean RMSE of the Geographical Cross Validation is", round(rmse.geo,3 ))


## -------------------------------------------------------------------------------
r2.geo <- mean(output_geo$rsq)
cat("the mean R-Squared of the Geographical Cross Validation is",round(r2.geo,3) )



## -------------------------------------------------------------------------------
comp.table <- as.data.frame(
  rbind(cbind(rmse.bas, r2.bas),
        cbind(rmse.geo, r2.geo)
  )
)

colnames(comp.table) <- c("RMSE", "R2")
rownames(comp.table) <- c("Random CV" , "Geographic CV")

round(comp.table,3)


