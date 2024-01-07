
## ----echo = FALSE---------------------------------------------------------------
dfs_env <- dfs
dfs_env$map <- (dfs_env$map-mean(dfs_env$map) )/sd(dfs_env$map)
dfs_env$mat <- (dfs_env$mat-mean(dfs_env$mat) )/sd(dfs_env$mat)


## ----echo = FALSE---------------------------------------------------------------
clusters_env <- kmeans(
  dfs_env |> dplyr::select(map, mat),
  centers = 5
)

dfs_env<- cbind(dfs_env, clusters_env[["cluster"]]) |> rename(cluster_env = 11)


## ----echo = FALSE---------------------------------------------------------------
env_clusters_map <- ggplot() +
  
  # plot coastline
  geom_sf(data = coast,
          colour = 'black',
          size = 0.3) +
  
  # set extent in longitude and latitude
  coord_sf(
    ylim = c(-60, 80),
    expand = FALSE) +  # to draw map strictly bounded by the specified extent
  
  # plot points on map
  geom_point(data = dfs_env, aes(x = lon, y = lat, col = as.factor(cluster_env)),
             size = 0.5) +
  labs(x = "", y = "", col = "Environmental Cluster",
       title = "Environmental Clusters based on mean annual temperature and precipitation") +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size=5)))

ggsave("../plots/ex_upscale/env_clusters_map.png", plot = env_clusters_map)


## ----echo = FALSE---------------------------------------------------------------
# Distribution curve plot
curve_plot <- ggplot(dfs_env, aes(x = leafN, color = as.factor(cluster_env))) +
  geom_density(alpha = 0.7) +
  labs(x = "Leaf N content (%)", y = "Density", col = "Cluster") +
  theme_minimal()

# Boxplot plot
boxplot_plot <- ggplot(dfs_env, aes(x = cluster, y = leafN, fill = as.factor(cluster_env))) +
  geom_boxplot() +
  labs(x = "Cluster", y = "Leaf N content (%)", fill = "Cluster") +
  theme_minimal()

# Combine both plots using cowplot package
combined_env_plot <- plot_grid(curve_plot, boxplot_plot, ncol = 2, labels = c("A", "B"))

ggsave("../plots/ex_upscale/combined_env_plot.png", plot = combined_env_plot)

## ----echo = FALSE---------------------------------------------------------------
# create folds based on envrionmental clusters
group_folds_train <- purrr::map(
  seq(length(unique(dfs_env$cluster_env))),
  ~ {
    dfs_env |> 
      select(cluster_env) |> 
      mutate(idx = 1:n()) |> 
      filter(cluster_env != .) |> 
      pull(idx)
  }
)

group_folds_test <- purrr::map(
  seq(length(unique(dfs_env$cluster_env))),
  ~ {
    dfs_env |> 
      select(cluster_env) |> 
      mutate(idx = 1:n()) |> 
      filter(cluster_env == .) |> 
      pull(idx)
  }
)


## ----echo = FALSE---------------------------------------------------------------
# create a function that trains a random forest model on a given set of rows and 
# predicts on a disjunct set of rows
train_test_by_fold <- function(train_idx, val_idx) {
  
  # Train the model
  mod <- ranger(
    formula = leafN ~ elv + mat + map + ndep + mai + Species, 
    data = dfs_env[train_idx, ],
    mtry = 3,
    min.node.size = 12,
    num.trees = 1000
  )
  
  # Predict on the validation set
  pred <- predict(mod, data = dfs_env[val_idx, ])
  
  # Calculate R-squared on the validation set
  rsq <- cor(pred$predictions, dfs_env$leafN[val_idx])^2
  
  # Calculate RMSE on the validation set
  rmse <- sqrt(mean((pred$predictions - dfs_env$leafN[val_idx])^2))
  
  return(tibble(rsq = rsq, rmse = rmse))
}

# apply function on each custom fold and collect validation results in a nice
# data frame
output_env <- purrr::map2_dfr(
  group_folds_train,
  group_folds_test,
  ~train_test_by_fold(.x, .y)
) |> 
  mutate(test_fold = 1:5)


## ----echo = FALSE---------------------------------------------------------------
output_env



## -------------------------------------------------------------------------------
rmse.env <- (mean(output_env$rmse))
# cat("the RMSE of the environmental Cross Validation is", rmse.env)


## -------------------------------------------------------------------------------
# cat("the R-Squared of the environmental Cross Validation is", mean(output_env$rsq))



## -------------------------------------------------------------------------------
rmse.table <- as.data.frame(rbind(
  round(c(rmse.bas, rmse.geo, rmse.env),3),
  round(c(rf_1000_5$results$Rsquared, mean(output_geo$rsq), mean(output_env$rsq)),3)
) )
rownames(rmse.table) <- c("RMSE", "R2")
colnames(rmse.table) <- c("Random CV", "Geographical  CV", "Environmental CV")
# rmse.table


## -------------------------------------------------------------------------------
spat.table <- rbind(output_geo, output_env)
spat.table$type <- c(rep("geo", 5), rep("env", 5))




#save first plot
png("../plots/ex_upscale/boxplot.rmse.png", width = 800, height = 600)
boxplot(rmse~type,data = spat.table, main = "RMSE")
dev.off()

#save second plot
png("../plots/ex_upscale/boxplot.r2.png", width = 800, height = 600)
boxplot(rsq~type,data = spat.table, main = "R-Squared")
dev.off()
