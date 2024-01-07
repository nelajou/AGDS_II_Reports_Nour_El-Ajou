

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
