library(ggplot2)

make_model_comparison_plot <- function(results_df) {
  ggplot(results_df, aes(x = feature_set, y = AUC, fill = model)) +
    geom_col(position = "dodge") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Model Comparison by Feature Set",
         x = "Feature Engineering Strategy",
         y = "AUC")
}

# in rmd I should change every code for creating plots to a code like this
# this way, rmd will be only presentation layer
# all the real plotting code will be in plots.4
# ```{r}
# source("scripts/plots.R")

# p <- make_model_comparison_plot(model_performance_tbl)
# print(p)
