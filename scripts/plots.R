library(ggplot2)
library(patchwork)

make_univariate_plots <- function(df, max_cols = 3) {
  plots <- list()

  num_cols <- names(df)[sapply(df, is.numeric)]
  for (col_name in num_cols) {
    p <- ggplot(df, aes(x = .data[[col_name]])) +
      geom_histogram(bins = 30, fill = "#66C2C2") +
      theme_minimal() +
      labs(x = col_name, y = NULL)

    plots[[length(plots) + 1]] <- p
  }

  cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  for (col_name in cat_cols) {
    p <- ggplot(df, aes(x = .data[[col_name]])) +
      geom_bar(fill = "#66C2C2") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = col_name, y = NULL)

    plots[[length(plots) + 1]] <- p
  }

  if (length(plots) > 0) {
    return(wrap_plots(plots, ncol = min(max_cols, length(plots))))
  } else {
    return(NULL)
  }
}


# in rmd I should change every code for creating plots to a code like this
# this way, rmd will be only presentation layer
# all the real plotting code will be in plots.4
# ```{r}
# source("scripts/plots.R")

# p <- make_model_comparison_plot(model_performance_tbl)
# print(p)
