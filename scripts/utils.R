# ----------------------------------------------------------------- #
#                           utils.R                                  #
# ----------------------------------------------------------------- #

# Define organized package list
required_packages <- c(
  # Wrangling
  "dplyr", "tidyr", "forcats", "readxl", "tools",
  # Modeling & Stats
  "caret", "psych", "e1071", "class", "pROC", "nnet",
  # Tree-Based / Boosting
  "randomForest", "xgboost", "lightgbm",
  # Viz
  "ggplot2", "patchwork", "gridExtra",
  # Reporting
  "knitr", "rmarkdown", "kableExtra", "DT"
)

# ----------------------------------------------------------------- #
# Helper to install & load required packages if not already installed
# ----------------------------------------------------------------- #
ensure_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing missing package:", pkg))
    install.packages(pkg, dependencies = TRUE, repos = "http://cloud.r-project.org")
  }
  
  # Try to load, but continue on error
  tryCatch(
    {
      library(pkg, character.only = TRUE)
      message(paste("Loaded package:", pkg))
    },
    error = function(e) {
      warning(paste("Failed to load package:", pkg, "->", e$message))
    }
  )
}

# ----------------------------------------------------------------- #
# Install & load all packages
# ----------------------------------------------------------------- #
invisible(lapply(required_packages, ensure_installed))

