remove_duplicate <- function(df){
  df <- df %>% distinct()
  return(df)
}

remove_ws <- function(df){
  df[] <- lapply(df, function(x) {
    if (is.character(x)) trimws(x)
    else if (is.factor(x)) factor(trimws(as.character(x)))
    else x
  })
  return(df)
}

replace_special_char <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x[x == "?"] <- "Unknown"
    } else if (is.factor(x)) {
      levels(x) <- trimws(levels(x))
      if (!"Unknown" %in% levels(x)) {
        levels(x) <- c(levels(x), "Unknown")
      }
      x[x == "?"] <- "Unknown"
    }
    x
  })
  return(df)
}


save_eda_results_rdata <- function(df){
 
  strong_numeric  <- c("age", "education.num", "hours.per.week")
  
  strong_cat     <- c("workclass", "education", "marital.status", "occupation", "relationship")
  
  weak_cat        <- c("race","sex")
  
  weak_numeric    <- c("")
  
  predictors <- list(
    strong_numeric = strong_numeric,
    strong_cat     = strong_cat,
    weak_numeric   = weak_numeric,
    weak_cat       = weak_cat
  )
  
  target_col <- names(df)[ncol(df)]
  target_classes <- unique(df[[target_col]])
  target_dist <- mean(df[[target_col]] == target_classes[1], na.rm = TRUE) 
  
  # Store all objects in .RData file
  save(predictors ,target_col, target_classes, target_dist, file = "model_metadata.RData")
  
}

# Function to standardize target column
standardize_target <- function(df_list, target_col, target_classes) {
  lapply(df_list, function(df) {
    df[[target_col]] <- factor(df[[target_col]], levels = target_classes)
    df
  })
}

cap_outliers <- function(df, cols_to_cap) {
 
   # Only numeric cols
  cols_to_cap <- intersect(cols_to_cap, names(df))
  numeric_cols <- cols_to_cap[sapply(df[cols_to_cap], is.numeric)]
  
  # Cap a column using IQR
  cap_column_iqr <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    
    lower <- Q1 - 1.5 * IQR_val
    upper <- Q3 + 1.5 * IQR_val
    
    x[x < lower] <- lower
    x[x > upper] <- upper
    return(x)
  }
  
  # Apply capping to all numeric columns
  df[numeric_cols] <- lapply(df[numeric_cols], cap_column_iqr)
  
  return(df)
}

check_remaining_outliers <- function(df, cols = NULL) {
  # If no specific columns provided, use all numeric columns
  if (is.null(cols)) {
    cols <- names(df)[sapply(df, is.numeric)]
  } else {
    cols <- intersect(cols, names(df))
  }
  
  outlier_counts <- sapply(cols, function(x) {
    col <- df[[x]]
    Q1 <- quantile(col, 0.25, na.rm = TRUE)
    Q3 <- quantile(col, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    sum(col < (Q1 - 1.5*IQR) | col > (Q3 + 1.5*IQR), na.rm = TRUE)
  })
  
  # Return only columns with remaining outliers
  data.frame(
    Column = names(outlier_counts),
    Remaining_Outliers = as.integer(outlier_counts)
  ) |> dplyr::filter(Remaining_Outliers > 0) |> dplyr::arrange(desc(Remaining_Outliers))
}




