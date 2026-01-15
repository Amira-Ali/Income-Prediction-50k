# ================================================================= #
#            Data Understanding Pipeline (EDA + Diagnostics)        #
# ================================================================= #

# ---- Purpose ----

# This pipeline supports the *data understanding phase* of an analysis
# or modeling workflow. Its role is to expose data issues, quantify
# distributions, and surface early analytical signals before any
# transformation, encoding, or modeling decisions are made.

# ----- Design Principles -----

# - Automated but interpretable: every check produces explainable output
# - Analyst-in-the-loop: user comments complement automated findings
# - Decision-oriented: outputs are meant to drive cleaning, grouping,
#   encoding, and feature-selection strategies
# - Reusable across datasets with minimal configuration

# ----------------------------------------------------------------- #
# Part 1: Data Quality & Integrity Diagnostics (DQA)                #
# ----------------------------------------------------------------- #

# Objective:
# Detect structural, syntactic, and semantic data issues that may bias
# analysis or invalidate downstream assumptions.

# --------------- Integrity Diagnostics Covered # ---------------

# - check_missing()            : Missing values (NA, blanks, whitespace)
# - check_special_characters() : Unexpected or invalid characters
# - check_duplicates_sample()  : Fully duplicated records (with samples)
# - check_outliers()            : Numeric outliers via IQR logic
# - check_type_mismatch()       : Numeric or date values stored as text
# - check_empty_rows_cols()     : Completely empty rows or columns
# - check_value_ranges()        : Domain rule violations (e.g. age, dates)
#
# Each diagnostic may be accompanied by analyst commentary to explain
# business context, severity, or remediation decisions.

# ----------------------------------------------------------------- #
# Part 2: Exploratory Data Analysis (EDA)                            #
# ----------------------------------------------------------------- #

# Objective:
# Quantify distributions, dominance, skewness, noise, and variability
# to support informed feature engineering and modeling choices.

# ------------ Focus Areas:------------
# - Distribution shape and stability
# - Category dominance vs. fragmentation
# - Early indicators of grouping, binning, or exclusion
# - Statistical reliability of summary estimates

# ----------- Descriptive Statistics -----------

# - check_distributions_numeric()     : Central tendency, spread, shape
# - check_distributions_categorical() : Frequencies, proportions, noise

#  ---------- Helper & Interpretation Layer  ----------   

# These functions turn checks results into actionable insights for the analyst:
# - Standardized missing detection
# - Column-type summarization
# - Statistical interpretation (skewness, kurtosis, SE/RSE)
# - Automated + user-comment note synthesis
# - Category noise diagnostics and grouping recommendations
#
# This layer bridges automated checks with human judgment.



is_value_missing <- function(value) {
  if (is.character(value)) {
    is.na(value) | value == "" | grepl("^\\s+$", value)
  } else {
    is.na(value)
  }
}

summarize_column_types <- function(df) {
  
  total_cols <- ncol(df)
  num_cols <- sum(sapply(df, is.numeric))
  cat_cols <- sum(sapply(df, function(x)
    is.factor(x) || is.character(x)))
  other_cols <- total_cols - num_cols - cat_cols
  
  paste(c(
    paste0("The dataset has **", total_cols, "** columns"),
    if (num_cols > 0)
      paste0("**", num_cols, "** numeric"),
    if (cat_cols > 0)
      paste0("**", cat_cols, "** categorical"),
    if (other_cols > 0)
      paste0("**", other_cols, "** other types")
    else
      "no other types"
  ),
  collapse = ", ")
}

interpret_skew <- function(skew_value, col) {
  if (is.na(skew_value))
    return(paste("\n- Skewness not available for ", col))
  else
  {
    if (abs(skew_value) < 0.5) {skew_type <- "relatively **symmetric"}
    else if (skew_value > 0) {skew_type <- "**right-skewed"}
    else {skew_type <-"**left-skewed" }
    paste0("\n* The distribution is ", skew_type," (skewness = ", round(skew_value, 2), ")**, ")
  }
}

interpret_kurtosis <- function(kurt_value, col) {
  if (is.na(kurt_value))
    return(paste("\n- Kurtosis not available for", col))
  else
    {
      # Kurtosis measures how thick a distribution's tails are, and for a perfectly normal bell curve, that measurement is always 3.
      # We subtract 3 to reset this natural "normal" value to zero, establishing a simple baseline.
      # This makes it instantly clear how much a specific dataset's tail thickness deviates (above or below zero) from the standard expectation.
      
      # Excess kurtosis for easier comparison against 0
      excess_kurtosis <- kurt_value - 3.0
      
      if (abs(excess_kurtosis) < 2) {
        kurt_type <- "near **normal "
        kurt_auto_comment <- ""
      }
      else if (excess_kurtosis < 0) {
        kurt_type <- "**platykurtic "
        kurt_auto_comment <- "indicating lighter tails and fewer extreme outliers than a normal distribution."
      }
      else {
        kurt_type <- "**leptokurtic "
        kurt_auto_comment <- "indicating heavy tails and more outliers than a normal distribution"
      }
      
      paste0("\n* The distribution is ", kurt_type, " (kurtosis = ", round(kurt_value, 2),")**, ",kurt_auto_comment)
      
  }
}

interpret_rse <- function(rse, rse_perc, col) {
  if (is.na(rse))
    return(paste("\n- RSE not available for", col))
  else
    {
      # The Standard Error (SE) measures how much our calculated sample mean is likely to vary from the true population mean.
      # But the SE value itself is meaningless without context.
      # So, to determine if the SE is high or low, we need to calculate the Relative Standard Error (RSE).
      # RSE let's us see how large the error of the sample mean is compared to the mean itself.
      # "sampling error" is the noise introduced by not measuring everyone in the population
     
      output <- paste0("\n* **RSE (", rse_perc , "):** Comparing to the sample mean, RSE is ")
      
      if (rse < 0.10) { rse_comment <- "low; the sample mean is reliable."} 
      else if (rse < 0.25) { rse_comment <- "moderate; the sample mean is an accepted reliable estimate." } 
      else if (rse < 0.5) { rse_comment <- "**high (> 25%)**; the sample mean should be used with caution as an estimate of the population mean." } 
      else{"The sampling error is too large. And this sample is unreliable. "}
      paste0(output, rse_comment) 
  }
}

# ----------------------- Notes Layer ------------------------------ #

generate_notes <- function(summary, type, df = NULL) {
  if (is.null(summary) || nrow(summary) == 0) return(character(0))
  
  notes <- list()
  
  if (type == "numeric" && !is.null(df)) {
   
    for (col in rownames(summary)) {
      
      mean_val <- summary[col, "mean"]
      skew <- summary[col, "skew"]
      kurt <- summary[col, "kurtosis"]
      rse   <- summary[col, "rse"]
      rse_perc   <- summary[col, "rse_perc"]
      
      notes[[col]] <- paste0(
        "\n**", col, " (n=", format(sum(!is.na(df[[col]])), big.mark = ","), ")**\n",
        
        interpret_skew(skew, col),
        if (!is.null(comments$numeric[[col]]$skew))
          paste0(comments$numeric[[col]]$skew, "\n") else "\n",
        
        interpret_kurtosis(kurt, col),
        if (!is.null(comments$numeric[[col]]$kurtosis))
          paste0(comments$numeric[[col]]$kurtosis, "\n") else "\n",
        
        interpret_rse(rse, rse_perc, col),
        if (!is.null(comments$numeric[[col]]$rse))
          paste0(comments$numeric[[col]]$rse, "\n") else "\n"
      )
     
    }
    
    if (length(comments$stats_insights) > 0 && comments$stats_insights != "") {
      notes[["Global_Note"]] <- paste0("\n\n",comments$stats_insights)  }
  } 
    
  if (type == "outliers") {
    for (i in seq_len(nrow(summary))) {
      col   <- summary$Column[i]
      count <- summary$Count[i]
      notes[[col]] <- paste0("\n- **", col, "** has **", count, "**  outliers. ")
    }
  } 
  
  notes
}

combine_outliers_notes <- function(auto_notes, comments_list) {
 #auto_notes <- paste0("- **", col, "** has **", count, "**  outliers. "
  all_cols <- union(names(auto_notes), names(comments_list))
  
  if (length(all_cols) == 0) {return(list())}
  
  notes_combined <- lapply(all_cols, function(col) {
    
    auto_note <- auto_notes[[col]]
    user_note <- comments_list[[col]]
    
    parts <- c(
      if (!is.null(auto_note)) auto_note else NULL,
      if (!is.null(user_note)) user_note else NULL
    )
    
    # Join the auto note and user note with a space:
    paste(parts, collapse = " ") 
    
  })
  
  names(notes_combined) <- all_cols
  return(notes_combined)
}


generate_stats_table <- function(cat_summary, dominance_threshold = 0.5, rare_threshold = 0.05, grouping_threshold = 5) {
  
  if (is.null(cat_summary) || length(cat_summary) == 0) return(data.frame())
  
  summary_list <- list()
  
  for (col in names(cat_summary)) {
    
    df_col <- cat_summary[[col]]  # data frame with Category and Count
    counts <- df_col$Count        # category counts
    total <- sum(counts)          # total records in column
    cardinality <- nrow(df_col)   # number of categories
    
    # Dominant category
    most_freq_cat <- which.max(counts)
    dominant <- if ((counts[most_freq_cat] / total) > dominance_threshold) {
      df_col$Category[most_freq_cat]
    } else { "None" }
    
    # Rare categories
    is_cat_low_freq <- counts / total < rare_threshold
    rare_cats <- df_col$Category[is_cat_low_freq]
    rare_string <- if (length(rare_cats) > 0) paste(rare_cats, collapse = ", ") else "None"
    
    # Grouping recommendation based on total noise & cardinality thresholds
    rare_sum_pct <- sum(counts[is_cat_low_freq]) / total * 100
    
    # Noise & Grouping
    if (rare_sum_pct == 0) {
      total_noise <- "No Noise"
      to_group <- "Keep as-is"
      
    } else if (rare_sum_pct < 10) {
      total_noise <- "Low (<10%)"
      if (cardinality <= grouping_threshold) {
        to_group <- "Keep as-is"
      } else {
        to_group <- "Domain grouping"
      }
      
    } else if (rare_sum_pct <= 20) {
      total_noise <- "Moderate (10–20%)"
      if (cardinality <= grouping_threshold) {
        to_group <- "Keep as-is"
      } else {
        to_group <- "Domain grouping"
      }
      
    } else { # rare_sum_pct > 20
      total_noise <- "High (>20%)"
      to_group <- "Domain grouping"
    }
    
    
    
    summary_list[[col]] <- data.frame(
      Column = col,
      No_of_Categories = cardinality, 
      Dominant_Category = dominant,
      Rare_Categories = rare_string,
      Total_Noise  = total_noise,
      Grouping = to_group,
      stringsAsFactors = FALSE
    )
  }
  
  # Combine all rows
  summary_tbl <- do.call(rbind, summary_list)
  
  summary_tbl <- summary_tbl[order(summary_tbl$No_of_Categories, decreasing = TRUE), ]
  colnames(summary_tbl) <- c("Column", "# Cat.", "Dominant Cat. (>50%)", "Rare Cat. (each < 5%)", "Total Noise", "Group Recommendation")

  return(summary_tbl)
}


is_mostly_numeric <- function(col, threshold) {
  
  # We're only interested in checking character columns that have numeric values
  if (!is.character(col)) {return(FALSE)}
  
  # Try converting all values in the character col to numeric.
  numeric_values <- suppressWarnings(as.numeric(col))
  
  # Check for NA results
  successfully_converted <- !is.na(numeric_values)
  
  # Calculate the mean of the logical vector
  proportion_numeric <- mean(successfully_converted)
  
  # Check if the proportion of numeric values >= threshold
  return(proportion_numeric >= threshold)
}

is_mostly_date <- function(col, threshold, date_formats) {
  
  # If it's not character, exit function because only character cols can contain date strings
  if (!is.character(col)) {return(FALSE)}
  
  # Attempt to parse each value using the allowed date formats
  parsed_dates <- sapply(col, function(x) {
    any(!is.na(as.Date(x, format = date_formats)))
  })
  
  # Check if the proportion of successfully parsed dates >= threshold
  proportion_date <- mean(parsed_dates)
  return(proportion_date >= threshold)
}
  
check_suspicious_types <- function(col, date_formats, threshold = 0.9) {
  
  is_mostly_numeric(col, threshold) || is_mostly_date(col, threshold, date_formats)
}

check_disallowed_characters <- function(col, pattern) {
  
  if (!is.character(col))  return(NULL)
  
  # Use gregexpr instead of gripl to find all positions of characters not in the allowed set
  matches_pos <- gregexpr(pattern, col, perl = TRUE)
  
  # Use regmatches to extract the actual found text 
  bad_chars_list <- regmatches(col, matches_pos)
  
  # Convert bad_chars_list to a vector
  unique_bad_chars <- unique(unlist(bad_chars_list))
  
  # Remove empty strings if any
  unique_bad_chars <- unique_bad_chars[unique_bad_chars != ""]
  
  count_bad <- sum(lengths(bad_chars_list) > 0, na.rm = TRUE) # Count rows affected
  
  list(
    count = count_bad, 
    unique_chars_found = unique_bad_chars
   )
}

build_categorical_summary_table <- function(categorical_summary) {
  
  if (is.null(categorical_summary)) {
    return(NULL)
  }
  
  summary_table <- lapply(names(categorical_summary), function(col) {
    
    df_cat <- categorical_summary[[col]]
    
    top_row <- df_cat %>%
      arrange(desc(Num_Categories)) %>%
      slice(1)
    
    data.frame(
      Column         = col,
      Num_Categories = nrow(df_cat),
      Most_Frequent  = top_row$Category,
      #Frequency      = top_row$Frequency,
      Percentage     = top_row$Percentage,
      stringsAsFactors = FALSE
    )
  }) %>%
    bind_rows()
  
  summary_table
}



# ----------------------------------------------------------------- #
#                       Core DQA Checks                              #
# ----------------------------------------------------------------- #

check_missing <- function(df) {
  # a logical matrix where TRUE means missing, FALSE means not missing.
  missing_matrix <- sapply(df, is_value_missing)
  overall_missing <- mean(missing_matrix) * 100
  # Calculates the mean of each column in the matrix and * 100.
  col_missing <- colMeans(missing_matrix) * 100
  # Keep only columns with >0% missing
  col_missing_filtered <- col_missing[col_missing > 0]
  # Let's create a vector where each element is followed by "%"
  col_percentage <- paste0(round(col_missing_filtered, 2), "%")
  affected_rows <- df[rowSums(missing_matrix) > 0, , drop = FALSE]
  
  if (overall_missing > 0) {
    col_percentage <- paste0(names(col_missing_filtered), ": ", round(col_missing_filtered, 2),"%")
    output <- paste0("- Overall Missingness: ", round(overall_missing, 2),"%\n",
                     "- Columns: ", paste(col_percentage, collapse = ", "),"\n",
      "- Rows affected: ", nrow(affected_rows), " of ", nrow(df), "\n")
  } else{ output <- "The dataset is completely clean (no missing data)."  }
  
  list(
    output = output,
    summary = data.frame(
      Column = names(col_missing_filtered),
      Missing_Percent = round(col_missing_filtered, 2),
      row.names = NULL
    ),
    rows = affected_rows
  )
  
}

check_special_characters <- function(df, pattern = "[^A-Za-z0-9.=<> -]") {

  # Since lapply expects a function name, not a function call, we need to wrap the call in an anonymous function
  # The anonymous function acts as a bridge for the pattern
  # it works like this:
  # lapply takes the first column of df -> df[, 1]
  # lapply calls the anonymous function and pass df[, 1] into the wrapper's local variable col.
  # The wrapper calls check_disallowed_characters with two arguments (col and pattern)

  details <- lapply(df, function(col) {
    check_disallowed_characters(col, pattern) 
  })
  
  
  # Calculate percentages and show chars_found
  total_rows <- nrow(df)
  if (total_rows == 0) {
    
    # if dataframe is empty, fill each cell with zero to avoid division by zero
    percentages <- sapply(details, function(x) 0) 
    chars_found <- sapply(details, function(x) "")
    
  } else {
    
    percentages <- sapply(details, function(x) {
      if (is.null(x)) 0 else {x$count / total_rows * 100}
    })
    
    chars_found <- sapply(details, function(x) {
      if (is.null(x) || length(x$unique_chars_found) == 0) "" 
      else paste(x$unique_chars_found, collapse = ", ")
    })
    
  }
   
  summary <- data.frame(
    Column = names(details),
    Chars_Found = chars_found, 
    Percentage = round(percentages, 0),
    row.names = NULL
  )
  
  # Only show columns with % > 0
  summary <- summary[summary$Percentage > 0, ]
  if (nrow(summary) > 0) {
    summary$Percentage <- paste0(summary$Percentage, "%")
  }
  
  summary 
}


check_type_mismatch <- function(df, date_formats, threshold = 0.9) {
 
  col_type <- sapply(df, class)
  char_cols <- names(col_type)[col_type == "character"]
  
  # Identify suspicious columns (they are numeric\date cols stored as character)
  # Check each text column to see if it mostly contains numbers or dates
  # Creates a TRUE/FALSE list (a logical vector) showing which columns might have the wrong type
  
   suspicious_list <- sapply(
     df[char_cols],
     check_suspicious_types,
     date_formats = date_formats,
     threshold = threshold
   )
   
   # From all text columns we identified previously, keep only those that look like numbers or dates.
   suspicious_cols <- char_cols[suspicious_list]
  
  # Generate a summary data frame only if suspicious columns were found
  if (length(suspicious_cols) > 0) {
    summary_df <- data.frame(
      Column        = suspicious_cols,
      Detected_Type = "character",
      Note          = "Possible numeric/date stored as character"
    )
  } else {
    summary_df <- NULL # Otherwise, set the summary to NULL
  }
  
  list(
    details = list(checked_columns = char_cols),
    summary = summary_df
  )
  
}

check_empty_rows_cols <- function(df) {
  empty_rows <- which(apply(df, 1, function(r)
    all(
      is.na(r) | r == "" | grepl("^\\s+$", as.character(r))
    )))
  empty_cols <- which(apply(df, 2, function(c)
    all(
      is.na(c) |
        (is.character(c) & c == "") | grepl("^\\s+$", as.character(c))
    )))
  summary <- data.frame(
    Type = c("Empty Rows", "Empty Columns"),
    Count = c(length(empty_rows), length(empty_cols)),
    Details = c(
      paste(empty_rows, collapse = ", "),
      paste(names(df)[empty_cols], collapse = ", ")
    )
  )
  list(empty_rows = empty_rows,
       empty_columns = empty_cols,
       summary = summary)
}

check_value_ranges <- function(df) {
  issues <- list()
  if ("age" %in% names(df)) {
    invalid_age <- sum(df$age < 0 |
                         df$age > 120, na.rm = TRUE)
    if (invalid_age > 0)
      issues$age <- invalid_age
  }
  if ("date" %in% names(df)) {
    min_d <- as.Date("1900-01-01")
    max_d <- as.Date("2050-12-31")
    valid_dates <- df$date[!is.na(df$date)]
    viol <- sum(valid_dates < min_d |
                  valid_dates > max_d, na.rm = TRUE)
    if (viol > 0)
      issues$date <- viol
  }
  issues
}

check_outliers <- function(df) {
  num_cols <- sapply(df, is.numeric)
  df_num <- df[, num_cols, drop = FALSE]
  
  data.frame(
    Column = names(df_num),
    Count = sapply(df_num, function(x) {
      Q1 <- quantile(x, 0.25, na.rm = TRUE)
      Q3 <- quantile(x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      if (IQR == 0) return(0)
      sum(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR), na.rm = TRUE)
    }),
    row.names = NULL
  ) |>
    dplyr::filter(Count > 0) |>
    dplyr::arrange(desc(Count)) # high to low
}


check_distributions_numeric <- function(df) {
  num_cols <- sapply(df, is.numeric)
  df_num <- df[, num_cols, drop = FALSE]
  desc_all <- describe(df_num)
  desc_df <- as.data.frame(
    desc_all[, !(colnames(desc_all) %in% c("vars", "trimmed", "mad"))]
  )
  
  desc_df$rse <- ifelse(desc_df$mean == 0, NA, desc_df$se / desc_df$mean)# in case mean=0
  
  desc_df$rse_perc <- paste0(round(desc_df$rse * 100, 2), "%")
  desc_df$se <- NULL
  
  # round numerical values for the report
  desc_df[] <- lapply(desc_df, function(x) {
    if (is.numeric(x)) round(x, 2) else x
  })
  
  desc_df
}

check_distributions_categorical <- function(df) {
  cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  if (length(cols) == 0) return(NULL)
  
  summaries <- lapply(cols, function(col) {
    freq <- table(df[[col]], useNA = "ifany")
    perc <- round(100 * prop.table(freq), 1)
    
    df_out <- data.frame(
      Category   = names(freq),
      Count      = as.numeric(freq),
      Percentage = paste0(perc, "%"),
      row.names  = NULL
    )
    
    df_out[order(df_out$Count, decreasing = TRUE), ]
  })
  
  names(summaries) <- cols
  summaries
}



# ----------------------------------------------------------------- #
#                       EDA Pipeline Wrapper                       #
# ----------------------------------------------------------------- #

explore_data <- function(df, pattern, date_formats , comments ) {
  
 
  missing = check_missing(df)
  special_chars = check_special_characters(df, pattern)#summary: dataframe  
  duplicates = sum(duplicated(df))
 
  type_mismatch = check_type_mismatch(df, date_formats)
  empty_rows_cols = check_empty_rows_cols(df)
  range_validations = check_value_ranges(df)
  
  # Outliers
  outlier_summary <- check_outliers(df)
  outlier_auto <- generate_notes(outlier_summary, "outliers")
  outlier_notes <- combine_outliers_notes(outlier_auto, comments$outliers)
  
  # Numeric Stats
  numeric_summary <- check_distributions_numeric(df)
  numeric_notes_list <- generate_notes(numeric_summary, "numeric", df)
  
  # Categorical Stats
  cat_summary <- check_distributions_categorical(df)#list[Category, Count, Percentage, Num_Categories] 
  cat_desc_stat_tbl <- generate_stats_table(cat_summary)#a summary table for cat cols 
  

  list(
    df = df,
    
    col_types = summarize_column_types(df),
    
    integrity_checks = list(
      missing = missing,
      special_chars = special_chars,
      duplicates = duplicates,
      type_mismatch = type_mismatch,
      empty_rows_cols = empty_rows_cols,
      range_validations = range_validations
    ),
    
    outliers = list(
      summary = outlier_summary,
      notes_list = outlier_notes # combined_notes_list
    ),
    
    descriptive_stats = list(
      numeric_summary = list(summary = numeric_summary, notes_list  = numeric_notes_list),
      categorical_summary = list(summary = cat_summary),
      stats_tbl = cat_desc_stat_tbl
    ),
    
    startegies = list(
      grouping = comments$strategy["grouping"],
      univariate_highlights = comments$univariate_highlights,
      encoding  = comments$strategy["encoding"],
      data_issues = comments$data_issues
    )
    
  )
  
  
}
