# ----------------------------------------------------------------- #
#                       FEATURE ENGINEERING Pipeline                #
# ----------------------------------------------------------------- #

fix_skew_with_log1p <- function(col) {
  col_log <- log1p(col)
  return (col_log)
}

fix_skew_with_cox <- function(col) {
  # First, estimate the transformation 
  trans <- BoxCoxTrans(col)
  # Return transformed values
  col_cox <- predict(trans, col)
  return (col_cox)
}

product_strong <- function(col1, col2) {
  new_col <- col1 * col2
  return (new_col)
}

group_by_domain_knowledge <- function(df, strong_cat) {
  df %>%
    mutate(
      across(
        any_of(intersect(strong_cat, "marital.status")),
        ~ fct_collapse(
          factor(as.character(.x)), 
          "Married" = c("Married-civ-spouse","Married-AF-spouse","Married-spouse-absent"),
          "Single"  = c("Never-married", "Divorced", "Widowed", "Separated")
        )
      ),
      
      across(
        any_of(intersect(strong_cat, "workclass")),
        ~ fct_collapse(
          factor(as.character(.x)),
          "Government"    = c("Local-gov", "State-gov", "Federal-gov"),
          "Self-Employed" = c("Self-emp-not-inc", "Self-emp-inc"),
          "Other"         = c("Unknown", "Without-pay", "Never-worked")
        )
      ),
      
      across(
        any_of(intersect(strong_cat, "education")),
        ~ fct_collapse(
          factor(as.character(.x)),
          "No School"    = c("Preschool", "1st-4th", "5th-6th"),
          "Middle" = c("7th-8th", "9th", "10th", "11th", "12th", "HS-grad"),
          "Some College"       = "Some-college", "Assoc-acdm", "Assoc-voc",
          "Bachelor"         = c("Bachelors"),
          "Post-graduate" = c("Masters", "Doctorate", "Prof-school")
        )
      ),
      
      across(
        any_of(intersect(strong_cat, "occupation")),
        ~ fct_collapse(
          factor(as.character(.x)),
          "Professional" = c("Prof-specialty", "Exec-managerial", "Tech-support"),
          "Blue-Collar"  = c("Craft-repair", "Machine-op-inspct", "Transport-moving"),
          "Office-support"  = c("Adm-clerical", "Sales"),
          "Service"         = c("Other-service", "Protective-serv", "Priv-house-serv", "Armed-Forces"),
          "Labor"           = c("Handlers-cleaners", "Farming-fishing"),
          "Other"           = c("Unknown")
        )
      ),
      
      across(
        any_of(intersect(strong_cat, "relationship")),
        ~ fct_collapse(
          factor(as.character(.x)),
          "Breadwinner" = c("Husband", "Wife"),
          "Dependent" = c("Own-child", "Other-relative"),
          "Not-Family" = c("Not-in-family", "Unmarried")
        )
      )
      
    )
}

collapse_rare <- function(x, threshold = 0.02, min_levels = 3, min_support = 0.05){
  # min_levels: minimum number of unique categories to keep after collapsing
# min_support ensures we don’t collapse categories that are small but  predictive
  freq <- prop.table(table(x)) # a vector of proprtions of each category
  rare <- names(freq[freq < threshold | freq < min_support])# Any category with frequency < threshold or < min_support is considered rare
  
  # Only collapse if it doesn’t reduce to fewer than min_levels
  if(length(unique(x)) - length(rare) + 1 < min_levels){
    rare <- character(0)
  }
  
  x <- as.character(x)
  x[x %in% rare] <- "Other"
  factor(x)
}
 
deviation_formula <- function (target_col,target_val, col_name){
  as.formula(paste0("(", target_col, " == '", target_val, "') ~ ", col_name))
}

# Returns mean, deviation, count for every category x
calc_deviation_stats <- function(x, target_dist) {
  m <- mean(x, na.rm = TRUE)
  c(mean = m, deviation = m - target_dist, count = length(x))
}

calc_categories_deviations <- function(df, strong_cat, target_col, target_val, target_dist) {
 
   res_list <- lapply(strong_cat, function(col_name) {
    agg <- aggregate(
      deviation_formula(target_col,target_val, col_name),
      data = df, 
      FUN = function(x) calc_deviation_stats(x, target_dist)
    )
    
    stats <- as.data.frame(agg[, 2])
    
    final_df <- data.frame(
      variable = col_name,
      category = agg[, 1],
      group_mean = stats$mean,
      deviation  = stats$deviation,
      count      = stats$count
    )
    
    return(final_df)
  })
  
  do.call(rbind, res_list)
}

merge_similar_categories <- function(deviations_df, epsilon = 0.03, min_count = 50, min_levels = 3) {
  
  deviations_df %>%
    mutate(
      deviation_sign = case_when(
        deviation >  epsilon ~ "positive",
        deviation < -epsilon ~ "negative",
        TRUE                 ~ "neutral"
      ),
      deviation_bin = case_when(
        count < min_count ~ "low_support",
        deviation_sign == "neutral" ~ "neutral",
        TRUE ~ paste0(deviation_sign, "_signal")
      )
    ) %>%
    group_by(variable, deviation_bin) %>%
    summarise(
      merged_categories = paste(category, collapse = " | "),
      avg_deviation = mean(deviation),
      total_count = sum(count),
      .groups = "drop"
    ) %>%
    filter(n_distinct(deviation_bin) >= min_levels)  # skip merges that would reduce levels too much
}

apply_category_merges <- function(df, merged_df, feature, min_levels = 3) {
  
  mapping <- merged_df %>%
    filter(variable == feature) %>%
    select(deviation_bin, merged_categories)
  
  # Skip if merging would reduce below min_levels
  if(length(unique(mapping$deviation_bin)) + 1 < min_levels) return(df)
  
  df[[feature]] <- as.character(df[[feature]])
  
  for (i in seq_len(nrow(mapping))) {
    cats <- unlist(strsplit(mapping$merged_categories[i], " \\| "))
    df[[feature]][df[[feature]] %in% cats] <- mapping$deviation_bin[i]
  }
  
  # Ensure factor with minimum levels
  df[[feature]] <- factor(df[[feature]])
  if(nlevels(df[[feature]]) < min_levels){
    df[[feature]] <- factor(df[[feature]], levels = c(levels(df[[feature]]), "Other"))
    df[[feature]][is.na(df[[feature]]) | df[[feature]] == ""] <- "Other"
  }
  df
}


# ----------------------------------------------------------------- #
#                       Main Pipeline                               #
# ----------------------------------------------------------------- #

  
feature_engineer <- function(df, predictors, target_col, target_classes, target_dist, min_levels = 3) {
  
  strong_numeric <- predictors$strong_numeric
  strong_cat <- predictors$strong_cat
  weak_numeric <- predictors$weak_numeric
  weak_cat <- predictors$weak_cat

  #First, drop weak predictors
  df <- df %>% select(-weak_cat)
  
  
  # ---- Numeric FE ----
  df_log <- df
  df_cox <- df
  df_binary <- df
  df_prod_num <- df
  
  # Handling numeric data issues
  
  if (length(strong_numeric) > 0) {
    
    # Feature Engineering (pass directly to tree-based models)
    
    col_age <- df[["age"]]
    col_edu <- df[["education.num"]]
    col_hrw <- df[["hours.per.week"]]
   
    # 1. Binary feature out of raw strong features (skewed or not)
    df_binary$is_old <- col_age >= 45
    df_binary$is_high_eduacted <- col_edu >= 12
    df_binary$is_workaholic <- col_hrw >= 40
    
    # 2. Product feature out of raw strong features (skewed or not)
    df_prod_num$age_edu <- product_strong(col_age, col_edu)
    df_prod_num$age_hrw <- product_strong(col_age, col_hrw)
    df_prod_num$edu_hrw <- product_strong(col_edu, col_hrw)
    
    strong_product <- c("age_edu", "age_hrw", "edu_hrw")
    product_features <- df_prod_num[, strong_product]
    
   
    # Check Original skew
    original_strong_numeric <- df[, strong_numeric, drop = FALSE]
    original_skew_values <- sapply(original_strong_numeric, skewness, na.rm = TRUE)
    skewed_orig_numeric <- names(original_skew_values[abs(original_skew_values) > 0.5])
  
    # Check product skew 
    product_skew_values <- sapply(product_features, skewness, na.rm = TRUE)
    skewed_product_numeric <- names(product_skew_values[abs(product_skew_values) > 0.5])
    
    # Remove education.num and hours.per.week from transformations
    all_skewed_numeric <- setdiff(
      unique(c(skewed_orig_numeric, skewed_product_numeric)),
      c("education.num", "hours.per.week")
    )
    
    for (col_name in all_skewed_numeric) {
      
      col <- if (col_name %in% names(df)) {
        df[[col_name]]
      } else {
        df_prod_num[[col_name]]
      }
      
      # Apply log transform
      if (all(col > -1, na.rm = TRUE)) {
        df_log[[col_name]] <- fix_skew_with_log1p(col)
      }
      
      # Apply Box-Cox transform
      if (all(col > 0, na.rm = TRUE)) {
        df_cox[[col_name]] <- fix_skew_with_cox(col)
      }
      
    }
    
    # Keep education.num and hours.per.week unchanged
    df_log[["education.num"]] <- df[["education.num"]]
    df_log[["hours.per.week"]] <- df[["hours.per.week"]]
    
    df_cox[["education.num"]] <- df[["education.num"]]
    df_cox[["hours.per.week"]] <- df[["hours.per.week"]]
  
  # ---- Categorical FE ----
  
  
  # ---- Grouping ----
  
  df_domain <- df
  df_rare <- df
  df_dev <- df
  
  if (length(strong_cat) >0) { 
    
    # 1. Domain Knowledge Grouping to Capture meaningful categories before auto grouping
    df_domain <- group_by_domain_knowledge(df_domain, strong_cat) 
    
    # 2. Rare Category Collapsing
    df_rare <- df_domain
    for(col in strong_cat){
      df_rare[[col]] <- collapse_rare(df_rare[[col]], threshold = 0.02, min_levels = min_levels)
    }
    
    # Calculate categories deviations
    target_val <- target_classes[2]
    deviations_df <- calc_categories_deviations(df_rare,strong_cat,target_col,target_val,target_dist)
    
    # Merge categories based on deviation
    merged_deviations <- merge_similar_categories(deviations_df)
    
    # 3. Apply the newly merged categories back to the dataframe
    df_dev <- df_rare
    for(feature in strong_cat){
      df_dev <- apply_category_merges(df_dev, merged_deviations, feature, min_levels = min_levels)
    }
    
    # 4. Create new features out of the strong ones
    df_dev$workclass_marital <- interaction(df_dev$workclass, df_dev$marital.status)
    #  df_dev$workclass_marital <- collapse_rare(df_dev$workclass_marital, threshold = 0.05)
    
    
    # Ensure all categorical columns are factors
    df_dev[] <- lapply(df_dev, function(x) if(is.character(x)) factor(x) else x)
    
    # Standardize target
    df_dev[[target_col]] <- factor(df_dev[[target_col]], levels = target_classes)
  }
  
  # ---- FE Results ----
  
  list(
    original = df,
    df_binary = df_binary,
    prod_num = df_prod_num,
    df_log = df_log,
    df_cox = df_cox,
    df_domain = df_domain,
    df_rare = df_rare,
    df_dev = df_dev,
    diagnostics = list(deviations_raw = deviations_df, deviations_merged = merged_deviations)
  )
  
  } else{
    return(
      list(
        original   = df,
        df_binary  = df,
        prod_num   = df,
        df_log     = df,
        df_cox     = df,
        df_domain  = df,
        df_rare    = df,
        df_dev     = df,
        diagnostics = list(
          deviations_raw = NULL,
          deviations_merged = NULL,
          note = "No strong numeric predictors supplied"
        )
      )
    )
  }
  
  
  
}
