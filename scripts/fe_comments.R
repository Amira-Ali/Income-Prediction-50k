# ----------------------------------------------------------------- #
#                       FEATURE ENGINEERING Pipeline                #
# ----------------------------------------------------------------- #

fix_skew_with_log <- function(col) {
  col_log <- log1p(col)
  return (col_log)
}

product_numeric_strong <- function(col1, col2) {
  new_col <- col1 * col2
  return (new_col)
}

group_by_domain_knowledge <- function(df, strong_cat) {
  df %>%
    mutate(
      # Apply marital.status logic only if it exists in df and strong_cat
      across(
        any_of(intersect(strong_cat, "marital.status")),
        ~ fct_collapse(
          factor(as.character(.x)), # represents the current column being transformed
          "Married" = c(
            "Married-civ-spouse",
            "Married-AF-spouse",
            "Married-spouse-absent"
          ),
          "Single"  = c("Never-married", "Divorced", "Widowed", "Separated")
        )
      ),
      
      # Apply workclass logic only if it exists in df and strong_cat
      across(
        any_of(intersect(strong_cat, "workclass")),
        ~ fct_collapse(
          factor(as.character(.x)),
          "Government"    = c("Local-gov", "State-gov", "Federal-gov"),
          "Self-Employed" = c("Self-emp-not-inc", "Self-emp-inc"),
          "Other"         = c("Unknown", "Without-pay", "Never-worked")
        )
      ),
      
      # Apply Education logic only if it exists in df and strong_cat
      across(
        any_of(intersect(strong_cat, "Education")),
        ~ fct_collapse(
          factor(as.character(.x)),
          "No School"    = c("Preschool", "1st-4th", "5th-6th"),
          "Middle" = c("7th-8th", "9th", "10th", "11th", "12th", "HS-grad"),
          "Some College"       = "Some-college, Assoc-acdm, Assoc-voc",
          "Bachelor"         = c("Bachelors"),
          "post-graduate" = c("Masters", "Doctorate", "Prof-school")
        )
      )
      
    )
}

collapse_rare <- function(x, threshold = 0.02) {
  freq <- prop.table(table(x))
  rare <- names(freq[freq < threshold])
  x <- as.character(x)
  x[x %in% rare] <- "Other"
  factor(x)
}

df_rare_collapsed <- function(df, strong_cat, rare_threshold = 0.02) {
  #strong_cat <- c("workclass", "Education", "marital.status", "Occupation", "Relationship")
  
  for (col in strong_cat) {
    df[[col]] <- collapse_rare(df[[col]], rare_threshold)
  }
  df
}

# Hard coded: deviation_formula <- (Income == ">50K.") ~ workclass
# the dynamic version of the same above code
deviation_formula <- function (target_col,target_val, col_name){
  as.formula(paste0("(", target_col, " == '", target_val, "') ~ ", col_name))
}

# Returns mean, deviation, count for every category x
calc_deviation_stats <- function(x, target_dist) {
  m <- mean(x, na.rm = TRUE)
  c(mean = m, deviation = m - target_dist, count = length(x))
}

# Since target is binary, we're only sending the first class; <=50k, not the whole dist.
# Calculate categories deviations from avg target distr -> target_dist
calc_categories_deviations <- function(df, strong_cat, target_col, target_val, target_dist) {
  
  # Iterate through each column in strong_cat
  # returns list of data frames containing categories, means, and deviations for every feature
  res_list <- lapply(strong_cat, function(col_name) {
    agg <- aggregate(
      deviation_formula(target_col,target_val, col_name), # this the 1st line to execute
      data = df, 
      FUN = function(x) calc_deviation_stats(x, target_dist) # FUN is a keyword
      # creates a logical vector where if the target cell value in this row = <=50k -> true
      # false otherwise
      # result will be like (0,0,1,1)
      # ~ df[[col_name]] -> group the (0,0,1,1) by the categories in current feature
      # ex: if workclass has only 2 categories: private, local-gov
      # aggregate will create 2 buckets (vectors) as follows:
      # Bucket "Private": Contains values from rows 1, 2, and 4 c(0, 0, 1)
      # Bucket "Local-gov": Contains value from row 3  c(1)
      # then aggregate will send each bucket to calc_deviation_stats
      # aggregate then combines these into a table.
    )
    #agg returns one dataframe for each feature, ex:
    # workclass	  mean	 deviation	count
    # Local-gov	   1.00	   +0.50	    1
    # Private	     0.33	   -0.17	    3
    
    stats <- as.df.frame(agg[, 2])
    
    final_df <- df.frame(
      variable = col_name,
      category = agg[, 1],
      group_mean = stats$mean,
      deviation  = stats$deviation,
      count      = stats$count
    )
    
    return(final_df)
  })
  
  # Combine the list of data frames into one big data frame to plot it or analyse it if needed
  do.call(rbind, res_list)
}

merge_similar_categories <- function(deviations_df, epsilon = 0.03, min_count = 50) {
  
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
    )
}

apply_category_merges <- function(df, merged_df, feature) {
  
  mapping <- merged_df %>%
    filter(variable == feature) %>%
    select(deviation_bin, merged_categories)
  
  df[[feature]] <- as.character(df[[feature]])
  
  for (i in seq_len(nrow(mapping))) {
    cats <- unlist(strsplit(mapping$merged_categories[i], " \\| "))
    df[[feature]][df[[feature]] %in% cats] <- mapping$deviation_bin[i]
  }
  
  df[[feature]] <- factor(df[[feature]])
  df
}


# ----------------------------------------------------------------- #
#                       Main Pipeline                               #
# ----------------------------------------------------------------- #

feature_engineer <- function(df, predictors, target_col, target_classes, target_dist) {
  
  df <- df
  
  strong_numeric <- predictors$strong_numeric
  strong_cat <- predictors$strong_cat
  weak_numeric <- predictors$weak_numeric
  weak_cat <- predictors$weak_cat
  
  # ---- Numeric FE ----
  # Handle skew:
  # We will experiment with:
  # 1. raw Age
  # 2. transform Age (log, Box-Cox)
  # 3. create a new binary feature out of the skewed feature. ex: is_old or not
  # evaluate which method yields the best model performance
  
  # 1. Raw Age:
  
  
  # 2. Transformation, since Age is right-skewed -> we will use logp1() as it is primarily used to reduce right-skewness:
  #if (length(strong_numeric) > 0) df$age_log <- fix_skew_with_log(df[[strong_numeric[1]]])
  
  # 3. Add a binary feature
  #if (length(strong_numeric) > 0) df$is_senior <- df$Age >= 45
  
  # 4. evaluate based on best model performance
  
  
  # Create New features out of the strong numeric features
  # if (length(strong_numeric) > 0) {
  #   df$age_education <- product_numeric_strong(df[[strong_numeric[1]]], df[[strong_numeric[2]]])
  # }
  
  # Skip for now
  #df$age_hours <- product_numeric_strong(df[[strong_numeric[1]]], df[[strong_numeric[3]]])
  #df$education_hours <- product_numeric_strong(df[[strong_numeric[2]]], df[[strong_numeric[3]]])
  
  
  # Scale
  # Scaling happens after train/test split, not in FE.R.
  
  # ---- Categorical FE ----
  # Grouping
  # Domain Knowledge Grouping
  if (length(strong_cat) >0) df <- group_by_domain_knowledge(df, strong_cat) # Capture meaningful categories before auto grouping
  
  # Rare Category Collapsing
  # rare here is 0.02. In eda it was 0.05 because there we're only investigating, but now we have to be more strict, that's why we lowered the threshold to only 0.02
  
  if (length(strong_cat) >0) df <- df_rare_collapsed(df, strong_cat) # reduce noise + prevent overfit
  
  # ---- Deviation Diagnostics ----
  # Combine categories with same target distribution. Categories with similar deviations -> merge
  # Categories with opposite deviations -> keep separate
  
  # Deviation calculation
  # all these parameters were collected from model_metadata.RData
  deviations_df <- calc_categories_deviations(
    df = df,
    strong_cat = strong_cat,
    target_col = target_col,
    target_val = target_classes[1], # Use the first class
    target_dist = target_dist
  )
  
  # Merge based on deviation
  merged_deviations <- merge_similar_categories(deviations_df)
  
  
  # test and evaluate model performance before and after grouping
  
  # Encoding
  
  
  # Handle Missing Values: Impute any missing data in both numerical and categorical features first.
  # Transform Numerical Features: Apply transformations like log1p() to address skewness in features like Age.
  # Encode Categorical Features: Convert your categorical data (like 'workclass', 'marital.status') into a numerical format using techniques like one-hot encoding or label encoding.
  # Scale Features: Apply scaling (e.g., standardization or normalization) to your numerical features to ensure they are on the same scale.
  # Group/Engineer Features
  
  
  return(
    list(
      original              = df_original,
      domain_grouped        = df_domain,
      rare_collapsed        = df_rare,
      deviation_merged      = df_dev,
      diagnostics = list(deviations_raw   = deviations_df, deviations_merged = merged_deviations)
    )
  )
}
