# 1. Environment Setup
source("scripts/utils.R") 

# 2. Load All Helping files
source("scripts/config.R")
source("scripts/eda.R")
source("scripts/data_processing.R")
source("scripts/fe.R")
source("scripts/model_eval.R")


# 3. Data Loading & Cleaning
# file.path instead of manual "" for better compatibility across different computers
df <- read.csv(
  file.path("data/", paste0(dataset_name, ".csv")),
  header = TRUE,
  stringsAsFactors = FALSE
)

# fix columns names to avoid problems later
names(df) <- make.names(names(df))
names(df)[names(df) == "education_num"] <- "education.num"

#names(df)

display_names <- toTitleCase(gsub("_-|\\.", " ", names(df)))
name_map <- setNames(display_names, names(df))

target_col <- "Income"

# Set the >50k as the positive outcome, the one we're after
# Positive class = class with ">" sign 
target_vals <- sort(unique(df[[target_col]])) # "<=50K." ">50K." 

if (">50K." %in% target_vals) {
  target_vals <- c(setdiff(target_vals, ">50K."), ">50K.")
}

# Convert the target to factor before anything
df[[target_col]] <- factor(df[[target_col]], levels = target_vals)


# 4. Exploratory data analysis
eda_results <- explore_data(
  df,
  pattern,
  date_formats,
  comments #send and process comments first before including them in the final report
)

df <- remove_ws(df)
df <- replace_special_char(df)
df <- remove_duplicate(df)

save_eda_results_rdata(df)# Save EDA results in a .RData file which is a binary workspace file that stores one or more R objects


load("model_metadata.RData")

strong_numeric <- c("age", "education.num", "hours.per.week")
df <- cap_outliers(df, strong_numeric)

# 5. Feature Engineer
fe_results <- feature_engineer(
  df,
  predictors,
  target_col,
  target_classes,
  target_dist
)

# fe_results is a list of 8 data frames, each representing a feature-engineered version of df (scaled-df, merged-df, categoried-df,...)
# Selects the first 8 data frames and Apply a function on each one of them
# The function converts the target column to a factor and explicitly sets the factor levels
# Returns the modified data frame
# This ensures that:
# The target variable is consistently a factor
# Factor levels are in a fixed, predefined order
# Prevents issues in:
# Modeling
# Cross-validation
# Confusion matrices
# Predictions where a class might be missing in a subset
# This is especially critical for:
# Classification models
# Imbalanced datasets
# Train/test splits

fe_results[1:8] <- lapply(fe_results[1:8], function(df) {
  df[[target_col]] <- factor(df[[target_col]], levels = target_classes)
  df
})



# for (i in 1:8) {
#   cat("Dataframe", i, "columns:\n")
#   for (col_name in colnames(fe_results[[i]])) {
#     cat("  ", col_name, "\n")
#   }
#   cat("\n")
# }


# df is a dataframe where each row is a fe experiment and each col is a performance metric
# this function returns a list of 8 tibbles. each tibble is only 1 row, representing fe experiment
if (file.exists("output/model_comparison.csv")) {
  # Read the saved model performance
  model_performance_tbl <- read.csv("output/model_comparison.csv", stringsAsFactors = FALSE)
  message("Loaded existing model performance from output/model_comparison.csv")
} else {
  # Train models if file doesn't exist
 model_performance_tbl <- bind_rows(lapply(names(fe_results)[1:8], function(fe_name) {
   df <- fe_results[[fe_name]]
   bind_rows(lapply(names(model_settings), function(model_key) {
     evaluate_model(
       df = df,
       target_col = target_col,
       model_key = model_key,
       model_name = model_settings[[model_key]]$name,
       experiment = fe_name
     )
   }))

 }))
 
write.csv(model_performance_tbl, file = "output/model_comparison.csv", row.names = FALSE)
message("Model performance saved to output/model_comparison.csv")
}

#print(model_performance_tbl)

 # Render Report
 render(
   input = "reports/DQR.Rmd",
   output_file = paste0("../output/", dataset_name, "_DQR_", Sys.Date(), ".html"),
   params = list(
     dataset_name = dataset_name,
     eda_results = eda_results,
     glossary = report_glossary,
     comments = comments,
     fe_results = fe_results,
     model_performance_tbl = model_performance_tbl
   )
 )
 