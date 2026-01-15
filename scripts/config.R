# The idea is to provide Encapsulation to the project, the user_comments and definitions are moved to config.R. 
# This allows you to swap datasets by simply pointing to a different config file.

# ---- Project Configuration ----
#main_folder <- "C:/Users/ashlibek/Documents/DataScience/"
dataset_name <- "Income"
#final_path <- file.path(main_folder, dataset_name)
#setwd(final_path)
# Pattern for allowed special characters in character columns
pattern <- "[^A-Za-z0-9.=<> -]"

# Possible date formats in dataset
date_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d-%b-%y", "%d/%m/%Y", "%m-%d-%Y")


numeric_cols <- names(df)[sapply(df, is.numeric)]
cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]


# ---- Report Definitions ----
report_glossary <- list(
  "bi_analysis" = "Bivariate analysis examines the relationship between two variables; a predictor and a target. It's not about fixing the data, rather it's about deciding what is useful for the model.\n\n",
  "bi_analysis_goals"= "Analyzing the relationship with the target helps in:\n
**1. Feature Selection:** Identify which features have a clear relationship with the target and prioritize them for machine learning models, while dropping features with weak connection with the target, especially if they are highly skewed or imbalanced.\n
**2. Grouping:** Group categories that have similar target probabilities.\n
"
)

#---- Comments structure ----
# not all checks have corresponding user comments, some have only auto-comments that are being generated automatically by the application itself 
comments <- list(
  
  # Technical Validity
  type_mismatch = list(),
  
  # Descriptive Statistics
  # Numerical Features
  numeric = setNames( # assigns the column names as names of the list elements.
    vector("list", length(numeric_cols)),  # creates an empty list with length equal to the number of numeric columns.
    numeric_cols                          
  ),
  
  # Statistical Insights
  stats_insights = c(),
  
  # Feature Cardinality and Balance
  # Interpretation and Grouping Strategy
  startegy = list(
    grouping = c(),
    encoding = c()
  ),
 
   # Univariate Distributions Highlights
  univariate_highlights = c(),
  
  # Integrity & Completeness Checks
  missing = list(),
  duplicates = list(),#special char is not included because it does not need user comments. auto-notes are enough to explain.
  
  # Anomalies & Outliers
  outliers = setNames(
    vector("list", length(numeric_cols)),  # outliers only meaningful in numeric cols
    numeric_cols                            
  ),
  
  # Data Issues & Bivariate Analysis
  data_issues = c(),
  
  bivariate_numeric_highlights = setNames(
    vector("list", length(numeric_cols)),  
    numeric_cols                            
  ),
  
  bivariate_cat_highlights = setNames(
    vector("list", length(cat_cols)),  
    cat_cols                            
  )
)

    # Align comments names with actual dataframe column names
names(comments$outliers) <- names(df)[sapply(df, is.numeric)]
names(comments$numeric)  <- names(df)[sapply(df, is.numeric)]

# ---- Numerical features ----
comments$numeric <- list(
  age = list(
    skew = " With a median of **37**, a mean of **38.75**, and a range of **17** to **90**. This suggests the data is concentrated around younger ages. However, further visualization is needed to confirm this and to determine whether the higher ages are true outliers pulling the distribution to the right and causing the skewness, or just a normal part of the data.",
    kurtosis = " ",
    rse = " "
  ),
  
  education.num = list(
    skew = " Most individuals have lower education, fewer with higher levels.",
    kurtosis = "",
    rse = ""
  ),
  
  hours.per.week = list(
    skew = " Most individuals work fewer than the typical 40hr/week, with a smaller number (the outliers) working a lot more hours.",
    kurtosis = "",
    rse = ""
  )
)


# ----Stats Insights ----
comments$stats_insights <- paste0(
  "**Overall Precision**\n
  All numerical features demonstrate **RSE < 0.5%**, well within the **Agency for Healthcare Research and Quality (AHRQ) precision standard of 30%** (Agency for Healthcare Research and Quality, n.d.). This high precision, resulting from the large sample size (n=10,000), ensures the sample means are excellent estimates of population mean (μ). Population mean can be reliably estimated using 95% confidence intervals: CI = x̄ ± (SE * 1.96)")
 

# ---- Grouping ----
comments$strategy["grouping"] <- c(paste0("The summary descriptive table shows that half of the categorical features in this dataset have many small categories, which can negatively impact the model's performance.\n
**Cons of Small Categories:**\n
* **Visual Noise:** Plotting these features produces cluttered charts.
* **Overfitting Risk:** Small categories increase the risk of overfitting because the model learn patterns from them.
* **Skewness:** Too many small categories can skew the data distribution.\n
**Pros of Grouping:**\n
* **Enhanced Clarity:** Merging into broader categories makes the distribution more clear and easy to read.
* **Better Summarization:** The descriptive table becomes more concise, highlighting only the most impactful dominant and rare categories.
* **Enhanced Performance:** Grouping reduces multi-cardinality and skewness, which leads to more robust model performance.\n
**How grouping is performed:**\n
Grouping will be done mainly using domain knowledge. Rare categories will be combined into an 'Other' category if needed. We considered automatically grouping rare categories, but it wasn’t necessary because domain grouping already merged most high-cardinality features. This method ensures that meaningful small categories are logically grouped rather than blindly merged into an 'Other' category.\n
**Target Variable Analysis (Class Imbalance)**:\n\n
The target variable **(Income)** shows a **signifiant class imbalance**, with the majority class **(<=50K)** representing **76.4%** of the dataset. **Imbalanced classes** have a very **negative impact on model's performance**, as they can cause models to become bias toward the majority class, consequently, the model can easily **achieve 76.4% accuracy by predicting <=50K for all observations**.\n
Hence, we need to consider resampling (oversampling-undersampling) or evaluate performance using metrics other than accuracy, such as Precision, Recall, and F1-Score. 
However, it is best to plot these features first to visualize the distribution and decide on the most logical strategies to deal with the issues of multi-cardinality and the imbalanced distributions.\n"))
 
# ----Univariate Highlights ----
comments$univariate_highlights = c("Reviewing the distributions confirms earlier findings and reveals further patterns:\n

**1. Skewness Patterns**\n 
* Right-Skewed: As previously observed, Age is right-skewed. Relationship and Income are the same.
* Left-Skewed: Education and Race. Their skewness is probably a result from their imbalanced distribution.
* Symmetric distributions: Education-Num and Hours-per-Week shows relatively balanced distributions around their means.\n 

**2. Multi-Cardinality**\n
Except for Race and Sex, all categorical features have high cardinality and need dimensionality reduction.\n

**3. Class imbalance**\n
Except for Age and Sex, all features exhibit significant class imbalance."
)
 
comments$strategy["encoding"] <- c(paste0(""))

# ---- Outliers ----
comments$outliers <- list(
  age = c("Most individuals are young with few older people causing right skew."),
  education.num =  c("Most individuals have lower education, fewer with higher levels."),
  hours.per.week = c("Most individuals work ~ 40hr/week, with a smaller number (outliers) working a lot more hours."
  )
)
 
# ----Data Issues ----
comments$data_issues <- c(paste0("Till this point of our EDA, we have been able to identify these issues:\n
- Skewed data\n - Multi-cardinality\n - Imbalanced-class\n - Duplicated data\n - Special Characters\n - Outliers\n 
The only issues we **can** and **should** handle now are duplicated data and special characters as they will hinder our progress, as for the other issues, before deciding on how to handle them, we still have one last thing to do; **Bivariate Analysis** (visualizing feature's relationships with the target).\n"))
 
# ----Bivariate Numerical Analysis ----

comments$bivariate_numeric_guidence <- list(
  "Guidance for Identifying Strong Predictors" = c(
    "Features with **significantly different medians** across target classes **→ Strong predictor**",
    "Features with **little to no overlapped boxes → Strong predictor**",
    "**Outliers concentrated in one target class → Important data**, not errors to be removed"
  )
)

comments$bivariate_numeric_highlights <- list(
  "Findings" = c(
    "The lower-income group **<=50k** is generally **younger** with **less education**, while the higher-income group **>50k** tend to be **older** and **highly educated**.",
    "The **<=50k** group works a **typical 40 hrs/week**, whereas the **>50k** group **works more**, with a median of **43 hrs/week**.",
    "**Age, education.num** and **hours.per.week** are all **strong** predictors.",
    "In all features, the lower-income **<=50k** class contained significantly **more outliers** than the higher-income class **>50k.**"
  )
)

# ----Bivariate Categorical Analysis ----

comments$bivariate_cat_guidence <- list(
  "Guidance for Identifying Strong Predictors" = c(
    "Features whose categories (bars) exhibit **significant variance or deviate greatly from the average target distribution → Strong predictor**",
    "Categories with **~0 deviation from the average target distribution → Weak predictors**. They should be **grouped into an 'Other' category to reduces noise and prevent overfitting**"
  )
)

comments$bivariate_cat_highlights <- list(
  "Findings" = c(
    "**Race** and **Sex** are **weak** predictors, as they do not deviate very much from the average target distribution (76% / 24%). In other words, they don’t give much information about income.",
    "**Education, Occupation, workclass, Marital_status, Relationship** are **strong** predictors, as their categories show significant differences in income distribution. This means these features give useful clues that help tell who is more likely to have a higher or lower income."
  )
)



 
