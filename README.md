# Income Prediction Project

## Overview
This project predicts whether an individual's income exceeds $50,000 per year based on census data. The prediction uses machine learning classification techniques implemented in R, comparing multiple models and feature engineering approaches.

## Project Goal & Research Question
Rather than simply finding the most accurate model, this project is designed as an experimental study to answer the following question:
Can well-designed feature engineering outperform advanced machine learning models on structured census data?

To investigate this, the project:
- Systematically applies **8 different feature engineering strategies**
- Evaluates them across **6 model families**, from simple (Logistic Regression) to complex (Neural Networks, XGBoost, LightGBM)
- Compares not only raw accuracy, but **AUC, stability, and generalization**

## Dataset
The project uses census income data (`Income.csv`) containing demographic and employment information including:
- Age, education level, occupation
- Work class, marital status
- Hours worked per week
- Other relevant features

**Dataset Location:** `Income/data/Income.csv`

## Project Structure
```
Income/
├── data/               # Dataset files (Income.csv)
├── scripts/            # R analysis scripts
├── output/             # Model results and HTML reports
├── reports/            # Report generation scripts
├── support/            # Supporting functions and utilities
├── run_all.R           # Main script to execute entire analysis
├── Income.Rproj        # RStudio project file
└── README.md           # This file
```

## Methods
The analysis includes multiple experiments with different feature engineering approaches:
- **Original:** Baseline features
- **df_binary:** Binary encoding of categorical variables
- **prod_num:** Product of numerical features
- **df_log:** Log transformations
- **df_cox:** Cox-Box transformations
- **df_domain:** Domain knowledge-based features
- **df_rare:** Rare category handling
- **df_dev:** Deviation-based features

Each experiment tests six classification models:
- Logistic Regression
- Random Forest
- Support Vector Machine (SVM)
- Neural Network
- XGBoost
- LightGBM

## Technologies Used
- **R** (version 2025.09.2 Build 418)
- **RStudio**
- Key R packages:
  - **Data Manipulation:** `dplyr`, `tidyr`, `forcats`, `readxl`, `tools`
  - **Modeling & Statistics:** `caret`, `psych`, `e1071`, `class`, `pROC`, `nnet`
  - **Tree-Based Models:** `randomForest`, `xgboost`, `lightgbm`
  - **Visualization:** `ggplot2`, `patchwork`, `gridExtra`
  - **Reporting:** `knitr`, `rmarkdown`, `kableExtra`, `DT`

## Installation & Setup
1. Clone this repository:
   ```bash
   git clone https://github.com/Amira-Ali/Income-Prediction-50k.git
   ```

2. Open the project in RStudio by double-clicking `Income.Rproj`

3. Install required packages (run this in R console):
   ```r
   packages <- c("dplyr", "tidyr", "forcats", "readxl", "tools",
                 "caret", "psych", "e1071", "class", "pROC", "nnet",
                 "randomForest", "xgboost", "lightgbm",
                 "ggplot2", "patchwork", "gridExtra",
                 "knitr", "rmarkdown", "kableExtra", "DT")
   
   install.packages(packages)
   ```

## Usage
To run the complete analysis pipeline:

1. Open the project in RStudio
2. Run the main script:
   ```r
   source("run_all.R")
   ```

The `run_all.R` script will:
- Execute all analysis scripts from the `scripts/`, `support/`, and `reports/` folders
- Generate comprehensive model comparison results
- Export an HTML report to the `output/` folder

## Results

### 🏆 Final Model Selection Strategy
The final model was not chosen solely because it achieved the highest score, but because it best supported the project’s central hypothesis:
**Strong feature engineering can allow simple models to outperform or rival complex models on structured data**

Although Neural Networks reached a slightly higher accuracy (94%), **Logistic Regression using deviation-based features** was chosen as the final model. This version performed best because it:
- Achieved the best overall score (AUC: 0.835) out of all 48 tests.
- Proven that smart data preparation mattered more than using a complex algorithm.
- Is easier to explain and faster to use in a real-world setting.
  
**Final Results:**
**Accuracy:** 92.2% (Percentage of correct guesses)
**Recall:** 1.000 (Caught 100% of high-income individuals)
**F1 Score:** 0.959 (Strong balance between precision and recall)
**AUC: 0.835** (Highest overall reliability score)

### Model Comparison Summary

| Category | Model | Performance |
|----------|-------|-------------|
| **Best Overall** | Logistic Regression (df_dev) | AUC: 0.835, Accuracy: 92.2% |
| **Most Stable** | Neural Networks | AUC Range: 0.795-0.817, High reliability |
| **Most Balanced** | Random Forest | F1: 0.94-0.96, Strong stability |
| **Poorest** | XGBoost | AUC: ~0.68, High variance |

### Key Insights from 48 Experiments
✓ **Feature engineering** drove performance more than algorithm complexity  
✓ **df_dev strategy** (deviation-based merging) achieved best results  
✓ **Domain grouping + rare merging** consistently improved performance  
✓ **Log/Box-Cox transformations** provided only marginal gains  
✓ **Simple models + strong features** outperformed complex models with weak features  

### Baseline Comparison
- **Baseline Accuracy:** 76.62%
- **Final Model Lift:** 15.53% improvement over baseline
- All models achieved 100% recall, ensuring no high-income individuals were missed

## Future Improvements
- Feature selection optimization
- Hyperparameter tuning
- Testing additional algorithms
- Deploying the model as a web application

## Conclusion
This project demonstrates that:
**Thoughtful feature engineering often delivers greater value than simply increasing model complexity.**

By systematically comparing various feature strategies across different models, we have confirmed that:
- **Simple models can beat complex algorithms** when the data is represented effectively.
- **Performance improvements** were primarily driven by **how we prepared the data**, not which model we picked.
- **Feature engineering** should be a core part of the initial model design, not a step done at the end.


## Author
**Amira Ali**
- GitHub: [@Amira-Ali](https://github.com/Amira-Ali)
