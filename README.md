# Income Prediction Project

## Overview
This project predicts whether an individual's income exceeds $50,000 per year based on census data. The prediction uses machine learning classification techniques implemented in R, comparing multiple models and feature engineering approaches.

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

### Best Performing Models
Across all experiments, the top performing models based on accuracy were:

| Experiment | Model | Accuracy | AUC | F1 Score |
|------------|-------|----------|-----|----------|
| **df_domain** | **Neural Network** | **94.00%** | **0.809** | **0.969** |
| df_dev | Neural Network | 93.94% | 0.797 | 0.969 |
| df_domain | Random Forest | 93.59% | 0.775 | 0.967 |

### Key Findings
- **Neural Networks** consistently performed best across most feature engineering approaches
- **Domain knowledge-based features** (df_domain) yielded the highest accuracy (94.00%)
- All models achieved 100% recall, indicating excellent detection of high-income individuals
- Baseline accuracy was 76.62%, with best models achieving a lift of 17.38%
- XGBoost showed inconsistent performance, suggesting potential need for hyperparameter tuning

### Performance Metrics Summary
- **Best Accuracy:** 94.00% (Neural Network with domain features)
- **Best AUC:** 0.835 (Logistic Regression with deviation features)
- **Best F1 Score:** 0.969 (Neural Network with domain features)
- **Baseline Accuracy:** 76.62%

## Future Improvements
- Feature selection optimization
- Hyperparameter tuning
- Testing additional algorithms
- Deploying the model as a web application

## Author
**Amira Ali**
- GitHub: [@Amira-Ali](https://github.com/Amira-Ali)
- [Add LinkedIn or other contact if you'd like]

## License
[Choose a license, e.g., MIT License, or remove this section]

## Acknowledgments
- Course: KL7010 - Principles of Data Science
- Institution: Northumbria University
- [Any other acknowledgments]
