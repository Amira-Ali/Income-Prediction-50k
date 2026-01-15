# ----------------------------------------------------------------- #
#                       Evaluate Model Performance                  #
# ----------------------------------------------------------------- #

encode_one_hot <- function(train, test, target_col) {
  x_train <- train %>% select(-all_of(target_col))
  x_test  <- test  %>% select(-all_of(target_col))
  
  dummies <- dummyVars( ~ ., data = x_train, fullRank = TRUE)
  
  train_enc <- predict(dummies, x_train) %>% as.data.frame()
  test_enc  <- predict(dummies, x_test)  %>% as.data.frame()
  
  list(
    train = cbind(train_enc, target = train[[target_col]]),
    test  = cbind(test_enc, target = test[[target_col]])
  )
}

# --- model_settings --- #
model_settings <- list(
  logistic = list(
    name = "Logistic Regression",
    type = "linear",
    train_fn = function(train_df, target_col) {
      glm(as.formula(paste(target_col, "~ .")), data = train_df, family = binomial)
    },
    predict_type = "response"
  ),
  
  rf = list(
    name = "Random Forest",
    type = "tree",
    train_fn = function(train_df, target_col) {
      randomForest(as.formula(paste(target_col, "~ .")), data = train_df)
    },
    predict_type = "prob"
  ),
  
  svm = list(
    name = "SVM",
    type = "linear",
    # can be "linear" or "kernel"
    train_fn = function(train_df, target_col) {
      svm(as.formula(paste(target_col, "~ .")),
          data = train_df,
          probability = TRUE)
    },
    predict_type = "prob"
  ),
  
  # knn = list(
  #   name = "k-NN",
  #   type = "nonparametric",
  #   train_fn = function(train_df, target_col) {
  #     # k-NN uses train/test during prediction, so we just return a list
  #     list(
  #       train = train_df,
  #       target_col = target_col
  #     )
  #   },
  #   predict_type = "prob"
  # ),
  
  nn = list(
    name = "Neural Network",
    type = "nonlinear",
    train_fn = function(train_df, target_col) {
      nnet(
        as.formula(paste(target_col, "~ .")),
        data = train_df,
        size = 5,
        # hidden units
        decay = 0.1,
        # regularization
        maxit = 200,
        trace = FALSE
      )
    },
    predict_type = "prob"
  ),
  
  xgboost = list(
    name = "XGBoost",
    type = "tree",
    train_fn = function(train_df, target_col) {
      # One-hot encode categorical variables
      x <- train_df %>% select(-all_of(target_col))
      x <- x %>% mutate(across(where(is.factor), as.character)) # ensure factors become character for dummyVars
      dummies <- dummyVars( ~ ., data = x, fullRank = TRUE)
      x_enc <- predict(dummies, x) %>% as.data.frame()
      
      # Numeric matrix
      x_matrix <- as.matrix(x_enc)
      
      # Target: numeric 0/1
      y <- as.numeric(train_df[[target_col]]) - 1
      
      # Create DMatrix
      dtrain <- xgb.DMatrix(data = x_matrix, label = y)
      
      # Train XGBoost
      xgb.train(
        params = list(objective = "binary:logistic", eval_metric = "auc"),
        data = dtrain,
        nrounds = 100,
        verbose = 0
      )
    },
    predict_type = "prob"
  ),
  
  lightgbm = list(
    name = "LightGBM",
    type = "tree",
    train_fn = function(train_df, target_col) {
      label <- as.numeric(train_df[[target_col]]) - 1
      dtrain <- lgb.Dataset(data = as.matrix(train_df[, setdiff(names(train_df), target_col)]), label = label)
      lgb.train(
        params = list(objective = "binary", metric = "auc"),
        data = dtrain,
        nrounds = 100
      )
    },
    predict_type = "prob"
  )
)
#--- End of Model Settigns ----

# Encoding + Scaling
preprocess_for_model <- function(train, test, target_col, model_family) {
  if (model_family == "linear") {
    encoded <- encode_one_hot(train, test, target_col)
    
    train_enc <- encoded$train
    test_enc  <- encoded$test
    
    encoded_cols <- setdiff(names(train_enc), "target")
    
    # Align test columns to train columns
    missing_cols <- setdiff(names(train_enc), names(test_enc))
    if (length(missing_cols) > 0) {
      test_enc[missing_cols] <- 0
    }
    
    # Ensure identical column order
    test_enc <- test_enc[, names(train_enc)]
    
    scaler <- preProcess(train_enc[, encoded_cols], method = c("center", "scale"))
    
    train_enc[, encoded_cols] <- predict(scaler, train_enc[, encoded_cols])
    test_enc[, encoded_cols]  <- predict(scaler, test_enc[, encoded_cols])
    
    list(train = train_enc,
         test  = test_enc,
         target_name = "target")
    
  } else {
    list(train = train,
         test  = test,
         target_name = target_col)
  }
}



train_logistic <- function(train_df, target_col) {
  # Generalized Linear Model
  glm(
    as.formula(paste(target_col, "~ .")),
    data = train_df,
    family = binomial # Logistic Regression Model
  )
}


evaluate_model <- function(df, target_col, model_key, model_name, experiment, seed = 42, threshold = 0.5) {
  set.seed(seed)
  
  # Ensure target is factor
  target_vals <- sort(unique(df[[target_col]]))  # dynamic unique values
  if (length(target_vals) != 2)
    stop("Target must be binary")
  # Ensure positive class is second level
  positive_class <- target_vals[2]
  df[[target_col]] <- factor(df[[target_col]], levels = target_vals)
  
  # Split dataset
  idx <- createDataPartition(df[[target_col]],
                             p = 0.8,
                             list = FALSE,
                             times = 1)
  train <- df[idx, ]
  test  <- df[-idx, ]
  
  # Only handle imbalance on training set
  train_balanced <- upSample(x = train %>% select(-all_of(target_col)),
                             y = train[[target_col]],
                             yname = target_col)
  train <- train_balanced
  
  
  # Ensure factor columns in test have same levels as training
  for (col in names(train)) {
    if (is.factor(train[[col]])) {
      test[[col]] <- factor(test[[col]], levels = levels(train[[col]]))
    }
  }
  
  # To ensure both classes exist in training set
  if (length(unique(train[[target_col]])) < 2) {
    stop("Training set contains only one class")
  }
  
  # Preprocess features (encoding + scaling for linear models)
  model_spec <- model_settings[[model_key]]
  processed <- preprocess_for_model(train, test, target_col, model_spec$type)
  
  # Train the model
  model <- model_spec$train_fn(processed$train, processed$target_name)
  
  # Predict probabilities
  if (model_key == "knn") {
    # k-NN: we must use numeric preprocessed features
    train_data <- processed$train %>% select(-all_of(processed$target_name))
    test_data  <- processed$test  %>% select(-all_of(processed$target_name))
    train_labels <- processed$train[[processed$target_name]]
    
    # Drop rows with NAs
    na_train <- !apply(train_data, 1, function(x)
      any(is.na(x)))
    na_test  <- !apply(test_data, 1, function(x)
      any(is.na(x)))
    train_data <- train_data[na_train, , drop = FALSE]
    train_labels <- train_labels[na_train]
    test_data <- test_data[na_test, , drop = FALSE]
    
    # Adjust k if too large
    k_use <- min(k_knn, nrow(train_data))
    if (k_use < 1)
      stop("Not enough training samples for k-NN")
    
    pred_class <- knn(train_data,
                      test_data,
                      cl = train_labels,
                      k = k_use,
                      prob = TRUE)
    prob <- attr(pred_class, "prob")
    prob <- ifelse(pred_class == positive_class, prob, 1 - prob)
  } else if (model_key == "logistic") {
    prob <- as.numeric(predict(model, processed$test, type = "response"))
    
  } else if (model_key == "rf") {
    tmp <- predict(model, processed$test, type = "prob")
    prob <- as.numeric(tmp[, positive_class])
    
  } else if (model_key == "nn") {
    tmp <- predict(model, processed$test, type = "raw")
    prob <- as.numeric(tmp[, 1])  # binary NN
    
  } else if (model_key == "svm") {
    tmp <- predict(model, processed$test, probability = TRUE)
    prob <- attr(tmp, "probabilities")[, positive_class]
    
  } else if (model_key == "xgboost") {
    # Extract features (exclude target)
    x_test <- processed$test %>% select(-all_of(processed$target_name))
    
    # Convert factors/characters to numeric using same dummyVars from training
    # Assume 'dummies' was created during training
    # If not stored, create it again from training features
    x_train <- processed$train %>% select(-all_of(processed$target_name))
    dummies <- dummyVars(~ ., data = x_train, fullRank = TRUE)
    
    x_test_enc <- predict(dummies, x_test) %>% as.data.frame()
    
    # Convert to numeric matrix
    dtest <- xgb.DMatrix(data = as.matrix(x_test_enc))
    
    # Predict
    prob <- predict(model, dtest)
  } else if (model_key == "lightgbm") {
    test_matrix <- as.matrix(processed$test %>% select(-all_of(processed$target_name)))
    prob <- predict(model, test_matrix)
  } else {
    stop("Unknown model_key")
  }
  
  #  Convert probabilities to class predictions
  pred <- factor(ifelse(prob >= threshold, positive_class, levels(df[[target_col]])[1]),
                 levels = levels(df[[target_col]]))
  
  true <- processed$test[[processed$target_name]]
  
  valid_idx <- !is.na(pred) & !is.na(true)
  
  pred <- pred[valid_idx]
  true <- true[valid_idx]
  prob <- prob[valid_idx]
  
  baseline_acc <- max(prop.table(table(test[[target_col]])))
  
  #  Compute metrics
  if (length(levels(factor(true))) < 2) {
    warning("Test set contains only one class; some metrics may be NA")
    recall <- precision <- F1 <- AUC <- NA
  } else {
    recall    <- sensitivity(pred, true)
    precision <- posPredValue(pred, true)
    F1        <- F_meas(pred, true)
    AUC       <- as.numeric(auc(true, prob))
  }
  
  # ---------- Return results as tibble ----------
  tibble(
    experiment,
    model      = model_name,
    experiment_model = paste(experiment, model_name, sep = "_"),
    seed       = seed,
    n_train    = nrow(train),
    n_test     = nrow(test),
    AUC        = AUC,
    Accuracy   = mean(pred == true),
    Baseline   = baseline_acc,
    Lift       = mean(pred == true) - baseline_acc,
    Recall     = recall,
    Precision  = precision,
    F1         = F1
  )
}