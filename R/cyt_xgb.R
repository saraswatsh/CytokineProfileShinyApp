#' Run XGBoost Classification on Cytokine Data.
#'
#' This function trains and evaluates an XGBoost classification model on cytokine data.
#' It allows for hyperparameter tuning, cross-validation, and visualizes feature importance.
#'
#' @param data A data frame containing the cytokine data, with one column as the grouping variable
#'   and the rest as numerical features.
#' @param group_col A string representing the name of the column with the grouping variable
#'   (i.e., the target variable for classification).
#' @param train_fraction A numeric value between 0 and 1 representing the proportion of data to use for training (default is 0.7).
#' @param nrounds An integer specifying the number of boosting rounds (default is 500).
#' @param max_depth An integer specifying the maximum depth of the trees (default is 6).
#' @param learning_rate A numeric value representing the learning rate (default is 0.1).
#' @param nfold An integer specifying the number of folds for cross-validation (default is 5).
#' @param cv A logical value indicating whether to perform cross-validation (default is FALSE).
#' @param objective A string specifying the XGBoost objective function (default is "multi:softprob" for multi-class classification).
#' @param early_stopping_rounds An integer specifying the number of rounds with no improvement to stop training early (default is NULL).
#' @param eval_metric A string specifying the evaluation metric (default is "mlogloss").
#' @param min_split_loss A numeric value for the minimum loss reduction required to make a further partition (default is 0).
#' @param colsample_bytree A numeric value specifying the subsample ratio of columns when constructing each tree (default is 1).
#' @param subsample A numeric value specifying the subsample ratio of the training instances (default is 1).
#' @param min_child_weight A numeric value specifying the minimum sum of instance weight needed in a child (default is 1).
#' @param top_n_features An integer specifying the number of top features to display in the importance plot (default is 10).
#' @param plot_roc A logical value indicating whether to plot the ROC curve and calculate the AUC for binary classification (default is FALSE).
#' @param output_file Optional. A file path to save the outputs as a PDF file. If provided, outputs are written to the file and results are returned invisibly.
#' @param progress Optional. A Shiny \code{Progress} object for reporting progress updates.
#' @return A list containing:
#'   \item{summary_text}{A character string summarizing key results (interactive mode only).}
#'   \item{model}{The trained XGBoost model.}
#'   \item{confusion_matrix}{The confusion matrix (test set).}
#'   \item{importance}{The feature importance data for the top features.}
#'   \item{class_mapping}{A named vector showing the mapping from class labels to numeric values used for training.}
#'   \item{cv_results}{Cross-validation results if performed (otherwise NULL).}
#'   \item{plot}{A ggplot object showing the feature importance plot.}
#'
#' @examples
#' # Example usage:
#' data_df0 <- ExampleData1
#' data_df <- data.frame(data_df0[, 1:3], log2(data_df0[, -c(1:3)]))
#' data_df <- data_df[, -c(2,3)]
#' data_df <- dplyr::filter(data_df, Group != "ND")
#'
#' cyt_xgb(
#'   data = data_df, group_col = "Group",
#'   nrounds = 500, max_depth = 4, learning_rate = 0.05,
#'   nfold = 5, cv = FALSE, eval_metric = "mlogloss",
#'   early_stopping_rounds = NULL, top_n_features = 10,
#'   plot_roc = TRUE
#' )
#'
#' @importFrom xgboost xgb.DMatrix xgb.train xgb.importance xgb.ggplot.importance xgb.cv
#' @importFrom caret createDataPartition confusionMatrix
#' @import ggplot2
#' @importFrom pROC roc auc ggroc
#' @export
cyt_xgb <- function(
  data,
  group_col,
  train_fraction = 0.7,
  nrounds = 500,
  max_depth = 6,
  learning_rate = 0.1,
  nfold = 5,
  cv = FALSE,
  objective = "multi:softprob",
  early_stopping_rounds = NULL,
  eval_metric = "mlogloss",
  min_split_loss = 0,
  colsample_bytree = 1,
  subsample = 1,
  min_child_weight = 1,
  top_n_features = 10,
  plot_roc = FALSE,
  output_file = NULL,
  progress = NULL
) {
  # Start progress if provided
  if (!is.null(progress)) {
    progress$set(message = "Starting XGBoost analysis", value = 0)
  }

  # Make sure group_col is a factor
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Setting up data...")
  }
  data[[group_col]] <- as.factor(data[[group_col]])

  # Create mapping from group names to numeric labels
  class_labels <- levels(data[[group_col]])
  class_mapping <- stats::setNames(0:(length(class_labels) - 1), class_labels)

  # Convert grouping variable to numeric for xgboost
  data[[group_col]] <- as.numeric(data[[group_col]]) - 1

  # Prepare dataset
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Preparing dataset...")
  }
  X <- as.matrix(data[, setdiff(colnames(data), group_col)])
  y <- data[[group_col]]

  # Train/test split
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Splitting data into train and test sets...")
  }
  set.seed(123)
  train_indices <- caret::createDataPartition(
    y,
    p = train_fraction,
    list = FALSE
  )
  X_train <- X[train_indices, ]
  y_train <- y[train_indices]
  X_test <- X[-train_indices, ]
  y_test <- y[-train_indices]

  num_class = length(unique(y))

  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgboost::xgb.DMatrix(data = X_test, label = y_test)

  # Set xgboost parameters
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Setting XGBoost parameters...")
  }
  params <- list(
    objective = objective,
    eval_metric = eval_metric,
    num_class = length(unique(y)),
    max_depth = max_depth,
    learning_rate = learning_rate,
    min_split_loss = min_split_loss,
    colsample_bytree = colsample_bytree,
    subsample = subsample,
    min_child_weight = min_child_weight
  )

  # Train model
  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Training XGBoost model...")
  }
  xgb_model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    evals = list(train = dtrain, test = dtest),
    early_stopping_rounds = early_stopping_rounds,
    verbose = FALSE
  )

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Evaluating model...")
  }
  # Best iteration
  eval_col_name <- paste0("test_", eval_metric)
  best_iter <- which.min(xgb_model$evaluation_log[[eval_col_name]])

  # Predictions and confusion matrix
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Making predictions on test set...")
  }
  preds <- stats::predict(xgb_model, X_test)

  if (is.matrix(preds)) {
    preds_matrix <- preds
  } else {
    preds_matrix <- matrix(preds, ncol = num_class, byrow = TRUE)
  }

  # Hard labels for confusion matrix (0 .. num_class-1)
  pred_labels <- max.col(preds_matrix) - 1

  # Build ROC plot if applicable (for binary classification)
  roc_plot <- NULL
  auc_value <- NA

  if (plot_roc) {
    if (num_class == 2) {
      # probability of class "1" (label 1)
      xgb_prob <- preds_matrix[, 2]

      if (length(xgb_prob) != length(y_test)) {
        warning(
          "Length mismatch between predicted probabilities and true labels; ",
          "skipping ROC/AUC."
        )
      } else {
        roc_obj <- pROC::roc(y_test, xgb_prob, quiet = TRUE)
        auc_value <- pROC::auc(roc_obj)

        roc_plot <- pROC::ggroc(
          roc_obj,
          color = "blue",
          linewidth = 1.5,
          legacy.axes = TRUE
        ) +
          ggplot2::geom_abline(
            linetype = "dashed",
            color = "red",
            linewidth = 1
          ) +
          ggplot2::labs(
            title = "ROC Curve (Test Set)",
            x = "1 - Specificity",
            y = "Sensitivity"
          ) +
          ggplot2::annotate(
            "text",
            x = 0.75,
            y = 0.25,
            label = paste("AUC =", round(auc_value, 3)),
            size = 5,
            color = "blue"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "grey90"),
            panel.grid.minor = element_line(color = "grey95")
          )

        print(roc_plot)
      }
    }
  }
  test_levels <- sort(unique(y)) # global class set

  test_pred <- factor(pred_labels, levels = test_levels)
  test_ref <- factor(y_test, levels = test_levels)

  confusion_mat <- caret::confusionMatrix(test_pred, test_ref)

  accuracy_test <- confusion_mat$overall["Accuracy"]

  # Feature importance
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Calculating feature importance...")
  }
  importance_matrix <- xgboost::xgb.importance(
    feature_names = colnames(X),
    model = xgb_model
  )
  top_features <- utils::head(importance_matrix, top_n_features)
  vip_plot <- xgboost::xgb.ggplot.importance(
    importance_matrix = top_features,
    top_n = top_n_features
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "red2", show.legend = FALSE) +
    ggplot2::ggtitle("Top Features by Gain") +
    ggplot2::ylab("Importance (Gain)") +
    ggplot2::xlab("Features") +
    ggplot2::theme_minimal()

  # Cross-validation (if requested)
  cv_results <- NULL
  best_cv_iter <- NULL
  if (cv) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Performing cross-validation...")
    }
    xgb_cv <- xgboost::xgb.cv(
      params = params,
      data = dtrain,
      nrounds = nrounds,
      nfold = nfold,
      early_stopping_rounds = early_stopping_rounds,
      verbose = FALSE,
      prediction = TRUE
    )
    eval_col_name_cv <- paste0("test_", eval_metric, "_mean")
    best_cv_iter <- which.min(xgb_cv$evaluation_log[[eval_col_name_cv]])
    cv_results <- xgb_cv
  }

  # If in interactive mode, build a summary text and return results
  if (is.null(output_file)) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Finalizing interactive results...")
    }
    summary_text <- utils::capture.output({
      cat("### XGBOOST RESULTS ###\n\n")
      cat("1) Group -> Numeric Label Mapping:\n")
      print(class_mapping)
      cat("\n2) Confusion Matrix on Test Set:\n")
      print(confusion_mat$table)
      cat("\nTest Accuracy:", round(accuracy_test, 3), "\n")
      if (length(unique(y_test)) == 2) {
        cat("\nSensitivity:", confusion_mat$byClass["Sensitivity"], "\n")
        cat("Specificity:", confusion_mat$byClass["Specificity"], "\n")
        if (plot_roc) {
          cat(
            "\nAUC:",
            ifelse(is.na(auc_value), "N/A", round(auc_value, 3)),
            "\n"
          )
        }
      }
      cat("\n3) Top", top_n_features, "Important Features:\n")
      print(top_features)
      if (cv && !is.null(cv_results)) {
        cat("\n4) CROSS-VALIDATION RESULTS:\n")
        cat("Best iteration from cross-validation:\n")
        if (!is.null(best_cv_iter)) {
          print(cv_results$evaluation_log[best_cv_iter, ])
        }
      }
    })
    summary_text <- paste(summary_text, collapse = "\n")

    return(list(
      summary_text = summary_text,
      model = xgb_model,
      confusion_matrix = confusion_mat$table,
      importance = top_features,
      class_mapping = class_mapping,
      cv_results = cv_results,
      plot = vip_plot,
      roc_plot = roc_plot
    ))
  } else {
    # PDF mode: print outputs to PDF
    grDevices::pdf(file = output_file, width = 8, height = 8)
    cat("### XGBOOST RESULTS ###\n\n")
    cat("Group -> Numeric Label Mapping:\n")
    print(class_mapping)
    cat("\nBest Iteration:\n")
    print(xgb_model$evaluation_log[best_iter, ])
    cat("\nConfusion Matrix on Test Set:\n")
    print(confusion_mat$table)
    cat("\nTest Accuracy:", round(accuracy_test, 3), "\n")
    if (!is.null(roc_plot)) {
      print(roc_plot)
    }
    print(vip_plot)
    if (cv && !is.null(best_cv_iter)) {
      cat("\nCross-Validation Best Iteration:\n")
      print(cv_results$evaluation_log[best_cv_iter, ])
    }
    grDevices::dev.off()
    return(invisible(NULL))
  }
}
