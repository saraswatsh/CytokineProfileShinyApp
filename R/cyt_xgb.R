#' Run XGBoost Classification on Cytokine Data
#'
#' @description
#' This function trains and evaluates an XGBoost classification model on
#' cytokine data.  It allows for hyperparameter tuning, cross-validation,
#' and visualizes feature importance.
#'
#' @param data A data frame containing numeric predictor variables and one
#'   grouping column.  Non-numeric predictor columns will be coerced to
#'   numeric if possible.
#' @param group_col Character string naming the column that contains the class
#'   labels (target variable).  Required.
#' @param train_fraction Numeric between 0 and 1 specifying the proportion of
#'   samples used for model training.  Default is \code{0.7}.
#' @param nrounds Integer specifying the number of boosting rounds.  Default
#'   is \code{500}.
#' @param max_depth Integer specifying the maximum depth of each tree.
#'   Default is \code{6}.
#' @param learning_rate Numeric specifying the learning rate.  Default is
#'   \code{0.1}.
#' @param nfold Integer specifying the number of folds for cross-validation
#'   when \code{cv = TRUE}.  Default is \code{5}.
#' @param cv Logical indicating whether to perform cross-validation using
#'   \code{xgb.cv}.  Default is \code{FALSE}.
#' @param objective Character string specifying the objective function.
#'   Default is \code{"multi:softprob"}.
#' @param early_stopping_rounds Integer.  Number of rounds without improvement
#'   before training stops early.  Default is \code{NULL}.
#' @param eval_metric Character specifying the evaluation metric.  Default is
#'   \code{"mlogloss"}.
#' @param min_split_loss Numeric.  Minimum loss reduction required to make a
#'   further partition.  Default is \code{0}.
#' @param colsample_bytree Numeric.  Subsample ratio of columns per tree.
#'   Default is \code{1}.
#' @param subsample Numeric.  Subsample ratio of training instances.  Default
#'   is \code{1}.
#' @param min_child_weight Numeric.  Minimum sum of instance weight in a
#'   child.  Default is \code{1}.
#' @param top_n_features Integer.  Number of top features to display in the
#'   importance plot.  Default is \code{10}.
#' @param verbose Integer (0, 1, or 2) controlling verbosity of
#'   \code{xgb.train}.  Default is \code{0}.
#' @param plot_roc Logical.  Whether to plot the ROC curve and compute AUC
#'   for binary classification.  Default is \code{FALSE}.
#' @param print_results Logical.  If \code{TRUE}, prints the confusion matrix,
#'   top features, and cross-validation metrics to the console.  Default is
#'   \code{FALSE}.
#' @param seed Optional integer seed for reproducibility.  Default is
#'   \code{123}.
#' @param scale Character string specifying a transformation to apply to
#'   numeric predictors prior to model fitting.  One of \code{"none"}
#'   (default), \code{"log2"}, \code{"log10"}, \code{"zscore"}, or
#'   \code{"custom"}.
#' @param custom_fn A custom transformation function used when
#'   \code{scale = "custom"}.  Ignored otherwise.
#' @param output_file Optional.  A file path to save outputs as a PDF.  If
#'   \code{NULL} (default), a named list is returned for interactive display.
#' @param font_settings Optional named list of font sizes for supported plot
#'   text elements.
#' @param progress Optional.  A Shiny \code{Progress} object for reporting
#'   progress updates.
#'
#' @return When \code{output_file} is \code{NULL}, an invisible named list
#'   with elements \code{summary_text}, \code{model}, \code{confusion_matrix},
#'   \code{importance}, \code{class_mapping}, \code{cv_results},
#'   \code{importance_plot}, and \code{roc_plot}.  When \code{output_file} is
#'   provided, a PDF is written and the function returns \code{NULL}
#'   invisibly.
#' @examples
#' data_df0 <- ExampleData1
#' data_df  <- data.frame(data_df0[, 1:3], log2(data_df0[, -c(1:3)]))
#' data_df  <- data_df[, -c(2:3)]
#' data_df  <- dplyr::filter(data_df, Group != "ND")
#' cyt_xgb(data = data_df, group_col = "Group", nrounds = 250,
#'         max_depth = 4, learning_rate = 0.05, nfold = 5,
#'         cv = FALSE, objective = "multi:softprob",
#'         eval_metric = "auc", plot_roc = TRUE, print_results = FALSE)
#'
#' @importFrom xgboost xgb.DMatrix xgb.train xgb.importance xgb.ggplot.importance xgb.cv getinfo
#' @importFrom caret createDataPartition confusionMatrix
#' @importFrom data.table copy
#' @import ggplot2
#' @importFrom pROC roc auc ggroc
#' @importFrom utils capture.output head
#' @importFrom stats reorder setNames predict
#' @author Shubh Saraswat and Xiaohua Douglas Zhang
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
  verbose = 0,
  plot_roc = FALSE,
  print_results = FALSE,
  seed = 123,
  scale = c("none", "log2", "log10", "zscore", "custom"),
  custom_fn = NULL,
  output_file = NULL,
  font_settings = NULL,
  progress = NULL
) {
  format_class_balance <- function(values, labels = NULL) {
    factor_values <- if (is.null(labels)) {
      droplevels(as.factor(values))
    } else {
      factor(values, levels = seq_along(labels) - 1L, labels = labels)
    }
    counts <- table(factor_values)
    if (!length(counts)) {
      return("NA")
    }

    props <- round(100 * counts / sum(counts), 1)
    paste(
      paste0(names(counts), "=", as.integer(counts), " (", props, "%)"),
      collapse = ", "
    )
  }

  # ── 0. Initialize ──────────────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$set(message = "Running XGBoost...", value = 0)
  }

  names(data) <- make.names(names(data), unique = TRUE)
  data <- as.data.frame(data)
  scale <- match.arg(scale)
  resolved_fonts <- normalize_font_settings(
    font_settings = font_settings,
    supported_fields = c(
      "base_size", "plot_title", "x_title", "y_title",
      "x_text", "y_text", "legend_title", "legend_text"
    ),
    activate = !is.null(font_settings)
  )

  if (!group_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in data.", group_col))
  }

  # ── 1. Scaling via shared utility ──────────────────────────────────────────
  numeric_cols <- setdiff(names(data)[sapply(data, is.numeric)], group_col)
  if (scale != "none" && length(numeric_cols) > 0L) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = paste("Applying", scale, "transformation"))
    }
    data <- apply_scale(
      data = data,
      columns = numeric_cols,
      scale = scale,
      custom_fn = custom_fn
    )
  }

  # ── 2. Class mapping & numeric encoding ────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Preparing class labels")
  }
  data[[group_col]] <- as.factor(data[[group_col]])
  original_group <- droplevels(data[[group_col]])
  class_labels <- levels(original_group)
  class_mapping <- stats::setNames(seq_along(class_labels) - 1L, class_labels)

  if (print_results) {
    cat("\nGroup to Numeric Label Mapping\n")
    print(class_mapping)
  }

  data[[group_col]] <- as.numeric(original_group) - 1L

  # ── 3. Prepare matrices ────────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Preparing predictor matrix")
  }
  X <- as.matrix(data[, setdiff(names(data), group_col), drop = FALSE])
  y <- data[[group_col]]
  num_class <- length(unique(y))

  # ── 4. Train / test split ──────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Splitting data into training and test sets")
  }
  set.seed(seed)
  train_idx <- caret::createDataPartition(y, p = train_fraction, list = FALSE)
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test <- X[-train_idx, ]
  y_test <- y[-train_idx]
  total_samples <- nrow(X)
  train_samples <- nrow(X_train)
  test_samples <- nrow(X_test)

  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgboost::xgb.DMatrix(data = X_test, label = y_test)

  # ── 5. Hyperparameter list ─────────────────────────────────────────────────
  params <- list(
    objective = objective,
    eval_metric = eval_metric,
    num_class = num_class,
    max_depth = max_depth,
    learning_rate = learning_rate,
    min_split_loss = min_split_loss,
    colsample_bytree = colsample_bytree,
    subsample = subsample,
    min_child_weight = min_child_weight
  )

  # ── 6. Train model ─────────────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.15, detail = "Training XGBoost model")
  }
  if (print_results) {
    cat("\nTRAINING XGBOOST MODEL\n")
  }

  xgb_model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    evals = list(train = dtrain, test = dtest),
    early_stopping_rounds = early_stopping_rounds,
    verbose = verbose
  )

  # ── 7. Predictions & confusion matrix ─────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Making predictions on test set")
  }
  preds <- stats::predict(xgb_model, X_test)

  if (is.matrix(preds)) {
    preds_matrix <- preds
    pred_labels <- max.col(preds_matrix) - 1L
  } else if (num_class == 2L && length(preds) == nrow(X_test)) {
    # Binary classification: predict() returned a length-N probability vector
    # for the positive class. Avoid reshaping into 2 columns with recycling.
    preds_matrix <- cbind(1 - preds, preds)
    pred_labels <- ifelse(preds >= 0.5, 1L, 0L)
  } else {
    # Multi-class probabilities (e.g., 'multi:softprob'): reshape flat vector
    preds_matrix <- matrix(preds, ncol = num_class, byrow = TRUE)
    pred_labels <- max.col(preds_matrix) - 1L
  }

  test_levels <- sort(unique(y))
  confusion_mat <- caret::confusionMatrix(
    factor(pred_labels, levels = test_levels),
    factor(y_test, levels = test_levels)
  )
  accuracy_test <- confusion_mat$overall["Accuracy"]

  if (print_results) {
    cat("\nConfusion Matrix on Test Set\n")
    print(confusion_mat)
  }

  # ── 8. Feature importance plot ─────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Building feature importance plot")
  }
  importance_matrix <- xgboost::xgb.importance(
    feature_names = colnames(X_train),
    model = xgb_model
  )
  top_features <- utils::head(importance_matrix, top_n_features)

  if (print_results) {
    cat("\nTop", top_n_features, "Important Features\n")
    print(top_features)
  }

  if (!requireNamespace("Ckmeans.1d.dp", quietly = TRUE)) {
    warning(
      "Install 'Ckmeans.1d.dp' for a clustered importance plot. ",
      "Falling back to a basic bar chart."
    )
    imp_plot <- ggplot2::ggplot(
      top_features,
      ggplot2::aes(x = reorder(Feature, Gain), y = Gain)
    ) +
      ggplot2::geom_bar(stat = "identity", fill = "red2") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Top Features by Gain",
        x = "Features",
        y = "Importance (Gain)"
      ) +
      ggplot2::theme_minimal()
  } else {
    imp_plot <- xgboost::xgb.ggplot.importance(
      importance_matrix = data.table::copy(top_features),
      top_n = top_n_features
    ) +
      ggplot2::geom_bar(stat = "identity", fill = "red2", show.legend = FALSE) +
      ggplot2::ggtitle("Top Features by Gain") +
      ggplot2::ylab("Importance (Gain)") +
      ggplot2::xlab("Features") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        panel.background = ggplot2::element_rect(
          fill = "white",
          colour = "white"
        ),
        plot.background = ggplot2::element_rect(
          fill = "white",
          colour = "white"
        ),
        axis.title = ggplot2::element_text(
          color = "black",
          size = 12,
          face = "bold"
        )
      )
  }
  imp_plot <- apply_font_settings_ggplot(imp_plot, resolved_fonts)

  # ── 9. ROC curve (binary only) ─────────────────────────────────────────────
  roc_plot <- NULL
  auc_value <- NA_real_
  if (plot_roc) {
    if (num_class == 2L) {
      if (!is.null(progress)) {
        progress$inc(0.05, detail = "Building ROC curve")
      }
      xgb_prob <- preds_matrix[, 2]
      if (length(xgb_prob) == length(y_test)) {
        roc_obj <- pROC::roc(y_test, xgb_prob, quiet = TRUE)
        auc_value <- pROC::auc(roc_obj)
        if (print_results) {
          cat("\nAUC:", round(auc_value, 3), "\n")
        }

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
            panel.background = ggplot2::element_rect(
              fill = "white",
              color = NA
            ),
            plot.background = ggplot2::element_rect(fill = "white", color = NA),
            panel.grid.major = ggplot2::element_line(color = "gray90"),
            panel.grid.minor = ggplot2::element_line(color = "gray95")
          )
        roc_plot <- apply_font_settings_ggplot(roc_plot, resolved_fonts)
      } else {
        warning(
          "Length mismatch between predicted probabilities and true labels; skipping ROC/AUC."
        )
      }
    } else {
      warning("ROC curve is only available for binary classification.")
    }
  }

  # ── 10. xgb.cv cross-validation ────────────────────────────────────────────
  cv_results <- NULL
  best_cv_iter <- NULL
  cv_accuracy <- NA_real_
  if (cv) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Running xgb.cv cross-validation")
    }
    xgb_cv <- xgboost::xgb.cv(
      params = params,
      data = dtrain,
      nrounds = nrounds,
      nfold = nfold,
      early_stopping_rounds = early_stopping_rounds,
      verbose = verbose,
      prediction = TRUE
    )
    eval_col_cv <- paste0("test_", eval_metric, "_mean")
    # Prefer xgb.cv's best_iteration when early stopping is enabled
    if (!is.null(early_stopping_rounds) && !is.null(xgb_cv$best_iteration)) {
      best_cv_iter <- xgb_cv$best_iteration
    } else {
      # Some metrics are better when higher (e.g. auc); others when lower (e.g. logloss)
      higher_is_better <- eval_metric %in% c("auc", "aucpr", "map", "ndcg")
      eval_values <- xgb_cv$evaluation_log[[eval_col_cv]]
      best_cv_iter <- if (higher_is_better) {
        which.max(eval_values)
      } else {
        which.min(eval_values)
      }
    }
    cv_results <- xgb_cv

    # CV confusion matrix
    cv_preds <- xgb_cv$pred
    if (is.matrix(cv_preds)) {
      cv_pred_labels <- max.col(cv_preds) - 1L
    } else {
      cv_pred_labels <- ifelse(cv_preds > 0.5, 1L, 0L)
    }
    actual_labels <- xgboost::getinfo(dtrain, "label")
    if (length(cv_pred_labels) == length(actual_labels)) {
      class_levels <- 0L:(num_class - 1L)
      cv_confusion_mat <- caret::confusionMatrix(
        factor(cv_pred_labels, levels = class_levels),
        factor(actual_labels, levels = class_levels)
      )
      if (print_results) {
        cat("\nCross-Validation Confusion Matrix\n")
        print(cv_confusion_mat)
      }
      cv_accuracy <- sum(cv_pred_labels == actual_labels) / length(actual_labels)
      if (print_results) {
        cat("\nCross-Validation Accuracy:", round(cv_accuracy, 3), "\n")
      }
    }
  }

  # ── 11. Build summary text ─────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Formatting results")
  }
  summary_text <- paste(
    utils::capture.output({
      cat("### XGBOOST RESULTS ###\n\n")
      cat("1) Analysis Design:\n")
      cat("Performance provenance: holdout train/test split.\n")
      cat("Samples after filtering:", total_samples, "\n")
      cat("Predictors used:", ncol(X), "\n")
      cat("Requested train fraction:", round(train_fraction, 3), "\n")
      cat(
        "Observed split: train =",
        train_samples,
        ", test =",
        test_samples,
        "\n"
      )
      cat("Class balance (all data):", format_class_balance(y, class_labels), "\n")
      cat("Class balance (train):", format_class_balance(y_train, class_labels), "\n")
      cat("Class balance (test):", format_class_balance(y_test, class_labels), "\n")
      cat("Seed:", seed, "\n")
      cat(
        "Hyperparameters: nrounds =",
        nrounds,
        ", max_depth =",
        max_depth,
        ", learning_rate =",
        learning_rate,
        ", min_split_loss =",
        min_split_loss,
        ", colsample_bytree =",
        colsample_bytree,
        ", subsample =",
        subsample,
        ", min_child_weight =",
        min_child_weight,
        "\n"
      )
      cat("Objective:", objective, "\n")
      cat("Evaluation metric:", eval_metric, "\n")
      cat(
        "Early stopping rounds:",
        if (is.null(early_stopping_rounds)) "None" else early_stopping_rounds,
        "\n"
      )
      cat("Cross-validation enabled:", if (cv) "Yes" else "No", "\n")
      if (cv) {
        cat("Cross-validation folds:", nfold, "\n")
      }
      cat("\n2) Group -> Numeric Label Mapping:\n")
      print(class_mapping)
      cat("\n3) Confusion Matrix on Test Set:\n")
      print(confusion_mat$table)
      cat("\nTest Accuracy:", round(accuracy_test, 3), "\n")
      if (num_class == 2L) {
        cat(
          "\nSensitivity:",
          round(confusion_mat$byClass["Sensitivity"], 3),
          "\n"
        )
        cat(
          "Specificity:",
          round(confusion_mat$byClass["Specificity"], 3),
          "\n"
        )
        if (plot_roc && !is.na(auc_value)) {
          cat("\nAUC:", round(auc_value, 3), "\n")
        }
      }
      cat("\n4) Top", top_n_features, "Important Features:\n")
      print(top_features)
      if (cv && !is.null(cv_results)) {
        cat("\n5) Cross-Validation Results:\n")
        cat("CV provenance: xgb.cv run on the training split only.\n")
        cat("CV Accuracy:", round(cv_accuracy, 3), "\n")
        cat("Best iteration:\n")
        print(cv_results$evaluation_log[best_cv_iter, ])
      }
    }),
    collapse = "\n"
  )

  # ── 12. Output ─────────────────────────────────────────────────────────────
  if (!is.null(output_file)) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Writing output file")
    }
    grDevices::pdf(file = output_file, width = 8, height = 8)
    on.exit(grDevices::dev.off(), add = TRUE)
    cat(summary_text)
    print(imp_plot)
    if (!is.null(roc_plot)) {
      print(roc_plot)
    }
    if (!is.null(progress)) {
      progress$inc(0.02, detail = "Finished writing output file")
      progress$set(message = "Running XGBoost...", value = 1, detail = "Finished")
    }
    return(invisible(NULL))
  }

  if (!is.null(progress)) {
    progress$set(message = "Running XGBoost...", value = 1, detail = "Finished")
  }

  invisible(list(
    summary_text = summary_text,
    model = xgb_model,
    confusion_matrix = confusion_mat$table,
    importance = top_features,
    class_mapping = class_mapping,
    cv_results = cv_results,
    importance_plot = imp_plot,
    roc_plot = roc_plot
  ))
}
