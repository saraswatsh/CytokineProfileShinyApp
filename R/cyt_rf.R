#' Run Random Forest Classification on Cytokine Data
#'
#' @description
#' This function trains and evaluates a Random Forest classification model on
#' cytokine data.  It includes variable importance visualization,
#' cross-validation for feature selection, and performance metrics such as
#' accuracy, sensitivity, and specificity.  For binary classification the
#' function can also plot the ROC curve and compute the AUC.
#'
#' @param data A data frame containing the cytokine measurements.  One column
#'   should correspond to the grouping variable (the outcome) and the
#'   remaining columns should be numeric predictors.
#' @param group_col A string naming the column in \code{data} that contains
#'   the grouping variable.
#' @param ntree Integer specifying the number of trees to grow.  Default is
#'   \code{500}.
#' @param mtry Integer specifying the number of variables randomly sampled at
#'   each split.  Default is \code{5}.
#' @param train_fraction Numeric between 0 and 1 giving the proportion of
#'   data used for training.  Default is \code{0.7}.
#' @param plot_roc Logical.  If \code{TRUE} and the problem is binary, an ROC
#'   curve and AUC are computed and returned.  Default is \code{FALSE}.
#' @param k_folds Integer specifying the number of folds for \code{rfcv} when
#'   \code{run_rfcv = TRUE}.  Default is \code{5}.
#' @param step Numeric specifying the fraction of variables removed at each
#'   step during \code{rfcv}.  Default is \code{0.5}.
#' @param run_rfcv Logical indicating whether to run Random Forest
#'   cross-validation for feature selection.  Default is \code{TRUE}.
#' @param verbose Logical.  When \code{TRUE}, training and test performance
#'   metrics, confusion matrices, and cross-validation details are printed.
#'   Default is \code{FALSE}.
#' @param seed Optional integer seed for reproducibility.  Default is
#'   \code{123}.
#' @param cv Logical indicating whether to perform a separate k-fold
#'   classification cross-validation using \code{caret}.  Default is
#'   \code{FALSE}.
#' @param cv_folds Integer specifying the number of folds for classification
#'   cross-validation when \code{cv = TRUE}.  Default is \code{5}.
#' @param scale Character string specifying a transformation to apply to
#'   numeric predictor columns prior to model fitting.  One of \code{"none"}
#'   (default), \code{"log2"}, \code{"log10"}, \code{"zscore"}, or
#'   \code{"custom"}.
#' @param custom_fn A custom transformation function used when
#'   \code{scale = "custom"}.
#' @param output_file Optional.  A file path to save outputs as a PDF.  If
#'   \code{NULL} (default), a named list is returned for interactive display.
#' @param font_settings Optional named list of font sizes for supported plot
#'   text elements.
#' @param progress Optional.  A Shiny \code{Progress} object for reporting
#'   progress updates.
#'
#' @return When \code{output_file} is \code{NULL}, an invisible named list
#'   with elements \code{summary_text}, \code{vip_plot}, \code{roc_plot},
#'   \code{rfcv_plot}, \code{rfcv_data}, \code{importance_data}, and
#'   \code{cv_results}.  When \code{output_file} is provided, a PDF is
#'   written and the function returns \code{NULL} invisibly.
#' @author Shubh Saraswat and Xiaohua Douglas Zhang
#' @examples
#' data.df0 <- ExampleData1
#' data.df  <- data.frame(data.df0[, 1:3], log2(data.df0[, -c(1:3)]))
#' data.df  <- data.df[, -c(2:3)]
#' data.df  <- dplyr::filter(data.df, Group != "ND")
#' cyt_rf(data = data.df, group_col = "Group", k_folds = 5, ntree = 1000,
#'        mtry = 4, run_rfcv = TRUE, plot_roc = TRUE, verbose = FALSE)
#'
#' @import ggplot2
#' @importFrom randomForest randomForest rfcv importance
#' @importFrom caret createDataPartition confusionMatrix trainControl train
#' @importFrom pROC roc auc ggroc
#' @importFrom utils capture.output
#' @export
cyt_rf <- function(
  data,
  group_col,
  ntree = 500,
  mtry = 5,
  train_fraction = 0.7,
  plot_roc = FALSE,
  k_folds = 5,
  step = 0.5,
  run_rfcv = TRUE,
  verbose = FALSE,
  seed = 123,
  cv = FALSE,
  cv_folds = 5,
  scale = c("none", "log2", "log10", "zscore", "custom"),
  custom_fn = NULL,
  output_file = NULL,
  font_settings = NULL,
  progress = NULL
) {
  format_class_balance <- function(values) {
    counts <- table(droplevels(as.factor(values)))
    if (!length(counts)) {
      return("NA")
    }

    props <- round(100 * counts / sum(counts), 1)
    paste(
      paste0(names(counts), "=", as.integer(counts), " (", props, "%)"),
      collapse = ", "
    )
  }

  # 0. Initialize
  if (!is.null(progress)) {
    progress$set(message = "Running Random Forest...", value = 0)
  }

  names(data) <- make.names(names(data), unique = TRUE)
  scale <- match.arg(scale)
  resolved_fonts <- normalize_font_settings(
    font_settings = font_settings,
    supported_fields = c(
      "base_size", "plot_title", "x_title", "y_title",
      "x_text", "y_text", "legend_title", "legend_text"
    ),
    activate = !is.null(font_settings)
  )

  if (!group_col %in% colnames(data)) {
    stop(sprintf("Column '%s' not found in data.", group_col))
  }

  # 1. Scaling via shared utility
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

  # 2. Coerce grouping variable
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Preparing outcome groups")
  }
  data[[group_col]] <- as.factor(data[[group_col]])
  if (length(levels(data[[group_col]])) < 2L) {
    stop("Grouping variable must have at least two levels.")
  }

  # 3. Train / test split
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Splitting data into training and test sets")
  }
  set.seed(seed)
  train_idx <- caret::createDataPartition(
    data[[group_col]],
    p = train_fraction,
    list = FALSE
  )
  train_data <- data[train_idx, , drop = FALSE]
  test_data <- data[-train_idx, , drop = FALSE]
  total_samples <- nrow(data)
  train_samples <- nrow(train_data)
  test_samples <- nrow(test_data)

  # 4. Fit Random Forest
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Preparing model formula")
  }
  predictors <- setdiff(colnames(data), group_col)
  rf_formula <- stats::as.formula(
    paste(group_col, "~", paste(predictors, collapse = "+"))
  )

  if (!is.null(progress)) {
    progress$inc(0.15, detail = "Fitting Random Forest model")
  }
  rf_model <- randomForest::randomForest(
    rf_formula,
    data = train_data,
    ntree = ntree,
    mtry = mtry,
    importance = TRUE,
    do.trace = FALSE
  )

  if (verbose) {
    cat("\n### RANDOM FOREST RESULTS ON TRAINING SET ###\n")
    cat(paste(utils::capture.output(rf_model), collapse = "\n"), "\n")
  }

  # 5. Training set performance
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Evaluating training set performance")
  }
  train_pred <- stats::predict(rf_model, newdata = train_data)
  train_conf_mat <- caret::confusionMatrix(train_pred, train_data[[group_col]])
  accuracy_train <- train_conf_mat$overall["Accuracy"]

  if (verbose) {
    cat("\nAccuracy on training set:", round(accuracy_train, 3), "\n")
    train_conf <- train_conf_mat$table
    for (i in seq_len(nrow(train_conf))) {
      tp <- train_conf[i, i]
      fn <- sum(train_conf[i, ]) - tp
      fp <- sum(train_conf[, i]) - tp
      tn <- sum(train_conf) - (tp + fn + fp)
      sens <- tp / (tp + fn)
      spec <- tn / (tn + fp)
      cat(sprintf(
        "\nTraining - Class '%s': Sensitivity=%.3f  Specificity=%.3f\n",
        rownames(train_conf)[i],
        sens,
        spec
      ))
    }
  }

  # 6. Test set performance
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Evaluating test set performance")
  }
  test_pred <- stats::predict(rf_model, newdata = test_data)
  test_conf_mat <- caret::confusionMatrix(test_pred, test_data[[group_col]])
  accuracy_test <- test_conf_mat$overall["Accuracy"]

  if (verbose) {
    cat("\n### PREDICTIONS ON TEST SET ###\n")
    cat(
      paste(utils::capture.output(print(test_conf_mat$table)), collapse = "\n"),
      "\n"
    )
    cat("\nAccuracy on test set:", round(accuracy_test, 3), "\n")
  }

  # 7. ROC curve (binary only)
  roc_plot <- NULL
  auc_value <- NA_real_
  if (plot_roc && length(levels(data[[group_col]])) == 2L) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Building ROC curve")
    }
    rf_prob <- stats::predict(rf_model, newdata = test_data, type = "prob")[, 2]
    roc_obj <- pROC::roc(test_data[[group_col]], rf_prob, quiet = TRUE)
    auc_value <- pROC::auc(roc_obj)
    if (verbose) {
      cat("\nAUC:", round(auc_value, 3), "\n")
    }

    roc_plot <- pROC::ggroc(
      roc_obj,
      color = "blue",
      linewidth = 1.5,
      legacy.axes = TRUE
    ) +
      ggplot2::geom_abline(linetype = "dashed", color = "red", linewidth = 1) +
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
      ggplot2::theme_minimal()
    roc_plot <- apply_font_settings_ggplot(roc_plot, resolved_fonts)
  }

  # 8. Variable importance
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Building variable importance plot")
  }
  imp_data <- data.frame(
    Variable = rownames(randomForest::importance(rf_model)),
    Gini = randomForest::importance(rf_model)[, "MeanDecreaseGini"]
  )
  vip_plot <- ggplot2::ggplot(
    imp_data,
    ggplot2::aes(x = stats::reorder(Variable, Gini), y = Gini)
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "red2") +
    ggplot2::coord_flip() +
    ggplot2::ggtitle("Variable Importance Plot (Mean Decrease in Gini)") +
    ggplot2::xlab("Features") +
    ggplot2::ylab("Importance (Gini Index)") +
    ggplot2::theme_minimal()
  vip_plot <- apply_font_settings_ggplot(vip_plot, resolved_fonts)

  # 9. RFCV
  rfcv_result <- NULL
  rfcv_data <- NULL
  rfcv_plot <- NULL
  if (run_rfcv) {
    if (!is.null(progress)) {
      progress$inc(
        0.05,
        detail = "Running Random Forest cross-validation (rfcv)"
      )
    }
    x_train <- train_data[, predictors, drop = FALSE]
    y_train <- train_data[[group_col]]
    rfcv_result <- randomForest::rfcv(
      x_train,
      y_train,
      cv.fold = k_folds,
      step = step,
      do.trace = FALSE
    )
    rfcv_data <- data.frame(
      Variables = rfcv_result$n.var,
      Error = rfcv_result$error.cv
    )
    rfcv_plot <- ggplot2::ggplot(
      rfcv_data,
      ggplot2::aes(x = Variables, y = Error)
    ) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_point(color = "blue") +
      ggplot2::ggtitle("Cross-Validation Error vs. Number of Variables") +
      ggplot2::xlab("Number of Variables") +
      ggplot2::ylab("Cross-Validation Error") +
      ggplot2::theme_minimal()
    rfcv_plot <- apply_font_settings_ggplot(rfcv_plot, resolved_fonts)
    if (verbose) cat("Random Forest CV completed for feature selection.\n")
  }

  # 10. Optional caret k-fold CV
  cv_results <- NULL
  if (cv) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Running caret k-fold cross-validation")
    }
    tr_ctrl <- caret::trainControl(
      method = "cv",
      number = cv_folds,
      classProbs = TRUE
    )
    cv_results <- caret::train(
      x = data[, predictors, drop = FALSE],
      y = data[[group_col]],
      method = "rf",
      trControl = tr_ctrl,
      tuneGrid = data.frame(mtry = mtry),
      ntree = ntree
    )
    if (verbose) {
      cat(
        "Cross-validation Accuracy:",
        round(max(cv_results$results$Accuracy), 3),
        "\n"
      )
    }
  }

  # 11. Build summary text
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Formatting results")
  }
  summary_text <- paste(
    utils::capture.output({
      cat("### RANDOM FOREST RESULTS ###\n\n")
      cat("--- Analysis Design ---\n")
      cat("Performance provenance: holdout train/test split.\n")
      cat("Samples after filtering:", total_samples, "\n")
      cat("Predictors used:", length(predictors), "\n")
      cat("Requested train fraction:", round(train_fraction, 3), "\n")
      cat(
        "Observed split: train =",
        train_samples,
        ", test =",
        test_samples,
        "\n"
      )
      cat("Class balance (all data):", format_class_balance(data[[group_col]]), "\n")
      cat("Class balance (train):", format_class_balance(train_data[[group_col]]), "\n")
      cat("Class balance (test):", format_class_balance(test_data[[group_col]]), "\n")
      cat("Seed:", seed, "\n")
      cat("Hyperparameters: ntree =", ntree, ", mtry =", mtry, "\n")
      cat("RFCV enabled:", if (run_rfcv) "Yes" else "No", "\n")
      if (run_rfcv) {
        cat("RFCV folds:", k_folds, "\n")
        cat("RFCV step:", step, "\n")
      }
      cat("Caret CV enabled:", if (cv) "Yes" else "No", "\n")
      if (cv) {
        cat("Caret CV folds:", cv_folds, "\n")
      }
      cat("\n")
      cat("--- Training Set ---\n")
      cat("Confusion Matrix:\n")
      print(train_conf_mat$table)
      cat("\nAccuracy:", round(accuracy_train, 3), "\n")
      if (nlevels(data[[group_col]]) == 2L) {
        cat(
          "\nSensitivity (train):",
          round(train_conf_mat$byClass["Sensitivity"], 3),
          "\n"
        )
        cat(
          "Specificity (train):",
          round(train_conf_mat$byClass["Specificity"], 3),
          "\n"
        )
      } else {
        cat("\nPer-Class Sensitivity (train):\n")
        print(round(train_conf_mat$byClass[, "Sensitivity"], 3))
        cat("\nPer-Class Specificity (train):\n")
        print(round(train_conf_mat$byClass[, "Specificity"], 3))
      }
      cat("\n--- Test Set ---\n")
      cat("Confusion Matrix:\n")
      print(test_conf_mat$table)
      cat("\nAccuracy:", round(accuracy_test, 3), "\n")
      if (plot_roc && !is.na(auc_value)) {
        cat("\nAUC:", round(auc_value, 3), "\n")
      }
      if (nlevels(data[[group_col]]) == 2L) {
        cat(
          "\nSensitivity (test):",
          round(test_conf_mat$byClass["Sensitivity"], 3),
          "\n"
        )
        cat(
          "Specificity (test):",
          round(test_conf_mat$byClass["Specificity"], 3),
          "\n"
        )
      } else {
        cat("\nPer-Class Sensitivity (test):\n")
        print(round(test_conf_mat$byClass[, "Sensitivity"], 3))
        cat("\nPer-Class Specificity (test):\n")
        print(round(test_conf_mat$byClass[, "Specificity"], 3))
      }
      if (cv && !is.null(cv_results)) {
        cat("\n--- Caret k-Fold CV ---\n")
        cat("CV provenance: caret k-fold cross-validation across all filtered samples.\n")
        cat("CV Accuracy:", round(max(cv_results$results$Accuracy), 3), "\n")
      }
    }),
    collapse = "\n"
  )

  # 12. Output
  if (!is.null(output_file)) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Writing output file")
    }
    grDevices::pdf(file = output_file, width = 8, height = 8)
    on.exit(grDevices::dev.off(), add = TRUE)
    cat(summary_text)
    print(vip_plot)
    if (!is.null(roc_plot)) {
      print(roc_plot)
    }
    if (!is.null(rfcv_plot)) {
      print(rfcv_plot)
    }
    if (!is.null(progress)) {
      progress$inc(0.02, detail = "Finished writing output file")
      progress$set(message = "Running Random Forest...", value = 1, detail = "Finished")
    }
    return(invisible(NULL))
  }

  if (!is.null(progress)) {
    progress$set(message = "Running Random Forest...", value = 1, detail = "Finished")
  }

  invisible(list(
    summary_text = summary_text,
    vip_plot = vip_plot,
    roc_plot = roc_plot,
    rfcv_plot = rfcv_plot,
    rfcv_data = rfcv_data,
    importance_data = imp_data,
    cv_results = cv_results
  ))
}
