#' Run Random Forest Classification on Cytokine Data.
#'
#' This function trains and evaluates a Random Forest classification model on cytokine data.
#' It includes variable importance visualization, cross-validation for feature selection,
#' and performance metrics such as accuracy, sensitivity, and specificity. For binary classification,
#' the function can also plot the ROC curve and compute the AUC.
#'
#' @param data A data frame containing the cytokine data, with one column as the grouping variable
#' (target variable) and the rest as numerical features.
#' @param group_col A string representing the name of the column with the grouping variable.
#' @param ntree An integer specifying the number of trees to grow in the forest (default is 500).
#' @param mtry An integer specifying the number of variables randomly selected at each split (default is 5).
#' @param train_fraction A numeric value between 0 and 1 representing the proportion of data to use for training (default is 0.7).
#' @param plot_roc A logical value indicating whether to plot the ROC curve and compute the AUC for binary classification (default is FALSE).
#' @param k_folds An integer specifying the number of folds for cross-validation (default is 5).
#' @param step A numeric value specifying the fraction of variables to remove at each step during cross-validation for feature selection (default is 0.5).
#' @param run_rfcv A logical value indicating whether to run Random Forest cross-validation for feature selection (default is TRUE).
#' @param output_file Optional. A file path to save the outputs (plots and summaries) as a PDF file.
#' If NULL (default), the function returns a list of objects for interactive display.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{model}: the trained Random Forest model,
#'   \item \code{train_confusion}: confusion matrix from the training set,
#'   \item \code{accuracy_train}: overall training set accuracy,
#'   \item \code{test_confusion}: confusion matrix from the test set,
#'   \item \code{accuracy_test}: overall test set accuracy,
#'   \item \code{vip_plot}: a ggplot object of variable importance,
#'   \item \code{importance_data}: a data frame with variable importance metrics,
#'   \item \code{roc_plot}: (if applicable) a ggplot object of the ROC curve,
#'   \item \code{rfcv_result}: (if run_rfcv is TRUE) cross-validation results,
#'   \item \code{rfcv_data}: (if run_rfcv is TRUE) a data frame of RF CV results,
#'   \item \code{rfcv_plot}: (if run_rfcv is TRUE) a ggplot object of RF CV error vs. number of variables.
#' }
#' If \code{output_file} is provided, a PDF is generated and the function returns \code{NULL} invisibly.
#'
#' @examples
#' data.df0 <- ExampleData1
#' data.df <- data.frame(data.df0[, 1:3], log2(data.df0[, -c(1:3)]))
#' data.df <- data.df[, -c(2:3)]
#' data.df <- dplyr::filter(data.df, Group != "ND")
#'
#' cyt_rf(data = data.df, group_col = "Group", k_folds = 5, ntree = 1000,
#'   mtry = 4, run_rfcv = TRUE, plot_roc = TRUE)
#'
#' @import ggplot2
#' @importFrom randomForest randomForest rfcv importance
#' @importFrom caret createDataPartition confusionMatrix
#' @importFrom pROC roc auc ggroc
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
  output_file = NULL,
  progress = NULL
) {
  # Start progress
  if (!is.null(progress))
    progress$set(message = "Starting Random Forest Analysis", value = 0)

  # Ensure the grouping variable is a factor
  data[[group_col]] <- as.factor(data[[group_col]])
  if (!is.null(progress))
    progress$inc(0.05, detail = "Converting grouping variable to factor")

  # Split data into training and test sets
  set.seed(123) # for reproducibility
  train_indices <- caret::createDataPartition(
    data[[group_col]],
    p = train_fraction,
    list = FALSE
  )
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  if (!is.null(progress))
    progress$inc(0.1, detail = "Splitting data into training and test sets")

  # Prepare formula for the random forest model
  predictors <- setdiff(colnames(data), group_col)
  formula_rf <- stats::as.formula(paste(
    group_col,
    "~",
    paste(predictors, collapse = "+")
  ))
  if (!is.null(progress)) progress$inc(0.05, detail = "Preparing model formula")

  # Fit the Random Forest model on training data
  rf_model <- randomForest::randomForest(
    formula_rf,
    data = train_data,
    ntree = ntree,
    mtry = mtry,
    importance = TRUE
  )
  if (!is.null(progress))
    progress$inc(0.1, detail = "Fitting Random Forest model")

  # Predict on training set for confusion matrix
  train_pred <- stats::predict(rf_model, newdata = train_data)
  train_conf_mat <- caret::confusionMatrix(train_pred, train_data[[group_col]])
  accuracy_train <- train_conf_mat$overall["Accuracy"]
  if (!is.null(progress))
    progress$inc(0.05, detail = "Evaluating training set performance")

  # Predict on the test set
  test_pred <- stats::predict(rf_model, newdata = test_data)
  test_conf_mat <- caret::confusionMatrix(test_pred, test_data[[group_col]])
  accuracy_test <- test_conf_mat$overall["Accuracy"]
  if (!is.null(progress))
    progress$inc(0.05, detail = "Evaluating test set performance")

  # Build ROC plot if binary classification
  roc_plot <- NULL
  if (plot_roc && length(levels(data[[group_col]])) == 2) {
    rf_prob <- stats::predict(rf_model, newdata = test_data, type = "prob")[, 2]
    roc_obj <- pROC::roc(test_data[[group_col]], rf_prob)
    auc_value <- pROC::auc(roc_obj)
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
    if (!is.null(progress)) progress$inc(0.05, detail = "Building ROC curve")
  }

  # Variable importance
  imp_data <- data.frame(
    Variable = rownames(randomForest::importance(rf_model)),
    Gini = randomForest::importance(rf_model)[, "MeanDecreaseGini"]
  )
  vip_plot <- ggplot2::ggplot(
    imp_data,
    aes(x = stats::reorder(Variable, Gini), y = Gini)
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "red2") +
    ggplot2::coord_flip() +
    ggplot2::ggtitle("Variable Importance Plot (Mean Decrease in Gini)") +
    ggplot2::xlab("Features") +
    ggplot2::ylab("Importance (Gini Index)") +
    ggplot2::theme_minimal()
  if (!is.null(progress))
    progress$inc(0.05, detail = "Generating variable importance plot")

  # Run RFCV if requested
  rfcv_result <- NULL
  rfcv_data <- NULL
  rfcv_plot <- NULL
  if (run_rfcv) {
    x_train <- train_data[, predictors, drop = FALSE]
    y_train <- train_data[[group_col]]
    rfcv_result <- randomForest::rfcv(
      x_train,
      y_train,
      cv.fold = k_folds,
      step = step
    )
    rfcv_data <- data.frame(
      Variables = rfcv_result$n.var,
      Error = rfcv_result$error.cv
    )
    rfcv_plot <- ggplot2::ggplot(rfcv_data, aes(x = Variables, y = Error)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_point(color = "blue") +
      ggplot2::ggtitle("Cross-Validation Error vs. Number of Variables") +
      ggplot2::xlab("Number of Variables") +
      ggplot2::ylab("Cross-Validation Error") +
      ggplot2::theme_minimal()
    if (!is.null(progress))
      progress$inc(0.05, detail = "Performing Random Forest cross-validation")
  }

  # Return interactive results or write PDF if output_file is provided
  if (is.null(output_file)) {
    # Build summary text
    summary_text <- utils::capture.output({
      cat("### RANDOM FOREST RESULTS ###\n\n")

      cat("--- Training Set ---\n")
      cat("Confusion Matrix:\n")
      print(train_conf_mat$table)
      cat("\nAccuracy:", round(accuracy_train, 3), "\n")

      if (nlevels(data[[group_col]]) == 2) {
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

      if (plot_roc) {
        cat("\nAUC:", round(auc_value, 3), "\n")
      }
      if (nlevels(data[[group_col]]) == 2) {
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
    })
    summary_text <- paste(summary_text, collapse = "\n")

    return(list(
      summary_text = summary_text,
      vip_plot = vip_plot,
      roc_plot = roc_plot,
      rfcv_plot = rfcv_plot
    ))
  } else {
    # PDF mode: print outputs to PDF
    grDevices::pdf(file = output_file, width = 8, height = 8)
    cat("Training Confusion Matrix:\n")
    print(train_conf_mat$table)
    cat("\nTest Confusion Matrix:\n")
    print(test_conf_mat$table)

    if (!is.null(roc_plot)) print(roc_plot)
    print(vip_plot)
    if (!is.null(rfcv_plot)) print(rfcv_plot)
    grDevices::dev.off()
    return(invisible(NULL))
  }
}
