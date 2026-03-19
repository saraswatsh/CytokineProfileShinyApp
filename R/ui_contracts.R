#' Internal UI contract helpers
#'
#' Shared helper values for UI labels, help text, and conditional expressions
#' that need to stay consistent across app code and tests.
#'
#' @name ui_contract_helpers
#' @noRd
ui_two_group_test_choices <- function() {
  c(
    "Auto" = "auto",
    "Welch t-test" = "ttest",
    "Wilcoxon" = "wilcox"
  )
}

ui_two_group_univariate_help_text <- function() {
  paste(
    "Choose how the app compares two groups for each numeric outcome.",
    "Use Auto if you are unsure.",
    "The app will pick between Welch's t-test and the Wilcoxon test based on the data,",
    "while the other options force one method for every outcome."
  )
}

ui_error_bar_test_help_text <- function() {
  paste(
    "Choose which statistical test is used when the plot adds pairwise significance labels.",
    "Use Auto if you are unsure.",
    "The app will choose between Welch's t-test and the Wilcoxon test,",
    "while the other options force one method across all outcomes."
  )
}

ui_imputation_method_input_id <- function() {
  "impute_method"
}

ui_imputation_method_choices <- function() {
  c(
    "Mean" = "mean",
    "Median" = "median",
    "Mode" = "mode",
    "kNN (sample-wise)" = "knn_sample",
    "kNN (feature-wise)" = "knn_feature"
  )
}

ui_imputation_knn_method_values <- function() {
  c("knn_sample", "knn_feature")
}

ui_imputation_knn_condition_expr <- function(
  input_id = ui_imputation_method_input_id()
) {
  knn_values <- ui_imputation_knn_method_values()
  paste(
    sprintf("input.%s == '%s'", input_id, knn_values),
    collapse = " || "
  )
}
