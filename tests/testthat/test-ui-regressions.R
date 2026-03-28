ui_two_group_test_choices <- getFromNamespace(
  "ui_two_group_test_choices",
  "CytokineProfileShinyApp"
)
ui_two_group_univariate_help_text <- getFromNamespace(
  "ui_two_group_univariate_help_text",
  "CytokineProfileShinyApp"
)
ui_error_bar_test_help_text <- getFromNamespace(
  "ui_error_bar_test_help_text",
  "CytokineProfileShinyApp"
)
ui_imputation_method_input_id <- getFromNamespace(
  "ui_imputation_method_input_id",
  "CytokineProfileShinyApp"
)
ui_imputation_method_choices <- getFromNamespace(
  "ui_imputation_method_choices",
  "CytokineProfileShinyApp"
)
ui_imputation_knn_condition_expr <- getFromNamespace(
  "ui_imputation_knn_condition_expr",
  "CytokineProfileShinyApp"
)

test_that("two-group UI contract exposes Welch choices and help text", {
  expect_equal(
    ui_two_group_test_choices(),
    c(
      "Auto" = "auto",
      "Welch t-test" = "ttest",
      "Wilcoxon" = "wilcox"
    )
  )

  expect_match(
    ui_two_group_univariate_help_text(),
    "Welch's t-test",
    fixed = TRUE
  )
  expect_match(
    ui_error_bar_test_help_text(),
    "Welch's t-test",
    fixed = TRUE
  )
})

test_that("imputation UI contract uses install-safe helper values", {
  expect_identical(
    ui_imputation_method_input_id(),
    "imputation_method"
  )

  expect_equal(
    ui_imputation_method_choices(),
    c(
      "Mean" = "mean",
      "Median" = "median",
      "Mode" = "mode",
      "kNN (sample-wise)" = "knn_sample",
      "kNN (feature-wise)" = "knn_feature"
    )
  )

  expect_equal(
    unname(ui_imputation_method_choices()[4:5]),
    c("knn_sample", "knn_feature")
  )

  expect_identical(
    ui_imputation_knn_condition_expr(),
    "input.imputation_method === 'knn_sample' || input.imputation_method === 'knn_feature'"
  )
})
