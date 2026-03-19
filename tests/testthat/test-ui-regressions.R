test_that("two-group UI contract exposes Welch choices and help text", {
  expect_equal(
    CytokineProfileShinyApp:::ui_two_group_test_choices(),
    c(
      "Auto" = "auto",
      "Welch t-test" = "ttest",
      "Wilcoxon" = "wilcox"
    )
  )

  expect_match(
    CytokineProfileShinyApp:::ui_two_group_univariate_help_text(),
    "Welch's t-test",
    fixed = TRUE
  )
  expect_match(
    CytokineProfileShinyApp:::ui_error_bar_test_help_text(),
    "Welch's t-test",
    fixed = TRUE
  )
})

test_that("imputation UI contract uses install-safe helper values", {
  expect_identical(
    CytokineProfileShinyApp:::ui_imputation_method_input_id(),
    "impute_method"
  )

  expect_equal(
    CytokineProfileShinyApp:::ui_imputation_method_choices(),
    c(
      "Mean" = "mean",
      "Median" = "median",
      "Mode" = "mode",
      "kNN (sample-wise)" = "knn_sample",
      "kNN (feature-wise)" = "knn_feature"
    )
  )

  expect_equal(
    CytokineProfileShinyApp:::ui_imputation_knn_method_values(),
    c("knn_sample", "knn_feature")
  )

  expect_identical(
    CytokineProfileShinyApp:::ui_imputation_knn_condition_expr(),
    "input.impute_method == 'knn_sample' || input.impute_method == 'knn_feature'"
  )
})
