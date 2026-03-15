test_that("cyt_univariate returns the expected formatted binary comparisons", {
  univariate_result <- cyt_univariate(
    ex1_binary_group_treatment[, c("Group", "Treatment", "IL.10", "IL.17F"), drop = FALSE],
    scale = "log2",
    method = "auto",
    format_output = TRUE
  )

  expect_named(
    univariate_result,
    c("Outcome", "Categorical", "Comparison", "Test", "Estimate", "Statistic", "P_value")
  )
  expect_equal(nrow(univariate_result), 4)
  expect_true(all(univariate_result$Outcome %in% c("IL.10", "IL.17F")))
  expect_true(all(univariate_result$Categorical %in% c("Group", "Treatment")))
  expect_true(all(is.finite(univariate_result$P_value)))
})

test_that("cyt_univariate_multi supports one-way kruskal output", {
  multi_result <- cyt_univariate_multi(
    ex1_full[, c("Group", "Treatment", "Time", "IL.10", "IL.17F"), drop = FALSE],
    method = "kruskal",
    design = "one_way",
    cat_vars = c("Group", "Treatment"),
    cont_vars = c("IL.10", "IL.17F"),
    format_output = TRUE
  )

  expect_true(is.list(multi_result))
  expect_gt(nrow(multi_result$results), 0)
  expect_gt(nrow(multi_result$pairwise), 0)
  expect_null(multi_result$assumptions)
})

test_that("cyt_univariate_multi supports one-way anova assumption summaries", {
  multi_result <- cyt_univariate_multi(
    ex1_full[, c("Group", "Treatment", "Time", "IL.10", "IL.17F"), drop = FALSE],
    method = "anova",
    design = "one_way",
    cat_vars = c("Group", "Treatment"),
    cont_vars = c("IL.10", "IL.17F"),
    format_output = TRUE
  )

  expect_true(is.list(multi_result))
  expect_gt(nrow(multi_result$results), 0)
  expect_gt(nrow(multi_result$pairwise), 0)
  expect_gt(nrow(multi_result$assumptions), 0)
})

test_that("cyt_univariate_multi supports two-way ANOVA with interaction", {
  multi_result <- suppressWarnings(
    cyt_univariate_multi(
      ex1_full[, c("Group", "Treatment", "Time", "IL.10", "IL.17F"), drop = FALSE],
      design = "two_way",
      primary_cat_var = "Group",
      secondary_cat_var = "Treatment",
      cont_vars = "IL.10",
      include_primary_secondary_interaction = TRUE,
      format_output = TRUE
    )
  )

  expect_true(is.list(multi_result))
  expect_gt(nrow(multi_result$results), 0)
  expect_gt(nrow(multi_result$pairwise), 0)
  expect_gt(nrow(multi_result$assumptions), 0)
})

test_that("cyt_univariate_multi supports ANCOVA with interaction", {
  multi_result <- cyt_univariate_multi(
    ex1_full[, c("Group", "Treatment", "Time", "IL.10", "IL.17F"), drop = FALSE],
    design = "ancova",
    primary_cat_var = "Group",
    covariate_col = "Time",
    cont_vars = "IL.10",
    include_primary_covariate_interaction = TRUE,
    format_output = TRUE
  )

  expect_true(is.list(multi_result))
  expect_gt(nrow(multi_result$results), 0)
  expect_gt(nrow(multi_result$pairwise), 0)
  expect_gt(nrow(multi_result$assumptions), 0)
})
