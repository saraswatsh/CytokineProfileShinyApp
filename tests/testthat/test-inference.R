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

test_that("cyt_univariate supports raw list output with explicit t-tests", {
  univariate_result <- cyt_univariate(
    ex1_binary_group_treatment[, c("Group", "Treatment", "IL.10", "IL.17F"), drop = FALSE],
    method = "ttest",
    format_output = FALSE
  )

  expect_true(is.list(univariate_result))
  expect_true(length(univariate_result) > 0)
  expect_true(all(vapply(univariate_result, inherits, logical(1), "htest")))
  expect_true(all(nzchar(names(univariate_result))))
})

test_that("cyt_univariate supports adjusted wilcoxon output", {
  raw_tests <- cyt_univariate(
    ex1_binary_group_treatment[, c("Group", "Treatment", "IL.10", "IL.17F"), drop = FALSE],
    method = "wilcox",
    format_output = FALSE
  )
  univariate_result <- cyt_univariate(
    ex1_binary_group_treatment[, c("Group", "Treatment", "IL.10", "IL.17F"), drop = FALSE],
    method = "wilcox",
    format_output = TRUE,
    p_adjust_method = "BH"
  )

  expect_true("P_adj" %in% names(univariate_result))
  expect_true(all(is.finite(univariate_result$P_adj)))
  expected_p_adj <- round(
    adjust_p(
      vapply(raw_tests, function(x) x$p.value, numeric(1)),
      method = "BH"
    ),
    3
  )
  expected_keys <- names(raw_tests)
  observed_keys <- paste(
    univariate_result$Outcome,
    univariate_result$Categorical,
    sep = "_"
  )
  expect_equal(univariate_result$P_adj, unname(expected_p_adj[observed_keys]))
})

test_that("cyt_univariate warns and returns empty output when no valid tests can run", {
  invalid_df <- data.frame(
    Group = factor(c("A", "A", "B", "B")),
    IL.10 = c(1, 1, 1, 1)
  )

  warning_messages <- character()
  univariate_result <- withCallingHandlers(
    cyt_univariate(invalid_df, format_output = TRUE),
    warning = function(w) {
      warning_messages <<- c(warning_messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_s3_class(univariate_result, "data.frame")
  expect_equal(nrow(univariate_result), 0)
  expect_true(any(grepl("No valid tests were performed", warning_messages, fixed = TRUE)))
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

test_that("cyt_univariate_multi adjusts one-way kruskal pairwise p-values with adjust_p", {
  raw_result <- cyt_univariate_multi(
    ex1_full[, c("Group", "IL.10", "IL.17F"), drop = FALSE],
    method = "kruskal",
    design = "one_way",
    cat_vars = "Group",
    cont_vars = c("IL.10", "IL.17F"),
    p_adjust_method = "none",
    format_output = FALSE
  )
  bonf_result <- cyt_univariate_multi(
    ex1_full[, c("Group", "IL.10", "IL.17F"), drop = FALSE],
    method = "kruskal",
    design = "one_way",
    cat_vars = "Group",
    cont_vars = c("IL.10", "IL.17F"),
    p_adjust_method = "bonferroni",
    format_output = FALSE
  )

  expect_named(bonf_result, names(raw_result))
  for (result_name in names(raw_result)) {
    expect_equal(
      unname(bonf_result[[result_name]]),
      unname(round(adjust_p(raw_result[[result_name]], method = "bonferroni"), 4))
    )
  }
})

test_that("cyt_ttest supports apply_scale transformations", {
  ttest_result <- suppressWarnings(
    cyt_ttest(
      ex1_binary_group_treatment[, c("Group", "Treatment", "IL.10", "IL.17F"), drop = FALSE],
      scale = "zscore",
      format_output = TRUE
    )
  )

  expect_true(is.list(ttest_result))
  expect_true("out_df" %in% names(ttest_result))
  expect_gt(nrow(ttest_result$out_df), 0)
  expect_true(all(is.finite(ttest_result$out_df$P_value)))
})
