# ── test-inference.R ──────────────────────────────────────────────────────────

# ── cyt_univariate ────────────────────────────────────────────────────────────

test_that("cyt_univariate returns the expected formatted binary comparisons", {
  univariate_result <- cyt_univariate(
    ex1_binary_group_treatment[,
      c("Group", "Treatment", "IL.10", "IL.17F"),
      drop = FALSE
    ],
    scale = "log2",
    method = "auto",
    format_output = TRUE
  )

  expect_named(
    univariate_result,
    c(
      "Outcome",
      "Categorical",
      "Comparison",
      "Test",
      "Estimate",
      "Statistic",
      "P_value"
    )
  )
  expect_equal(nrow(univariate_result), 4)
  expect_true(all(univariate_result$Outcome %in% c("IL.10", "IL.17F")))
  expect_true(all(univariate_result$Categorical %in% c("Group", "Treatment")))
  expect_true(all(is.finite(univariate_result$P_value)))
})

test_that("cyt_univariate supports raw list output with explicit t-tests", {
  univariate_result <- cyt_univariate(
    ex1_binary_group_treatment[,
      c("Group", "Treatment", "IL.10", "IL.17F"),
      drop = FALSE
    ],
    method = "ttest",
    format_output = FALSE
  )

  expect_true(is.list(univariate_result))
  expect_true(length(univariate_result) > 0)
  expect_true(all(vapply(univariate_result, inherits, logical(1), "htest")))
  expect_true(all(nzchar(names(univariate_result))))
})

test_that("cyt_univariate formatted output reports Welch t-test labels", {
  univariate_result <- cyt_univariate(
    ex1_binary_group_treatment[,
      c("Group", "Treatment", "IL.10", "IL.17F"),
      drop = FALSE
    ],
    method = "ttest",
    format_output = TRUE
  )

  expect_true(all(grepl("Welch", univariate_result$Test, fixed = TRUE)))
})

test_that("cyt_univariate supports adjusted wilcoxon output", {
  raw_tests <- cyt_univariate(
    ex1_binary_group_treatment[,
      c("Group", "Treatment", "IL.10", "IL.17F"),
      drop = FALSE
    ],
    method = "wilcox",
    format_output = FALSE
  )
  univariate_result <- cyt_univariate(
    ex1_binary_group_treatment[,
      c("Group", "Treatment", "IL.10", "IL.17F"),
      drop = FALSE
    ],
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
  expect_true(any(grepl(
    "No valid tests were performed",
    warning_messages,
    fixed = TRUE
  )))
})

test_that("cyt_univariate applies scale transformations before testing", {
  # Using log2 vs. none should change the test statistics, confirming that
  # the scale is actually applied to the data before running the test.
  result_log2 <- cyt_univariate(
    ex1_binary_group_treatment[, c("Group", "IL.10", "IL.17F"), drop = FALSE],
    method = "ttest",
    scale = "log2",
    format_output = TRUE
  )
  result_none <- cyt_univariate(
    ex1_binary_group_treatment[, c("Group", "IL.10", "IL.17F"), drop = FALSE],
    method = "ttest",
    scale = "none",
    format_output = TRUE
  )

  expect_true(is.data.frame(result_log2))
  expect_true(is.data.frame(result_none))
  expect_gt(nrow(result_log2), 0)
  # The t-statistics must differ because the data are on different scales
  expect_false(all(result_log2$Statistic == result_none$Statistic))
})

# ── cyt_univariate_multi ──────────────────────────────────────────────────────

test_that("cyt_univariate_multi supports one-way kruskal output", {
  multi_result <- cyt_univariate_multi(
    ex1_full[,
      c("Group", "Treatment", "Time", "IL.10", "IL.17F"),
      drop = FALSE
    ],
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
    ex1_full[,
      c("Group", "Treatment", "Time", "IL.10", "IL.17F"),
      drop = FALSE
    ],
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
      ex1_full[,
        c("Group", "Treatment", "Time", "IL.10", "IL.17F"),
        drop = FALSE
      ],
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
    ex1_full[,
      c("Group", "Treatment", "Time", "IL.10", "IL.17F"),
      drop = FALSE
    ],
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

test_that("cyt_univariate_multi supports secondary covariate ANCOVA follow-up output", {
  testthat::skip_if_not_installed("car")
  testthat::skip_if_not_installed("emmeans")

  ancova_df <- make_ancova_interaction_fixture()
  multi_result <- suppressWarnings(
    cyt_univariate_multi(
      ancova_df[,
        c("Group", "Treatment", "Time", "IL.10", "IL.17F"),
        drop = FALSE
      ],
      design = "ancova",
      primary_cat_var = "Group",
      secondary_cat_var = "Treatment",
      covariate_col = "Time",
      cont_vars = c("IL.10", "IL.17F"),
      include_secondary_covariate_interaction = TRUE,
      format_output = TRUE
    )
  )

  expect_true(all(
    c(
      "Primary_Slope_Homogeneity_P",
      "Primary_Slope_Homogeneity_Met",
      "Secondary_Slope_Homogeneity_P",
      "Secondary_Slope_Homogeneity_Met"
    ) %in%
      names(multi_result$assumptions)
  ))
  expect_true(all(
    multi_result$assumptions$Secondary_Slope_Homogeneity_Met == "Modeled"
  ))
  expect_false(any(is.na(
    multi_result$assumptions$Primary_Slope_Homogeneity_Met
  )))
  expect_true(any(
    multi_result$pairwise$Effect == "Treatment" &
      multi_result$pairwise$Comparison_Type == "Slope comparison"
  ))
  expect_true(any(
    multi_result$pairwise$Effect == "Treatment" &
      multi_result$pairwise$Comparison_Type == "Contrast at covariate value"
  ))
  expect_true(any(
    multi_result$pairwise$Effect == "Group" &
      multi_result$pairwise$Comparison_Type == "Marginal mean contrast"
  ))
  expect_true(all(
    c(
      "Covariate_Reference",
      "Covariate_Reference_Label"
    ) %in%
      names(multi_result$pairwise)
  ))
})

test_that("cyt_univariate_multi supports primary and secondary covariate interactions together", {
  testthat::skip_if_not_installed("car")
  testthat::skip_if_not_installed("emmeans")

  ancova_df <- make_ancova_interaction_fixture()
  multi_result <- suppressWarnings(
    cyt_univariate_multi(
      ancova_df[, c("Group", "Treatment", "Time", "IL.10"), drop = FALSE],
      design = "ancova",
      primary_cat_var = "Group",
      secondary_cat_var = "Treatment",
      covariate_col = "Time",
      cont_vars = "IL.10",
      include_primary_covariate_interaction = TRUE,
      include_secondary_covariate_interaction = TRUE,
      format_output = TRUE
    )
  )

  expect_true(all(
    multi_result$assumptions$Primary_Slope_Homogeneity_Met == "Modeled"
  ))
  expect_true(all(
    multi_result$assumptions$Secondary_Slope_Homogeneity_Met == "Modeled"
  ))
  expect_true(any(
    multi_result$pairwise$Effect == "Group" &
      multi_result$pairwise$Comparison_Type == "Slope comparison"
  ))
  expect_true(any(
    multi_result$pairwise$Effect == "Treatment" &
      multi_result$pairwise$Comparison_Type == "Slope comparison"
  ))
  expect_true(any(multi_result$pairwise$Covariate_Reference_Label == "Mean"))
})

test_that("cyt_univariate_multi reports secondary slope checks when interaction is not modeled", {
  testthat::skip_if_not_installed("car")
  testthat::skip_if_not_installed("emmeans")

  ancova_df <- make_ancova_interaction_fixture()
  multi_result <- cyt_univariate_multi(
    ancova_df[, c("Group", "Treatment", "Time", "IL.10"), drop = FALSE],
    design = "ancova",
    primary_cat_var = "Group",
    secondary_cat_var = "Treatment",
    covariate_col = "Time",
    cont_vars = "IL.10",
    format_output = TRUE
  )

  expect_false(any(
    multi_result$assumptions$Secondary_Slope_Homogeneity_Met == "Modeled"
  ))
  expect_true(all(
    multi_result$pairwise$Comparison_Type == "Marginal mean contrast"
  ))
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
      unname(round(
        adjust_p(raw_result[[result_name]], method = "bonferroni"),
        4
      ))
    )
  }
})

# ── cyt_ttest ─────────────────────────────────────────────────────────────────

test_that("cyt_ttest supports apply_scale transformations", {
  ttest_result <- suppressWarnings(
    cyt_ttest(
      ex1_binary_group_treatment[,
        c("Group", "Treatment", "IL.10", "IL.17F"),
        drop = FALSE
      ],
      scale = "zscore",
      format_output = TRUE
    )
  )

  expect_true(is.list(ttest_result))
  expect_true("out_df" %in% names(ttest_result))
  expect_gt(nrow(ttest_result$out_df), 0)
  expect_true(all(is.finite(ttest_result$out_df$P_value)))
})

test_that("cyt_ttest format_output = TRUE returns a data frame with expected columns", {
  ttest_result <- suppressWarnings(
    cyt_ttest(
      ex1_binary_group_treatment[,
        c("Group", "Treatment", "IL.10", "IL.17F"),
        drop = FALSE
      ],
      scale = "none",
      format_output = TRUE
    )
  )

  expect_true(is.list(ttest_result))
  out_df <- ttest_result$out_df
  expect_s3_class(out_df, "data.frame")
  expected_cols <- c("Outcome", "Categorical", "Comparison", "P_value")
  expect_true(all(expected_cols %in% names(out_df)))
  # P-values must be in [0, 1]
  expect_true(all(out_df$P_value >= 0 & out_df$P_value <= 1, na.rm = TRUE))
  # Outcome and Categorical columns must have recognised values
  expect_true(all(out_df$Outcome %in% c("IL.10", "IL.17F")))
  expect_true(all(out_df$Categorical %in% c("Group", "Treatment")))
})

# ── cyt_anova ─────────────────────────────────────────────────────────────────

test_that("cyt_anova format_output = TRUE returns a valid list with correct structure and p-values", {
  result <- suppressWarnings(
    cyt_anova(anova_test_df, format_output = TRUE)
  )

  # Top-level structure
  expect_true(is.list(result))
  expect_true(all(c("out_df", "tukey_list", "message") %in% names(result)))

  # out_df column names
  out_df <- result$out_df
  expect_s3_class(out_df, "data.frame")
  expect_true(all(
    c("Outcome", "Categorical", "Comparison", "P_adj") %in% names(out_df)
  ))

  # There should be results — anova_test_df has two factor columns with valid levels
  expect_gt(nrow(out_df), 0)

  # Adjusted p-values must lie in [0, 1]
  expect_true(all(out_df$P_adj >= 0 & out_df$P_adj <= 1))

  # The tukey_list must be non-empty
  expect_true(is.list(result$tukey_list))
  expect_gt(length(result$tukey_list), 0)
})

test_that("cyt_anova format_output = FALSE returns a named list of p-value vectors", {
  result <- suppressWarnings(
    cyt_anova(anova_test_df, format_output = FALSE)
  )

  expect_true(is.list(result))
  expect_gt(length(result), 0)
  # Each element should be a named numeric vector of adjusted p-values
  for (pv in result) {
    expect_true(is.numeric(pv))
    expect_true(all(pv >= 0 & pv <= 1))
  }
})

test_that("cyt_anova reports when no valid comparisons can be performed", {
  result <- suppressWarnings(cyt_anova(anova_invalid_df))

  expect_match(result, "No valid comparisons were performed")
})

test_that("cyt_anova skips categorical predictors with only one level", {
  # Factor with a single level should be skipped entirely
  one_level_df <- data.frame(
    Group = factor(rep("A", 6)),
    Outcome = c(1, 2, 3, 4, 5, 6)
  )
  result <- suppressWarnings(cyt_anova(one_level_df))

  expect_match(result, "No valid comparisons were performed")
})

test_that("cyt_anova uses ExampleData1 via its documented example columns", {
  result <- suppressWarnings(
    cyt_anova(ExampleData1[, c(1:2, 5:6)], format_output = TRUE)
  )

  expect_true(is.list(result))
  expect_true("out_df" %in% names(result))
  expect_gt(nrow(result$out_df), 0)
})
