# test-helpers.R
# Tests for exported utility helpers: adjust_p, apply_scale, and internal
# numeric-validation helpers (summarize_invalid_numeric_columns,
# format_invalid_numeric_summary, safe_zscore_column).

# adjust_p
test_that("adjust_p matches stats::p.adjust for multiple methods", {
  p_values <- c(0.001, 0.02, 0.15, 0.9)

  expect_equal(adjust_p(p_values), stats::p.adjust(p_values, method = "BH"))
  expect_equal(
    adjust_p(p_values, method = "bonferroni"),
    stats::p.adjust(p_values, method = "bonferroni")
  )
})

test_that("adjust_p handles edge cases: single value, all-NA, and empty input", {
  expect_equal(adjust_p(0.05), 0.05)
  expect_equal(adjust_p(numeric(0)), numeric(0))
  # NAs are passed through by p.adjust
  expect_true(is.na(adjust_p(NA_real_)))
})

# apply_scale
test_that("apply_scale preserves structure and transforms requested columns", {
  input_df <- ex1_full[1:5, c("Group", "IL.10", "IL.17F"), drop = FALSE]

  none_scaled <- apply_scale(
    input_df,
    columns = c("IL.10", "IL.17F"),
    scale = "none"
  )
  expect_equal(none_scaled, input_df)

  log_scaled <- apply_scale(
    input_df,
    columns = c("IL.10", "IL.17F"),
    scale = "log2"
  )
  expect_identical(names(log_scaled), names(input_df))
  expect_equal(dim(log_scaled), dim(input_df))
  expect_equal(log_scaled$IL.10, log2(input_df$IL.10))
  expect_equal(log_scaled$IL.17F, log2(input_df$IL.17F))

  z_scaled <- apply_scale(
    input_df,
    columns = c("IL.10", "IL.17F"),
    scale = "zscore"
  )
  expect_equal(
    z_scaled$IL.10,
    as.numeric(scale(input_df$IL.10)),
    tolerance = 1e-10
  )
  expect_equal(
    z_scaled$IL.17F,
    as.numeric(scale(input_df$IL.17F)),
    tolerance = 1e-10
  )

  custom_scaled <- apply_scale(
    input_df,
    columns = "IL.17F",
    scale = "custom",
    custom_fn = function(x) x + 1
  )
  expect_equal(custom_scaled$IL.17F, input_df$IL.17F + 1)
  expect_equal(custom_scaled$IL.10, input_df$IL.10)
})

test_that("apply_scale log10 produces correct values and leaves non-target columns unchanged", {
  input_df <- ex1_full[1:5, c("Group", "IL.10", "IL.17F"), drop = FALSE]

  log10_scaled <- apply_scale(
    input_df,
    columns = c("IL.10", "IL.17F"),
    scale = "log10"
  )

  expect_identical(names(log10_scaled), names(input_df))
  expect_equal(log10_scaled$IL.10, log10(input_df$IL.10))
  expect_equal(log10_scaled$IL.17F, log10(input_df$IL.17F))
  # Non-targeted column must be unmodified
  expect_equal(log10_scaled$Group, input_df$Group)
})

test_that("apply_scale auto-detects all numeric columns when columns = NULL", {
  input_df <- ex1_full[1:5, c("Group", "IL.10", "IL.17F"), drop = FALSE]

  result <- apply_scale(input_df, columns = NULL, scale = "log2")

  # Group is not numeric -- should be unchanged
  expect_equal(result$Group, input_df$Group)
  # Both numeric columns should be log2-transformed
  expect_equal(result$IL.10, log2(input_df$IL.10))
  expect_equal(result$IL.17F, log2(input_df$IL.17F))
})

test_that("apply_scale errors on log transform with non-positive values", {
  bad_df <- data.frame(x = c(1, 0, 3), y = c(2, -1, 4))

  expect_error(
    apply_scale(bad_df, scale = "log2"),
    "finite and greater than 0"
  )
  expect_error(
    apply_scale(bad_df, scale = "log10"),
    "finite and greater than 0"
  )
})

test_that("apply_scale errors on zscore with non-finite values", {
  bad_df <- data.frame(x = c(1, Inf, 3), y = c(2, 4, 6))

  expect_error(
    apply_scale(bad_df, columns = "x", scale = "zscore"),
    "non-finite non-missing values"
  )
})

test_that("apply_scale errors on zscore when a column is entirely missing", {
  bad_df <- data.frame(
    x = c(NA_real_, NA_real_, NA_real_),
    y = c(1, 2, 3)
  )

  expect_error(
    apply_scale(bad_df, columns = c("x", "y"), scale = "zscore"),
    "only missing values"
  )
})

test_that("apply_scale errors when requested columns are absent from data", {
  input_df <- ex1_full[1:5, c("Group", "IL.10"), drop = FALSE]

  expect_error(
    apply_scale(input_df, columns = c("IL.10", "NotAColumn"), scale = "log2"),
    "missing"
  )
})

test_that("apply_scale errors when custom scale is used without a function", {
  input_df <- ex1_full[1:5, c("IL.10", "IL.17F"), drop = FALSE]

  expect_error(
    apply_scale(input_df, scale = "custom", custom_fn = NULL),
    "valid function"
  )
})

test_that("apply_scale returns data unchanged when there are no numeric columns to act on", {
  char_df <- data.frame(
    a = c("x", "y"),
    b = c("p", "q"),
    stringsAsFactors = FALSE
  )

  result <- apply_scale(char_df, scale = "log2")
  expect_equal(result, char_df)
})

# safe_zscore_column
test_that("safe_zscore_column standardises a normal vector correctly", {
  x <- c(2, 4, 6, 8, 10)
  result <- CytokineProfileShinyApp:::safe_zscore_column(x)

  expect_equal(result, as.numeric(scale(x)), tolerance = 1e-12)
  expect_equal(mean(result), 0, tolerance = 1e-12)
})

test_that("safe_zscore_column centres a constant vector instead of dividing by zero", {
  x <- c(5, 5, 5, 5)
  result <- CytokineProfileShinyApp:::safe_zscore_column(x)

  # sd == 0 path: returns x - mean(x) = all zeros
  expect_equal(result, x - mean(x))
  expect_true(all(result == 0))
})

test_that("safe_zscore_column ignores NAs when computing mean and sd", {
  x <- c(2, NA_real_, 4, 6)
  result <- CytokineProfileShinyApp:::safe_zscore_column(x)

  obs <- x[!is.na(x)]
  expected <- (x - mean(obs)) / stats::sd(obs)
  expect_equal(result, expected, tolerance = 1e-12)
  expect_true(is.na(result[2]))
})

test_that("safe_zscore_column errors when all values are missing", {
  expect_error(
    CytokineProfileShinyApp:::safe_zscore_column(c(NA_real_, NA_real_)),
    "only missing values"
  )
})

test_that("safe_zscore_column errors when non-missing values are non-finite", {
  expect_error(
    CytokineProfileShinyApp:::safe_zscore_column(c(1, Inf, 3)),
    "to be finite"
  )
})

# summarize_invalid_numeric_columns
test_that("summarize_invalid_numeric_columns correctly identifies NA, NaN, Inf, and -Inf", {
  result <- CytokineProfileShinyApp:::summarize_invalid_numeric_columns(
    helper_invalid_numeric_df
  )

  expect_s3_class(result, "data.frame")
  # "good" column should NOT appear -- it has no issues
  expect_false("good" %in% result$column)
  # The four bad columns should each be flagged
  expect_true("has_na" %in% result$column)
  expect_true("has_nan" %in% result$column)
  expect_true("has_inf" %in% result$column)
  expect_true("has_ninf" %in% result$column)

  # Check specific issue labels
  expect_equal(
    result[result$column == "has_na", "issues"],
    "NA"
  )
  expect_equal(
    result[result$column == "has_nan", "issues"],
    "NaN"
  )
  expect_equal(
    result[result$column == "has_inf", "issues"],
    "Inf"
  )
  expect_equal(
    result[result$column == "has_ninf", "issues"],
    "-Inf"
  )
})

test_that("summarize_invalid_numeric_columns returns an empty frame for clean data", {
  clean_df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))

  result <- CytokineProfileShinyApp:::summarize_invalid_numeric_columns(
    clean_df
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("column", "issues"))
})

test_that("summarize_invalid_numeric_columns respects a column subset", {
  result <- CytokineProfileShinyApp:::summarize_invalid_numeric_columns(
    helper_invalid_numeric_df,
    columns = "has_na"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$column, "has_na")
})

# format_invalid_numeric_summary
test_that("format_invalid_numeric_summary produces semicolon-delimited label text", {
  issue_df <- data.frame(
    column = c("x", "y"),
    issues = c("NA", "Inf"),
    stringsAsFactors = FALSE
  )

  result <- CytokineProfileShinyApp:::format_invalid_numeric_summary(issue_df)

  expect_type(result, "character")
  expect_match(result, "x [NA]", fixed = TRUE)
  expect_match(result, "y [Inf]", fixed = TRUE)
  expect_match(result, ";", fixed = TRUE)
})

test_that("format_invalid_numeric_summary returns empty string for NULL or zero-row input", {
  expect_equal(
    CytokineProfileShinyApp:::format_invalid_numeric_summary(NULL),
    ""
  )
  expect_equal(
    CytokineProfileShinyApp:::format_invalid_numeric_summary(
      data.frame(column = character(), issues = character())
    ),
    ""
  )
})
