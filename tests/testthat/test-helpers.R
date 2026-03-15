test_that("adjust_p matches stats::p.adjust for multiple methods", {
  p_values <- c(0.001, 0.02, 0.15, 0.9)

  expect_equal(adjust_p(p_values), stats::p.adjust(p_values, method = "BH"))
  expect_equal(
    adjust_p(p_values, method = "bonferroni"),
    stats::p.adjust(p_values, method = "bonferroni")
  )
})

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
