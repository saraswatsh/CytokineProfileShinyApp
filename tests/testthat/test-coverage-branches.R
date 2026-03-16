test_that("cyt_anova returns formatted and raw results with progress updates", {
  progress <- make_progress_recorder()
  binary_anova_df <- subset(ex1_full, Group != "ND", select = c("Group", "IL.10", "IL.17F"))

  formatted <- suppressWarnings(
    cyt_anova(binary_anova_df, format_output = TRUE, progress = progress)
  )
  raw <- suppressWarnings(
    cyt_anova(binary_anova_df, format_output = FALSE)
  )

  expect_true(is.list(formatted))
  expect_true(all(c("out_df", "tukey_list", "message") %in% names(formatted)))
  expect_gt(nrow(formatted$out_df), 0)
  expect_true(is.list(raw))
  expect_gt(length(raw), 0)
  expect_gt(length(progress$get_log()$inc), 0)
})

test_that("cyt_anova reports when no valid comparisons can be performed", {
  result <- suppressWarnings(cyt_anova(anova_invalid_df))

  expect_match(result, "No valid comparisons were performed")
})

test_that("cyt_ttest supports raw output and empty-result branches", {
  raw_result <- suppressWarnings(
    cyt_ttest(
      ex1_binary_group_treatment[, c("Group", "Treatment", "IL.10"), drop = FALSE],
      scale = "none",
      format_output = FALSE
    )
  )
  empty_result <- suppressWarnings(
    cyt_ttest(
      data.frame(Group = factor(c("A", "B")), Outcome = c(1, 2)),
      format_output = FALSE
    )
  )

  expect_true(is.list(raw_result))
  expect_true("test_results" %in% names(raw_result))
  expect_gt(length(raw_result$test_results), 0)
  expect_identical(empty_result, "No valid tests were performed.")
})

test_that("cyt_bp2 supports PDF output, y limits, progress, and validation errors", {
  with_temp_pdf_device({
    output_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(output_file), add = TRUE)
    progress <- make_progress_recorder()

    pdf_result <- suppressWarnings(
      cyt_bp2(
        bp2_test_df,
        output_file = output_file,
        y_lim = c(0, 10),
        scale = "log2",
        progress = progress
      )
    )
    plot_result <- suppressWarnings(
      cyt_bp2(
        bp2_test_df,
        output_file = NULL,
        y_lim = c(0, 10),
        scale = "log2"
      )
    )

    expect_null(pdf_result)
    expect_true(file.exists(output_file))
    expect_true(is.list(plot_result))
    expect_equal(plot_result[[1]]$coordinates$limits$y, c(0, 10))
    expect_gt(length(progress$get_log()$inc), 0)
  })

  expect_error(
    suppressWarnings(cyt_bp2(data.frame(Group = factor(c("A", "B"))))),
    "at least one numeric column"
  )
  expect_error(
    suppressWarnings(cyt_bp2(data.frame(Marker = c(1, 2, 3)))),
    "at least one factor column"
  )
})

test_that("cyt_export supports named subsetting, recorded plots, closures, and svg output", {
  with_temp_pdf_device({
    base_plot <- ggplot2::ggplot(ex1_full, ggplot2::aes(Group, IL.10)) +
      ggplot2::geom_boxplot()
    recorded_plot <- grDevices::recordPlot()
    png_base <- tempfile()
    svg_base <- tempfile()
    on.exit(
      {
        unlink(paste0(png_base, "_001.png"))
        unlink(paste0(png_base, "_002.png"))
        unlink(paste0(png_base, "_003.png"))
        unlink(paste0(svg_base, "_001.svg"))
      },
      add = TRUE
    )

    expect_warning(
      cyt_export(
        list(
          first = base_plot,
          second = recorded_plot,
          third = function() print(base_plot),
          nested = list(base_plot)
        ),
        filename = png_base,
        format = "png"
      ),
      "Skipping nested list element"
    )
    cyt_export(list(main = base_plot), filename = svg_base, format = "svg", which = "main")

    expect_true(file.exists(paste0(png_base, "_001.png")))
    expect_true(file.exists(paste0(png_base, "_002.png")))
    expect_true(file.exists(paste0(png_base, "_003.png")))
    expect_true(file.exists(paste0(svg_base, "_001.svg")))
  })

  base_plot <- ggplot2::ggplot(ex1_full, ggplot2::aes(Group, IL.10)) +
    ggplot2::geom_boxplot()

  expect_error(
    cyt_export(list(main = base_plot), filename = tempfile(), which = "missing"),
    "plot names were not found"
  )
  expect_error(
    cyt_export(list(main = base_plot), filename = tempfile(), which = list(1)),
    "must be NULL, numeric or character vector"
  )
  expect_error(
    cyt_export(list(1), filename = tempfile(), format = "pdf"),
    "Unsupported plot type detected"
  )
  expect_error(
    cyt_export(list(1), filename = tempfile(), format = "png"),
    "Unsupported plot type"
  )
})

test_that("cyt_heatmap covers validation and annotation warning branches", {
  expect_error(cyt_heatmap(as.matrix(ex1_group)), "`data` must be a data.frame.")
  expect_error(cyt_heatmap(ex1_group, scale = "bad"), "`scale` must be NULL or one of")
  expect_error(
    cyt_heatmap(data.frame(Group = factor(c("A", "B")))),
    "No numeric columns found"
  )
  expect_error(
    cyt_heatmap(heatmap_constant_df, scale = "row_zscore"),
    "row_zscore scaling cannot be applied"
  )
  expect_error(
    cyt_heatmap(heatmap_constant_df, scale = "col_zscore"),
    "col_zscore scaling cannot be applied"
  )
  expect_error(
    cyt_heatmap(data.frame(Marker1 = c(NA_real_, 1), Marker2 = c(NA_real_, 2)), scale = "zscore"),
    "contains only missing values"
  )
  expect_error(
    cyt_heatmap(heatmap_bad_df, scale = NULL),
    "numeric matrix contains invalid values before clustering"
  )
  expect_warning(
    cyt_heatmap(ex1_group, annotation_col = 1:5),
    "must be a column in `data` or a vector matching rows or columns"
  )
  expect_warning(
    cyt_heatmap(
      ex1_group,
      annotation_col = rep("A", sum(vapply(ex1_group, is.numeric, logical(1)))),
      annotation_side = "row"
    ),
    "length does not match the chosen side"
  )
})

test_that("cyt_univariate_multi validates complex-design arguments before fitting", {
  testthat::skip_if_not_installed("car")
  testthat::skip_if_not_installed("emmeans")

  complex_df <- anova_test_df
  complex_df$Covariate <- seq_len(nrow(complex_df))
  complex_df$PrimaryNum <- seq_len(nrow(complex_df))

  expect_error(
    cyt_univariate_multi(complex_df, method = "kruskal", design = "two_way"),
    "'kruskal' is only supported when design = 'one_way'"
  )
  expect_error(
    cyt_univariate_multi(complex_df, design = "two_way", primary_cat_var = "Group"),
    "requires both primary_cat_var and secondary_cat_var"
  )
  expect_error(
    cyt_univariate_multi(complex_df, design = "ancova", primary_cat_var = "Group"),
    "requires both primary_cat_var and covariate_col"
  )
  expect_error(
    cyt_univariate_multi(
      complex_df,
      design = "ancova",
      primary_cat_var = "Group",
      secondary_cat_var = "Group",
      covariate_col = "Covariate"
    ),
    "must be distinct"
  )
  expect_error(
    cyt_univariate_multi(
      complex_df,
      design = "ancova",
      primary_cat_var = "PrimaryNum",
      covariate_col = "Covariate"
    ),
    "primary_cat_var must be categorical"
  )
  expect_error(
    cyt_univariate_multi(
      complex_df,
      design = "ancova",
      primary_cat_var = "Group",
      covariate_col = "Batch"
    ),
    "covariate_col must be numeric"
  )
  expect_error(
    cyt_univariate_multi(
      complex_df,
      design = "ancova",
      primary_cat_var = "Group",
      covariate_col = "Covariate",
      include_primary_secondary_interaction = TRUE
    ),
    "requires secondary_cat_var"
  )
})

test_that("cyt_plsr and cyt_splsda validate common argument errors", {
  plsr_df <- ex1_binary_group[, c("Group", "IL.10", "IL.17F", "GM.CSF"), drop = FALSE]

  expect_error(
    cyt_plsr(plsr_df, response_col = "Missing"),
    "Valid numeric `response_col` must be provided"
  )
  expect_error(
    cyt_plsr(transform(plsr_df, IL.10 = as.character(IL.10)), response_col = "IL.10"),
    "`response_col` must be numeric"
  )
  expect_error(
    cyt_plsr(
      plsr_df,
      response_col = "IL.10",
      predictor_cols = "IL.17F",
      group_col = "Group"
    ),
    "Need at least 2 numeric predictors"
  )
  expect_error(
    cyt_plsr(
      plsr_df,
      response_col = "IL.10",
      predictor_cols = c("IL.17F", "GM.CSF"),
      sparse = TRUE
    ),
    "please set `var_num`"
  )
  expect_error(
    cyt_splsda(
      ex1_binary_group_treatment,
      group_col = NULL,
      group_col2 = NULL,
      var_num = 3
    ),
    "At least one grouping column"
  )
  expect_error(
    cyt_splsda(
      ex1_binary_group_treatment,
      group_col = "Missing",
      group_col2 = "Missing",
      var_num = 3
    ),
    "Grouping column 'Missing' not found"
  )
  expect_error(
    cyt_splsda(
      ex1_binary_group_treatment[, c("Group", "Treatment", "IL.10"), drop = FALSE],
      group_col = "Group",
      group_col2 = "Group",
      var_num = 2
    ),
    "Need at least 2 numeric predictors"
  )
  expect_error(
    cyt_splsda(
      transform(
        ex1_binary_group_treatment[, c("Group", "Treatment", "IL.10", "IL.17F"), drop = FALSE],
        Group = factor(rep("A", nrow(ex1_binary_group_treatment)))
      ),
      group_col = "Group",
      group_col2 = "Group",
      var_num = 2
    ),
    "Grouping variable must have at least two levels"
  )
})
