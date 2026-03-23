# ── test-coverage-branches.R ──────────────────────────────────────────────────
# Supplementary branch coverage: tests that exercise paths not covered in the
# primary test files.

# ── cyt_anova ─────────────────────────────────────────────────────────────────

test_that("cyt_anova format_output = TRUE with valid data returns a list with correct structure", {
  result <- suppressWarnings(
    cyt_anova(anova_test_df, format_output = TRUE)
  )

  expect_true(is.list(result))
  expect_true(all(c("out_df", "tukey_list", "message") %in% names(result)))

  out_df <- result$out_df
  expect_s3_class(out_df, "data.frame")
  expect_named(out_df, c("Outcome", "Categorical", "Comparison", "P_adj"))
  expect_gt(nrow(out_df), 0)

  # P-values must be in [0, 1]
  expect_true(all(out_df$P_adj >= 0 & out_df$P_adj <= 1))

  # Outcome values must match numeric column names in the fixture
  expect_true(all(out_df$Outcome %in% c("Outcome1", "Outcome2")))
  # Categorical values must match factor column names in the fixture
  expect_true(all(out_df$Categorical %in% c("Group", "Batch")))

  # message should be empty string for successful comparisons
  expect_identical(result$message, "")
})

test_that("cyt_anova progress object receives increment calls during a normal run", {
  progress <- make_progress_recorder()
  suppressWarnings(
    cyt_anova(anova_test_df, format_output = FALSE, progress = progress)
  )

  expect_gt(length(progress$get_log()$inc), 0)
})

test_that("cyt_anova reports when no valid comparisons can be performed", {
  result <- suppressWarnings(cyt_anova(anova_invalid_df))

  expect_match(result, "No valid comparisons were performed")
})

# ── cyt_ttest ─────────────────────────────────────────────────────────────────

test_that("cyt_ttest supports raw output and empty-result branches", {
  raw_result <- suppressWarnings(
    cyt_ttest(
      ex1_binary_group_treatment[,
        c("Group", "Treatment", "IL.10"),
        drop = FALSE
      ],
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

# ── cyt_bp2 ───────────────────────────────────────────────────────────────────

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

test_that("cyt_bp2 supports zscore scaling through apply_scale", {
  bp2_result <- suppressWarnings(
    cyt_bp2(
      ex1_full[, c("Group", "IL.10", "IL.17F"), drop = FALSE],
      output_file = NULL,
      scale = "zscore"
    )
  )

  expect_true(is.list(bp2_result))
  expect_length(bp2_result, 2)
  expect_true(all(vapply(bp2_result, inherits, logical(1), "ggplot")))
})

# ── cyt_export ────────────────────────────────────────────────────────────────

test_that("cyt_export supports named subsetting, recorded plots, closures, and svg output", {
  with_temp_pdf_device({
    base_plot <- ggplot2::ggplot(ex1_full, ggplot2::aes(Group, IL.10)) +
      ggplot2::geom_boxplot()
    recorded_plot <- record_test_plot({
      graphics::plot(
        x = seq_len(5),
        y = c(1, 3, 2, 5, 4),
        type = "b",
        main = "Recorded Plot Fixture"
      )
    })
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
    cyt_export(
      list(main = base_plot),
      filename = svg_base,
      format = "svg",
      which = "main"
    )

    exported_pngs <- Sys.glob(paste0(png_base, "_*.png"))
    expect_length(exported_pngs, 3)
    expect_true(file.exists(paste0(png_base, "_001.png")))
    expect_true(file.exists(paste0(png_base, "_002.png")))
    expect_true(file.exists(paste0(png_base, "_003.png")))
    expect_true(file.exists(paste0(svg_base, "_001.svg")))
  })

  base_plot <- ggplot2::ggplot(ex1_full, ggplot2::aes(Group, IL.10)) +
    ggplot2::geom_boxplot()

  expect_error(
    cyt_export(
      list(main = base_plot),
      filename = tempfile(),
      which = "missing"
    ),
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

test_that("cyt_export supports numeric which subsetting", {
  with_temp_pdf_device({
    base_plot <- ggplot2::ggplot(ex1_full, ggplot2::aes(Group, IL.10)) +
      ggplot2::geom_boxplot()

    png_base <- tempfile()
    on.exit(unlink(paste0(png_base, "_001.png")), add = TRUE)

    # which = 2 subsets the list to a single-element list before iterating,
    # so seq_along restarts at 1 and the only file produced is _001.png.
    cyt_export(
      list(a = base_plot, b = base_plot, c = base_plot),
      filename = png_base,
      format = "png",
      which = 2L
    )

    expect_true(file.exists(paste0(png_base, "_001.png")))
    expect_false(file.exists(paste0(png_base, "_002.png")))
    expect_false(file.exists(paste0(png_base, "_003.png")))
  })
})

# ── cyt_heatmap ───────────────────────────────────────────────────────────────

test_that("cyt_heatmap covers validation and annotation warning branches", {
  expect_error(
    cyt_heatmap(as.matrix(ex1_group)),
    "`data` must be a data.frame."
  )
  expect_error(
    cyt_heatmap(ex1_group, scale = "bad"),
    "`scale` must be NULL or one of"
  )
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
    cyt_heatmap(
      data.frame(Marker1 = c(NA_real_, 1), Marker2 = c(NA_real_, 2)),
      scale = "zscore"
    ),
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

# ── cyt_univariate_multi (complex design validation) ──────────────────────────

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
    cyt_univariate_multi(
      complex_df,
      design = "two_way",
      primary_cat_var = "Group"
    ),
    "requires both primary_cat_var and secondary_cat_var"
  )
  expect_error(
    cyt_univariate_multi(
      complex_df,
      design = "ancova",
      primary_cat_var = "Group"
    ),
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
  expect_error(
    cyt_univariate_multi(
      complex_df,
      design = "ancova",
      primary_cat_var = "Group",
      covariate_col = "Covariate",
      include_secondary_covariate_interaction = TRUE
    ),
    "include_secondary_covariate_interaction requires secondary_cat_var"
  )
})

# ── cyt_plsr / cyt_splsda (argument validation) ───────────────────────────────

test_that("cyt_plsr and cyt_splsda validate common argument errors", {
  plsr_df <- ex1_binary_group[,
    c("Group", "IL.10", "IL.17F", "GM.CSF"),
    drop = FALSE
  ]

  expect_error(
    cyt_plsr(plsr_df, response_col = "Missing"),
    "Valid numeric `response_col` must be provided"
  )
  expect_error(
    cyt_plsr(
      transform(plsr_df, IL.10 = as.character(IL.10)),
      response_col = "IL.10"
    ),
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
      ex1_binary_group_treatment[,
        c("Group", "Treatment", "IL.10"),
        drop = FALSE
      ],
      group_col = "Group",
      group_col2 = "Group",
      var_num = 2
    ),
    "Need at least 2 numeric predictors"
  )
  expect_error(
    cyt_splsda(
      transform(
        ex1_binary_group_treatment[,
          c("Group", "Treatment", "IL.10", "IL.17F"),
          drop = FALSE
        ],
        Group = factor(rep("A", nrow(ex1_binary_group_treatment)))
      ),
      group_col = "Group",
      group_col2 = "Group",
      var_num = 2
    ),
    "Grouping variable must have at least two levels"
  )
})

# ── cyt_corr ─────────────────────────────────────────────────────────────────

test_that("cyt_corr supports multi-method output without grouping or plots", {
  corr_result <- cyt_corr(
    ex1_full[, c("Group", "IL.10", "IL.17F", "GM.CSF", "IFN.G"), drop = FALSE],
    target = "IL.10",
    methods = c("spearman", "pearson"),
    plot = FALSE
  )

  expect_equal(sort(names(corr_result)), c("pearson", "spearman"))
  expect_null(corr_result$pearson$plot)
  expect_null(corr_result$pearson$groupwise)
  expect_null(corr_result$pearson$diff)
  expect_null(corr_result$spearman$plot)
  expect_null(corr_result$spearman$groupwise)
  expect_null(corr_result$spearman$diff)

  # Even without plots, the main table should have valid data
  expect_gt(nrow(corr_result$pearson$table), 0)
  expect_gt(nrow(corr_result$spearman$table), 0)
  expect_true(all(
    corr_result$pearson$table$p >= 0 & corr_result$pearson$table$p <= 1,
    na.rm = TRUE
  ))
})

test_that("cyt_corr errors for missing targets", {
  expect_error(
    cyt_corr(
      ex1_full[, c("Group", "IL.10"), drop = FALSE],
      target = "Missing"
    ),
    "`target` not found."
  )
})

# ── cyt_dualflashplot ─────────────────────────────────────────────────────────

test_that("cyt_dualflashplot returns a plot and computed statistics", {
  with_temp_pdf_device({
    flash_result <- cyt_dualflashplot(
      ex1_group,
      group_var = "Group",
      group1 = "T2D",
      group2 = "ND",
      ssmd_thresh = 0.2,
      log2fc_thresh = 1,
      top_labels = 5
    )

    expect_true(is.list(flash_result))
    expect_true(inherits(flash_result$plot, "ggplot"))
    expect_gt(nrow(flash_result$stats), 0)
    expect_true(all(
      c(
        "cytokine",
        "ssmd",
        "log2FC",
        "SSMD_Category",
        "Significant"
      ) %in%
        names(flash_result$stats)
    ))
  })
})

test_that("cyt_dualflashplot supports file output mode", {
  with_temp_pdf_device({
    output_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(output_file), add = TRUE)

    flash_result <- cyt_dualflashplot(
      ex1_group,
      group_var = "Group",
      group1 = "T2D",
      group2 = "ND",
      output_file = output_file
    )

    expect_true(file.exists(output_file))
    expect_null(flash_result)
  })
})

test_that("cyt_dualflashplot covers raster output, progress, fonts, and validation", {
  output_file <- tempfile(fileext = ".jpg")
  on.exit(unlink(output_file), add = TRUE)
  progress <- make_progress_recorder()

  flash_result <- cyt_dualflashplot(
    ex1_group,
    group_var = "Group",
    group1 = "T2D",
    group2 = "ND",
    output_file = output_file,
    font_settings = helper_font_settings,
    progress = progress
  )

  expect_true(file.exists(output_file))
  expect_null(flash_result)
  expect_gt(length(progress$get_log()$set), 0)
  expect_gt(length(progress$get_log()$inc), 0)
  expect_equal(
    progress$get_log()$set[[length(progress$get_log()$set)]]$detail,
    "Finished"
  )

  expect_error(
    cyt_dualflashplot(
      as.matrix(ex1_group),
      group_var = "Group",
      group1 = "T2D",
      group2 = "ND"
    ),
    "Input must be a data frame."
  )
})

test_that("cyt_dualflashplot warns and falls back to pdf for unknown extensions", {
  output_file <- tempfile(fileext = ".weird")
  on.exit(unlink(output_file), add = TRUE)

  expect_warning(
    flash_result <- cyt_dualflashplot(
      ex1_group,
      group_var = "Group",
      group1 = "T2D",
      group2 = "ND",
      output_file = output_file
    ),
    "Unknown file extension; defaulting to PDF"
  )

  expect_true(file.exists(output_file))
  expect_null(flash_result)
})

# ── cyt_skku ──────────────────────────────────────────────────────────────────

test_that("cyt_skku returns histograms and summary tables", {
  with_temp_pdf_device({
    skku_result <- cyt_skku(
      ex1_full[, -c(2:3), drop = FALSE],
      output_file = NULL,
      group_cols = "Group"
    )

    expect_true(inherits(skku_result$p_skew, "ggplot"))
    expect_true(inherits(skku_result$p_kurt, "ggplot"))
    expect_gt(nrow(skku_result$raw_results), 0)
    expect_gt(nrow(skku_result$log_results), 0)
  })
})

test_that("cyt_skku supports overall analysis without grouping columns", {
  with_temp_pdf_device({
    skku_result <- cyt_skku(
      ex1_full[, -c(1:3), drop = FALSE],
      group_cols = NULL
    )

    expect_true(inherits(skku_result$p_skew, "ggplot"))
    expect_identical(unique(skku_result$raw_results[, "group"]), "overall")
    expect_identical(unique(skku_result$log_results[, "group"]), "overall")
  })
})

test_that("cyt_skku supports file output mode", {
  with_temp_pdf_device({
    output_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(output_file), add = TRUE)

    skku_result <- cyt_skku(
      ex1_full[, -c(1:3), drop = FALSE],
      output_file = output_file,
      group_cols = NULL
    )

    expect_true(file.exists(output_file))
    expect_null(skku_result)
  })
})

# ── cyt_volc ─────────────────────────────────────────────────────────────────

test_that("cyt_volc returns the current list shape with non-empty statistics", {
  with_temp_pdf_device({
    volc_result <- cyt_volc(
      ex1_group,
      group_col = "Group",
      cond1 = "T2D",
      cond2 = "ND",
      fold_change_thresh = 2,
      top_labels = 5
    )

    expect_true(is.list(volc_result))
    expect_named(volc_result, c("plot", "stats"))
    expect_true(inherits(volc_result$plot, "ggplot"))
    expect_gt(nrow(volc_result$stats), 0)
    expect_true(all(
      c("variable", "fc_log", "p_log", "significant") %in%
        names(volc_result$stats)
    ))
  })
})

test_that("cyt_volc supports all-pairs file output mode", {
  with_temp_pdf_device({
    output_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(output_file), add = TRUE)

    volc_result <- cyt_volc(
      ex1_group,
      group_col = "Group",
      cond1 = NULL,
      cond2 = NULL,
      output_file = output_file
    )

    expect_true(file.exists(output_file))
    expect_null(volc_result)
  })
})

test_that("cyt_volc errors on invalid output file extensions", {
  expect_error(
    cyt_volc(
      ex1_group,
      group_col = "Group",
      output_file = "bad.txt"
    ),
    "must have extension"
  )
})

test_that("cyt_volc covers progress, font settings, raster output, and validation", {
  output_file <- tempfile(fileext = ".png")
  on.exit(unlink(output_file), add = TRUE)
  progress <- make_progress_recorder()

  volc_result <- cyt_volc(
    ex1_group,
    group_col = "Group",
    cond1 = NULL,
    cond2 = NULL,
    output_file = output_file,
    font_settings = helper_font_settings,
    progress = progress
  )

  expect_true(file.exists(output_file))
  expect_null(volc_result)
  expect_gt(length(progress$get_log()$set), 0)
  expect_gt(length(progress$get_log()$inc), 0)
  expect_equal(
    progress$get_log()$set[[length(progress$get_log()$set)]]$detail,
    "Finished"
  )

  expect_error(
    cyt_volc(as.matrix(ex1_group), group_col = "Group"),
    "Input data must be a data frame."
  )
  expect_error(
    cyt_volc(
      data.frame(Group = c("A", "B"), stringsAsFactors = FALSE),
      group_col = "Group"
    ),
    "No numeric columns found in data."
  )
})
