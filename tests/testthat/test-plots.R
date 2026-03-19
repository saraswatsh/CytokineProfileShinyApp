# ── test-plots.R ──────────────────────────────────────────────────────────────

# ── cyt_bp ────────────────────────────────────────────────────────────────────

test_that("cyt_bp supports ungrouped file output and grouped return values", {
  with_temp_pdf_device({
    output_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(output_file), add = TRUE)

    file_result <- cyt_bp(
      ex1_numeric,
      output_file = output_file,
      scale = "log2"
    )
    expect_true(file.exists(output_file))
    expect_true(is.list(file_result))
    expect_length(file_result, ceiling(ncol(ex1_numeric) / 25))

    grouped_result <- cyt_bp(
      ex1_group,
      group_by = "Group",
      scale = "zscore"
    )
    expect_true(is.list(grouped_result))
    expect_length(grouped_result, ncol(ex1_group) - 1)
    expect_true(all(vapply(grouped_result, inherits, logical(1), "ggplot")))
  })
})

test_that("cyt_bp y_lim is applied to ungrouped and grouped plots", {
  with_temp_pdf_device({
    y_limits <- c(0, 5)

    ungrouped <- cyt_bp(
      ex1_full[, c("IL.10", "IL.17F"), drop = FALSE],
      output_file = NULL,
      y_lim = y_limits
    )
    # coord_cartesian is added by the function; verify via the ggplot internals
    p <- ungrouped[[1]]
    expect_true(inherits(p, "ggplot"))
    built <- ggplot2::ggplot_build(p)
    expect_equal(built$layout$coord$limits$y, y_limits)

    grouped <- cyt_bp(
      ex1_group[, c("Group", "IL.10"), drop = FALSE],
      group_by = "Group",
      y_lim = y_limits
    )
    p_grp <- grouped[["IL.10"]]
    built_grp <- ggplot2::ggplot_build(p_grp)
    expect_equal(built_grp$layout$coord$limits$y, y_limits)
  })
})

test_that("cyt_bp with multiple group_by columns creates an interaction group column", {
  with_temp_pdf_device({
    result <- cyt_bp(
      ex1_full[, c("Group", "Treatment", "IL.10", "IL.17F"), drop = FALSE],
      group_by = c("Group", "Treatment"),
      output_file = NULL
    )

    expect_true(is.list(result))
    expect_length(result, 2L) # two numeric columns
    expect_true(all(vapply(result, inherits, logical(1), "ggplot")))
  })
})

test_that("cyt_bp errors when group_by column does not exist in data", {
  expect_error(
    cyt_bp(ex1_group, group_by = "NonExistentColumn"),
    "All group_by columns must exist in data"
  )
})

test_that("cyt_bp errors when there are no numeric columns", {
  char_df <- data.frame(
    A = c("x", "y", "z"),
    B = c("p", "q", "r"),
    stringsAsFactors = FALSE
  )
  expect_error(cyt_bp(char_df), "No numeric columns to plot")
})

# ── cyt_violin ────────────────────────────────────────────────────────────────

test_that("cyt_violin supports ungrouped file output and grouped return values", {
  with_temp_pdf_device({
    output_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(output_file), add = TRUE)

    file_result <- cyt_violin(
      ex1_numeric,
      output_file = output_file,
      scale = "zscore"
    )
    expect_true(file.exists(output_file))
    expect_true(is.list(file_result))
    expect_length(file_result, ceiling(ncol(ex1_numeric) / 25))

    grouped_result <- cyt_violin(
      ex1_group,
      group_by = "Group",
      scale = "log2",
      boxplot_overlay = TRUE
    )
    expect_true(is.list(grouped_result))
    expect_length(grouped_result, ncol(ex1_group) - 1)
    expect_true(all(vapply(grouped_result, inherits, logical(1), "ggplot")))
  })
})

# ── cyt_errbp ─────────────────────────────────────────────────────────────────

test_that("cyt_errbp writes a PDF and returns a ggplot object", {
  with_temp_pdf_device({
    output_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(output_file), add = TRUE)

    plot_result <- cyt_errbp(
      ex1_full[, c("Group", "CCL.20.MIP.3A", "IL.10"), drop = FALSE],
      group_col = "Group",
      output_file = output_file
    )

    expect_true(file.exists(output_file))
    expect_true(inherits(plot_result, "ggplot"))
  })
})

test_that("cyt_errbp supports non-default labels, adjustment, and palette branches", {
  with_temp_pdf_device({
    plot_result <- cyt_errbp(
      ex1_full[, c("Group", "CCL.20.MIP.3A", "IL.10"), drop = FALSE],
      group_col = "Group",
      method = "wilcox",
      class_symbol = TRUE,
      p_adjust_method = "BH",
      fill_palette = c("#1b9e77", "#d95f02", "#7570b3")
    )

    expect_true(inherits(plot_result, "ggplot"))
    expect_gte(length(plot_result$layers), 3)
    expect_equal(plot_result$theme$legend.position, "bottom")
  })
})

test_that("cyt_errbp errors when group_col is missing", {
  expect_error(
    cyt_errbp(
      ex1_full[, c("Group", "IL.10"), drop = FALSE],
      group_col = "Missing"
    ),
    "was not found"
  )
})

# ── cyt_heatmap ───────────────────────────────────────────────────────────────

test_that("cyt_heatmap writes an annotated PDF and returns a heatmap object", {
  with_temp_pdf_device({
    output_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(output_file), add = TRUE)

    heatmap_result <- cyt_heatmap(
      data = ex1_group,
      scale = "log2",
      annotation_col = "Group",
      annotation_side = "auto",
      title = "Example Heatmap",
      filename = output_file
    )

    expect_true(file.exists(output_file))
    expect_false(is.null(heatmap_result))
  })
})

test_that("cyt_heatmap supports row annotations with row z-score scaling", {
  with_temp_pdf_device({
    annotation_values <- factor(ex1_full$Group)
    heatmap_result <- cyt_heatmap(
      ex1_full[, -c(2:3), drop = FALSE],
      scale = "row_zscore",
      annotation_col = annotation_values,
      annotation_side = "row"
    )

    expect_false(is.null(heatmap_result))
    expect_named(heatmap_result, c("tree_row", "tree_col", "kmeans", "gtable"))
    expect_equal(length(heatmap_result$tree_row$order), nrow(ex1_full))
    expect_equal(
      length(heatmap_result$tree_col$order),
      sum(vapply(ex1_full[, -c(2:3), drop = FALSE], is.numeric, logical(1)))
    )
  })
})

test_that("cyt_heatmap supports log10 scaling", {
  with_temp_pdf_device({
    # ex1_group cytokine columns are all positive — log10 is valid
    heatmap_result <- cyt_heatmap(
      ex1_group,
      scale = "log10"
    )

    expect_false(is.null(heatmap_result))
    expect_named(heatmap_result, c("tree_row", "tree_col", "kmeans", "gtable"))
  })
})

test_that("cyt_heatmap supports col_zscore scaling (pheatmap column z-score)", {
  with_temp_pdf_device({
    heatmap_result <- cyt_heatmap(
      ex1_group,
      scale = "col_zscore"
    )

    expect_false(is.null(heatmap_result))
    expect_named(heatmap_result, c("tree_row", "tree_col", "kmeans", "gtable"))
  })
})

test_that("cyt_heatmap supports zscore (row then column) double-standardisation", {
  with_temp_pdf_device({
    # Use a small, well-conditioned subset to keep row-then-col z-score finite
    heatmap_result <- cyt_heatmap(
      ex1_group[,
        c("IL.10", "IL.17F", "GM.CSF", "IFN.G", "IL.13"),
        drop = FALSE
      ],
      scale = "zscore"
    )

    expect_false(is.null(heatmap_result))
    expect_named(heatmap_result, c("tree_row", "tree_col", "kmeans", "gtable"))
  })
})

test_that("cyt_heatmap works with no scaling (scale = NULL)", {
  with_temp_pdf_device({
    heatmap_result <- cyt_heatmap(
      ex1_group[, c("Group", "IL.10", "IL.17F", "GM.CSF"), drop = FALSE],
      scale = NULL,
      annotation_col = "Group"
    )

    expect_false(is.null(heatmap_result))
  })
})

test_that("cyt_heatmap errors on invalid filename extensions", {
  expect_error(
    cyt_heatmap(ex1_group, filename = "bad.txt"),
    "must end in '.pdf' or '.png'"
  )
})

# ── cyt_export ────────────────────────────────────────────────────────────────

test_that("cyt_export writes raster and PDF outputs for ggplot objects", {
  with_temp_pdf_device({
    base_plot <- ggplot2::ggplot(
      ex1_full,
      ggplot2::aes(x = Group, y = IL.10)
    ) +
      ggplot2::geom_boxplot()

    png_base <- tempfile()
    pdf_base <- tempfile()
    on.exit(
      {
        unlink(paste0(png_base, "_001.png"))
        unlink(paste0(pdf_base, ".pdf"))
      },
      add = TRUE
    )

    cyt_export(list(main = base_plot), filename = png_base, format = "png")
    cyt_export(list(main = base_plot), filename = pdf_base, format = "pdf")

    expect_true(file.exists(paste0(png_base, "_001.png")))
    expect_true(file.exists(paste0(pdf_base, ".pdf")))
  })
})

test_that("cyt_export writes tiff output correctly", {
  with_temp_pdf_device({
    base_plot <- ggplot2::ggplot(
      ex1_full,
      ggplot2::aes(x = Group, y = IL.10)
    ) +
      ggplot2::geom_boxplot()

    tiff_base <- tempfile()
    on.exit(unlink(paste0(tiff_base, "_001.tiff")), add = TRUE)

    cyt_export(list(main = base_plot), filename = tiff_base, format = "tiff")

    expect_true(file.exists(paste0(tiff_base, "_001.tiff")))
  })
})

test_that("cyt_export writes jpeg output correctly", {
  with_temp_pdf_device({
    base_plot <- ggplot2::ggplot(
      ex1_full,
      ggplot2::aes(x = Group, y = IL.10)
    ) +
      ggplot2::geom_boxplot()

    jpeg_base <- tempfile()
    on.exit(unlink(paste0(jpeg_base, "_001.jpeg")), add = TRUE)

    cyt_export(list(main = base_plot), filename = jpeg_base, format = "jpeg")

    expect_true(file.exists(paste0(jpeg_base, "_001.jpeg")))
  })
})

test_that("cyt_export skips NULL elements without error", {
  with_temp_pdf_device({
    base_plot <- ggplot2::ggplot(
      ex1_full,
      ggplot2::aes(x = Group, y = IL.10)
    ) +
      ggplot2::geom_boxplot()

    png_base <- tempfile()
    on.exit(
      {
        unlink(paste0(png_base, "_001.png"))
        unlink(paste0(png_base, "_002.png"))
      },
      add = TRUE
    )

    # Second element is NULL — should be skipped; only _001 and _003 produced
    # (indexes are based on list position, so _002 should NOT exist for the NULL)
    cyt_export(
      list(first = base_plot, second = NULL, third = base_plot),
      filename = png_base,
      format = "png"
    )

    expect_true(file.exists(paste0(png_base, "_001.png")))
    expect_false(file.exists(paste0(png_base, "_002.png")))
  })
})

# ── cyt_corr ─────────────────────────────────────────────────────────────────

test_that("cyt_corr returns method-keyed correlation results and plots", {
  with_temp_pdf_device({
    corr_result <- suppress_known_plot_warnings(
      cyt_corr(
        ex1_full[,
          c(
            "Group",
            "Treatment",
            "IL.10",
            "IL.17F",
            "GM.CSF",
            "IFN.G",
            "IL.13"
          ),
          drop = FALSE
        ],
        target = "IL.10",
        methods = "spearman",
        group_var = "Group",
        compare_groups = TRUE,
        plot = TRUE
      )
    )

    expect_s3_class(corr_result, "cyt_corr_dual")
    expect_named(corr_result, "spearman")

    method_result <- corr_result$spearman
    expect_true(all(
      c(
        "table",
        "heat_mat",
        "plot",
        "groupwise",
        "group_heat_mats",
        "group_plots",
        "diff"
      ) %in%
        names(method_result)
    ))
    expect_true(all(
      c(
        "variable",
        "r",
        "p",
        "n",
        "method",
        "p_bonf",
        "p_bh"
      ) %in%
        names(method_result$table)
    ))
    expect_true(is.matrix(method_result$heat_mat))
    expect_true(inherits(method_result$plot, "ggplot"))
    expect_gt(nrow(method_result$table), 0)
    expect_gt(nrow(method_result$groupwise), 0)
    expect_gt(nrow(method_result$diff), 0)
    expect_equal(
      method_result$table$p_bonf,
      round(adjust_p(method_result$table$p, method = "bonferroni"), 4)
    )
    expect_equal(
      method_result$table$p_bh,
      round(adjust_p(method_result$table$p, method = "BH"), 4)
    )
    expect_equal(
      method_result$groupwise$p_bonf,
      adjust_p(method_result$groupwise$p, method = "bonferroni")
    )
    expect_equal(
      method_result$groupwise$p_bh,
      adjust_p(method_result$groupwise$p, method = "BH")
    )
    expect_equal(
      method_result$diff$p_diff_bonf,
      adjust_p(method_result$diff$p_diff, method = "bonferroni")
    )
    expect_equal(
      method_result$diff$p_diff_bh,
      adjust_p(method_result$diff$p_diff, method = "BH")
    )
  })
})

test_that("cyt_corr pearson result table has correct values and correlation bounds", {
  corr_result <- cyt_corr(
    ex1_full[, c("Group", "IL.10", "IL.17F", "GM.CSF", "IFN.G"), drop = FALSE],
    target = "IL.10",
    methods = "pearson",
    plot = FALSE
  )

  table <- corr_result$pearson$table
  expect_gt(nrow(table), 0)
  # Pearson r must lie in [-1, 1]
  expect_true(all(table$r >= -1 & table$r <= 1, na.rm = TRUE))
  # p-values must lie in [0, 1]
  expect_true(all(table$p >= 0 & table$p <= 1, na.rm = TRUE))
  # The target itself should not appear as a row (it is the reference)
  expect_false("IL.10" %in% table$variable)
})
