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

test_that("cyt_corr returns method-keyed correlation results and plots", {
  with_temp_pdf_device({
    corr_result <- suppress_known_plot_warnings(
      cyt_corr(
        ex1_full[, c(
          "Group",
          "Treatment",
          "IL.10",
          "IL.17F",
          "GM.CSF",
          "IFN.G",
          "IL.13"
        ), drop = FALSE],
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
    expect_true(all(c(
      "table",
      "heat_mat",
      "plot",
      "groupwise",
      "group_heat_mats",
      "group_plots",
      "diff"
    ) %in% names(method_result)))
    expect_true(all(c(
      "variable",
      "r",
      "p",
      "n",
      "method",
      "p_bonf",
      "p_bh"
    ) %in% names(method_result$table)))
    expect_true(is.matrix(method_result$heat_mat))
    expect_true(inherits(method_result$plot, "ggplot"))
    expect_gt(nrow(method_result$table), 0)
    expect_gt(nrow(method_result$groupwise), 0)
    expect_gt(nrow(method_result$diff), 0)
  })
})

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
    expect_true(all(c(
      "cytokine",
      "ssmd",
      "log2FC",
      "SSMD_Category",
      "Significant"
    ) %in% names(flash_result$stats)))
  })
})

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
    expect_true(all(c("variable", "fc_log", "p_log", "significant") %in% names(volc_result$stats)))
  })
})
