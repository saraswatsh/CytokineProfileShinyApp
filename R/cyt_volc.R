#' Volcano Plot.
#'
#' This function subsets the numeric columns from the input data and compares them
#' based on a selected grouping column. It computes the fold changes (ratio of means)
#' and associated p-values (using two-sample t-tests) for each numeric variable between
#' two groups. The results are log2-transformed (for fold change) and -log10-transformed
#' (for p-values) to generate volcano plots.
#'
#' If both cond1 and cond2 are provided (non-empty), only that pair is compared.
#' Otherwise, the function automatically generates all possible pairwise comparisons.
#'
#' @param data A matrix or data frame containing the data to be analyzed.
#' @param group_col A character string specifying the column name used for comparisons.
#' @param cond1 A character string specifying the name of the first condition for comparison.
#'              If empty, all pairwise comparisons will be generated.
#' @param cond2 A character string specifying the name of the second condition for comparison.
#'              If empty, all pairwise comparisons will be generated.
#' @param fold_change_thresh A numeric threshold for the fold change. Default is 2.
#' @param p_value_thresh A numeric threshold for the p-value. Default is 0.05.
#' @param top_labels An integer specifying the number of top variables to label on the plot.
#'                   Default is 10.
#' @param output_file Optional. A file path to save the plot. If NULL (default), the function
#'                    returns a list of ggplot objects.
#' @param font_settings Optional named list of font sizes for supported plot
#'   text elements.
#' @param progress Optional. A Shiny \code{Progress} object for reporting progress updates.
#' @return If output_file is NULL, a list of ggplot objects (one per pair) is returned.
#'         If output_file is provided, the plot(s) are written to that file and the function returns NULL invisibly.
#'
#' @import ggplot2
#' @importFrom dplyr arrange mutate desc row_number
#' @importFrom ggrepel geom_text_repel
#' @export
#' @author Xiaohua Douglas Zhang and Shubh Saraswat
#' @examples
#' # Loading data
#' data_df <- ExampleData1[,-c(2:3)]
#'
#' cyt_volc(data_df, "Group", cond1 = "T2D", cond2 = "ND",
#' fold_change_thresh = 2.0, top_labels= 15)
#'
cyt_volc <- function(
  data,
  group_col,
  cond1 = NULL,
  cond2 = NULL,
  fold_change_thresh = 2,
  p_value_thresh = 0.05,
  top_labels = 10,
  output_file = NULL,
  font_settings = NULL,
  progress = NULL
) {
  resolved_fonts <- normalize_font_settings(
    font_settings = font_settings,
    supported_fields = c(
      "base_size", "plot_title", "x_title", "y_title",
      "x_text", "y_text", "legend_title", "legend_text",
      "annotation_text"
    ),
    activate = !is.null(font_settings)
  )
  label_size <- if (is.null(resolved_fonts)) {
    3
  } else {
    font_settings_ggplot_text_size(
      resolved_fonts$annotation_text,
      default_size = 3
    )
  }

  if (!is.null(progress)) {
    progress$set(message = "Running Volcano Plot...", value = 0)
    progress$inc(0.05, detail = "Checking inputs")
  }
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }

  if (!is.null(cond1) && nzchar(cond1) && !is.null(cond2) && nzchar(cond2)) {
    condition_pairs <- list(c(cond1, cond2))
  } else {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Building comparison list")
    }
    conditions <- unique(data[[group_col]])
    condition_pairs <- utils::combn(conditions, 2, simplify = FALSE)
  }

  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) == 0) {
    stop("No numeric columns found in data.")
  }
  data_numeric <- data[, numeric_cols, drop = FALSE]

  plot_list <- list()
  total_pairs <- length(condition_pairs)
  pair_count <- 0
  pair_inc <- if (total_pairs > 0L) 0.70 / total_pairs else 0

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Calculating volcano statistics")
  }
  for (pair in condition_pairs) {
    pair_count <- pair_count + 1
    current_cond1 <- pair[1]
    current_cond2 <- pair[2]

    data_cond1 <- data[data[[group_col]] == current_cond1, ]
    data_cond2 <- data[data[[group_col]] == current_cond2, ]

    means_cond1 <- colMeans(
      data_cond1[, numeric_cols, drop = FALSE],
      na.rm = TRUE
    )
    means_cond2 <- colMeans(
      data_cond2[, numeric_cols, drop = FALSE],
      na.rm = TRUE
    )

    fold_changes <- mapply(
      function(x, y) {
        if (length(x) < 2 || length(y) < 2) {
          NA
        } else {
          mean(y, na.rm = TRUE) / mean(x, na.rm = TRUE)
        }
      },
      as.list(data_cond1[, numeric_cols, drop = FALSE]),
      as.list(data_cond2[, numeric_cols, drop = FALSE])
    )

    p_values <- mapply(
      function(x, y) {
        if (length(x) < 2 || length(y) < 2) NA else stats::t.test(x, y)$p.value
      },
      as.list(data_cond1[, numeric_cols, drop = FALSE]),
      as.list(data_cond2[, numeric_cols, drop = FALSE])
    )

    fc_log <- log2(fold_changes)
    p_log <- -log10(p_values)

    plot_data <- data.frame(
      variable = names(fold_changes),
      fc_log = fc_log,
      p_log = p_log,
      stringsAsFactors = FALSE
    )

    plot_data <- plot_data |>
      dplyr::mutate(
        significant = (abs(fc_log) >= log2(fold_change_thresh)) &
          (p_log >= -log10(p_value_thresh))
      ) |>
      dplyr::arrange(dplyr::desc(significant), dplyr::desc(p_log)) |>
      dplyr::mutate(
        label = ifelse(dplyr::row_number() <= top_labels, variable, "")
      )

    if (!is.null(progress)) {
      progress$inc(
        pair_inc,
        detail = paste(
          "Building comparison",
          pair_count,
          "of",
          total_pairs,
          ":",
          current_cond1,
          "vs",
          current_cond2
        )
      )
    }
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = fc_log, y = p_log, label = label)
    ) +
      ggplot2::geom_point(ggplot2::aes(color = significant), size = 2) +
      ggplot2::geom_vline(
        xintercept = c(log2(fold_change_thresh), -log2(fold_change_thresh)),
        linetype = "dashed",
        color = "blue"
      ) +
      ggplot2::geom_hline(
        yintercept = -log10(p_value_thresh),
        linetype = "dashed",
        color = "blue"
      ) +
      ggrepel::geom_text_repel(size = label_size, max.overlaps = 50) +
      ggplot2::scale_color_manual(
        values = c("FALSE" = "gray", "TRUE" = "red")
      ) +
      ggplot2::labs(
        title = paste("Volcano Plot:", current_cond1, "vs", current_cond2),
        x = "Log2 Fold Change",
        y = "-Log10 P-Value"
      ) +
      ggplot2::theme_minimal()

    p <- apply_font_settings_ggplot(p, resolved_fonts)

    plot_list[[paste(current_cond1, "vs", current_cond2)]] <- p
  }

  if (!is.null(output_file)) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Writing output file")
    }
    ext <- tools::file_ext(output_file)
    if (tolower(ext) == "pdf") {
      grDevices::pdf(file = output_file, width = 7, height = 5)
      for (p in plot_list) {
        print(p)
      }
      grDevices::dev.off()
      if (!is.null(progress)) {
        progress$inc(0.05, detail = "Finished writing output file")
        progress$set(message = "Running Volcano Plot...", value = 1, detail = "Finished")
      }
      return(invisible(NULL))
    } else if (tolower(ext) %in% c("png", "jpg", "jpeg")) {
      grDevices::png(
        filename = output_file,
        res = 300,
        width = 2100,
        height = 1500,
        units = "px"
      )
      print(plot_list[[1]])
      grDevices::dev.off()
      if (!is.null(progress)) {
        progress$inc(0.05, detail = "Finished writing output file")
        progress$set(message = "Running Volcano Plot...", value = 1, detail = "Finished")
      }
      return(invisible(NULL))
    } else {
      stop("Output file must have extension .pdf, .png, .jpg, or .jpeg")
    }
  } else {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Formatting results")
      progress$set(message = "Running Volcano Plot...", value = 1, detail = "Finished")
    }
    return(list(plot = p, stats = plot_data[, -5]))
  }
}
