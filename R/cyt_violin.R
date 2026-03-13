#' Violin Plots for Continuous Variables with Optional Grouping
#'
#' @description
#' \code{cyt_violin} produces violin plots for each numeric variable in
#' \code{data}, optionally grouped by one or more categorical variables.
#' When grouping is not specified, the function behaves similarly to
#' \code{cyt_bp} but uses violins instead of boxplots and supports
#' pagination via the \code{bin_size} argument.  When grouping is provided,
#' a separate violin is drawn for each level (or interaction of levels) of
#' the grouping variables.  Users may optionally overlay boxplots within
#' each violin to visualize the median and interquartile range.
#'
#' @param data A matrix or data frame containing numeric and categorical
#'   variables.
#' @param output_file Optional string specifying the name of the file to be
#'   created.  When \code{NULL} (default), plots are drawn on the current
#'   graphics device.  Ensure the file extension matches the desired format
#'   (e.g., \code{".pdf"}, \code{".png"}, \code{".tiff"}).
#' @param group_by Optional character vector specifying one or more columns
#'   to use for grouping.  If \code{NULL} (default) no grouping is applied.
#' @param bin_size Integer.  Maximum number of violins per page when grouping
#'   is not used.  Default is \code{25}, mirroring \code{cyt_bp}.
#' @param y_lim Optional numeric vector giving y-axis limits for the plots.
#'   Applies to all plots.
#' @param scale Character specifying a transformation for numeric variables.
#'   One of \code{"none"}, \code{"log2"}, \code{"log10"}, \code{"zscore"},
#'   or \code{"custom"}.  When \code{"custom"}, supply a function via
#'   \code{custom_fn}.
#' @param custom_fn A user-supplied function to transform numeric columns
#'   when \code{scale = "custom"}.
#' @param boxplot_overlay Logical.  When \code{TRUE}, a narrow boxplot is
#'   drawn inside each violin to summarize the median and quartiles.
#'   Default is \code{FALSE}.
#' @param progress Optional.  A Shiny \code{Progress} object for reporting
#'   progress updates.
#'
#' @return Invisibly returns a list of \code{ggplot} objects.  When
#'   \code{output_file} is provided, plots are written to the specified file.
#' @author Shubh Saraswat
#' @examples
#' # Violin plots without grouping
#' cyt_violin(ExampleData1[, -c(1:3)], output_file = NULL, scale = "zscore")
#' # Violin plots grouped by Group with boxplot overlay
#' cyt_violin(ExampleData1[, -c(3, 5:28)], group_by = "Group",
#'            boxplot_overlay = TRUE, scale = "log2")
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
cyt_violin <- function(
  data,
  output_file = NULL,
  group_by = NULL,
  bin_size = 25,
  y_lim = NULL,
  scale = c("none", "log2", "log10", "zscore", "custom"),
  custom_fn = NULL,
  boxplot_overlay = FALSE,
  progress = NULL
) {
  # ── 0. Initialize ──────────────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$set(message = "Violin plots: initializing...", value = 0)
  }

  names(data) <- make.names(names(data), unique = TRUE)
  scale <- match.arg(scale)
  df <- as.data.frame(data)

  # ── 1. Validate group_by ───────────────────────────────────────────────────
  if (!is.null(group_by)) {
    if (!all(group_by %in% names(df))) {
      stop("All group_by columns must exist in data.")
    }
  }

  # ── 2. Identify numeric columns ────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Identifying numeric columns")
  }

  numeric_cols <- names(df)[
    sapply(df, is.numeric) & !(names(df) %in% group_by)
  ]
  if (length(numeric_cols) == 0L) {
    stop("No numeric columns to plot.")
  }

  # ── 3. Scaling via shared utility ──────────────────────────────────────────
  if (scale != "none") {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = paste("Applying", scale, "transformation"))
    }
    df <- apply_scale(
      data = df,
      columns = numeric_cols,
      scale = scale,
      custom_fn = custom_fn
    )
  }

  # ── 4. Resolve grouping column ─────────────────────────────────────────────
  if (!is.null(group_by)) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Converting grouping columns to factors")
    }

    for (g in group_by) {
      if (is.character(df[[g]])) df[[g]] <- factor(df[[g]])
    }

    if (length(group_by) > 1L) {
      df$._group <- interaction(df[, group_by], sep = ":")
      grp_col <- "._group"
    } else {
      grp_col <- group_by
    }
  } else {
    grp_col <- NULL
  }

  plot_list <- list()

  # ── 5a. GROUPED path ───────────────────────────────────────────────────────
  if (!is.null(grp_col)) {
    iter_inc <- 0.65 / length(numeric_cols)

    for (var in numeric_cols) {
      if (!is.null(progress)) {
        progress$inc(iter_inc, detail = paste("Plotting:", var))
      }

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = .data[[grp_col]],
          y = .data[[var]],
          fill = .data[[grp_col]]
        )
      ) +
        ggplot2::geom_violin(trim = FALSE, alpha = 0.6)

      if (boxplot_overlay) {
        p <- p +
          ggplot2::geom_boxplot(width = 0.1, alpha = 0.5, outlier.shape = NA)
      }

      p <- p +
        ggplot2::labs(
          title = paste0(var, " by ", paste(group_by, collapse = ":")),
          x = paste(group_by, collapse = ":"),
          y = var
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )

      if (!is.null(y_lim)) {
        p <- p + ggplot2::coord_cartesian(ylim = y_lim)
      }

      plot_list[[var]] <- p
    }

    # ── 5b. UNGROUPED path ─────────────────────────────────────────────────────
  } else {
    n_col <- length(numeric_cols)
    n_chunks <- ceiling(n_col / bin_size)
    iter_inc <- 0.65 / n_chunks

    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Generating violin plot pages")
    }

    y_label <- if (scale == "none") {
      "Value"
    } else {
      paste0("Value (", scale, "-transformed)")
    }

    for (i in seq_len(n_chunks)) {
      start_idx <- (i - 1L) * bin_size + 1L
      end_idx <- min(i * bin_size, n_col)
      chunk_cols <- numeric_cols[start_idx:end_idx]

      melted <- reshape2::melt(
        df[, chunk_cols, drop = FALSE],
        measure.vars = chunk_cols,
        variable.name = "Variable",
        value.name = "Value"
      )

      p <- ggplot2::ggplot(
        melted,
        ggplot2::aes(x = Variable, y = Value)
      ) +
        ggplot2::geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.7)

      if (boxplot_overlay) {
        p <- p +
          ggplot2::geom_boxplot(width = 0.1, alpha = 0.5, outlier.shape = NA)
      }

      p <- p +
        ggplot2::labs(
          title = "Violin Plots for Variables:",
          x = "Variable",
          y = y_label
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            vjust = 0.5,
            hjust = 1
          )
        ) +
        ggplot2::scale_x_discrete(
          guide = ggplot2::guide_axis(n.dodge = 1)
        )

      if (!is.null(y_lim)) {
        p <- p + ggplot2::coord_cartesian(ylim = y_lim)
      }

      plot_list[[paste0("page", i)]] <- p

      if (!is.null(progress)) {
        progress$inc(iter_inc, detail = paste("Page", i, "of", n_chunks))
      }
    }
  }

  # ── 6. Output ──────────────────────────────────────────────────────────────
  if (!is.null(output_file)) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Saving plots to file")
    }

    ext <- tolower(tools::file_ext(output_file))

    if (ext == "pdf") {
      grDevices::pdf(file = output_file, width = 7, height = 5)
      on.exit(grDevices::dev.off(), add = TRUE)
      for (p in plot_list) {
        print(p)
      }
    } else {
      for (i in seq_along(plot_list)) {
        fname <- if (length(plot_list) > 1L) {
          sub(paste0("\\.", ext, "$"), paste0("_", i, ".", ext), output_file)
        } else {
          output_file
        }
        ggplot2::ggsave(fname, plot_list[[i]], width = 7, height = 5)
      }
    }

    if (!is.null(progress)) {
      progress$inc(0.05, detail = "File saved")
    }
  } else {
    for (p in plot_list) {
      print(p)
    }

    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Complete")
    }
  }

  invisible(plot_list)
}
