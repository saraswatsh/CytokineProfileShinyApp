#' Error-bar Plot
#'
#' @description
#' This function generates an error-bar plot to visually compare different
#' groups against a designated baseline group. It displays the central
#' tendency (mean or median) as a bar and overlays error bars to represent
#' the data's spread (e.g., standard deviation, MAD, or standard error).
#' The plot can also include p-value and effect size labels (based on SSMD),
#' presented either as symbols or numeric values, to highlight significant
#' differences and the magnitude of effects.
#' When an output filename is provided the plot is saved to disk; otherwise
#' the ggplot object is returned and drawn on the current graphics device.
#'
#' @param data A data frame containing at least one numeric column and a
#'   grouping column.
#' @param group_col Character string naming the column that defines groups.
#'   This column will be coerced to a factor.
#' @param p_lab Logical. If \code{TRUE} (default) p-value labels are
#'   displayed for group comparisons.
#' @param es_lab Logical. If \code{TRUE} (default) effect-size labels are
#'   displayed.
#' @param class_symbol Logical. If \code{TRUE}, p-value and effect-size
#'   labels are encoded using symbols (e.g., \code{*}, \code{>>>}). If
#'   \code{FALSE} (default), numeric values are shown instead.
#' @param x_lab Character string for the x-axis label. If empty a default
#'   label is generated.
#' @param y_lab Character string for the y-axis label. If empty a default
#'   label is generated.
#' @param title Character string for the plot title. If empty a default
#'   title is generated.
#' @param stat Character. Central tendency statistic to use. One of
#'   \code{"mean"} (default) or \code{"median"}.
#' @param error Character. Error measure visualized around the statistic.
#'   One of \code{"se"} (standard error; default), \code{"sd"} (standard
#'   deviation), \code{"mad"} (median absolute deviation), or \code{"ci"}
#'   (approximate 95% confidence interval).
#' @param scale Character controlling data transformation before analysis.
#'   One of \code{"none"} (default), \code{"log2"}, \code{"log10"},
#'   \code{"zscore"}, or \code{"custom"}.
#' @param custom_fn A user-supplied function applied to numeric columns when
#'   \code{scale = "custom"}.
#' @param method Character controlling the statistical test used for pairwise
#'   comparisons. One of \code{"auto"} (default; choose between t-test and
#'   Wilcoxon based on a normality test), \code{"ttest"}, or \code{"wilcox"}.
#' @param p_adjust_method Character. If non-\code{NULL}, specifies the method
#'   passed to \code{p.adjust()} to correct p-values across all comparisons
#'   (e.g., \code{"BH"} for Benjamini-Hochberg). \code{NULL} (default) skips
#'   adjustment.
#' @param label_size Numeric. Font size for p-value and effect-size labels.
#'   Default is \code{4}.
#' @param n_col Integer. Number of columns in the facet grid. If \code{NULL}
#'   (default), uses \code{min(3, number_of_cytokines)}.
#' @param base_size Numeric. Base font size for the plot theme. Default is
#'   \code{11}.
#' @param fill_palette Character vector of colours for the group bars. If
#'   \code{NULL} (default), all bars are filled with \code{"grey80"}.
#'   Supply one colour per group level (e.g. \code{c("#4E79A7", "#F28E2B",
#'   "#E15759")}).
#' @param output_file Optional file path. If provided, the plot is saved
#'   using \code{ggsave()}; otherwise the plot is returned and automatically
#'   printed.
#' @param progress Optional. A Shiny \code{Progress} object for reporting
#'   progress updates.
#'
#' @return A \code{ggplot} object. When \code{output_file} is specified the
#'   plot is saved to disk and returned invisibly.
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @importFrom stats t.test wilcox.test shapiro.test mad sd median
#' @author Xiaohua Douglas Zhang and Shubh Saraswat
#' @export
#'
#' @examples
#' data(ExampleData1)
#' df <- ExampleData1[, c("Group", "CCL.20.MIP.3A", "IL.10")]
#' cyt_errbp(df, group_col = "Group")
#' cyt_errbp(df, group_col = "Group", stat = "mean", error = "sd",
#'           scale = "log2", class_symbol = TRUE, method = "ttest")
cyt_errbp <- function(
  data,
  group_col = NULL,
  p_lab = TRUE,
  es_lab = TRUE,
  class_symbol = FALSE,
  x_lab = "",
  y_lab = "",
  title = "",
  stat = c("mean", "median"),
  error = c("se", "sd", "mad", "ci"),
  scale = c("none", "log2", "log10", "zscore", "custom"),
  custom_fn = NULL,
  method = c("auto", "ttest", "wilcox"),
  p_adjust_method = NULL,
  label_size = 4,
  n_col = NULL,
  base_size = 11,
  fill_palette = NULL,
  output_file = NULL,
  progress = NULL
) {
  # ── 0. Initialise ──────────────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$set(message = "Error bar plot: initialising...", value = 0)
  }

  names(data) <- make.names(names(data), unique = TRUE)

  # Validate group_col
  if (
    is.null(group_col) || !is.character(group_col) || length(group_col) != 1
  ) {
    stop(
      "'group_col' must be provided as a single character string naming a column in 'data'.",
      call. = FALSE
    )
  }
  if (!group_col %in% names(data)) {
    stop(
      paste0(
        "The specified group_col '",
        group_col,
        "' was not found in the data frame."
      ),
      call. = FALSE
    )
  }

  stat <- match.arg(stat)
  error <- match.arg(error)
  scale <- match.arg(scale)
  method <- match.arg(method)
  data <- as.data.frame(data)

  # ── 1. Convert grouping column to factor ───────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Converting grouping column to factor")
  }
  data[[group_col]] <- as.factor(data[[group_col]])

  # ── 2. Identify numeric columns ────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Identifying numeric columns")
  }
  num_vars <- names(data)[sapply(data, is.numeric) & names(data) != group_col]
  if (length(num_vars) == 0L) {
    stop("No numeric columns found in the data.")
  }

  # ── 3. Scaling via shared utility ──────────────────────────────────────────
  if (scale != "none") {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = paste("Applying", scale, "transformation"))
    }
    data <- apply_scale(
      data = data,
      columns = num_vars,
      scale = scale,
      custom_fn = custom_fn
    )
  }

  # ── 4. Reshape to long format ──────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Reshaping data to long format")
  }
  long_df <- data |>
    dplyr::select(dplyr::all_of(c(group_col, num_vars))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(num_vars),
      names_to = "Measure",
      values_to = "Value"
    )

  # ── 5. Summary statistics ──────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Computing summary statistics")
  }
  summarised <- long_df |>
    dplyr::group_by(.data[[group_col]], Measure) |>
    dplyr::summarise(
      n = sum(!is.na(Value)),
      center = if (stat == "mean") {
        mean(Value, na.rm = TRUE)
      } else {
        stats::median(Value, na.rm = TRUE)
      },
      sd = stats::sd(Value, na.rm = TRUE),
      .groups = "drop"
    )

  spread_values <- if (error == "sd") {
    summarised$sd
  } else if (error == "se") {
    summarised$sd / sqrt(summarised$n)
  } else if (error == "mad") {
    mapply(
      function(g, m) {
        stats::mad(
          long_df$Value[long_df$Measure == m & long_df[[group_col]] == g],
          na.rm = TRUE
        )
      },
      summarised[[group_col]],
      summarised$Measure
    )
  } else if (error == "ci") {
    (summarised$sd / sqrt(summarised$n)) * 1.96
  } else {
    summarised$sd / sqrt(summarised$n)
  }

  summarised <- summarised |>
    dplyr::mutate(spread = spread_values)

  # ── 6. P-values and effect sizes ───────────────────────────────────────────
  group_levels <- levels(data[[group_col]])
  baseline <- group_levels[1]

  summarised <- summarised |>
    dplyr::mutate(P_value = NA_real_, EffectSize = NA_real_)

  if (p_lab || es_lab) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Computing p-values and effect sizes")
    }

    total_iter <- length(unique(summarised$Measure)) *
      (length(group_levels) - 1L)
    iter_inc <- 0.30 / max(total_iter, 1L)

    for (m in unique(summarised$Measure)) {
      baseline_values <- long_df$Value[
        long_df$Measure == m & long_df[[group_col]] == baseline
      ]
      base_mean <- mean(baseline_values, na.rm = TRUE)
      base_sd <- stats::sd(baseline_values, na.rm = TRUE)

      for (g in setdiff(group_levels, baseline)) {
        if (!is.null(progress)) {
          progress$inc(iter_inc, detail = paste0("Testing: ", m, " ~ ", g))
        }

        idx <- summarised$Measure == m & summarised[[group_col]] == g
        grp_values <- long_df$Value[
          long_df$Measure == m & long_df[[group_col]] == g
        ]

        # Choose test
        use_test <- method
        if (method == "auto") {
          combined <- c(baseline_values, grp_values)
          p_norm <- tryCatch(
            stats::shapiro.test(combined)$p.value,
            error = function(e) NA_real_
          )
          use_test <- if (!is.na(p_norm) && p_norm < 0.05) "wilcox" else "ttest"
        }

        if (use_test == "ttest") {
          tt <- stats::t.test(grp_values, baseline_values)
          p_val <- tt$p.value
          grp_mean <- mean(grp_values, na.rm = TRUE)
          grp_sd <- stats::sd(grp_values, na.rm = TRUE)
          pooled_sd <- sqrt(
            ((length(grp_values) - 1) *
              grp_sd^2 +
              (length(baseline_values) - 1) * base_sd^2) /
              (length(grp_values) + length(baseline_values) - 2)
          )
          eff <- ifelse(
            pooled_sd == 0,
            NA_real_,
            (grp_mean - base_mean) / pooled_sd
          )
        } else {
          wt <- stats::wilcox.test(grp_values, baseline_values, exact = FALSE)
          p_val <- wt$p.value
          u <- wt$statistic
          eff <- (u / (length(grp_values) * length(baseline_values))) * 2 - 1
        }

        summarised$P_value[idx] <- p_val
        summarised$EffectSize[idx] <- eff
      }
    }

    # P-value adjustment
    if (!is.null(p_adjust_method)) {
      summarised$P_adj <- adjust_p(summarised$P_value, method = p_adjust_method)
    } else {
      summarised$P_adj <- summarised$P_value
    }
  } else {
    summarised$P_adj <- summarised$P_value
  }

  # ── 7. Label positions ─────────────────────────────────────────────────────
  y_range <- diff(range(summarised$center + summarised$spread, na.rm = TRUE))
  summarised <- summarised |>
    dplyr::mutate(
      p_y = center + spread + 0.05 * y_range,
      es_y = center + spread + 0.15 * y_range
    )

  # ── 8. Build text labels ───────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Building annotation labels")
  }

  significance_mark_fn <- function(p_value) {
    if (is.na(p_value)) {
      return(NA_character_)
    }
    if (p_value <= 0.00001) {
      return("*****")
    }
    if (p_value <= 0.0001) {
      return("****")
    }
    if (p_value <= 0.001) {
      return("***")
    }
    if (p_value <= 0.01) {
      return("**")
    }
    if (p_value <= 0.05) {
      return("*")
    }
    ""
  }

  effect_size_mark_fn <- function(es) {
    if (is.na(es)) {
      return(NA_character_)
    }
    if (es >= 5) {
      return(">>>>>")
    }
    if (es >= 3) {
      return(">>>>")
    }
    if (es >= 1.645) {
      return(">>>")
    }
    if (es >= 1) {
      return(">>")
    }
    if (es > 0.25) {
      return(">")
    }
    if (es >= -0.25) {
      return(" ")
    }
    if (es > -1) {
      return("<")
    }
    if (es > -1.645) {
      return("<<")
    }
    if (es > -3) {
      return("<<<")
    }
    if (es > -5) {
      return("<<<<")
    }
    "<<<<<"
  }

  p_symbol <- sapply(summarised$P_adj, significance_mark_fn)
  es_symbol <- sapply(summarised$EffectSize, effect_size_mark_fn)

  p_numeric <- ifelse(
    is.na(summarised$P_adj),
    NA_character_,
    ifelse(
      summarised$P_adj > 0.001,
      sprintf("p=%.3f", summarised$P_adj),
      paste0("p=", formatC(summarised$P_adj, format = "e", digits = 1))
    )
  )
  es_numeric <- ifelse(
    is.na(summarised$EffectSize),
    NA_character_,
    sprintf("es=%.3f", summarised$EffectSize)
  )

  summarised$p_label <- unlist(mapply(
    function(sym, num) {
      if (p_lab && class_symbol) {
        if (!is.na(sym) && sym != "" && sym != " " && !is.na(num)) {
          return(paste(sym, num))
        }
        if (!is.na(num)) {
          return(num)
        }
        return(NA_character_)
      } else if (p_lab) {
        return(ifelse(is.na(num), NA_character_, num))
      } else if (class_symbol) {
        return(ifelse(
          is.na(sym) || sym == "" || sym == " ",
          NA_character_,
          sym
        ))
      }
      NA_character_
    },
    p_symbol,
    p_numeric,
    SIMPLIFY = FALSE
  ))

  summarised$es_label <- unlist(mapply(
    function(sym, num) {
      if (es_lab && class_symbol) {
        if (!is.na(sym) && sym != "" && sym != " " && !is.na(num)) {
          return(paste(sym, num))
        }
        if (!is.na(num)) {
          return(num)
        }
        return(NA_character_)
      } else if (es_lab) {
        return(ifelse(is.na(num), NA_character_, num))
      } else if (class_symbol) {
        return(ifelse(
          is.na(sym) || sym == "" || sym == " ",
          NA_character_,
          sym
        ))
      }
      NA_character_
    },
    es_symbol,
    es_numeric,
    SIMPLIFY = FALSE
  ))

  # ── 9. Default axis / title labels ────────────────────────────────────────
  if (x_lab == "") {
    x_lab <- group_col
  }
  if (y_lab == "") {
    y_lab <- paste(stat, "value")
  }
  if (title == "") {
    title <- paste(
      "Error Bar Plots for",
      paste(unique(summarised$Measure), collapse = ", ")
    )
  }

  # ── 10. Build plot ─────────────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Building plot")
  }

  # Resolve ncol for facet grid
  n_facets_total <- length(unique(summarised$Measure))
  facet_ncol <- if (!is.null(n_col) && n_col > 0L) {
    as.integer(n_col)
  } else {
    min(3L, n_facets_total)
  }

  # Resolve fill: group-coloured or flat grey
  if (!is.null(fill_palette)) {
    group_levels <- levels(factor(summarised[[group_col]]))
    fill_map <- stats::setNames(
      rep_len(fill_palette, length(group_levels)),
      group_levels
    )
    col_aes <- ggplot2::aes(fill = .data[[group_col]])
    fill_scale <- ggplot2::scale_fill_manual(values = fill_map)
  } else {
    col_aes <- ggplot2::aes()
    fill_scale <- ggplot2::scale_fill_manual(values = NULL)
  }

  p <- ggplot2::ggplot(
    summarised,
    ggplot2::aes(x = .data[[group_col]], y = center)
  ) +
    (if (!is.null(fill_palette)) {
      ggplot2::geom_col(ggplot2::aes(fill = .data[[group_col]]))
    } else {
      ggplot2::geom_col(fill = "grey80")
    }) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = center - spread, ymax = center + spread),
      width = 0.2
    ) +
    ggplot2::facet_wrap(
      ~Measure,
      scales = "free_y",
      ncol = facet_ncol
    ) +
    ggplot2::labs(x = x_lab, y = y_lab, title = title) +
    (if (!is.null(fill_palette)) fill_scale else ggplot2::guides()) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = if (!is.null(fill_palette)) "bottom" else "none"
    )

  if (p_lab) {
    p <- p +
      ggplot2::geom_text(
        data = summarised |>
          dplyr::filter(.data[[group_col]] != baseline & !is.na(p_label)),
        ggplot2::aes(x = .data[[group_col]], y = p_y, label = p_label),
        size = label_size,
        colour = "black",
        vjust = 0,
        na.rm = TRUE
      )
  }

  if (es_lab) {
    p <- p +
      ggplot2::geom_text(
        data = summarised |>
          dplyr::filter(.data[[group_col]] != baseline & !is.na(es_label)),
        ggplot2::aes(x = .data[[group_col]], y = es_y, label = es_label),
        size = label_size,
        colour = "black",
        vjust = 0,
        na.rm = TRUE
      )
  }

  # ── 11. Output ─────────────────────────────────────────────────────────────
  if (!is.null(output_file)) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Saving plot to file")
    }
    n_facets <- length(unique(summarised$Measure))
    n_cols <- facet_ncol
    n_rows <- ceiling(n_facets / n_cols)
    out_width <- n_cols * 4
    out_height <- n_rows * 4
    ggplot2::ggsave(output_file, p, width = out_width, height = out_height)
    if (!is.null(progress)) {
      progress$inc(0.02, detail = "File saved")
    }
    return(invisible(p))
  }

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Complete")
  }

  print(p)
  invisible(p)
}
