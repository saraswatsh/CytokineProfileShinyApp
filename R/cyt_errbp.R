#' Error Bar Plot with P-value and Effect Size Annotations
#'
#' This function automatically detects a numeric measurement column from the provided data and
#' calculates group metrics (sample size, mean, standard deviation, standard error) and performs a
#' t-test (comparing each group to the baseline) to obtain p-values and a standardized mean difference
#' (as a proxy for effect size). When a grouping variable is provided, it is used to separate the data
#' into groups (the first level is taken as the baseline). If no grouping variable is provided, the metrics
#' are computed for the overall data. The function then produces a ggplot2 bar plot with error bars and,
#' if requested, overlays p-value and effect size annotations (displayed as symbols by default).
#'
#' @param data A data frame containing at least one numeric variable. If a grouping variable is provided,
#'   it must be one of the columns.
#' @param group_col Character. (Optional) The name of the grouping (categorical) variable.
#'   If not provided, metrics are calculated for the overall data.
#' @param p_lab Logical. Whether to display p-value labels. Default is \code{FALSE}.
#' @param es_lab Logical. Whether to display effect size labels. Default is \code{FALSE}.
#' @param class_symbol Logical. If \code{TRUE}, p-values and effect sizes are shown as symbols;
#'   if \code{FALSE}, numeric values are displayed. Default is \code{FALSE}.
#' @param x_lab Character. Label for the x-axis. Defaults to the grouping variable name if provided,
#'   or "Group" otherwise.
#' @param y_lab Character. Label for the y-axis. Defaults to the name of the selected numeric variable.
#' @param title Character. The plot title.
#' @param log2 Logical. If \code{TRUE}, transforms numeric variables using log2 transformation. If
#'   \code{FALSE}, uses raw values from provided data. Default is \code{FALSE}.
#' @param output_file Optional. A string representing the file path for the PDF file to be created.
#'   If NULL (default), the function returns a list of ggplot objects.
#'
#' @return A ggplot2 object representing the error bar plot.
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @export
#'
#' @examples
#' data <- ExampleData1
#'
#' cyt_errbp(data[,c("Group", "CCL.20.MIP.3A", "IL.10")], group_col = "Group",
#' p_lab = TRUE, es_lab = TRUE, class_symbol = TRUE, x_lab = "Cytokines",
#' y_lab = "Concentrations in log2 scale", log2 = TRUE)
#'

cyt_errbp <- function(
  data,
  group_col = NULL,
  p_lab = FALSE,
  es_lab = FALSE,
  class_symbol = TRUE,
  x_lab = "",
  y_lab = "",
  title = NULL,
  progress = NULL,
  log2 = FALSE,
  output_file = NULL
) {
  # Update progress if provided.
  if (!is.null(progress)) {
    progress$set(message = "Starting error bar plot...", value = 0)
  }
  # If log2 transformation is requested, transform all numeric columns (or a subset if desired)
  if (log2) {
    # Add a small offset (e.g., 1) to avoid log(0)
    numeric_cols <- sapply(data, is.numeric)
    data[numeric_cols] <- lapply(data[numeric_cols], function(x) log2(x))
    if (!is.null(progress)) {
      progress$set(
        message = "Applied log2 transformation to numeric variables."
      )
    }
  }
  # Convert the specified group column to a factor
  data[[group_col]] <- factor(data[[group_col]])

  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Identifying numeric columns...")
  }
  # Identify all numeric columns, excluding the grouping column.
  num_vars <- names(data)[sapply(data, is.numeric) & names(data) != group_col]
  if (length(num_vars) == 0) stop("No numeric columns found in the data.")
  if (y_lab == "") y_lab <- "Value"

  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Reshaping data to long format...")
  }
  # Reshape the data into long format (one row per numeric measurement)
  long_df <- data %>%
    dplyr::select(dplyr::all_of(c(group_col, num_vars))) %>%
    tidyr::pivot_longer(
      cols = all_of(num_vars),
      names_to = "Measure",
      values_to = "Value"
    )

  if (!is.null(progress)) {
    progress$inc(0.15, detail = "Calculating summary statistics...")
  }
  # Calculate summary statistics (sample size, mean, standard deviation) per group and per measure.
  metrics <- long_df %>%
    dplyr::group_by(.data[[group_col]], Measure) %>%
    dplyr::summarize(
      n = sum(!is.na(Value)),
      center = mean(Value, na.rm = TRUE),
      sd = stats::sd(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(spread = sd / sqrt(n))

  # Determine the baseline group: using the first level of the grouping variable.
  group_levels <- levels(data[[group_col]])
  baseline <- group_levels[1]

  # Initialize columns for p-value and effect size.
  metrics <- metrics %>%
    dplyr::mutate(p.value = NA_real_, effect.size = NA_real_)

  if (!is.null(progress)) {
    progress$inc(
      0.1,
      detail = "Performing t-tests and computing effect sizes..."
    )
  }
  # For each measure, perform a t-test comparing each group against the baseline and compute an effect size.
  unique_measures <- unique(metrics$Measure)
  for (m in unique_measures) {
    baseline_row <- metrics %>%
      dplyr::filter(Measure == m, .data[[group_col]] == baseline)
    if (nrow(baseline_row) == 0) next
    base_mean <- baseline_row$center
    base_sd <- baseline_row$sd
    base_n <- baseline_row$n

    # Get indices for non-baseline groups in the measure m.
    non_base_idx <- which(
      metrics$Measure == m & metrics[[group_col]] != baseline
    )
    for (i in non_base_idx) {
      current_group <- metrics[[group_col]][i]
      baseline_data <- long_df %>%
        dplyr::filter(Measure == m, .data[[group_col]] == baseline) %>%
        dplyr::pull(Value)
      grp_data <- long_df %>%
        dplyr::filter(Measure == m, .data[[group_col]] == current_group) %>%
        dplyr::pull(Value)

      tt <- stats::t.test(grp_data, baseline_data)
      metrics$p.value[i] <- tt$p.value

      grp_mean <- metrics$center[i]
      grp_sd <- metrics$sd[i]
      grp_n <- metrics$n[i]
      pooled_sd <- sqrt(
        ((base_n - 1) * base_sd^2 + (grp_n - 1) * grp_sd^2) /
          (base_n + grp_n - 2)
      )
      d <- (grp_mean - base_mean) / pooled_sd
      metrics$effect.size[i] <- d
    }
  }

  # Create labels for p-values and effect sizes according to class_symbol.
  if (p_lab) {
    if (class_symbol) {
      significance_mark_fn <- function(p_value) {
        if (is.na(p_value)) return(NA_character_)
        if (p_value <= 0.00001) return("*****")
        if (p_value <= 0.0001) return("****")
        if (p_value <= 0.001) return("***")
        if (p_value <= 0.01) return("**")
        if (p_value <= 0.05) return("*")
        return("")
      }
      metrics <- metrics %>%
        dplyr::mutate(p_label = sapply(p.value, significance_mark_fn))
    } else {
      metrics <- metrics %>%
        dplyr::mutate(
          p_label = paste0(
            "p=",
            ifelse(
              p.value > 0.001,
              round(p.value, 3),
              formatC(p.value, format = "e", digits = 1)
            )
          )
        )
    }
  }

  if (es_lab) {
    if (class_symbol) {
      effect_size_mark_fn <- function(es) {
        if (is.na(es)) return(NA_character_)
        if (es >= 5) return(">>>>>")
        if (es >= 3) return(">>>>")
        if (es >= 1.645) return(">>>")
        if (es >= 1) return(">>")
        if (es > 0.25) return(">")
        if (es >= -0.25) return(" ")
        if (es > -1) return("<")
        if (es > -1.645) return("<<")
        if (es > -3) return("<<<")
        if (es > -5) return("<<<<")
        return("<<<<<")
      }
      metrics <- metrics %>%
        dplyr::mutate(es_label = sapply(effect.size, effect_size_mark_fn))
    } else {
      metrics <- metrics %>%
        dplyr::mutate(es_label = round(effect.size, 3))
    }
  }

  # Compute a rough y-range to position annotations.
  y_range <- diff(range(metrics$center + metrics$spread, na.rm = TRUE))
  metrics <- metrics %>%
    dplyr::mutate(
      p_text_y = center +
        ifelse(center >= 0, spread + y_range / 20, -spread - y_range / 20),
      es_text_y = center +
        ifelse(center >= 0, spread + y_range / 4, -spread - y_range / 4)
    )

  # Set default axis labels and title if not provided.
  if (x_lab == "") x_lab <- group_col
  if (is.null(title))
    title <- paste(
      "Error Bar Plots for",
      paste(unique_measures, collapse = ", ")
    )

  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Generating plot...")
  }
  # Build the faceted ggplot (one facet per numeric measure).
  p <- ggplot2::ggplot(metrics, aes(x = .data[[group_col]], y = center)) +
    ggplot2::geom_bar(stat = "identity", fill = "gray", width = 0.7) +
    ggplot2::geom_errorbar(
      aes(ymin = center - spread, ymax = center + spread),
      width = 0.2
    ) +
    ggplot2::facet_wrap(~Measure, scales = "free_y") +
    ggplot2::labs(x = x_lab, y = y_lab, title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Add text annotations if requested.
  if (p_lab) {
    p <- p +
      ggplot2::geom_text(
        data = metrics %>% filter(.data[[group_col]] != baseline),
        ggplot2::aes(x = .data[[group_col]], y = p_text_y, label = p_label),
        size = 4,
        vjust = 0
      )
  }
  if (es_lab) {
    p <- p +
      ggplot2::geom_text(
        data = metrics %>% filter(.data[[group_col]] != baseline),
        ggplot2::aes(
          x = .data[[group_col]],
          y = es_text_y,
          label = es_label
        ),
        size = 4,
        vjust = 0
      )
  }

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Plotting complete.")
  }

  if (!is.null(output_file)) {
    if (!is.null(progress)) progress$inc(0.05, detail = "Saving plots to PDF")
    grDevices::pdf(file = output_file, width = 7, height = 5)
    print(p)
    grDevices::dev.off()

    if (!is.null(progress)) progress$inc(0.05, detail = "PDF saved")
    return(invisible(NULL))
  } else {
    if (!is.null(progress))
      progress$inc(0.05, detail = "Returning list of plots")
    return(p)
  }
}
