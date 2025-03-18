#' Distribution of the Data Set Shown by Skewness and Kurtosis (Revised).
#'
#' This function computes summary statistics (sample size, mean, standard error, skewness, kurtosis)
#' for each numeric measurement column in a data set, optionally grouped by columns in `group_cols`.
#' It produces overlayed histograms for skewness and kurtosis for both raw and log2-transformed data.
#' If an output_file is provided, the two plots are arranged side by side in a single PDF page;
#' otherwise, a list of two ggplot objects is returned.
#'
#' @param data A data frame or matrix of raw data.
#' @param group_cols A character vector specifying the names of the grouping columns.
#' If not provided, the entire dataset is treated as one group ("overall").
#' @param output_file Optional. A file path to save the plots (with extension .pdf).
#' If NULL (default), the function returns a list of two ggplot objects.
#' @param print_res_raw Logical. If TRUE, prints and returns the summary statistics for raw data.
#' @param print_res_log Logical. If TRUE, prints and returns the summary statistics for log2 data.
#'
#' @return If output_file is NULL, returns a list with:
#'   - p_skew: Overlayed histogram of raw and log2 skewness.
#'   - p_kurt: Overlayed histogram of raw and log2 kurtosis.
#' Otherwise, writes the plots to a PDF and returns NULL invisibly.
#'
#' @import e1071 
#' @import ggplot2 
#' @import dplyr
#' @import gridExtra
#' @export
cyt_skku <- function(data, group_cols = NULL, output_file = NULL,
                     print_res_raw = FALSE, print_res_log = FALSE, progress = NULL) {
  if (!is.null(progress)) progress$inc(0.05, detail = "Loading libraries and data")
  library(e1071)
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
  
  if (!is.null(progress)) progress$inc(0.05, detail = "Determining grouping and measurement columns")
  if (!is.null(group_cols)) {
    measure_cols <- setdiff(names(data), group_cols)
    grouping <- apply(data[, group_cols, drop = FALSE], 1, paste, collapse = "_")
  } else {
    measure_cols <- names(data)
    grouping <- rep("overall", nrow(data))
  }
  
  measure_mat <- data[, measure_cols, drop = FALSE]
  condt <- !is.na(measure_mat) & (measure_mat > 0)
  cutoff <- min(measure_mat[condt], na.rm = TRUE) / 10
  
  compute_metrics <- function(Y, groups) {
    n <- tapply(Y, groups, function(x) sum(!is.na(x)))
    center <- tapply(Y, groups, mean, na.rm = TRUE)
    spread <- tapply(Y, groups, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
    skew <- tapply(Y, groups, skewness, na.rm = TRUE)
    kurt <- tapply(Y, groups, kurtosis, na.rm = TRUE)
    data.frame(
      group = names(n),
      n = as.numeric(n),
      center = as.numeric(center),
      spread = as.numeric(spread),
      skewness = as.numeric(skew),
      kurtosis = as.numeric(kurt),
      stringsAsFactors = FALSE
    )
  }
  
  if (!is.null(progress)) progress$inc(0.1, detail = "Computing metrics for raw and log2 data")
  raw_list <- list()
  log_list <- list()
  for (col in measure_cols) {
    Y_raw <- data[[col]]
    df_raw <- compute_metrics(Y_raw, grouping)
    df_raw$measurement <- col
    raw_list[[col]] <- df_raw
    
    Y_log <- log2(Y_raw + cutoff)
    df_log <- compute_metrics(Y_log, grouping)
    df_log$measurement <- col
    log_list[[col]] <- df_log
    if (!is.null(progress)) progress$inc(0.01, detail = paste("Processed", col))
  }
  
  raw_results <- do.call(rbind, raw_list)
  log_results <- do.call(rbind, log_list)
  
  df_skew <- data.frame(value = c(raw_results$skewness, log_results$skewness),
                        Transformation = rep(c("Raw", "Log2"),
                                             times = c(nrow(raw_results), nrow(log_results))))
  df_kurt <- data.frame(value = c(raw_results$kurtosis, log_results$kurtosis),
                        Transformation = rep(c("Raw", "Log2"),
                                             times = c(nrow(raw_results), nrow(log_results))))
  
  if (!is.null(progress)) progress$inc(0.1, detail = "Generating histograms")
  p_skew <- ggplot(df_skew, aes(x = value, fill = Transformation)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(x = "Skewness", title = "Distribution of Skewness") +
    theme_minimal()
  
  p_kurt <- ggplot(df_kurt, aes(x = value, fill = Transformation)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(x = "Kurtosis", title = "Distribution of Kurtosis") +
    theme_minimal()
  
  if (print_res_raw) print(raw_results)
  if (print_res_log) print(log_results)
  
  if (!is.null(output_file)) {
    if (!is.null(progress)) progress$inc(0.05, detail = "Saving histograms to PDF")
    pdf(file = output_file, width = 10, height = 5)
    gridExtra::grid.arrange(p_skew, p_kurt, ncol = 2)
    dev.off()
    if (!is.null(progress)) progress$inc(0.05, detail = "PDF saved")
    return(invisible(NULL))
  } else {
    if (!is.null(progress)) progress$inc(0.05, detail = "Returning histogram plots")
    return(list(p_skew = p_skew, p_kurt = p_kurt, raw_results = raw_results, log_results = log_results))
  }
}

