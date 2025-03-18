#' Dual-flashlight Plot.
#'
#' This function reshapes the input data and computes summary statistics
#' (mean and variance) for each variable grouped by a specified grouping column.
#' It then calculates the SSMD (Strictly Standardized Mean Difference) and log2 fold change
#' between two groups (group1 and group2) and categorizes the effect strength.
#' A dual flash plot is generated using ggplot2 where the x-axis represents the average log2 fold change
#' and the y-axis represents the SSMD.
#'
#' @param data A data frame containing the input data.
#' @param group_var A string specifying the name of the grouping column in the data.
#' @param group1 A string representing the name of the first group for comparison.
#' @param group2 A string representing the name of the second group for comparison.
#' @param ssmd_thresh A numeric threshold for the SSMD value used to determine significance (default = 1).
#' @param log2fc_thresh A numeric threshold for the log2 fold change used to determine significance (default = 1).
#' @param top_labels An integer specifying the number of top variables (based on absolute SSMD) to label in the plot (default = 15).
#' @param output_file Optional. A file path to save the plot as a PDF (or PNG if extension is .png). If NULL (default),
#' the function returns a ggplot object.
#'
#' @return If output_file is NULL, a ggplot object representing the dual-flash plot is returned;
#' otherwise, the plot is saved to the specified file and the function returns NULL invisibly.
#'
#' @examples
#' \dontrun{
#' # Interactive mode:
#' p <- cyt_dualflashplot(cytodata, group_var = "Group", group1 = "T2D", group2 = "ND",
#'                         ssmd_thresh = 1, log2fc_thresh = 1, top_labels = 15)
#' print(p)
#'
#' # Download mode:
#' cyt_dualflashplot(cytodata, group_var = "Group", group1 = "T2D", group2 = "ND",
#'                    ssmd_thresh = 1, log2fc_thresh = 1, top_labels = 15,
#'                    output_file = "dualflashplot.pdf")
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import ggrepel
#' @export
cyt_dualflashplot <- function(data, group_var, group1, group2, ssmd_thresh = 1,
                              log2fc_thresh = 1, top_labels = 15, output_file = NULL,
                              progress = NULL) {
  if (!is.null(progress)) progress$inc(0.05, detail = "Validating input data")
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  if (!is.null(progress)) progress$inc(0.05, detail = "Reshaping data to long format")
  data_long <- data %>%
    tidyr::pivot_longer(cols = -all_of(group_var), names_to = "cytokine", values_to = "level")
  
  if (!is.null(progress)) progress$inc(0.1, detail = "Calculating summary statistics")
  stats <- data_long %>%
    group_by(cytokine, .data[[group_var]]) %>%
    summarise(
      mean = mean(level, na.rm = TRUE),
      variance = var(level, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(names_from = .data[[group_var]], values_from = c(mean, variance)) %>%
    mutate(
      ssmd = (get(paste0("mean_", group1)) - get(paste0("mean_", group2))) /
        sqrt((get(paste0("variance_", group1)) + get(paste0("variance_", group2))) / 2),
      log2FC = log2(get(paste0("mean_", group1)) / get(paste0("mean_", group2))),
      SSMD_Category = case_when(
        abs(ssmd) >= 1 ~ "Strong Effect",
        abs(ssmd) >= 0.5 ~ "Moderate Effect",
        TRUE ~ "Weak Effect"
      ),
      Significant = (abs(ssmd) >= ssmd_thresh) & (abs(log2FC) >= log2fc_thresh)
    )
  
  if (!is.null(progress)) progress$inc(0.1, detail = "Selecting top variables for labels")
  top_stats <- dplyr::top_n(stats, n = top_labels, wt = abs(ssmd))
  
  if (!is.null(progress)) progress$inc(0.1, detail = "Generating plot")
  p <- ggplot(stats, aes(x = log2FC, y = ssmd, label = cytokine)) +
    geom_point(aes(color = SSMD_Category, shape = Significant)) +
    ggrepel::geom_text_repel(data = top_stats, size = 3, vjust = 1.5, hjust = 1.1, max.overlaps = 50) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = c(log2fc_thresh, -log2fc_thresh),
               linetype = "dashed", color = "blue") +
    labs(x = "Average log2 Fold Change", y = "SSMD",
         title = paste("SSMD vs log2FC for", group1, "vs", group2)) +
    theme_minimal()
  
  if (!is.null(output_file)) {
    if (!is.null(progress)) progress$inc(0.1, detail = "Saving plot to file")
    ext <- tools::file_ext(output_file)
    if (tolower(ext) == "pdf") {
      pdf(file = output_file, width = 7, height = 5)
      print(p)
      dev.off()
    } else if (tolower(ext) %in% c("png", "jpg", "jpeg")) {
      png(filename = output_file, res = 300, width = 2100, height = 1500, units = "px")
      print(p)
      dev.off()
    } else {
      warning("Unknown file extension; defaulting to PDF")
      pdf(file = output_file, width = 7, height = 5)
      print(p)
      dev.off()
    }
    if (!is.null(progress)) progress$inc(0.05, detail = "Plot saved")
    return(invisible(NULL))
  } else {
    if (!is.null(progress)) progress$inc(0.05, detail = "Returning plot")
    return(p)
  }
}

