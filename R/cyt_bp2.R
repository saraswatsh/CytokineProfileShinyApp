#' Boxplot Function Enhanced for Specific Group Comparisons.
#'
#' This function generates boxplots for each combination of numeric and factor variables in the provided data.
#' Character columns are converted to factors and the function checks that the data contains at least one numeric
#' and one factor column. If the `scale` argument is set to "log2", numeric columns are log2-transformed.
#' The function then creates boxplots using ggplot2 for each numeric variable grouped by each factor variable.
#' If `output_file` is provided, the plots are saved to that PDF file; otherwise, a list of ggplot objects is returned.
#'
#' @param data A matrix or data frame of raw data.
#' @param output_file Optional. A string representing the file path for the PDF file to be created.
#'   If NULL (default), the function returns a list of ggplot objects.
#' @param mf_row A numeric vector of length two specifying the layout (rows and columns) for the plots in the PDF output.
#'   Defaults to c(1, 1). (Ignored when returning ggplot objects.)
#' @param scale Transformation option for continuous variables. Options are NULL (default) and "log2".
#'   When set to "log2", numeric columns are transformed using the log2 function.
#' @param y_lim An optional numeric vector defining the y-axis limits for the plots.
#'
#' @return If `output_file` is NULL, returns a list of ggplot objects (named as "num_vs_factor" for each combination).
#'   If `output_file` is provided, a PDF file is created and the function returns NULL invisibly.
#'
#' @examples
#' # Loading data
#' data_df <- ExampleData1[, -c(1, 4)]
#'
#' # To display the boxplots interactively:
#' plots <- cyt_bp2(data_df, scale = "log2")
#' print(plots[["IL6_vs_Group"]])
#'
#' # To save the boxplots to a PDF file:
#' cyt_bp2(data_df, output_file = "boxplot2_output.pdf", scale = "log2")
#'
#' @import ggplot2
#' @import dplyr
#' @export
cyt_bp2 <- function(data, output_file = NULL, mf_row = c(1, 1),
                    scale = NULL, y_lim = NULL, progress = NULL) {
  if (!is.null(progress)) progress$inc(0.05, detail = "Converting data to data frame")
  data <- as.data.frame(data)
  char_cols <- sapply(data, is.character)
  if (any(char_cols)) {
    if (!is.null(progress)) progress$inc(0.05, detail = "Converting characters to factors")
    data[char_cols] <- lapply(data[char_cols], as.factor)
  }
  
  if (!is.null(progress)) progress$inc(0.05, detail = "Identifying numeric and factor columns")
  num_cols <- names(data)[sapply(data, is.numeric)]
  fac_cols <- names(data)[sapply(data, function(x) is.factor(x))]
  
  if (length(num_cols) == 0) stop("Data must contain at least one numeric column")
  if (length(fac_cols) == 0) stop("Data must contain at least one factor column")
  
  if (!is.null(scale) && scale == "log2") {
    if (!is.null(progress)) progress$inc(0.05, detail = "Applying log2 transformation")
    data[num_cols] <- lapply(data[num_cols], function(x) {
      x[x <= 0] <- NA
      log2(x)
    })
  }
  
  plot_list <- list()
  if (!is.null(progress)) progress$inc(0.05, detail = "Generating boxplots")
  for (num in num_cols) {
    for (fac in fac_cols) {
      plot_data <- data %>%
        select(!!num, !!fac) %>%
        rename(Outcome = !!num, Group = !!fac)
      
      p <- ggplot(plot_data, aes(x = Group, y = Outcome, fill = Group)) +
        geom_boxplot(alpha = 0.5) +
        geom_jitter(width = 0.2, alpha = 0.5) +
        labs(title = paste(num, "vs", fac),
             x = fac, y = num) +
        theme_minimal() +
        theme(legend.position = "none")
      
      if (!is.null(y_lim)) {
        p <- p + coord_cartesian(ylim = y_lim)
      }
      
      plot_name <- paste(num, "vs", fac, sep = "_")
      plot_list[[plot_name]] <- p
      if (!is.null(progress))
        progress$inc(0.01, detail = paste("Processed", plot_name))
    }
  }
  
  if (!is.null(output_file)) {
    if (!is.null(progress)) progress$inc(0.05, detail = "Saving plots to PDF")
    pdf(file = output_file, width = 7, height = 5)
    old_par <- par(mfrow = mf_row)
    on.exit(par(old_par), add = TRUE)
    for (p in plot_list) {
      print(p)
    }
    dev.off()
    if (!is.null(progress)) progress$inc(0.05, detail = "PDF saved")
    return(invisible(NULL))
  } else {
    if (!is.null(progress)) progress$inc(0.05, detail = "Returning list of plots")
    return(plot_list)
  }
}

