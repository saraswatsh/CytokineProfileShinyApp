#' Boxplots for Overall Comparisons by Continuous Variables.
#'
#' This function creates boxplots for the continuous variables in the provided data.
#' If the number of columns in `data` exceeds `bin_size`, the plots are split across multiple
#' chunks. If an `output_file` is provided, the function writes the plots to that PDF file;
#' otherwise, it returns a list of ggplot objects.
#'
#' @param data A matrix or data frame containing the raw data to be plotted.
#' @param output_file Optional. A file path to save the plots as a PDF file. If NULL, the function
#' returns a list of ggplot objects.
#' @param bin_size An integer specifying the maximum number of box plots to display per chunk.
#' @param mf_row A numeric vector of length two specifying the layout (rows and columns) in the PDF
#' output. (Not used when returning ggplot objects.)
#' @param y_lim An optional numeric vector defining the y-axis limits for the plots.
#' @param scale An optional character string. If set to "log2", numeric columns are log2-transformed.
#'
#' @return If \code{output_file} is NULL, a list of ggplot objects; otherwise, writes a PDF and returns NULL.
#'
#' @examples
#' data.df <- cytodata
#' # To display the boxplots natively:
#' plots <- cyt_bp(data.df, bin_size = 10, scale = "log2")
#' print(plots[[1]])
#'
#' # To save the boxplots to a PDF file:
#' cyt_bp(data.df, output_file = "boxplot_output.pdf", scale = "log2")
#'
#' @import ggplot2
#' @import reshape2
#' @export
cyt_bp <- function(data, output_file = NULL, bin_size = 25,
                   mf_row = c(1, 1), y_lim = NULL, scale = NULL, progress = NULL) {
  if (!is.null(progress)) progress$inc(0.05, detail = "Converting data to data frame")
  data <- as.data.frame(data)
  
  if (!is.null(scale) && scale == "log2") {
    if (!is.null(progress)) progress$inc(0.05, detail = "Applying log2 transformation")
    numeric_cols <- sapply(data, is.numeric)
    for (col in names(data)[numeric_cols]) {
      data[[col]][data[[col]] <= 0] <- NA
      data[[col]] <- log2(data[[col]])
    }
  }
  
  n_col <- ncol(data)
  if (n_col == 0) {
    stop("No columns to plot in 'data'.")
  }
  
  n_chunks <- ceiling(n_col / bin_size)
  plot_list <- list()
  
  if (!is.null(progress)) progress$inc(0.05, detail = "Generating boxplots")
  for (i in seq_len(n_chunks)) {
    start_idx <- (i - 1) * bin_size + 1
    end_idx   <- min(i * bin_size, n_col)
    chunk_cols <- names(data)[start_idx:end_idx]
    
    melted <- reshape2::melt(data[, chunk_cols, drop = FALSE],
                   measure.vars = chunk_cols,
                   variable.name = "Variable",
                   value.name = "Value")
    
    p <- ggplot(melted, aes(x = Variable, y = Value)) +
      geom_boxplot() +
      labs(title = paste("Boxplots for columns:", paste(chunk_cols, collapse = ", ")),
           x = "Variable", y = "Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    if (!is.null(y_lim)) {
      p <- p + coord_cartesian(ylim = y_lim)
    }
    plot_list[[i]] <- p
    if (!is.null(progress))
      progress$inc(0.05 / n_chunks, detail = paste("Chunk", i, "of", n_chunks))
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

