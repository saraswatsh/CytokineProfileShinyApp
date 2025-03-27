#' Heat Map.
#'
#' @param data A data frame containing the input data. Only numeric columns will be used.
#' @param scale Character. An optional scaling option. If set to "log2", numeric data will be log2-transformed (with non-positive values set to NA). Default is NULL.
#' @param annotation_col_name Character. Optional column name from data to be used for annotation colors.
#' @param output_file Optional. A file path to save the plot. If NULL, the function creates a temporary PNG file and returns its path.
#'
#' @return If output_file is NULL, returns a character string with the file path of the PNG. Otherwise, writes the plot and returns NULL invisibly.
#'
#' @examples
#' \dontrun{
#' # Interactive mode:
#' temp_file <- cyt_heatmap(cytodata[, -c(1,3,4)], scale = "log2", annotation_col_name = "Group", output_file = NULL)
#' # temp_file will contain the path to the generated PNG.
#'
#' # Download mode:
#' cyt_heatmap(cytodata[, -c(1,3,4)], scale = "log2", annotation_col_name = "Group", output_file = "myheatmap.pdf")
#' }
#'
#' @importFrom gplots heatmap.2
#' @export
cyt_heatmap <- function(data, scale = NULL, annotation_col_name = NULL, output_file = NULL, progress = NULL) {
  if (!is.null(progress)) progress$inc(0.05, detail = "Validating input data")
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame.")
  }
  
  if (!is.null(progress)) progress$inc(0.05, detail = "Subsetting numeric columns")
  numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
  if (ncol(data) != ncol(numeric_data)) {
    warning("Non-numeric columns detected. Subsetting to numeric columns only.")
  }
  
  if (!is.null(progress)) progress$inc(0.05, detail = "Applying log2 transformation if requested")
  if (!is.null(scale) && scale == "log2") {
    numeric_data[numeric_data <= 0] <- NA
    numeric_data <- log2(numeric_data)
  }
  
  if (!is.null(progress)) progress$inc(0.05, detail = "Determining output file")
  if (is.null(output_file)) {
    output_file <- file.path(tempdir(), paste0("cyt_heatmap_", Sys.Date(), ".png"))
  }
  
  ext <- tools::file_ext(output_file)
  if (!is.null(progress)) progress$inc(0.05, detail = "Opening graphics device")
  if (tolower(ext) == "png") {
    png(filename = output_file, res = 300, width = 2100, height = 2100, units = "px")
  } else if (tolower(ext) == "pdf") {
    pdf(file = output_file, width = 7, height = 7)
  } else {
    stop("Output file must have extension .png or .pdf")
  }
  
  if (!is.null(progress)) progress$inc(0.05, detail = "Building heatmap arguments")
  heatmap_args <- list(
    as.matrix(numeric_data),
    distfun = function(x) dist(x, method = "euclidean"),
    hclustfun = function(x) hclust(x, method = "complete"),
    dendrogram = "both",
    trace = "column",
    key = TRUE,
    cexCol = 1,
    margins = c(10, 10)
  )
  
  if (!is.null(annotation_col_name) && annotation_col_name %in% names(data)) {
    if (!is.null(progress)) progress$inc(0.05, detail = "Adding annotation colors")
    ann_data <- as.factor(data[[annotation_col_name]])
    if (length(ann_data) >= ncol(numeric_data)) {
      num_levels <- length(levels(ann_data))
      side_colors <- rainbow(num_levels)[as.integer(ann_data[seq_len(ncol(numeric_data))])]
      heatmap_args$ColSideColors <- side_colors
    } else {
      warning("Length of annotation column is less than number of numeric columns. Skipping annotation colors.")
    }
  }
  
  if (!is.null(progress)) progress$inc(0.1, detail = "Generating heatmap")
  do.call(gplots::heatmap.2, heatmap_args)
  
  if (dev.cur() > 1) dev.off()
  if (!is.null(progress)) progress$inc(0.05, detail = "Closing graphics device")
  if (!is.null(progress)) progress$inc(0.05, detail = "Returning output file path")
  return(output_file)
}
