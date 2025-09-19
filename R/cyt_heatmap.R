#' Heat Map.
#'
#' @param data A data frame containing the input data. Only numeric columns will be used.
#' @param scale Character. Optional scaling method. One of NULL (no scaling), "log2" (log2 transformation), "row_zscore" (z-score normalization by row), or "col_zscore" (z-score normalization by column). Default is NULL.
#' @param annotation_col Optional. A character string specifying a column name in `data` or a vector of annotations matching the number of rows or columns in `data` for adding colored annotations to the heatmap.
#' @param annotation_side Character. Specifies whether the annotation should be applied to rows or columns. One of "auto" (automatically determine based on length), "row", or "col". Default is "auto".
#'
#' @return A pheatmap object representing the generated heatmap.
#' @examples
#' # Load sample data
#' data("ExampleData1")
#' data_df <- ExampleData1
#' # Generate a heatmap with log2 scaling and annotation based on
#' # the "Group" column
#' cyt_heatmap(
#'   data = data_df[, -c(2:3)],
#'   scale = "log2",  # Optional scaling
#'   annotation_col = "Group"
#' )
#'
#' @importFrom pheatmap pheatmap
#' @export
cyt_heatmap <- function(
  data,
  scale = c(NULL, "log2", "row_zscore", "col_zscore"),
  annotation_col = NULL,
  annotation_side = c("auto", "row", "col"),
  progress = NULL
) {
  # progress: validate
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Validating input data")
  }

  if (is.null(scale)) {
    scale <- "none"
  } else {
    scale <- match.arg(scale, c("log2", "row_zscore", "col_zscore", "none"))
  }
  annotation_side <- match.arg(annotation_side)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }

  # progress: subset numeric
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Subsetting numeric columns")
  }
  num <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
  if (!ncol(num)) {
    stop("No numeric columns found in `data`.")
  }

  mat <- as.matrix(num)
  if (is.null(rownames(mat))) {
    rownames(mat) <- seq_len(nrow(mat))
  }
  if (is.null(colnames(mat))) {
    colnames(mat) <- paste0("V", seq_len(ncol(mat)))
  }

  # progress: scaling / transform
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Applying log2 transformation if requested")
  }
  pm_scale <- "none"
  if (identical(scale, "log2")) {
    # mirror package behavior (small offset to avoid log(0))
    mat <- log2(mat + 0.005)
  } else if (identical(scale, "row_zscore")) {
    pm_scale <- "row"
  } else if (identical(scale, "col_zscore")) {
    pm_scale <- "column"
  }

  # Build annotations (package logic)
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Building heatmap arguments")
  }

  ann_row <- ann_col <- NULL
  ann_colors <- NULL
  ann_title <- NULL

  if (!is.null(annotation_col)) {
    if (
      is.character(annotation_col) &&
        length(annotation_col) == 1 &&
        annotation_col %in% names(data)
    ) {
      ann <- factor(data[[annotation_col]])
      ann_title <- annotation_col
    } else if (length(annotation_col) %in% c(nrow(mat), ncol(mat))) {
      ann <- factor(annotation_col)
      ann_title <- "Annotation"
    } else {
      ann <- NULL
      warning(
        "`annotation_col` must be a column in `data` or a vector matching rows or columns; skipping."
      )
    }

    if (!is.null(ann)) {
      # progress: add annotation colors
      if (!is.null(progress)) {
        progress$inc(0.05, detail = "Adding annotation colors")
      }

      side <- if (annotation_side == "auto") {
        if (length(ann) == nrow(mat)) {
          "row"
        } else if (length(ann) == ncol(mat)) {
          "col"
        } else {
          "row"
        }
      } else {
        annotation_side
      }

      levs <- levels(ann)
      cols <- grDevices::rainbow(length(levs))
      cmap <- stats::setNames(cols, levs)

      if (side == "row" && length(ann) == nrow(mat)) {
        ann_row <- stats::setNames(
          data.frame(ann, row.names = rownames(mat)),
          ann_title
        )
        ann_colors <- list()
        ann_colors[[ann_title]] <- cmap
      } else if (side == "col" && length(ann) == ncol(mat)) {
        ann_col <- stats::setNames(
          data.frame(ann, row.names = colnames(mat)),
          ann_title
        )
        ann_colors <- list()
        ann_colors[[ann_title]] <- cmap
      } else {
        warning(
          "`annotation_col` length does not match the chosen side; skipping annotation."
        )
      }
    }
  }

  # progress: generating heatmap
  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Generating heatmap")
  }
  p <- pheatmap::pheatmap(
    mat,
    scale = pm_scale,
    color = grDevices::colorRampPalette(c("#253494", "#f7f7f7", "#b30000"))(
      255
    ),
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    border_color = NA,
    annotation_row = ann_row,
    annotation_col = ann_col,
    annotation_colors = ann_colors,
    legend = TRUE,
    annotation_legend = TRUE,
    silent = TRUE
  )

  invisible(return(p))
}
