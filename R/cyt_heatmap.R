#' Heat Map
#'
#' @description
#' This function creates a heatmap using the numeric columns from the provided
#' data frame.  It provides the ability to hide row and column names, adjust
#' font sizes and clustering, and apply additional transformations such as
#' log10 or combined z-scoring.  An optional \code{title} is displayed as the
#' plot title; an optional \code{filename} (ending in \code{.pdf} or
#' \code{.png}) saves the heat map to disk.  Both may be supplied together.
#'
#' @param data A data frame.  Only numeric columns are used to construct the
#'   heat map.
#' @param scale Character specifying an optional scaling.  One of \code{NULL}
#'   (no scaling), \code{"log2"}, \code{"log10"}, \code{"row_zscore"},
#'   \code{"col_zscore"}, or \code{"zscore"} (apply both row and column
#'   z-scoring).  Default is \code{NULL}.
#' @param annotation_col Optional.  Either the name of a column in \code{data}
#'   or a vector of length equal to the number of rows or columns of the
#'   numeric matrix.  If a column name is supplied the function determines
#'   whether it annotates rows or columns based on its length or the value of
#'   \code{annotation_side}.
#' @param annotation_side Character.  One of \code{"auto"} (default),
#'   \code{"row"}, or \code{"col"}.  When \code{"auto"} the side is determined
#'   by matching the length of \code{annotation_col} to rows or columns.
#' @param show_row_names Logical.  If \code{TRUE} row names are shown.
#'   Default is \code{FALSE}.
#' @param show_col_names Logical.  If \code{FALSE} column names are hidden.
#'   Default is \code{TRUE}.
#' @param fontsize_row Numeric.  Font size for row names.  Default is
#'   \code{10}.
#' @param fontsize_col Numeric.  Font size for column names.  Default is
#'   \code{10}.
#' @param font_settings Optional named list of font sizes for supported heatmap
#'   text elements.
#' @param cluster_rows Logical.  If \code{TRUE} (default), rows are clustered.
#' @param cluster_cols Logical.  If \code{TRUE} (default), columns are
#'   clustered.
#' @param title Character.  Optional title displayed at the top of the heat
#'   map.  If \code{NULL} (default), no title is printed.
#' @param filename Character.  Optional file path ending in \code{".pdf"} or
#'   \code{".png"} (case insensitive).  When supplied the heat map is written
#'   to disk and screen rendering is suppressed.  If \code{NULL} (default),
#'   the plot is drawn on the active graphics device.
#' @param progress Optional.  A Shiny \code{Progress} object for reporting
#'   progress updates.
#'
#' @return Invisibly returns the pheatmap object created by
#'   \code{pheatmap::pheatmap()}.
#'
#' @author Shubh Saraswat
#' @export
#' @importFrom pheatmap pheatmap
#' @importFrom grDevices colorRampPalette rainbow
#' @importFrom stats setNames sd
#'
#' @examples
#' data("ExampleData1")
#' data_df <- ExampleData1
#' cyt_heatmap(
#'   data            = data_df[, -c(2:3)],
#'   scale           = "log2",
#'   annotation_col  = "Group",
#'   annotation_side = "auto",
#'   title           = "Cytokine Heatmap"
#' )
cyt_heatmap <- function(
  data,
  scale = NULL,
  annotation_col = NULL,
  annotation_side = c("auto", "row", "col"),
  show_row_names = FALSE,
  show_col_names = TRUE,
  fontsize_row = 10,
  fontsize_col = 10,
  font_settings = NULL,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  title = NULL,
  filename = NULL,
  progress = NULL
) {
  # ── 0. Initialize ──────────────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$set(message = "Heatmap: initializing...", value = 0)
  }

  names(data) <- make.names(names(data), unique = TRUE)
  valid_scales <- c("log2", "log10", "row_zscore", "col_zscore", "zscore")
  if (!is.null(scale)) {
    if (
      !is.character(scale) || length(scale) != 1L || !scale %in% valid_scales
    ) {
      stop(
        sprintf(
          "`scale` must be NULL or one of: %s.",
          paste(valid_scales, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  annotation_side <- match.arg(annotation_side)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }
  resolved_fonts <- normalize_font_settings(
    font_settings = font_settings,
    supported_fields = c("base_size", "row_names", "col_names", "cell_text"),
    legacy = list(
      base_size = 10,
      row_names = fontsize_row,
      col_names = fontsize_col
    ),
    activate = !is.null(font_settings) ||
      !identical(fontsize_row, 10) ||
      !identical(fontsize_col, 10)
  )
  heatmap_font_args <- font_settings_heatmap_args(resolved_fonts)

  # ── 1. Extract numeric columns ─────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Subsetting numeric columns")
  }
  num <- data[, vapply(data, is.numeric, logical(1L)), drop = FALSE]
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

  # ── 2. Scaling ─────────────────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(
      0.05,
      detail = if (!is.null(scale)) {
        paste("Applying", scale, "transformation")
      } else {
        "No transformation requested"
      }
    )
  }

  pm_scale <- "none"
  if (!is.null(scale)) {
    if (scale == "log2") {
      num <- apply_scale(num, scale = "log2")
      mat <- as.matrix(num)
    } else if (scale == "log10") {
      num <- apply_scale(num, scale = "log10")
      mat <- as.matrix(num)
    } else if (scale == "row_zscore") {
      pm_scale <- "row"
    } else if (scale == "col_zscore") {
      pm_scale <- "column"
    } else if (scale == "zscore") {
      zscore_axis <- function(x, axis_label, axis_name) {
        observed <- x[!is.na(x)]

        if (!length(observed)) {
          stop(
            sprintf(
              "Heatmap %s cannot be applied because %s '%s' contains only missing values.",
              scale,
              axis_label,
              axis_name
            ),
            call. = FALSE
          )
        }

        if (any(!is.finite(observed))) {
          stop(
            sprintf(
              "Heatmap %s cannot be applied because %s '%s' contains non-finite values.",
              scale,
              axis_label,
              axis_name
            ),
            call. = FALSE
          )
        }

        mu <- mean(observed)
        sdv <- stats::sd(observed)
        if (is.na(sdv) || !is.finite(sdv) || sdv == 0) {
          return(x - mu)
        }

        (x - mu) / sdv
      }

      row_ids <- rownames(mat)
      col_ids <- colnames(mat)
      mat <- t(vapply(
        seq_len(nrow(mat)),
        function(i) zscore_axis(mat[i, ], "row", row_ids[[i]]),
        numeric(ncol(mat))
      ))
      rownames(mat) <- row_ids
      colnames(mat) <- col_ids
      mat <- vapply(
        seq_len(ncol(mat)),
        function(j) zscore_axis(mat[, j], "column", col_ids[[j]]),
        numeric(nrow(mat))
      )
      rownames(mat) <- row_ids
      colnames(mat) <- col_ids
    }
  }

  if (is.null(rownames(mat))) {
    rownames(mat) <- seq_len(nrow(mat))
  }
  if (is.null(colnames(mat))) {
    colnames(mat) <- paste0("V", seq_len(ncol(mat)))
  }

  invalid_mat <- summarize_invalid_numeric_columns(as.data.frame(mat))
  if (nrow(invalid_mat) > 0) {
    stop(
      paste(
        "Heatmap cannot be generated because the numeric matrix contains invalid values before clustering:",
        format_invalid_numeric_summary(invalid_mat),
        "No offset or silent cleanup is applied."
      ),
      call. = FALSE
    )
  }

  if (identical(pm_scale, "row")) {
    row_sd <- apply(mat, 1, stats::sd)
    invalid_rows <- rownames(mat)[
      is.na(row_sd) | !is.finite(row_sd) | row_sd == 0
    ]
    if (length(invalid_rows) > 0) {
      stop(
        paste(
          "Heatmap row_zscore scaling cannot be applied because these rows have zero or undefined variance:",
          paste(invalid_rows, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (identical(pm_scale, "column")) {
    col_sd <- apply(mat, 2, stats::sd)
    invalid_cols <- colnames(mat)[
      is.na(col_sd) | !is.finite(col_sd) | col_sd == 0
    ]
    if (length(invalid_cols) > 0) {
      stop(
        paste(
          "Heatmap col_zscore scaling cannot be applied because these columns have zero or undefined variance:",
          paste(invalid_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # ── 3. Annotation handling ─────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Building annotation")
  }

  ann_row <- ann_col <- NULL
  ann_colors <- NULL
  ann_title <- NULL

  if (!is.null(annotation_col)) {
    ann <- NULL
    if (
      is.character(annotation_col) &&
        length(annotation_col) == 1L &&
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
        "`annotation_col` must be a column in `data` or a vector matching rows or columns; skipping annotation."
      )
    }

    if (!is.null(ann)) {
      if (!is.null(progress)) {
        progress$inc(0.05, detail = "Adding annotation colors")
      }

      side <- if (annotation_side == "auto") {
        if (length(ann) == nrow(mat)) {
          "row"
        } else if (length(ann) == ncol(mat)) {
          "col"
        } else {
          NA_character_
        }
      } else {
        annotation_side
      }

      if (identical(side, "row") && length(ann) == nrow(mat)) {
        ann_row <- data.frame(
          annotation = ann,
          row.names = rownames(mat),
          check.names = FALSE
        )
        colnames(ann_row) <- ann_title
      } else if (identical(side, "col") && length(ann) == ncol(mat)) {
        ann_col <- data.frame(
          annotation = ann,
          row.names = colnames(mat),
          check.names = FALSE
        )
        colnames(ann_col) <- ann_title
      } else {
        warning(
          "`annotation_col` length does not match the chosen side; skipping annotation."
        )
      }

      built_annotation <- ann_row
      if (is.null(built_annotation)) {
        built_annotation <- ann_col
      }

      if (!is.null(built_annotation)) {
        ann_keys <- colnames(built_annotation)
        levs <- levels(ann)
        cmap <- stats::setNames(grDevices::rainbow(length(levs)), levs)
        ann_colors <- stats::setNames(
          vector("list", length(ann_keys)),
          ann_keys
        )
        ann_colors[[ann_keys[[1]]]] <- cmap
      }
    }
  }

  built_annotation <- ann_row
  if (is.null(built_annotation)) {
    built_annotation <- ann_col
  }

  if (!is.null(built_annotation)) {
    if (
      !is.list(ann_colors) ||
        is.null(names(ann_colors)) ||
        any(!nzchar(names(ann_colors)))
    ) {
      stop(
        "`annotation_colors` must be a named list keyed by the annotation column names.",
        call. = FALSE
      )
    }

    missing_color_keys <- setdiff(colnames(built_annotation), names(ann_colors))
    if (length(missing_color_keys) > 0) {
      stop(
        paste(
          "Annotation color mapping is incomplete for:",
          paste(missing_color_keys, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # ── 4. Resolve title and filename ─────────────────────────────────────────
  if (
    !is.null(filename) && !grepl("\\.(pdf|png)$", filename, ignore.case = TRUE)
  ) {
    stop("`filename` must end in '.pdf' or '.png'.", call. = FALSE)
  }
  main <- title %||% NA_character_
  silent <- !is.null(filename)

  # ── 5. Draw heatmap ────────────────────────────────────────────────────────
  if (!is.null(progress)) {
    progress$inc(0.10, detail = "Generating heatmap")
  }

  ph <- pheatmap::pheatmap(
    mat,
    scale = pm_scale,
    color = grDevices::colorRampPalette(
      c("#253494", "#f7f7f7", "#b30000")
    )(255),
    cluster_rows = cluster_rows,
    cluster_cols = cluster_cols,
    border_color = NA,
    annotation_row = ann_row,
    annotation_col = ann_col,
    annotation_colors = ann_colors,
    legend = TRUE,
    annotation_legend = TRUE,
    show_rownames = show_row_names,
    show_colnames = show_col_names,
    fontsize = if (is.null(resolved_fonts)) 10 else heatmap_font_args$fontsize,
    fontsize_row = if (is.null(resolved_fonts)) {
      fontsize_row
    } else {
      heatmap_font_args$fontsize_row
    },
    fontsize_col = if (is.null(resolved_fonts)) {
      fontsize_col
    } else {
      heatmap_font_args$fontsize_col
    },
    fontsize_number = if (is.null(resolved_fonts)) {
      0.8 * 10
    } else {
      heatmap_font_args$fontsize_number
    },
    filename = filename %||% NA,
    main = main,
    silent = silent
  )

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Complete")
  }

  invisible(ph)
}
