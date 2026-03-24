# Utility functions used throughout the CytoProfile package.

read_uploaded_flat_file <- function(datapath, ext) {
  ext <- tolower(ext)

  read_args <- switch(
    ext,
    csv = list(
      input = datapath,
      data.table = FALSE,
      check.names = FALSE
    ),
    txt = list(
      input = datapath,
      sep = "\t",
      data.table = FALSE,
      check.names = FALSE
    ),
    NULL
  )

  if (is.null(read_args)) {
    stop(
      sprintf("Unsupported uploaded flat-file extension: %s", ext),
      call. = FALSE
    )
  }

  tryCatch(
    do.call(data.table::fread, read_args),
    error = function(e) {
      stop(
        sprintf(
          "Failed to read uploaded %s file: %s",
          toupper(ext),
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

summarize_invalid_numeric_columns <- function(data, columns = NULL) {
  data <- as.data.frame(data)

  if (is.null(columns)) {
    columns <- names(data)[vapply(data, is.numeric, logical(1))]
  } else {
    columns <- intersect(columns, names(data))
  }

  if (!length(columns)) {
    return(data.frame(
      column = character(),
      issues = character(),
      stringsAsFactors = FALSE
    ))
  }

  issue_rows <- lapply(columns, function(col) {
    values <- data[[col]]
    issues <- character()
    na_mask <- is.na(values)
    nan_mask <- is.nan(values)

    if (any(na_mask & !nan_mask)) {
      issues <- c(issues, "NA")
    }
    if (any(nan_mask, na.rm = TRUE)) {
      issues <- c(issues, "NaN")
    }
    if (any(is.infinite(values) & values > 0, na.rm = TRUE)) {
      issues <- c(issues, "Inf")
    }
    if (any(is.infinite(values) & values < 0, na.rm = TRUE)) {
      issues <- c(issues, "-Inf")
    }

    if (!length(issues)) {
      return(NULL)
    }

    data.frame(
      column = col,
      issues = paste(unique(issues), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })

  issue_rows <- Filter(Negate(is.null), issue_rows)
  if (!length(issue_rows)) {
    return(data.frame(
      column = character(),
      issues = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, issue_rows)
}

format_invalid_numeric_summary <- function(issue_df) {
  if (is.null(issue_df) || !nrow(issue_df)) {
    return("")
  }

  paste(
    sprintf("%s [%s]", issue_df$column, issue_df$issues),
    collapse = "; "
  )
}

safe_zscore_column <- function(x) {
  observed <- x[!is.na(x)]

  if (!length(observed)) {
    stop(
      "Z-score transformation cannot be applied to columns that contain only missing values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(observed))) {
    stop(
      "Z-score transformation requires all non-missing selected values to be finite.",
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

#' Apply a scale transformation to numeric columns
#'
#' This helper function applies a chosen scaling or transformation to
#' specified numeric columns in a data frame.  Supported built-in
#' transformations include no transformation ("none"), log2, log10,
#' and z-score scaling.  A custom function can also be supplied to
#' perform arbitrary transformations.
#'
#' @param data A data.frame or matrix containing the data to be
#'   transformed.
#' @param columns A character vector of column names to transform.  If
#'   NULL (default) all numeric columns will be transformed.
#' @param scale A character string specifying the transformation to
#'   apply.  Possible values are "none", "log2", "log10",
#'   "zscore", or "custom".  When set to "custom" the function
#'   specified in `custom_fn` will be applied to the columns.
#' @param custom_fn A function that takes a numeric vector and returns a
#'   transformed numeric vector.  Only used when `scale = "custom"`.
#' @return A data.frame with the same dimensions as `data` with
#'   transformed numeric columns.
#' @importFrom stats p.adjust
#' @export
apply_scale <- function(
  data,
  columns = NULL,
  scale = c("none", "log2", "log10", "zscore", "custom"),
  custom_fn = NULL
) {
  data <- as.data.frame(data)
  scale <- match.arg(scale)

  # Identify numeric columns if none supplied
  if (is.null(columns)) {
    num_cols <- names(data)[sapply(data, is.numeric)]
  } else {
    num_cols <- columns
  }

  # Bail early if no numeric columns
  if (length(num_cols) == 0) {
    return(data)
  }

  missing_cols <- setdiff(num_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "These columns were requested for transformation but are missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (scale %in% c("log2", "log10")) {
    invalid_cols <- vapply(
      num_cols,
      function(col) {
        vals <- data[[col]]
        any(!is.na(vals) & (!is.finite(vals) | vals <= 0))
      },
      logical(1)
    )

    if (any(invalid_cols)) {
      stop(
        sprintf(
          "The %s transformation requires all non-missing selected values to be finite and greater than 0. Fix these columns first: %s",
          scale,
          paste(num_cols[invalid_cols], collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (identical(scale, "zscore")) {
    zscore_nonfinite_cols <- vapply(
      num_cols,
      function(col) {
        vals <- data[[col]]
        any(!is.na(vals) & !is.finite(vals))
      },
      logical(1)
    )
    zscore_all_missing_cols <- vapply(
      num_cols,
      function(col) all(is.na(data[[col]])),
      logical(1)
    )

    if (any(zscore_nonfinite_cols) || any(zscore_all_missing_cols)) {
      parts <- character()
      if (any(zscore_nonfinite_cols)) {
        parts <- c(
          parts,
          paste(
            "non-finite non-missing values:",
            paste(num_cols[zscore_nonfinite_cols], collapse = ", ")
          )
        )
      }
      if (any(zscore_all_missing_cols)) {
        parts <- c(
          parts,
          paste(
            "only missing values:",
            paste(num_cols[zscore_all_missing_cols], collapse = ", ")
          )
        )
      }

      stop(
        paste(
          "The zscore transformation requires finite numeric values and at least one observed value per selected column.",
          paste(parts, collapse = "; ")
        ),
        call. = FALSE
      )
    }
  }

  # Transformation functions
  trans_fun <- switch(
    scale,
    none = function(x) x,
    log2 = function(x) log2(x),
    log10 = function(x) log10(x),
    zscore = safe_zscore_column,
    custom = {
      if (is.null(custom_fn) || !is.function(custom_fn)) {
        stop(
          "When scale = 'custom', a valid function must be provided to 'custom_fn'."
        )
      }
      custom_fn
    }
  )

  for (col in num_cols) {
    data[[col]] <- trans_fun(data[[col]])
  }
  data
}

#' Adjust p-values using a specified method
#'
#' A thin wrapper around `stats::p.adjust` that defaults to the
#' Benjamini-Hochberg procedure.  Useful for unifying multiple
#' testing adjustments across the package.
#'
#' @param p_values A numeric vector of raw p-values.
#' @param method A character string specifying the p-value adjustment
#'   method.  Passed directly to `p.adjust`.  Defaults to "BH"
#'   (Benjamini-Hochberg).  See `p.adjust.methods` for other
#'   options.
#' @return A numeric vector of adjusted p-values of the same length as
#'   `p_values`.
#' @export
adjust_p <- function(p_values, method = "BH") {
  p.adjust(p_values, method = method)
}
