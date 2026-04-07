#' Internal Step 2 helpers
#'
#' Shared helper functions for Step 2 column typing, missing-value handling,
#' and selection restoration.
#'
#' @name step2_internal_helpers
#' @noRd
NULL

step2_missing_token_mask <- function(x) {
  x_chr <- trimws(as.character(x))
  !is.na(x_chr) & tolower(x_chr) %in% c("", "na", "n/a", "null", "nan")
}

step2_normalize_missing_tokens <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  x_chr <- as.character(x)
  x_chr[step2_missing_token_mask(x_chr)] <- NA_character_
  x_chr
}

step2_parse_numeric_values <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x_chr <- step2_normalize_missing_tokens(x)
  base <- gsub("\\*|,", "", x_chr)
  base <- gsub("(?i)\\bOOR\\s*[<>]", "", base, perl = TRUE)

  parsed <- suppressWarnings(as.numeric(gsub("[^0-9eE+\\-\\.]", "", base)))
  parsed[is.na(x_chr)] <- NA_real_
  parsed
}

step2_is_numeric_like <- function(
  x,
  min_fraction = 0.7,
  max_letter_fraction = 0.4
) {
  if (is.numeric(x)) {
    return(TRUE)
  }

  x_chr <- step2_normalize_missing_tokens(x)
  observed <- !is.na(x_chr) & nzchar(trimws(x_chr))
  if (!any(observed)) {
    return(FALSE)
  }

  oor_token <- observed & grepl("(?i)\\bOOR\\s*[<>]", x_chr, perl = TRUE)
  parsed <- step2_parse_numeric_values(x_chr)
  parsed_ok <- observed & (!is.na(parsed) | oor_token)

  no_oor_letters <- gsub("(?i)\\bOOR\\b", "", x_chr, perl = TRUE)
  has_letters <- observed & grepl("[A-Za-z]", no_oor_letters)

  prop_numeric <- mean(parsed_ok[observed])
  prop_letters <- mean(has_letters[observed])

  isTRUE(prop_numeric >= min_fraction && prop_letters < max_letter_fraction)
}

step2_conflicting_type_cols <- function(
  factor_cols = NULL,
  numeric_cols = NULL
) {
  factor_cols <- if (is.null(factor_cols)) character(0) else factor_cols
  numeric_cols <- if (is.null(numeric_cols)) character(0) else numeric_cols

  intersect(factor_cols, numeric_cols)
}

step2_restore_bucket_selection <- function(
  selected_columns,
  available_choices
) {
  available_choices <- unique(as.character(available_choices))

  if (is.null(selected_columns)) {
    return(available_choices)
  }

  intersect(selected_columns, available_choices)
}

step2_classify_columns <- function(df, exclude = "..cyto_id..") {
  all_cols <- setdiff(names(df), exclude)
  is_numeric_col <- vapply(df[all_cols], is.numeric, logical(1))

  list(
    all = all_cols,
    categorical = all_cols[!is_numeric_col],
    numerical = all_cols[is_numeric_col]
  )
}

step2_apply_type_overrides <- function(
  df,
  factor_cols = NULL,
  numeric_cols = NULL,
  factor_order_enable = FALSE,
  factor_order_col = NULL,
  factor_levels_csv = NULL
) {
  if (is.null(df) || !ncol(df)) {
    return(df)
  }

  factor_cols <- intersect(
    if (is.null(factor_cols)) character(0) else factor_cols,
    names(df)
  )
  numeric_cols <- intersect(
    if (is.null(numeric_cols)) character(0) else numeric_cols,
    names(df)
  )

  if (length(step2_conflicting_type_cols(factor_cols, numeric_cols))) {
    return(df)
  }

  if (length(numeric_cols)) {
    df[numeric_cols] <- lapply(df[numeric_cols], step2_parse_numeric_values)
  }

  if (length(factor_cols)) {
    df[factor_cols] <- lapply(df[factor_cols], function(x) {
      factor(step2_normalize_missing_tokens(x))
    })
  }

  factor_order_col <- if (length(factor_order_col)) {
    factor_order_col[[1]]
  } else {
    NULL
  }
  factor_levels_csv <- if (is.null(factor_levels_csv)) "" else factor_levels_csv

  if (
    isTRUE(factor_order_enable) &&
      !is.null(factor_order_col) &&
      factor_order_col %in% factor_cols &&
      nzchar(trimws(factor_levels_csv))
  ) {
    levs <- trimws(strsplit(factor_levels_csv, ",", fixed = TRUE)[[1]])
    levs <- unique(levs[nzchar(levs)])
    if (length(levs)) {
      df[[factor_order_col]] <- factor(
        as.character(df[[factor_order_col]]),
        levels = levs
      )
    }
  }

  df
}
