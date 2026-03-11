#' Pairwise Univariate Tests Between Two Groups
#'
#' @description
#' `cyt_univariate` supports additional scaling options and explicit
#' choice of statistical test.
#' For each categorical predictor with exactly two levels and each
#' numeric outcome, a two‑sample t‑test or Wilcoxon rank–sum test is
#' performed.  Results are returned either as a list of test objects
#' or, if `format_output = TRUE`, as a tidy data frame with one
#' row per comparison.
#'
#' @param data A data frame or matrix containing both categorical
#'   and numeric variables.
#' @param scale A character specifying a transformation to apply to
#'   numeric variables prior to testing.  Choices are `NULL` (no
#'   transformation), "log2", "log10", "zscore", or
#'   "custom".  When set to "custom", supply a function via
#'   `custom_fn`.
#' @param method Character specifying the test to perform.  Use
#'   "auto" (default) to select between t‑test and Wilcoxon based
#'   on Shapiro–Wilk normality tests for each outcome; "ttest" to
#'   always use Student's t‑test; or "wilcox" to always use the
#'   Wilcoxon rank–sum test.
#' @param verbose Logical indicating whether to return the results.
#'   Provided for backward compatibility but has no effect on printing.
#' @param format_output Logical.  If `TRUE`, returns the results as
#'   a tidy data frame; if `FALSE` (default), returns a list of
#'   test objects similar to the original function.
#' @param custom_fn A function to apply when `scale = "custom"`.
#' @param p_adjust_method Character or \code{NULL}. Method passed to
#'   \code{p.adjust()} for correcting p-values across all comparisons
#'   (e.g., \code{"BH"} for Benjamini-Hochberg). If \code{NULL} (default)
#'   no adjustment is performed.
#' @param progress Optional. A Shiny \code{Progress} object for reporting
#'   progress updates during the analysis.
#' @return If `format_output = FALSE`, a named list of test objects
#'   keyed by "Outcome_Categorical".  If `format_output = TRUE`, a
#'   data frame with columns `Outcome`, `Categorical`, `Comparison`,
#'   `Test`, `Estimate`, `Statistic`, and `P_value`.
#' @examples
#' data_df <- ExampleData1[, -c(3)]
#' data_df <- dplyr::filter(data_df, Group != "ND", Treatment != "Unstimulated")
#' cyt_univariate(data_df[, c(1:2, 5:6)], scale = "log2",
#'                method = "auto", format_output = TRUE)
#' @author Shubh Saraswat
#' @importFrom stats as.formula sd shapiro.test t.test wilcox.test
#' @export
cyt_univariate <- function(
  data,
  scale = NULL,
  method = c("auto", "ttest", "wilcox"),
  verbose = TRUE,
  format_output = FALSE,
  custom_fn = NULL,
  p_adjust_method = NULL,
  progress = NULL
) {
  if (!is.null(progress)) {
    progress$set(message = "Starting univariate tests...", value = 0)
  }

  names(data) <- make.names(names(data), unique = TRUE)
  method <- match.arg(method)
  # Convert to data frame
  x1_df <- as.data.frame(data)

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Converting character columns to factors")
  }
  # Convert character variables to factors
  cat_vars <- sapply(x1_df, is.character)
  if (any(cat_vars)) {
    x1_df[cat_vars] <- lapply(x1_df[cat_vars], as.factor)
  }
  # Identify categorical predictors and continuous variables
  cat_preds <- sapply(x1_df, is.factor)
  cont_vars <- sapply(x1_df, is.numeric)
  # Apply scaling using shared utility for consistency
  if (!is.null(scale)) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = paste("Applying", scale, "transformation"))
    }
    x1_df <- apply_scale(
      data = x1_df,
      scale = scale,
      custom_fn = custom_fn
    )
    # Recompute predictors and continuous-variable indicators after scaling
    cat_preds <- sapply(x1_df, is.factor)
    cont_vars <- sapply(x1_df, is.numeric)
  }

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Identifying predictors and outcomes")
  }

  # Empty list to store test results
  test_results <- list()

  # Pre-compute per-iteration increment so the bar fills smoothly
  two_level_cats <- names(x1_df)[cat_preds][
    vapply(
      names(x1_df)[cat_preds],
      function(v) length(levels(x1_df[[v]])) == 2L,
      logical(1)
    )
  ]
  total_iter <- length(two_level_cats) * sum(cont_vars)
  iter_inc <- if (total_iter > 0) 0.70 / total_iter else 0

  # Loop over categorical predictors and continuous outcomes
  for (cat_var in names(x1_df)[cat_preds]) {
    # Only consider predictors with exactly two levels
    if (length(levels(x1_df[[cat_var]])) != 2) {
      next
    }
    for (outcome in names(x1_df)[cont_vars]) {
      if (!is.null(progress)) {
        progress$inc(
          iter_inc,
          detail = paste0("Testing: ", outcome, " ~ ", cat_var)
        )
      }

      group_levels <- levels(x1_df[[cat_var]])
      group1 <- x1_df[[outcome]][x1_df[[cat_var]] == group_levels[1]]
      group2 <- x1_df[[outcome]][x1_df[[cat_var]] == group_levels[2]]
      # Check for sufficient data and variance in both groups
      if (length(group1) < 2 || length(group2) < 2) {
        warning(
          "Skipping test for ",
          outcome,
          " in ",
          cat_var,
          " due to insufficient data in one of the groups."
        )
        next
      } else if (stats::sd(group1) == 0 || stats::sd(group2) == 0) {
        warning(
          "Skipping test for ",
          outcome,
          " in ",
          cat_var,
          " due to low variance."
        )
        next
      }
      # Determine test to use
      test_used <- switch(
        method,
        auto = {
          p1 <- tryCatch(
            stats::shapiro.test(group1)$p.value,
            error = function(e) 0
          )
          p2 <- tryCatch(
            stats::shapiro.test(group2)$p.value,
            error = function(e) 0
          )
          if (p1 > 0.05 && p2 > 0.05) "ttest" else "wilcox"
        },
        ttest = "ttest",
        wilcox = "wilcox"
      )
      # Perform the test
      if (test_used == "ttest") {
        tt <- stats::t.test(
          stats::as.formula(paste(outcome, "~", cat_var)),
          data = x1_df
        )
      } else {
        tt <- stats::wilcox.test(
          stats::as.formula(paste(outcome, "~", cat_var)),
          data = x1_df,
          conf.int = TRUE,
          exact = FALSE
        )
      }
      key <- paste(outcome, cat_var, sep = "_")
      test_results[[key]] <- tt
    }
  }
  if (length(test_results) == 0) {
    warning("No valid tests were performed.")
    if (!format_output) {
      return(list())
    } else {
      return(data.frame(
        Outcome = character(),
        Categorical = character(),
        Comparison = character(),
        Test = character(),
        Estimate = numeric(),
        Statistic = numeric(),
        P_value = numeric(),
        stringsAsFactors = FALSE
      ))
    }
  }
  # Return results in tidy format if requested
  if (!format_output) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Done")
    }
    return(test_results)
  }

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Formatting results table")
  }

  safe_numeric <- function(x) {
    if (is.null(x)) {
      return(NA_real_)
    }

    value <- suppressWarnings(as.numeric(unname(x)[1]))
    if (!length(value) || is.na(value)) {
      return(NA_real_)
    }

    value
  }

  normalize_test_object <- function(x) {
    if (inherits(x, "htest") || is.list(x)) {
      return(x)
    }

    list(
      method = as.character(x)[1],
      estimate = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_
    )
  }

  # Format into data frame - iterate over variables directly so column names
  # with underscores are never mis-parsed from the composite key
  cat_var_names <- names(x1_df)[cat_preds]
  cont_var_names <- names(x1_df)[cont_vars]

  out_df <- do.call(
    rbind,
    lapply(cat_var_names, function(cat_var) {
      if (length(levels(x1_df[[cat_var]])) != 2L) {
        return(NULL)
      }
      do.call(
        rbind,
        lapply(cont_var_names, function(outcome) {
          key <- paste(outcome, cat_var, sep = "_")
          if (!key %in% names(test_results)) {
            return(NULL)
          }
          tt <- normalize_test_object(test_results[[key]])
          lvls <- levels(x1_df[[cat_var]])
          comp <- paste(lvls[1], "vs", lvls[2])
          est <- safe_numeric(tt$estimate)
          stat <- safe_numeric(tt$statistic)
          p_value <- safe_numeric(tt$p.value)
          test_name <- if (!is.null(tt$method)) {
            as.character(tt$method)[1]
          } else {
            NA_character_
          }
          data.frame(
            Outcome = outcome,
            Categorical = cat_var,
            Comparison = comp,
            Test = test_name,
            Estimate = est,
            Statistic = stat,
            P_value = p_value,
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )
  if (!is.null(p_adjust_method)) {
    if (!is.null(progress)) {
      progress$inc(0.03, detail = paste("Adjusting p-values:", p_adjust_method))
    }
    out_df$P_adj <- adjust_p(out_df$P_value, method = p_adjust_method)
  }

  out_df$Estimate <- round(out_df$Estimate, 3)
  out_df$Statistic <- round(out_df$Statistic, 3)
  out_df$P_value <- round(out_df$P_value, 3)
  if ("P_adj" %in% names(out_df)) {
    out_df$P_adj <- round(out_df$P_adj, 3)
  }

  if (!is.null(progress)) {
    progress$inc(0.02, detail = "Complete")
  }

  out_df
}
