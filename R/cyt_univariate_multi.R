#' Univariate Tests for Multi‑Level Categorical Predictors
#'
#' @description
#' `cyt_univariate_multi`provides univariate statistical testing for
#' categorical predictors with more than two levels.  For each
#' categorical predictor and numeric outcome pair, a global test is
#' performed, followed by pairwise comparisons if the global test is
#' significant.  Users may choose between two methods,
#' classical ANOVA with Tukey's Honest Significant Difference (HSD)
#' or a non‑parametric Kruskal–Wallis test followed by pairwise
#' Wilcoxon rank–sum tests.  The return format can either be a list
#' of adjusted p‑values for each outcome–predictor pair or, if
#' `format_output = TRUE`, a tidy data frame summarizing all
#' pairwise comparisons.
#'
#' @param data A data frame or matrix containing both categorical
#'   and continuous variables.  Character columns will be converted
#'   to factors.
#' @param method Character specifying the type of global test to
#'   perform.  Use "anova" (default) for one‑way ANOVA with Tukey
#'   HSD or "kruskal" for Kruskal–Wallis with pairwise Wilcoxon
#'   tests.
#' @param cat_vars Optional character vector of predictor column
#'   names.  When `NULL`, all factor or character columns in `data`
#'   are used.
#' @param cont_vars Optional character vector of numeric outcome
#'   variable names.  When `NULL`, all numeric columns in `data` are
#'   used.
#' @param p_adjust_method Character string specifying the method for
#'   p‑value adjustment across pairwise comparisons.  Passed to
#'   `p.adjust`.  Default is "BH".
#' @param format_output Logical.  If `TRUE`, returns a tidy data
#'   frame; otherwise (default) returns a list of numeric vectors
#'   keyed by "Outcome_Categorical".  Each numeric vector contains
#'   adjusted p‑values for the pairwise comparisons.
#' @param progress Optional. A Shiny \code{Progress} object for reporting
#'   progress updates during the analysis.
#' @return Either a list (if `format_output = FALSE`) or a data
#'   frame (if `format_output = TRUE`).
#' @examples
#' data("ExampleData1")
#' cyt_univariate_multi(ExampleData1[, c(1:2, 5:6)], method = "kruskal",
#'                      format_output = TRUE)
#' @author Shubh Saraswat
#' @importFrom stats aov as.formula kruskal.test pairwise.wilcox.test TukeyHSD
#' @export
cyt_univariate_multi <- function(
  data,
  method = c("anova", "kruskal"),
  cat_vars = NULL,
  cont_vars = NULL,
  p_adjust_method = "BH",
  format_output = FALSE,
  progress = NULL
) {
  if (!is.null(progress)) {
    progress$set(
      message = "Starting multi-level univariate tests...",
      value = 0
    )
  }

  names(data) <- make.names(names(data), unique = TRUE)
  method <- match.arg(method)
  # Convert to data frame
  x1_df <- as.data.frame(data)

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Converting character columns to factors")
  }
  # Convert character variables to factors
  char_cols <- names(x1_df)[sapply(x1_df, is.character)]
  if (length(char_cols) > 0) {
    x1_df[char_cols] <- lapply(x1_df[char_cols], as.factor)
  }

  if (!is.null(progress)) {
    progress$inc(
      0.05,
      detail = "Identifying categorical predictors and outcomes"
    )
  }
  # Identify categorical predictors
  if (is.null(cat_vars)) {
    cat_vars <- names(x1_df)[sapply(x1_df, is.factor)]
  }
  # Identify continuous variables
  if (is.null(cont_vars)) {
    cont_vars <- names(x1_df)[sapply(x1_df, is.numeric)]
  }

  # Pre-compute per-iteration increment so the bar fills smoothly
  valid_cats <- cat_vars[vapply(
    cat_vars,
    function(v) {
      n <- length(levels(x1_df[[v]]))
      n > 1L && !(method == "anova" && n > 10L)
    },
    logical(1)
  )]
  total_iter <- length(valid_cats) * length(cont_vars)
  iter_inc <- if (total_iter > 0) 0.70 / total_iter else 0

  # Initialize list to store p-value vectors
  test_results <- list()

  for (cat_var in cat_vars) {
    # Skip predictors with insufficient levels
    levs <- levels(x1_df[[cat_var]])
    n_lvls <- length(levs)
    if (n_lvls <= 1) {
      next
    }
    # For ANOVA restrict to <=10 levels (as original)
    if (method == "anova" && n_lvls > 10) {
      next
    }
    for (outcome in cont_vars) {
      if (!is.null(progress)) {
        progress$inc(
          iter_inc,
          detail = paste0("Testing: ", outcome, " ~ ", cat_var)
        )
      }

      key <- paste(outcome, cat_var, sep = "_")
      if (method == "anova") {
        model <- stats::aov(
          stats::as.formula(paste(outcome, "~", cat_var)),
          data = x1_df
        )
        tuk <- TukeyHSD(model)[[cat_var]]
        p_vals <- tuk[, "p adj"]
        names(p_vals) <- rownames(tuk)
        test_results[[key]] <- round(p_vals, 4)
      } else {
        # Kruskal-Wallis global test
        kw <- stats::kruskal.test(
          stats::as.formula(paste(outcome, "~", cat_var)),
          data = x1_df
        )
        # Only perform pairwise comparisons if the global test is significant
        if (is.finite(kw$p.value) && kw$p.value < 0.05) {
          # Pairwise Wilcoxon with adjustment
          pw <- stats::pairwise.wilcox.test(
            x1_df[[outcome]],
            x1_df[[cat_var]],
            p.adjust.method = p_adjust_method
          )
          p_mat <- pw$p.value
          # Flatten symmetric matrix into named vector (remove NA values)
          pairs <- NULL
          pvals <- NULL
          for (i in seq_len(nrow(p_mat))) {
            for (j in seq_len(ncol(p_mat))) {
              if (!is.na(p_mat[i, j])) {
                pairs <- c(
                  pairs,
                  paste0(rownames(p_mat)[i], "-", colnames(p_mat)[j])
                )
                pvals <- c(pvals, p_mat[i, j])
              }
            }
          }
          names(pvals) <- pairs
          # Round values
          test_results[[key]] <- round(pvals, 4)
        } else {
          # Global test not significant: no pairwise comparisons returned
          test_results[[key]] <- numeric(0)
        }
      }
    }
  }

  # If no tests performed
  if (length(test_results) == 0) {
    warning(
      "No valid comparisons were performed. ",
      "Check that your data has numeric columns and factors with sufficient levels."
    )
    if (!format_output) {
      return(list())
    } else {
      return(data.frame(
        Outcome = character(),
        Categorical = character(),
        Comparison = character(),
        P_adj = numeric(),
        stringsAsFactors = FALSE
      ))
    }
  }

  if (!format_output) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Done")
    }
    return(test_results)
  }

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Formatting results table")
  }

  # Create tidy data frame
  rows <- list()
  for (outcome in cont_vars) {
    for (cat_var in cat_vars) {
      key <- paste(outcome, cat_var, sep = "_")
      if (!key %in% names(test_results)) {
        next
      }
      p_vec <- test_results[[key]]
      for (comp in names(p_vec)) {
        rows[[length(rows) + 1L]] <- data.frame(
          Outcome = outcome,
          Categorical = cat_var,
          Comparison = comp,
          P_adj = p_vec[comp],
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }
    }
  }
  out_df <- if (length(rows) > 0L) {
    do.call(rbind, rows)
  } else {
    data.frame(
      Outcome = character(),
      Categorical = character(),
      Comparison = character(),
      P_adj = numeric(),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Complete")
  }

  out_df
}
