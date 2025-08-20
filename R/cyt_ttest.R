#' Two Sample T-test Comparisons.
#'
#' This function performs pairwise comparisons between two groups for each combination
#' of a categorical predictor (with exactly two levels) and a continuous outcome variable.
#' It first converts any character variables in \code{data} to factors and, if specified,
#' applies a log2 transformation to the continuous variables. The function conducts a Shapiro-Wilk
#' normality test to decide whether to use a Two-Sample T-Test vs. a Wilcoxon Rank Sum Test.
#' The resulting p-values are printed and returned.
#'
#' @param data A matrix or data frame containing continuous and categorical variables.
#' @param scale A character specifying a transformation for continuous variables.
#'   Options are \code{NULL} (default) and \code{"log2"}. When \code{scale = "log2"},
#'   a log2 transformation is applied and a two-sample t-test is used; when \code{scale} is \code{NULL},
#'   a Mann-Whitney U test is performed.
#' @param format_output Logical. If TRUE, returns the results as a tidy data frame.
#'   Default is FALSE.
#'
#' @return If \code{format_output} is FALSE, returns a list of p-values (named by Outcome and Categorical variable).
#'   If TRUE, returns a data frame with columns "Outcome", "Categorical", "Comparison", and "P_value".
#'
#' @examples
#' data_df <- ExampleData1[, -c(3)]
#' data_df <- dplyr::filter(data_df, Group != "ND", Treatment != "Unstimulated")
#'
#' cyt_ttest(data_df[, c(1, 2, 5:6)], scale = "log2", format_output = TRUE)
#' @export
cyt_ttest <- function(
  data,
  scale = NULL,
  format_output = FALSE,
  progress = NULL
) {
  x1_df <- as.data.frame(data)
  # convert chars to factors
  char_cols <- sapply(x1_df, is.character)
  if (any(char_cols)) {
    x1_df[char_cols] <- lapply(x1_df[char_cols], as.factor)
  }

  # Add scale if user inputs scale
  if (!is.null(scale) && scale == "log2") {
    num_cols <- sapply(x1_df, is.numeric)
    x1_df[num_cols] <- lapply(x1_df[num_cols], function(x) {
      log2(x)
    })
  }
  # loop over every factor with exactly 2 levels against each numeric
  test_results <- list()
  for (cat_var in names(x1_df)[sapply(x1_df, is.factor)]) {
    if (length(levels(x1_df[[cat_var]])) != 2) {
      next
    }
    for (outcome in names(x1_df)[sapply(x1_df, is.numeric)]) {
      lvls <- levels(x1_df[[cat_var]])
      g1 <- x1_df[[outcome]][x1_df[[cat_var]] == lvls[1]]
      g2 <- x1_df[[outcome]][x1_df[[cat_var]] == lvls[2]]
      if (length(g1) < 2 || length(g2) < 2) {
        next
      }

      # normality check
      p1 <- tryCatch(stats::shapiro.test(g1)$p.value, error = function(e) 0)
      p2 <- tryCatch(stats::shapiro.test(g2)$p.value, error = function(e) 0)

      # pick the test
      if (p1 > 0.05 && p2 > 0.05) {
        tt <- stats::t.test(
          stats::as.formula(paste(outcome, "~", cat_var)),
          data = x1_df
        )
      } else {
        tt <- stats::wilcox.test(
          stats::as.formula(paste(outcome, "~", cat_var)),
          data = x1_df,
          conf.int = TRUE
        )
      }

      key <- paste(outcome, cat_var, sep = "_")
      test_results[[key]] <- tt
    }
  }

  if (length(test_results) == 0) {
    return("No valid tests were performed.")
  }

  # If user wants the tidy data.frame:
  if (isTRUE(format_output)) {
    out_df <- do.call(
      rbind,
      lapply(names(test_results), function(key) {
        tt <- test_results[[key]]
        parts <- strsplit(key, "_")[[1]]
        outcome <- parts[1]
        cat_var <- parts[2]
        lvls <- levels(x1_df[[cat_var]])
        comp <- paste(lvls[1], "vs", lvls[2])

        est <- if (!is.null(tt$estimate)) unname(tt$estimate)[1] else NA_real_
        stat <- if (!is.null(tt$statistic)) {
          unname(tt$statistic)[1]
        } else {
          NA_real_
        }

        data.frame(
          Outcome = outcome,
          Categorical = cat_var,
          Comparison = comp,
          Test = tt$method,
          Estimate = round(est, 3),
          Statistic = round(stat, 3),
          P_value = round(tt$p.value, 3),
          stringsAsFactors = FALSE
        )
      })
    )
    return(list(
      out_df = out_df
    ))
  }

  # otherwise return the raw list of htest objects
  return(list(
    test_results = test_results
  ))
}
