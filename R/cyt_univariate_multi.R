#' Univariate Tests for Multi-Level Categorical Predictors
#'
#' @description
#' `cyt_univariate_multi` provides univariate statistical testing for
#' categorical predictors with more than two levels. For each
#' categorical predictor and numeric outcome pair, a global test is
#' performed, followed by pairwise comparisons when appropriate.
#' Users may choose between two methods: classical ANOVA with Tukey's
#' Honest Significant Difference (HSD) or a non-parametric
#' Kruskal-Wallis test followed by pairwise Wilcoxon rank-sum tests.
#' The return format can either be a list of adjusted p-values for each
#' outcome-predictor pair or, if `format_output = TRUE`, a list
#' containing separate global and pairwise summary tables.
#'
#' @param data A data frame or matrix containing both categorical
#'   and continuous variables. Character columns will be converted
#'   to factors.
#' @param method Character specifying the type of global test to
#'   perform. Use `"anova"` (default) for one-way ANOVA with Tukey
#'   HSD or `"kruskal"` for Kruskal-Wallis with pairwise Wilcoxon
#'   tests.
#' @param cat_vars Optional character vector of predictor column
#'   names. When `NULL`, all factor or character columns in `data`
#'   are used.
#' @param cont_vars Optional character vector of numeric outcome
#'   variable names. When `NULL`, all numeric columns in `data` are
#'   used.
#' @param p_adjust_method Character string specifying the method for
#'   p-value adjustment across pairwise Kruskal-Wallis follow-up
#'   comparisons. Passed to `p.adjust`. Default is `"BH"`.
#' @param format_output Logical. If `TRUE`, returns a list with
#'   global results, pairwise results, and assumption summaries;
#'   otherwise (default) returns a list of numeric vectors keyed by
#'   `"Outcome_Categorical"`. Each numeric vector contains adjusted
#'   p-values for the pairwise comparisons.
#' @param progress Optional. A Shiny `Progress` object for reporting
#'   progress updates during the analysis.
#' @return Either a list of adjusted pairwise p-values (if
#'   `format_output = FALSE`) or a list with `results`, `pairwise`,
#'   and `assumptions` data frames (if `format_output = TRUE`).
#' @examples
#' data("ExampleData1")
#' cyt_univariate_multi(ExampleData1[, c(1:2, 5:6)], method = "kruskal",
#'   format_output = TRUE
#' )
#' @author Shubh Saraswat
#' @importFrom stats aov as.formula kruskal.test TukeyHSD
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

  empty_global_df <- function() {
    data.frame(
      Outcome = character(),
      Categorical = character(),
      Comparison = character(),
      Test = character(),
      Statistic = numeric(),
      P_value = numeric(),
      stringsAsFactors = FALSE
    )
  }

  empty_pairwise_df <- function() {
    data.frame(
      Outcome = character(),
      Categorical = character(),
      Comparison = character(),
      Test = character(),
      Estimate = numeric(),
      P_adj = numeric(),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(progress)) {
    progress$set(
      message = "Starting multi-level univariate tests...",
      value = 0
    )
  }

  names(data) <- make.names(names(data), unique = TRUE)
  method <- match.arg(method)
  if (is.null(p_adjust_method) || !nzchar(p_adjust_method)) {
    p_adjust_method <- "none"
  }

  x1_df <- as.data.frame(data)

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Converting character columns to factors")
  }
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
  if (is.null(cat_vars)) {
    cat_vars <- names(x1_df)[sapply(x1_df, is.factor)]
  }
  if (is.null(cont_vars)) {
    cont_vars <- names(x1_df)[sapply(x1_df, is.numeric)]
  }

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

  test_results <- list()
  global_rows <- list()
  pairwise_rows <- list()
  assumption_results <- list()

  for (cat_var in cat_vars) {
    levs <- levels(x1_df[[cat_var]])
    n_lvls <- length(levs)
    if (n_lvls <= 1) {
      next
    }
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
        model_summary <- summary(model)[[1]]
        global_rows[[length(global_rows) + 1L]] <- data.frame(
          Outcome = outcome,
          Categorical = cat_var,
          Comparison = "Overall",
          Test = "ANOVA",
          Statistic = safe_numeric(model_summary[1, "F value"]),
          P_value = safe_numeric(model_summary[1, "Pr(>F)"]),
          stringsAsFactors = FALSE,
          row.names = NULL
        )

        tuk <- TukeyHSD(model)[[cat_var]]
        p_vals <- tuk[, "p adj"]
        names(p_vals) <- rownames(tuk)
        test_results[[key]] <- round(p_vals, 4)

        pairwise_rows[[key]] <- data.frame(
          Outcome = outcome,
          Categorical = cat_var,
          Comparison = rownames(tuk),
          Test = "TukeyHSD",
          Estimate = as.numeric(tuk[, "diff"]),
          P_adj = as.numeric(tuk[, "p adj"]),
          stringsAsFactors = FALSE,
          row.names = NULL
        )

        resids <- stats::residuals(model)
        norm_p <- tryCatch(
          stats::shapiro.test(resids)$p.value,
          error = function(e) NA_real_
        )
        bartlett_p <- tryCatch(
          stats::bartlett.test(
            stats::as.formula(paste(outcome, "~", cat_var)),
            data = x1_df
          )$p.value,
          error = function(e) NA_real_
        )
        assumption_results[[length(assumption_results) + 1L]] <- list(
          Outcome = outcome,
          Categorical = cat_var,
          normality_p = round(norm_p, 4),
          homogeneity_p = round(bartlett_p, 4),
          normality_ok = !is.na(norm_p) && norm_p > 0.05,
          homogeneity_ok = !is.na(bartlett_p) && bartlett_p > 0.05
        )
      } else {
        kw <- stats::kruskal.test(
          stats::as.formula(paste(outcome, "~", cat_var)),
          data = x1_df
        )
        global_rows[[length(global_rows) + 1L]] <- data.frame(
          Outcome = outcome,
          Categorical = cat_var,
          Comparison = "Overall",
          Test = "Kruskal-Wallis",
          Statistic = safe_numeric(kw$statistic),
          P_value = safe_numeric(kw$p.value),
          stringsAsFactors = FALSE,
          row.names = NULL
        )

        if (is.finite(kw$p.value) && kw$p.value < 0.05) {
          observed_levels <- levels(base::droplevels(x1_df[[cat_var]]))
          pair_rows <- lapply(
            utils::combn(observed_levels, 2, simplify = FALSE),
            function(level_pair) {
              pair_df <- x1_df[
                x1_df[[cat_var]] %in% level_pair,
                c(outcome, cat_var),
                drop = FALSE
              ]
              pair_df <- stats::na.omit(pair_df)

              grp1 <- pair_df[[outcome]][pair_df[[cat_var]] == level_pair[1]]
              grp2 <- pair_df[[outcome]][pair_df[[cat_var]] == level_pair[2]]
              if (!length(grp1) || !length(grp2)) {
                return(NULL)
              }

              wt <- tryCatch(
                stats::wilcox.test(
                  grp1,
                  grp2,
                  conf.int = TRUE,
                  exact = FALSE
                ),
                error = function(e) NULL
              )
              if (is.null(wt)) {
                return(NULL)
              }

              data.frame(
                Outcome = outcome,
                Categorical = cat_var,
                Comparison = paste(level_pair, collapse = " vs "),
                Test = "Pairwise Wilcoxon",
                Estimate = safe_numeric(wt$estimate),
                P_adj_raw = safe_numeric(wt$p.value),
                stringsAsFactors = FALSE,
                row.names = NULL
              )
            }
          )

          pair_rows <- Filter(Negate(is.null), pair_rows)
          pair_df <- if (length(pair_rows) > 0L) {
            do.call(rbind, pair_rows)
          } else {
            data.frame(
              Outcome = character(),
              Categorical = character(),
              Comparison = character(),
              Test = character(),
              Estimate = numeric(),
              P_adj_raw = numeric(),
              stringsAsFactors = FALSE,
              row.names = NULL
            )
          }

          if (nrow(pair_df) > 0L) {
            pair_df$P_adj <- stats::p.adjust(
              pair_df$P_adj_raw,
              method = p_adjust_method
            )
            pair_df$P_adj_raw <- NULL
            test_results[[key]] <- stats::setNames(
              round(pair_df$P_adj, 4),
              pair_df$Comparison
            )
          } else {
            pair_df$P_adj_raw <- NULL
            pair_df$P_adj <- numeric(0)
            test_results[[key]] <- numeric(0)
          }

          pairwise_rows[[key]] <- pair_df
        } else {
          test_results[[key]] <- numeric(0)
          pairwise_rows[[key]] <- empty_pairwise_df()
        }
      }
    }
  }

  if (length(test_results) == 0 && length(global_rows) == 0) {
    warning(
      "No valid comparisons were performed. ",
      "Check that your data has numeric columns and factors with sufficient levels."
    )
    if (!format_output) {
      return(list())
    }

    return(list(
      results = empty_global_df(),
      pairwise = empty_pairwise_df(),
      assumptions = NULL
    ))
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

  global_df <- if (length(global_rows) > 0L) {
    do.call(rbind, global_rows)
  } else {
    empty_global_df()
  }

  pairwise_tables <- Filter(
    function(x) is.data.frame(x) && nrow(x) > 0L,
    pairwise_rows
  )
  pairwise_df <- if (length(pairwise_tables) > 0L) {
    do.call(rbind, pairwise_tables)
  } else {
    empty_pairwise_df()
  }

  global_df$Statistic <- round(global_df$Statistic, 3)
  global_df$P_value <- round(global_df$P_value, 3)

  if (nrow(pairwise_df) > 0L) {
    pairwise_df$Estimate <- round(pairwise_df$Estimate, 3)
    pairwise_df$P_adj <- round(pairwise_df$P_adj, 3)
  }

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Complete")
  }

  assumption_df <- if (length(assumption_results) > 0L) {
    do.call(
      rbind,
      lapply(assumption_results, function(a) {
        data.frame(
          Outcome = a$Outcome,
          Categorical = a$Categorical,
          Normality_P = a$normality_p,
          Normality_Met = ifelse(
            isTRUE(a$normality_ok),
            "\u2714 Yes",
            "\u2718 No"
          ),
          Homogeneity_P = a$homogeneity_p,
          Homogeneity_Met = ifelse(
            isTRUE(a$homogeneity_ok),
            "\u2714 Yes",
            "\u2718 No"
          ),
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      })
    )
  } else {
    NULL
  }

  list(
    results = global_df,
    pairwise = pairwise_df,
    assumptions = assumption_df
  )
}
