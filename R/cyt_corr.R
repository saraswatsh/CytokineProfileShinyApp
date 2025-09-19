#' Calculate correlations between a target variable and all other numeric variables.
#'
#' This function computes Pearson or Spearman correlation coefficients between a
#' specified target variable and all other numeric variables in the input data frame.
#' It also calculates p-values and adjusted p-values (Bonferroni and Benjamini-Hochberg).
#' Optionally, it can perform group-wise correlation analysis and compare correlations
#' between two groups using Fisher's z-transformation.
#'
#' @param data A data frame containing the variables.
#' @param target A character string specifying the name of the target variable.
#'   Must be a numeric column in `data`.
#' @param methods A character string indicating the correlation method to be used.
#'   Can be "pearson" (default) or "spearman".
#' @param group_var An optional character string specifying the name of a grouping
#'   variable in `data`. If provided, group-wise correlations will be calculated.
#' @param compare_groups Logical. If `TRUE` and `group_var` is provided with at
#'   least two levels, the function will compare the correlations between the
#'   first two levels of `group_var`.
#'
#' @return A list containing:
#'   \item{results}{A data frame with overall correlation results, including
#'     variable names, correlation coefficients (`r`), p-values (`p`), sample
#'     size (`n`), method, and adjusted p-values (`p_bonf`, `p_bh`), sorted by
#'     absolute correlation coefficient in descending order.}
#'   \item{results_groupwise}{A data frame with group-wise correlation results,
#'     if `group_var` is provided. Includes similar columns as `results`, plus
#'     the `group` identifier.}
#'   \item{results_diff}{A data frame with results of correlation comparison
#'     between the first two groups, if `compare_groups` is `TRUE` and
#'     `group_var` has at least two levels. Includes `variable`, `r_diff`
#'     (difference in correlations), `z` (Fisher's z-score), `p_diff` (p-value
#'     for the difference), and adjusted p-values (`p_diff_bonf`, `p_diff_bh`).}
#'
#' @export
cyt_corr <- function(
  data,
  target,
  methods = c("spearman", "pearson"),
  group_var = NULL,
  compare_groups = FALSE,
  progress = NULL
) {
  stopifnot(is.data.frame(data))
  methods <- tolower(unique(methods))
  methods <- intersect(methods, c("spearman", "pearson"))
  if (!length(methods)) {
    stop("`methods` must include 'spearman' and/or 'pearson'.")
  }

  if (!target %in% names(data)) {
    stop("`target` not found.")
  }
  num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  if (!target %in% num_cols) {
    stop("`target` must be numeric.")
  }
  others <- setdiff(num_cols, target)
  x <- data[[target]]

  if (!is.null(progress)) {
    progress$set(message = "Starting Correlation Analysis...", value = 0)
  }

  compute_one <- function(method) {
    if (!is.null(progress)) {
      progress$inc(0.1, detail = "Computing Metrics")
    }
    # overall table via cor.test for accurate p
    one_vs_all <- lapply(others, function(v) {
      y <- data[[v]]
      ok <- stats::complete.cases(x, y)
      n <- sum(ok)
      if (n < 3) {
        return(data.frame(
          variable = v,
          r = NA_real_,
          p = NA_real_,
          n = n,
          method = method
        ))
      }
      ct <- suppressWarnings(stats::cor.test(
        x[ok],
        y[ok],
        method = method,
        exact = FALSE
      ))
      data.frame(
        variable = v,
        r = signif(round(unname(ct$estimate), 2), 2),
        p = signif(round(ct$p.value, 4), 4),
        n = n,
        method = method
      )
    })
    res <- do.call(rbind, one_vs_all)
    res$p_bonf <- signif(round(stats::p.adjust(res$p, "bonferroni"), 4), 4)
    res$p_bh <- signif(round(stats::p.adjust(res$p, "BH"), 4), 4)
    res <- res[order(-abs(res$r)), ]

    # square heatmap matrix
    heat_vars <- unique(c(target, res$variable))

    if (!is.null(progress)) {
      progress$inc(0.1, detail = "Generating Correlation Heatmap")
    }

    heat_mat <- stats::cor(
      data[, heat_vars, drop = FALSE],
      use = "pairwise.complete.obs",
      method = method
    )

    # optional groupwise and Fisher z comparison
    groupwise <- diff_tbl <- NULL
    if (!is.null(group_var) && group_var %in% names(data)) {
      if (!is.null(progress)) {
        progress$inc(0.1, detail = "Computing Groupwise Metrics")
      }
      g <- factor(data[[group_var]])
      levs <- levels(g)

      groupwise <- do.call(
        rbind,
        lapply(levs, function(gl) {
          idx <- which(g == gl)
          gx <- x[idx]
          rows <- lapply(others, function(v) {
            gy <- data[[v]][idx]
            ok <- stats::complete.cases(gx, gy)
            n <- sum(ok)
            if (n < 3) {
              return(data.frame(
                variable = v,
                r = NA_real_,
                p = NA_real_,
                n = n,
                group = gl,
                method = method
              ))
            }
            ct <- suppressWarnings(stats::cor.test(
              gx[ok],
              gy[ok],
              method = method,
              exact = FALSE
            ))
            data.frame(
              variable = v,
              r = signif(round(unname(ct$estimate), 2), 2),
              p = signif(round(ct$p.value, 4), 4),
              n = n,
              group = gl,
              method = method
            )
          })
          do.call(rbind, rows)
        })
      )
      groupwise$p_bonf <- signif(
        round(stats::p.adjust(groupwise$p, "bonferroni"), 4),
        4
      )
      groupwise$p_bh <- signif(round(stats::p.adjust(groupwise$p, "BH"), 4), 4)
      groupwise <- groupwise[order(-abs(groupwise$r)), ]

      if (isTRUE(compare_groups) && length(levs) >= 2) {
        g1 <- levs[1]
        g2 <- levs[2]
        a <- subset(groupwise, group == g1, c("variable", "r", "n"))
        names(a)[2:3] <- c("r1", "n1")
        b <- subset(groupwise, group == g2, c("variable", "r", "n"))
        names(b)[2:3] <- c("r2", "n2")
        m <- merge(a, b, by = "variable", all = FALSE)
        z1 <- atanh(pmin(pmax(m$r1, -0.999999), 0.999999))
        z2 <- atanh(pmin(pmax(m$r2, -0.999999), 0.999999))
        se <- sqrt(pmax(1 / (m$n1 - 3) + 1 / (m$n2 - 3), .Machine$double.eps))
        z <- (z1 - z2) / se
        pz <- 2 * stats::pnorm(-abs(z))
        diff_tbl <- data.frame(
          variable = m$variable,
          r_diff = m$r1 - m$r2,
          z = z,
          p_diff = pz,
          p_diff_bonf = stats::p.adjust(pz, "bonferroni"),
          p_diff_bh = stats::p.adjust(pz, "BH"),
          g1 = g1,
          g2 = g2,
          stringsAsFactors = FALSE
        )
      }
    }
    if (!is.null(progress)) {
      progress$inc(0.7, detail = "Finishing up")
    }
    list(
      table = res,
      heat_mat = heat_mat,
      groupwise = groupwise,
      diff = diff_tbl
    )
  }

  out <- lapply(setNames(methods, methods), compute_one)
  class(out) <- "cyt_corr_dual"
  out
}
