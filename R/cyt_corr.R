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
#' @param plot Logical. If `TRUE`, the function will generate and return a correlation plot.
#' @param progress Optional. A Shiny \code{Progress} object for reporting progress updates.
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
#'   \item{plot}{A correlation plot, if `plot` is `TRUE`.}
#' @importFrom graphics legend mtext plot.new title
#' @importFrom stats na.omit predict setNames
#' @importFrom utils tail
#' @import ggcorrplot
#' @import ggplot2
#' @export
cyt_corr <- function(
  data,
  target,
  methods = c("spearman", "pearson"),
  group_var = NULL,
  compare_groups = FALSE,
  plot = FALSE,
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
        r = round(unname(ct$estimate), 2),
        p = round(ct$p.value, 4),
        n = n,
        method = method
      )
    })
    res <- do.call(rbind, one_vs_all)
    res$p_bonf <- round(stats::p.adjust(res$p, "bonferroni"), 4)
    res$p_bh <- round(stats::p.adjust(res$p, "BH"), 4)
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

    .build_corrplot <- function(mat, method, target, main_prefix = NULL) {
      # ensure target is first
      ord <- c(target, setdiff(colnames(mat), target))
      mat <- mat[ord, ord]

      # base heatmap
      p <- ggcorrplot::ggcorrplot(
        mat,
        hc.order = FALSE, # keep our order; don't re-cluster
        type = "full",
        lab = FALSE,
        show.diag = TRUE,
        outline.color = "white", # <-- documented arg name
        ggtheme = ggplot2::theme_minimal()
      ) +
        ggplot2::labs(
          title = paste0(
            if (is.null(main_prefix)) "" else paste0(main_prefix, ": "),
            tolower(method),
            ": ",
            target,
            " vs all features"
          ),
          x = NULL,
          y = NULL
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)
        )

      # --- add a visible outline for the target column and row (no warnings) ---
      n <- ncol(mat)
      idx <- match(target, colnames(mat))

      p +
        ggplot2::geom_rect(
          # outline the *column* of the target
          inherit.aes = FALSE,
          xmin = idx - 0.5,
          xmax = idx + 0.5,
          ymin = 0.5,
          ymax = n + 0.5,
          fill = NA,
          colour = "#cc0000",
          linewidth = 0.6
        ) +
        ggplot2::geom_rect(
          # outline the *row* of the target
          inherit.aes = FALSE,
          xmin = 0.5,
          xmax = n + 0.5,
          ymin = idx - 0.5,
          ymax = idx + 0.5,
          fill = NA,
          colour = "#cc0000",
          linewidth = 0.6
        )
    }

    # Creating heatmap of the correlation
    if (plot) {
      plt <- .build_corrplot(heat_mat, method, target)
    }

    # optional groupwise and Fisher z comparison
    groupwise <- diff_tbl <- NULL
    group_heat_mats <- group_plots <- NULL

    if (!is.null(group_var) && group_var %in% names(data)) {
      if (!is.null(progress)) {
        progress$inc(0.1, detail = "Computing Groupwise Metrics")
      }

      g <- factor(data[[group_var]])
      levs <- levels(g)

      # 1) stratified one-vs-all correlations (what you already do)
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
              exact = NULL # <-- changed
            ))
            data.frame(
              variable = v,
              r = unname(ct$estimate),
              p = ct$p.value,
              n = n,
              group = gl,
              method = method
            )
          })
          do.call(rbind, rows)
        })
      )

      # adjust p's (BH and Bonferroni)
      groupwise$p_bonf <- stats::p.adjust(groupwise$p, "bonferroni")
      groupwise$p_bh <- stats::p.adjust(groupwise$p, "BH")
      groupwise <- groupwise[order(-abs(groupwise$r)), ]

      # 2) per-group HEAT MATRICES + PLOTS (new)
      heat_vars <- unique(c(target, setdiff(num_cols, target)))
      group_heat_mats <- setNames(vector("list", length(levs)), levs)
      if (isTRUE(plot)) {
        group_plots <- setNames(vector("list", length(levs)), levs)
      }

      for (gl in levs) {
        sub <- data[g == gl, heat_vars, drop = FALSE]
        # require at least 3 complete rows for a sensible matrix
        if (nrow(na.omit(sub)) >= 3) {
          mat_g <- stats::cor(
            sub,
            use = "pairwise.complete.obs",
            method = method
          )
          # keep target first
          ord <- c(target, setdiff(colnames(mat_g), target))
          mat_g <- mat_g[ord, ord]
          group_heat_mats[[gl]] <- mat_g

          if (isTRUE(plot)) {
            group_plots[[gl]] <- .build_corrplot(
              mat_g,
              method,
              target,
              main_prefix = paste("Group", gl)
            )
          }
        } else {
          group_heat_mats[[gl]] <- NULL
          if (isTRUE(plot)) group_plots[[gl]] <- NULL
        }
      }

      # 3) optional Fisher z comparison for first two levels (your current behavior)
      if (isTRUE(compare_groups) && length(levs) >= 2) {
        g1 <- levs[1]
        g2 <- levs[2]
        a <- subset(groupwise, group == g1, c("variable", "r", "n"))
        names(a)[2:3] <- c("r1", "n1")
        b <- subset(groupwise, group == g2, c("variable", "r", "n"))
        names(b)[2:3] <- c("r2", "n2")
        m <- merge(a, b, by = "variable", all = FALSE)
        # Fisher r->z
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
      plot = if (isTRUE(plot)) plt else NULL,
      groupwise = groupwise,
      group_heat_mats = group_heat_mats,
      group_plots = group_plots,
      diff = diff_tbl
    )
  }

  out <- lapply(setNames(methods, methods), compute_one)
  class(out) <- "cyt_corr_dual"
  out
}
