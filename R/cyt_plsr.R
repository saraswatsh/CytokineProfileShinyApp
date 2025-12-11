#' Perform Partial Least Squares Regression (PLSR) or sparse PLSR (sPLSR) analysis
#'
#' This function wraps the mixOmics `pls()` and `spls()` functions to perform
#' regression of one or more response variables on a set of predictors.
#' It automatically handles optional log2 scaling, extraction of numeric columns
#' and grouping information, and returns a small list of recorded plots for
#' display in Shiny.  The first plot (`indiv_plot`) is a score plot of the
#' first two latent components colored by a grouping variable (if provided).
#' The second plot (`pred_plot`) is a scatterplot of the observed vs predicted
#' response values for the first response variable.  Additional plots could
#' easily be added (e.g. loadings), but these two provide basic diagnostics.
#'
#' @param data A data frame containing the predictor and response variables.
#' @param response_col The name of the column containing the response variable.
#'   Must be numeric.
#' @param group_col The name of the column to use for grouping samples in the
#'   score plot. If `NULL`, no grouping is applied.
#' @param ind_names Logical, or a character vector. If `TRUE`, sample names are
#'   displayed on the plot. If a character vector, these names are used.
#' @param comp_num The number of components to use in the PLSR model.
#' @param sparse Logical. If `TRUE`, performs sparse PLSR (sPLSR).
#' @param var_num The number of variables to keep per component for sPLSR.
#'   Required if `sparse = TRUE`.
#' @param cv_opt Character string specifying the cross-validation option:
#'   "None", "LOOCV" (Leave-One-Out Cross-Validation), or "Mfold" (M-fold
#'   Cross-Validation).
#' @param fold_num The number of folds for M-fold cross-validation. Only
#'   applicable if `cv_opt = "Mfold"`.
#' @param scale Character string. If "log2", applies log2 transformation to
#'   numeric predictor columns.
#' @param ellipse Logical. If `TRUE`, draws 95% confidence ellipses on the
#'   score plot.
#' @param pls_colors A character vector of colors to use for grouping.
#' @param output_file Optional. A file path to save the plots as a PDF.
#' @param progress Optional. A Shiny \code{Progress} object for reporting progress updates.
#' @return A list containing:
#'   \item{scores_plot}{A recorded plot of the PLSR scores.}
#'   \item{pred_vs_obs}{A recorded plot of predicted vs. observed response values.}
#'   \item{residuals_plot}{A recorded plot of residuals vs. fitted values.}
#'   \item{cv_plot}{A recorded plot of cross-validation performance (Q2 and RMSEP), if `cv_opt` is not "None".}
#'   \item{loadings}{A list of recorded plots for loadings of each component.}
#'   \item{vip_scores}{A list of `ggplot` objects of VIP scores for each component.}
#'   \item{vip_scores_indiv}{A recorded plot of the PLSR scores for variables
#'     with VIP > 1, if applicable.}
#'   \item{vip_cv_plot}{A recorded plot of cross-validation performance for
#'     variables with VIP > 1, if applicable.}
#'   \item{metrics_text}{Text summary of Q2 values, if available.}
#'   \item{pdf_file}{The path to the generated PDF file, if `output_file` was
#'     provided.}
#'
#' @importFrom mixOmics pls spls plotIndiv plotLoadings perf vip
#' @import ggplot2
#' @export
cyt_plsr <- function(
  data,
  response_col,
  group_col = NULL,
  ind_names = FALSE,
  comp_num = 2,
  sparse = FALSE,
  var_num = NULL,
  cv_opt = NULL,
  fold_num = 5,
  scale = NULL,
  ellipse = FALSE,
  pls_colors = NULL,
  output_file = NULL,
  progress = NULL
) {
  `%notin%` <- function(x, y) !(x %in% y)
  .nzchar <- function(x) !is.null(x) && length(x) && !identical(x, "")

  if (!.nzchar(response_col) || response_col %notin% names(data)) {
    stop("Valid numeric `response_col` must be provided.")
  }
  if (!is.numeric(data[[response_col]])) {
    stop("`response_col` must be numeric for regression.")
  }

  if (!is.null(progress)) {
    progress$set(message = "Starting (s)PLS regression...", value = 0)
  }

  # --- identify non-predictor cols
  id_cols <- unique(na.omit(c(response_col, group_col)))
  id_cols <- id_cols[id_cols %in% names(data)]

  # --- optional log2 transform (predictors only)
  if (identical(scale, "log2")) {
    num_cols <- names(data)[
      vapply(data, is.numeric, logical(1)) & names(data) %notin% id_cols
    ]
    if (length(num_cols)) {
      if (any(data[, num_cols] <= 0, na.rm = TRUE)) {
        min_pos <- suppressWarnings(min(
          data[, num_cols][data[, num_cols] > 0],
          na.rm = TRUE
        ))
        off <- if (is.finite(min_pos)) min_pos / 2 else 1e-6
        data[, num_cols] <- log2(data[, num_cols] + off)
        warning("Non-positive X detected; applied log2 with a small offset.")
      } else {
        data[, num_cols] <- log2(data[, num_cols])
      }
    }
  }

  # --- build X (numeric) / y
  .x_y <- function(df) {
    exclude <- unique(c(id_cols, "..cyto_id..", "cyto_id"))

    X <- df[, setdiff(names(df), exclude), drop = FALSE]
    X <- X[, vapply(X, is.numeric, logical(1)), drop = FALSE]
    if (ncol(X) < 2) {
      stop("Need at least 2 numeric predictors after filtering/selection.")
    }
    y <- df[[response_col]]
    ok <- stats::complete.cases(X) & !is.na(y)
    list(X = X[ok, , drop = FALSE], y = y[ok], ok = ok)
  }

  # --- grouping & colors (colors map to levels of group_col)
  grp <- if (!is.null(group_col) && group_col %in% names(data)) {
    factor(data[[group_col]])
  } else {
    factor(rep("All", nrow(data)))
  }
  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Preparing data")
  }
  xy <- .x_y(data)
  X <- xy$X
  y <- xy$y
  groups <- droplevels(factor(grp[xy$ok]))
  levs <- levels(groups)
  cols <- if (is.null(pls_colors) || !length(pls_colors)) {
    grDevices::rainbow(length(levs))
  } else {
    rep(pls_colors, length.out = length(levs))
  }
  names(cols) <- levs

  .resolve_ind_names <- function(df_ok, ind_names, parent_n, parent_rownm) {
    if (isTRUE(ind_names)) {
      return(TRUE)
    }
    if (!is.character(ind_names)) {
      return(FALSE)
    }
    rn_sub <- rownames(df_ok)
    if (length(ind_names) == nrow(df_ok) && is.null(names(ind_names))) {
      return(ind_names)
    }
    if (!is.null(names(ind_names)) && !is.null(rn_sub)) {
      lab <- ind_names[rn_sub]
      if (sum(!is.na(lab)) >= 1) return(lab)
    }
    if (
      length(ind_names) == parent_n &&
        !is.null(parent_rownm) &&
        !is.null(rn_sub)
    ) {
      return(ind_names[match(rn_sub, parent_rownm)])
    }
    warning(
      "`ind_names` provided but could not be aligned; using default names."
    )
    TRUE
  }

  .record_plot <- function(expr) {
    if (grDevices::dev.cur() == 1) {
      grDevices::png(tempfile(fileext = ".png"))
      on.exit(grDevices::dev.off(), add = TRUE)
    }
    grDevices::dev.control(displaylist = "enable")
    force(expr)
    grDevices::recordPlot()
  }

  .plot_indiv <- function(model, groups, title, ind_names_resolved) {
    args <- list(
      model,
      group = groups,
      legend = TRUE,
      col = cols[levels(groups)],
      title = title,
      legend.title = if (!is.null(group_col)) group_col else "Group",
      ellipse = isTRUE(ellipse)
    )
    if (isTRUE(ind_names_resolved) || is.character(ind_names_resolved)) {
      args$ind.names <- ind_names_resolved
    } else {
      args$ind.names <- FALSE
    }
    do.call(mixOmics::plotIndiv, args)
  }

  # --- fit model
  p <- ncol(X)
  comp_num_eff <- max(1, min(comp_num, p))
  if (isTRUE(sparse)) {
    if (is.null(var_num)) {
      stop("For sPLS (sparse=TRUE), please set `var_num`.")
    }
    keepx <- rep(max(1, min(var_num, p)), comp_num_eff)
  }
  if (!is.null(progress)) {
    progress$inc(0.1, detail = paste("Fitting", if (sparse) "sPLS" else "PLS"))
  }
  mdl <- if (isTRUE(sparse)) {
    mixOmics::spls(
      X = X,
      Y = y,
      ncomp = comp_num_eff,
      keepX = keepx,
      scale = TRUE,
      mode = "regression"
    )
  } else {
    mixOmics::pls(
      X = X,
      Y = y,
      ncomp = comp_num_eff,
      scale = TRUE,
      mode = "regression"
    )
  }

  # --- predictions & metrics
  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Predicting")
  }
  pr <- predict(mdl, newdata = X)
  yhat <- as.numeric(drop(pr$predict[, 1, comp_num_eff]))
  r2_train <- suppressWarnings(stats::cor(y, yhat, use = "complete.obs")^2)
  rmse_train <- sqrt(mean((y - yhat)^2, na.rm = TRUE))
  lab_res <- .resolve_ind_names(
    data[xy$ok, , drop = FALSE],
    ind_names,
    nrow(data),
    rownames(data)
  )

  # --- plots: scores, diagnostics
  scores_plot <- .record_plot(.plot_indiv(
    mdl,
    groups,
    sprintf(
      "Scores (Comp 1-%d) R-Squared(train)=%.2f  RMSE=%.3g",
      comp_num_eff,
      r2_train,
      rmse_train
    ),
    ind_names_resolved = lab_res
  ))

  pvo <- .record_plot({
    dfp <- data.frame(Observed = y, Predicted = yhat)
    p <- ggplot2::ggplot(dfp, ggplot2::aes(Observed, Predicted)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
      ggplot2::labs(
        title = "Predicted vs Observed",
        subtitle = sprintf(
          "R-Squared(train)=%.2f  RMSE=%.3g",
          r2_train,
          rmse_train
        )
      )
    print(p)
  })
  res_plot <- .record_plot({
    dfr <- data.frame(Fitted = yhat, Residuals = y - yhat)
    p <- ggplot2::ggplot(dfr, ggplot2::aes(Fitted, Residuals)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::labs(title = "Residuals vs Fitted")
    print(p)
  })

  # --- CV performance (optional)
  cv_plot <- NULL

  if (!is.null(cv_opt)) {
    # 1) Run perf() to perform cross-validation
    set.seed(123)
    cv_res <- if (identical(cv_opt, "loocv")) {
      mixOmics::perf(mdl, validation = "loo", progressBar = FALSE)
    } else if (identical(cv_opt, "mfold")) {
      folds_safe <- max(2, min(fold_num, nrow(X)))
      mixOmics::perf(
        mdl,
        validation = "Mfold",
        folds = folds_safe,
        nrepeat = 100,
        progressBar = FALSE
      )
    } else {
      NULL
    }

    if (!is.null(cv_res)) {
      K <- comp_num_eff
      cv_title <- if (identical(cv_opt, "loocv")) {
        "LOOCV"
      } else {
        sprintf("M-fold (k=%d, nrepeat=100)", folds_safe)
      }

      # 2) Extract the mean values from the '$summary$mean' column
      q2_vals <- as.numeric(cv_res$measures$Q2.total$summary$mean)
      msep_vals <- as.numeric(cv_res$measures$MSEP$summary$mean)

      # 3) Calculate RMSEP and handle cases where perf includes a "comp 0"
      rmsep_vals <- sqrt(tail(msep_vals, K))
      q2_vals <- tail(q2_vals, K)

      # 4) Build a tidy data frame for plotting
      df_perf <- data.frame(
        Component = c(seq_along(q2_vals), seq_along(rmsep_vals)),
        value = c(q2_vals, rmsep_vals),
        metric = factor(
          c(
            rep("Q-Squared", length(q2_vals)),
            rep("RMSEP", length(rmsep_vals))
          ),
          levels = c("Q-Squared", "RMSEP")
        )
      )
      df_perf <- df_perf[is.finite(df_perf$value), , drop = FALSE]

      # 5) Generate plot from the tidy data frame
      cv_plot <- .record_plot({
        if (nrow(df_perf) > 0) {
          g <- ggplot2::ggplot(
            df_perf,
            ggplot2::aes(x = Component, y = value)
          ) +
            ggplot2::geom_line() +
            ggplot2::geom_point(size = 3) +
            ggplot2::facet_wrap(~metric, scales = "free_y", nrow = 1) +
            ggplot2::scale_x_continuous(breaks = seq_len(K)) +
            ggplot2::labs(
              title = paste("Cross-validation:", cv_title),
              x = "Components",
              y = NULL
            ) +
            ggplot2::theme_minimal() +
            ggplot2::geom_hline(
              yintercept = 0.0975,
              linetype = "dashed",
              color = "grey40"
            )

          print(g)
        } else {
          print(
            ggplot2::ggplot() +
              ggplot2::theme_minimal() +
              labs(title = "CV results not available.")
          )
        }
      })
    }
  }

  # --- loadings
  loadings <- lapply(seq_len(comp_num_eff), function(k) {
    .record_plot({
      mixOmics::plotLoadings(
        mdl,
        comp = k,
        contrib = "max",
        method = "mean",
        size.names = 1,
        size.legend = 1,
        size.title = 1,
        legend = FALSE,
        title = paste("Loadings Comp", k)
      )
    })
  })

  # --- VIP & optional VIP>1 refit preview
  vip_scores <- vip_indiv_plot <- vip_cv_plot <- NULL
  vip_all <- try(mixOmics::vip(mdl), silent = TRUE)
  if (!inherits(vip_all, "try-error")) {
    vip_scores <- lapply(seq_len(comp_num_eff), function(k) {
      v <- data.frame(
        variable = rownames(vip_all),
        score = vip_all[, k],
        row.names = NULL
      )
      v <- v[order(v$score, decreasing = TRUE), ]
      ggplot2::ggplot(
        v,
        ggplot2::aes(x = stats::reorder(variable, score), y = score)
      ) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste("VIP Scores Comp", k),
          x = "Variable",
          y = "VIP"
        )
    })
    vip_filter <- vip_all[, 1] > 1
    if (sum(vip_filter) > 0) {
      Xvip <- X[, vip_filter, drop = FALSE]
      mdl_vip <- if (isTRUE(sparse)) {
        mixOmics::spls(
          Xvip,
          y,
          ncomp = comp_num_eff,
          keepX = rep(ncol(Xvip), comp_num_eff),
          scale = TRUE,
          mode = "regression"
        )
      } else {
        mixOmics::pls(
          Xvip,
          y,
          ncomp = comp_num_eff,
          scale = TRUE,
          mode = "regression"
        )
      }
      pr_vip <- predict(mdl_vip, newdata = Xvip)
      yhat_vip <- as.numeric(drop(pr_vip$predict[, 1, comp_num_eff]))
      r2_vip <- suppressWarnings(
        stats::cor(y, yhat_vip, use = "complete.obs")^2
      )
      vip_indiv_plot <- .record_plot(.plot_indiv(
        mdl_vip,
        groups,
        sprintf("Scores (VIP>1) R-Squared(train)=%.2f", r2_vip),
        ind_names_resolved = lab_res
      ))
      if (!is.null(cv_opt) && sum(vip_filter) > 0) {
        set.seed(123)

        # Define cv_title and run perf()
        cv_title <- NULL
        per2 <- if (identical(cv_opt, "loocv")) {
          cv_title <- "LOOCV"
          mixOmics::perf(
            mdl_vip,
            validation = "loo",
            progressBar = FALSE
          )
        } else if (identical(cv_opt, "mfold")) {
          folds_safe_vip <- max(2, min(fold_num, nrow(Xvip)))
          cv_title <- sprintf("M-fold (k=%d, nrepeat=100)", folds_safe_vip)
          mixOmics::perf(
            mdl_vip,
            validation = "Mfold",
            folds = folds_safe_vip,
            nrepeat = 100,
            progressBar = FALSE
          )
        } else {
          NULL
        }

        if (!is.null(per2)) {
          K <- comp_num_eff

          # Extract metrics using '$measures$METRIC$summary$mean'
          q2_vals <- as.numeric(per2$measures$Q2.total$summary$mean)
          msep_vals <- as.numeric(per2$measures$MSEP$summary$mean)

          # Calculate RMSEP and handle cases where perf includes a "comp 0"
          rmsep_vals <- sqrt(tail(msep_vals, K))
          q2_vals <- tail(q2_vals, K)

          # Build a tidy data frame for plotting
          df_vip_perf <- data.frame(
            Component = c(seq_along(q2_vals), seq_along(rmsep_vals)),
            value = c(q2_vals, rmsep_vals),
            metric = factor(
              c(
                rep("Q-Squared", length(q2_vals)),
                rep("RMSEP", length(rmsep_vals))
              ),
              levels = c("Q-Squared", "RMSEP")
            )
          )
          df_vip_perf <- df_vip_perf[
            is.finite(df_vip_perf$value),
            ,
            drop = FALSE
          ]

          vip_cv_plot <- .record_plot({
            if (nrow(df_vip_perf) > 0) {
              g <- ggplot2::ggplot(
                df_vip_perf,
                ggplot2::aes(x = Component, y = value)
              ) +
                ggplot2::geom_line(na.rm = TRUE) +
                ggplot2::geom_point(size = 2, na.rm = TRUE) +
                ggplot2::facet_wrap(~metric, scales = "free_y", nrow = 1) +
                ggplot2::scale_x_continuous(breaks = seq_len(K)) +
                ggplot2::labs(
                  # Use the new dynamic title here
                  title = paste("Cross-validation (VIP > 1):", cv_title),
                  x = "Components",
                  y = NULL
                ) +
                ggplot2::theme_minimal(base_size = 12) +
                ggplot2::geom_hline(
                  yintercept = 0.0975,
                  linetype = "dashed",
                  color = "grey40"
                )
              print(g)
            } else {
              print(ggplot2::ggplot() + ggplot2::theme_minimal())
            }
          })
        }
      }
    }
  }

  # --- Optional PDF output (regenerate key plots for device)
  if (!is.null(output_file)) {
    grDevices::pdf(output_file, width = 10, height = 8)
    on.exit(grDevices::dev.off(), add = TRUE)
    .plot_indiv(
      mdl,
      groups,
      sprintf(
        "Scores (Comp 1-%d) R-Squared(train)=%.2f RMSE=%.3g",
        comp_num_eff,
        r2_train,
        rmse_train
      ),
      ind_names_resolved = lab_res
    )
    print(replayPlot(pvo))
    print(replayPlot(res_plot))
    if (!is.null(cv_plot)) {
      print(replayPlot(cv_plot))
    }
    for (k in seq_len(comp_num_eff)) {
      mixOmics::plotLoadings(
        mdl,
        comp = k,
        contrib = "max",
        method = "mean",
        size.names = 1,
        size.legend = 1,
        size.title = 1,
        legend = FALSE,
        title = paste("Loadings Comp", k)
      )
    }
    if (!is.null(vip_indiv_plot)) {
      print(replayPlot(vip_indiv_plot))
    }
    if (!is.null(vip_cv_plot)) print(replayPlot(vip_cv_plot))
  }

  if (!is.null(progress)) {
    progress$inc(0.5, detail = "Wrapping up")
  }

  list(
    scores_plot = scores_plot,
    pred_vs_obs = pvo,
    residuals_plot = res_plot,
    cv_plot = cv_plot,
    loadings = loadings,
    vip_scores = vip_scores,
    vip_scores_indiv = vip_indiv_plot,
    vip_cv_plot = vip_cv_plot,
    pdf_file = output_file
  )
}
