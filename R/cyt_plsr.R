#' Perform Partial Least Squares Regression (PLSR) or sparse PLSR (sPLSR) analysis
#'
#' This function wraps the mixOmics `pls()` and `spls()` functions to perform
#' regression of one or more response variables on a set of predictors.
#' It automatically handles optional predictor scaling, extraction of numeric
#' columns and grouping information, and returns a small list of recorded plots
#' for display in Shiny. The first plot (`indiv_plot`) is a score plot of the
#' first two latent components colored by a grouping variable (if provided).
#' The second plot (`pred_plot`) is a scatterplot of the observed vs predicted
#' response values for the first response variable. Additional plots could
#' easily be added (e.g. loadings), but these two provide basic diagnostics.
#'
#' @param data A data frame containing the predictor and response variables.
#' @param response_col The name of the column containing the response variable.
#'   Must be numeric.
#' @param predictor_cols Character vector of column names to use as predictors.
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
#' @param scale Character string specifying an optional transformation for
#'   numeric predictor columns. Supported values are \code{NULL} (default; no
#'   transformation), \code{"none"}, \code{"log2"}, \code{"log10"},
#'   \code{"zscore"}, or \code{"custom"}.
#' @param custom_fn Optional transformation function used when
#'   \code{scale = "custom"}.
#' @param ellipse Logical. If `TRUE`, draws 95% confidence ellipses on the
#'   score plot.
#' @param pls_colors A character vector of colors to use for grouping.
#' @param output_file Optional. A file path to save the plots as a PDF.
#' @param font_settings Optional named list of font sizes for supported plot
#'   text elements.
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
#' @author Shubh Saraswat
#' @export
cyt_plsr <- function(
  data,
  response_col,
  predictor_cols = NULL,
  group_col = NULL,
  ind_names = FALSE,
  comp_num = 2,
  sparse = FALSE,
  var_num = NULL,
  cv_opt = NULL,
  fold_num = 5,
  scale = NULL,
  custom_fn = NULL,
  ellipse = FALSE,
  pls_colors = NULL,
  output_file = NULL,
  font_settings = NULL,
  progress = NULL
) {
  # normalize cv_opt input to lowercase for consistent handling
  cv_opt <- if (!is.null(cv_opt)) tolower(cv_opt) else NULL

  `%notin%` <- function(x, y) !(x %in% y)
  .nzchar <- function(x) !is.null(x) && length(x) && !identical(x, "")

  if (!.nzchar(response_col) || response_col %notin% names(data)) {
    stop("Valid numeric `response_col` must be provided.")
  }
  if (!is.numeric(data[[response_col]])) {
    stop("`response_col` must be numeric for regression.")
  }

  analysis_label <- paste0(
    "Running ",
    if (sparse) "sPLS" else "PLS",
    " Regression..."
  )
  if (!is.null(progress)) {
    progress$set(message = analysis_label, value = 0)
  }
  resolved_fonts <- normalize_font_settings(
    font_settings = font_settings,
    supported_fields = c(
      "base_size",
      "plot_title",
      "x_title",
      "y_title",
      "x_text",
      "y_text",
      "legend_title",
      "legend_text",
      "strip_text",
      "variable_names",
      "point_labels"
    ),
    activate = !is.null(font_settings)
  )
  mixomics_indiv_args <- font_settings_mixomics_indiv_args(resolved_fonts)
  mixomics_loadings_args <- font_settings_mixomics_loadings_args(resolved_fonts)
  metrics_notes <- character(0)

  # --- identify non-predictor cols
  id_cols <- unique(na.omit(c(response_col, group_col)))
  id_cols <- id_cols[id_cols %in% names(data)]

  scale_cols <- names(data)[
    vapply(data, is.numeric, logical(1)) & names(data) %notin% id_cols
  ]
  if (!is.null(predictor_cols) && length(predictor_cols)) {
    scale_cols <- intersect(scale_cols, predictor_cols)
  }

  if (!is.null(scale) && length(scale_cols) > 0L) {
    data <- apply_scale(
      data = data,
      columns = scale_cols,
      scale = scale,
      custom_fn = custom_fn
    )
  }

  # --- build X (numeric) / y
  .x_y <- function(df) {
    exclude <- unique(c(id_cols, "..cyto_id..", "cyto_id"))

    X <- df[, setdiff(names(df), exclude), drop = FALSE]
    X <- X[, vapply(X, is.numeric, logical(1)), drop = FALSE]
    # --- restrict to user-chosen predictors if provided ---
    if (!is.null(predictor_cols) && length(predictor_cols)) {
      X <- X[, intersect(names(X), predictor_cols), drop = FALSE]
    }

    if (ncol(X) < 2) {
      stop(
        "Need at least 2 numeric predictors. Check your predictor column selection."
      )
    }
    y_all <- df[[response_col]]
    keep_y <- !is.na(y_all)
    X <- X[keep_y, , drop = FALSE]
    y <- y_all[keep_y]

    if (any(!is.finite(y))) {
      stop(
        "`response_col` must contain only finite observed values for regression."
      )
    }

    predictor_issues <- lapply(names(X), function(col) {
      vals <- X[[col]]
      observed_vals <- vals[!is.na(vals)]
      finite_vals <- observed_vals[is.finite(observed_vals)]
      reasons <- character(0)

      if (any(!is.finite(observed_vals))) {
        reasons <- c(reasons, "contains non-finite observed values")
      }
      if (length(finite_vals) < 3L) {
        reasons <- c(
          reasons,
          sprintf("fewer than 3 observed X/Y pairs (n=%d)", length(finite_vals))
        )
      }

      sdv <- stats::sd(finite_vals)
      if (is.na(sdv) || !is.finite(sdv) || sdv == 0) {
        reasons <- c(reasons, "zero or undefined SD")
      }

      if (!length(reasons)) {
        return(NULL)
      }

      data.frame(
        predictor = col,
        observed_pairs = length(finite_vals),
        reasons = paste(unique(reasons), collapse = ", "),
        stringsAsFactors = FALSE
      )
    })

    predictor_issues <- Filter(Negate(is.null), predictor_issues)
    if (length(predictor_issues)) {
      predictor_issues <- do.call(rbind, predictor_issues)
      warning(
        paste(
          "PLSR dropped unusable predictors before fitting:",
          paste(
            sprintf(
              "%s [observed pairs=%d; reasons=%s]",
              predictor_issues$predictor,
              predictor_issues$observed_pairs,
              predictor_issues$reasons
            ),
            collapse = "; "
          ),
          "Consider Step 2 'Treat missing values' if you want to retain sparse predictors."
        ),
        call. = FALSE
      )
      metrics_notes <<- c(
        metrics_notes,
        paste(
          "Dropped predictors before fitting:",
          paste(
            sprintf(
              "%s (observed pairs=%d; %s)",
              predictor_issues$predictor,
              predictor_issues$observed_pairs,
              predictor_issues$reasons
            ),
            collapse = "; "
          )
        ),
        "Tip: use Step 2 'Treat missing values' to retain sparse predictors."
      )
      X <- X[, setdiff(names(X), predictor_issues$predictor), drop = FALSE]
    } else {
      predictor_issues <- data.frame(
        predictor = character(),
        observed_pairs = integer(),
        reasons = character(),
        stringsAsFactors = FALSE
      )
    }

    if (ncol(X) < 2) {
      drop_msg <- if (nrow(predictor_issues)) {
        paste(
          sprintf(
            "%s [%s]",
            predictor_issues$predictor,
            predictor_issues$reasons
          ),
          collapse = "; "
        )
      } else {
        "none"
      }
      stop(
        paste(
          "PLSR requires at least 2 usable numeric predictors after filtering missingness.",
          "Dropped predictors:",
          drop_msg
        )
      )
    }

    row_has_predictor <- rowSums(!is.na(as.matrix(X))) > 0
    X <- X[row_has_predictor, , drop = FALSE]
    y <- y[row_has_predictor]

    ok <- keep_y
    ok[keep_y] <- row_has_predictor

    if (nrow(X) < 3L) {
      stop(
        paste(
          "PLSR requires at least 3 rows with an observed response and",
          "at least one retained predictor value after filtering missingness."
        )
      )
    }

    if (length(unique(y[is.finite(y)])) < 2L) {
      stop(
        "`response_col` must contain at least 2 distinct finite values after filtering."
      )
    }

    list(
      X = X,
      y = y,
      ok = ok,
      predictor_issues = predictor_issues
    )
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
      tmp_png <- tempfile(fileext = ".png")
      grDevices::png(tmp_png)
      on.exit(
        {
          grDevices::dev.off()
          if (file.exists(tmp_png)) unlink(tmp_png)
        },
        add = TRUE
      )
    }
    grDevices::dev.control(displaylist = "enable")
    force(expr)
    grDevices::recordPlot()
  }

  .subset_ind_names <- function(ind_names_resolved, keep_rows) {
    if (!is.character(ind_names_resolved)) {
      return(ind_names_resolved)
    }
    ind_names_resolved[keep_rows]
  }

  .pseudo_inverse <- function(mat, tol = sqrt(.Machine$double.eps)) {
    s <- svd(mat)
    if (!length(s$d)) {
      stop("Cannot compute a pseudo-inverse for an empty matrix.")
    }

    keep <- s$d > max(tol * s$d[1], tol)
    if (!any(keep)) {
      stop("Matrix is numerically singular and cannot be inverted.")
    }

    v_keep <- s$v[, keep, drop = FALSE]
    u_keep <- s$u[, keep, drop = FALSE]
    d_inv <- diag(1 / s$d[keep], nrow = sum(keep), ncol = sum(keep))
    v_keep %*% d_inv %*% t(u_keep)
  }

  .stable_solve <- function(mat, rhs = NULL) {
    out <- tryCatch(
      if (is.null(rhs)) {
        solve(mat)
      } else {
        solve(mat, rhs)
      },
      error = function(e) NULL
    )

    if (!is.null(out)) {
      return(out)
    }

    pinv <- .pseudo_inverse(mat)
    if (is.null(rhs)) {
      pinv
    } else {
      pinv %*% rhs
    }
  }

  .safe_predict_pls <- function(model, newdata, ncomp_use) {
    x_train <- as.matrix(model$X)
    y_train <- model$Y
    if (is.null(dim(y_train))) {
      y_train <- matrix(
        y_train,
        ncol = 1,
        dimnames = list(rownames(x_train), response_col)
      )
    } else {
      y_train <- as.matrix(y_train)
    }
    if (is.null(colnames(y_train))) {
      colnames(y_train) <- response_col
    }

    newdata_mat <- as.matrix(newdata)
    if (is.null(dim(newdata_mat))) {
      newdata_mat <- matrix(newdata_mat, nrow = 1L)
    }
    if (is.null(colnames(newdata_mat))) {
      colnames(newdata_mat) <- colnames(x_train)
    }
    if (is.null(rownames(newdata_mat))) {
      rownames(newdata_mat) <- seq_len(nrow(newdata_mat))
    }

    if (!identical(colnames(newdata_mat), colnames(x_train))) {
      if (setequal(colnames(newdata_mat), colnames(x_train))) {
        newdata_mat <- newdata_mat[, colnames(x_train), drop = FALSE]
      } else {
        stop(
          "Prediction data must include the same predictor columns used to fit the model."
        )
      }
    }

    centered_newdata <- newdata_mat
    center_x <- attr(x_train, "scaled:center")
    if (!is.null(center_x)) {
      centered_newdata <- sweep(
        centered_newdata,
        2,
        STATS = center_x,
        FUN = "-"
      )
    }
    if (isTRUE(model$scale)) {
      scale_x <- attr(x_train, "scaled:scale")
      if (!is.null(scale_x)) {
        centered_newdata <- sweep(
          centered_newdata,
          2,
          STATS = scale_x,
          FUN = "/"
        )
      }
    }

    x_train[is.na(x_train)] <- 0
    centered_newdata[is.na(centered_newdata)] <- 0
    y_train[is.na(y_train)] <- 0

    variates_x <- as.matrix(model$variates$X)
    loadings_x <- as.matrix(model$loadings$X)
    if (is.null(dim(variates_x))) {
      variates_x <- matrix(variates_x, ncol = 1L)
    }
    if (is.null(dim(loadings_x))) {
      loadings_x <- matrix(loadings_x, ncol = 1L)
    }

    ncomp_use <- max(
      1L,
      min(
        as.integer(ncomp_use),
        ncol(variates_x),
        ncol(loadings_x)
      )
    )

    pmat <- crossprod(x_train, variates_x)
    cmat <- crossprod(y_train, variates_x)

    beta_list <- lapply(seq_len(ncomp_use), function(k) {
      w_k <- loadings_x[, seq_len(k), drop = FALSE]
      p_k <- pmat[, seq_len(k), drop = FALSE]
      c_k <- t(cmat)[seq_len(k), , drop = FALSE]
      w_k %*% .stable_solve(t(p_k) %*% w_k, c_k)
    })

    means_y_vec <- attr(model$Y, "scaled:center")
    if (is.null(means_y_vec)) {
      means_y_vec <- rep(0, ncol(y_train))
    }
    sigma_y_vec <- if (isTRUE(model$scale)) {
      attr(model$Y, "scaled:scale")
    } else {
      NULL
    }
    if (is.null(sigma_y_vec)) {
      sigma_y_vec <- rep(1, ncol(y_train))
    }

    means_y <- matrix(
      means_y_vec,
      nrow = nrow(centered_newdata),
      ncol = ncol(y_train),
      byrow = TRUE
    )
    sigma_y <- matrix(
      sigma_y_vec,
      nrow = nrow(centered_newdata),
      ncol = ncol(y_train),
      byrow = TRUE
    )

    predict_arr <- array(
      NA_real_,
      dim = c(nrow(centered_newdata), ncol(y_train), ncomp_use),
      dimnames = list(
        rownames(newdata_mat),
        colnames(y_train),
        paste0("dim", seq_len(ncomp_use))
      )
    )

    for (k in seq_len(ncomp_use)) {
      pred_k <- centered_newdata %*% beta_list[[k]]
      predict_arr[, , k] <- pred_k * sigma_y + means_y
    }

    tpred_raw <- centered_newdata %*%
      loadings_x[, seq_len(ncomp_use), drop = FALSE] %*%
      .stable_solve(
        t(pmat[, seq_len(ncomp_use), drop = FALSE]) %*%
          loadings_x[, seq_len(ncomp_use), drop = FALSE]
      )
    variate_norms <- apply(
      variates_x[, seq_len(ncomp_use), drop = FALSE],
      2,
      function(v) sum(v^2)
    )
    variates_pred <- sweep(tpred_raw, 2, STATS = variate_norms, FUN = "*")
    rownames(variates_pred) <- rownames(newdata_mat)
    colnames(variates_pred) <- paste0("dim", seq_len(ncomp_use))

    bhat_arr <- array(
      NA_real_,
      dim = c(ncol(newdata_mat), ncol(y_train), ncomp_use),
      dimnames = list(
        colnames(newdata_mat),
        colnames(y_train),
        paste0("dim", seq_len(ncomp_use))
      )
    )
    for (k in seq_len(ncomp_use)) {
      bhat_arr[, , k] <- beta_list[[k]]
    }

    list(
      predict = predict_arr,
      variates = variates_pred,
      B.hat = bhat_arr
    )
  }

  .predict_pls <- function(model, newdata, ncomp_use) {
    if (length(ncomp_use) == 1L && as.integer(ncomp_use) == 1L) {
      return(.safe_predict_pls(model, newdata, ncomp_use))
    }

    pred <- tryCatch(
      stats::predict(model, newdata = newdata),
      error = function(e) e
    )

    if (!inherits(pred, "error")) {
      return(pred)
    }

    if (grepl(
      "attempt to set 'rownames' on an object with no dimensions",
      conditionMessage(pred),
      fixed = TRUE
    )) {
      return(.safe_predict_pls(model, newdata, ncomp_use))
    }

    stop(conditionMessage(pred), call. = FALSE)
  }

  .plot_indiv <- function(model, groups, title, ind_names_resolved) {
    comp_available <- tryCatch(
      ncol(model$variates$X),
      error = function(e) 0L
    )

    if (is.null(comp_available) || comp_available < 2L) {
      scores <- as.numeric(model$variates$X[, 1])
      point_cols <- cols[as.character(groups)]
      point_cols[is.na(point_cols)] <- "black"
      y_pos <- rep(0, length(scores))

      graphics::plot(
        x = scores,
        y = y_pos,
        col = point_cols,
        pch = 19,
        xlab = "Component 1",
        ylab = "",
        yaxt = "n",
        main = title
      )
      graphics::abline(h = 0, lty = 2, col = "gray80")

      if (isTRUE(ind_names_resolved)) {
        labels_use <- rownames(model$variates$X)
        if (is.null(labels_use)) {
          labels_use <- as.character(seq_along(scores))
        }
        graphics::text(scores, y_pos, labels = labels_use, pos = 3, cex = 0.7)
      } else if (is.character(ind_names_resolved)) {
        graphics::text(
          scores,
          y_pos,
          labels = ind_names_resolved,
          pos = 3,
          cex = 0.7
        )
      }

      graphics::legend(
        "topright",
        legend = levels(groups),
        col = cols[levels(groups)],
        pch = 19,
        bty = "n",
        title = if (!is.null(group_col)) group_col else "Group"
      )
      return(invisible(NULL))
    }

    args <- c(
      list(
        model,
        group = groups,
        legend = TRUE,
        col = cols[levels(groups)],
        title = title,
        legend.title = if (!is.null(group_col)) group_col else "Group",
        ellipse = isTRUE(ellipse)
      ),
      mixomics_indiv_args
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
  comp_num_eff <- max(1, min(comp_num, ncol(X), nrow(X) - 1))
  if (isTRUE(sparse)) {
    if (is.null(var_num)) {
      stop("For sPLS (sparse=TRUE), please set `var_num`.")
    }
    keepx <- rep(max(1, min(var_num, p)), comp_num_eff)
  }
  if (!is.null(progress)) {
    progress$inc(
      0.1,
      detail = paste("Fitting", if (sparse) "sPLS" else "PLS", "model")
    )
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
    progress$inc(0.1, detail = "Calculating predictions")
  }
  pr <- .predict_pls(mdl, X, comp_num_eff)
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
  if (!is.null(progress)) {
    progress$inc(0.15, detail = "Building score and diagnostic plots")
  }
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
    p <- apply_font_settings_ggplot(p, resolved_fonts)
    print(p)
  })
  res_plot <- .record_plot({
    dfr <- data.frame(Fitted = yhat, Residuals = y - yhat)
    p <- ggplot2::ggplot(dfr, ggplot2::aes(Fitted, Residuals)) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::labs(title = "Residuals vs Fitted")
    p <- apply_font_settings_ggplot(p, resolved_fonts)
    print(p)
  })

  # --- CV performance (optional)
  cv_plot <- NULL

  if (!is.null(cv_opt)) {
    if (!is.null(progress)) {
      progress$inc(0.1, detail = "Running cross-validation")
    }
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
              data = data.frame(
                metric = factor("Q-Squared", levels = c("Q-Squared", "RMSEP")),
                yint = 0
              ),
              ggplot2::aes(yintercept = yint),
              linetype = "dashed",
              color = "gray40"
            )
          g <- apply_font_settings_ggplot(g, resolved_fonts)

          print(g)
        } else {
          empty_plot <- ggplot2::ggplot() +
            ggplot2::theme_minimal() +
            ggplot2::labs(title = "CV results not available.")
          empty_plot <- apply_font_settings_ggplot(empty_plot, resolved_fonts)
          print(empty_plot)
        }
      })
    }
  }

  # --- loadings
  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Building loadings and VIP summaries")
  }
  loadings <- lapply(seq_len(comp_num_eff), function(k) {
    .record_plot({
      do.call(
        mixOmics::plotLoadings,
        c(
          list(
            mdl,
            comp = k,
            contrib = "max",
            method = "mean",
            legend = FALSE,
            title = paste("Loadings Comp", k)
          ),
          mixomics_loadings_args
        )
      )
    })
  })

  # --- VIP & optional VIP>1 refit preview
  q2_vals_vip <- numeric(0)
  rmsep_vals_vip <- numeric(0)

  vip_scores <- vip_indiv_plot <- vip_cv_plot <- NULL
  vip_all <- try(mixOmics::vip(mdl), silent = TRUE)
  if (!inherits(vip_all, "try-error")) {
    vip_all <- as.matrix(vip_all)
    vip_scores <- lapply(seq_len(comp_num_eff), function(k) {
      v <- data.frame(
        variable = rownames(vip_all),
        score = vip_all[, k],
        row.names = NULL
      )
      v <- v[v$score > 0, ]
      if (nrow(v) == 0L) {
        return(NULL)
      }
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
        ) |>
          apply_font_settings_ggplot(font_settings = resolved_fonts)
    })
    vip_filter <- !is.na(vip_all[, 1]) & vip_all[, 1] > 1

    vip_count <- sum(vip_filter)
    if (vip_count == 1L) {
      metrics_notes <- c(
        metrics_notes,
        paste(
          "Skipped VIP>1 preview because only one predictor remained:",
          rownames(vip_all)[vip_filter]
        )
      )
    } else if (vip_count > 1L) {
      Xvip <- X[, vip_filter, drop = FALSE]
      vip_row_keep <- rowSums(!is.na(as.matrix(Xvip))) > 0
      Xvip_fit <- Xvip[vip_row_keep, , drop = FALSE]
      y_vip <- y[vip_row_keep]

      if (nrow(Xvip_fit) < 3L) {
        metrics_notes <- c(
          metrics_notes,
          paste(
            "Skipped VIP>1 preview because only",
            nrow(Xvip_fit),
            "rows had at least one retained VIP predictor."
          )
        )
      } else if (length(unique(y_vip[is.finite(y_vip)])) < 2L) {
        metrics_notes <- c(
          metrics_notes,
          paste(
            "Skipped VIP>1 preview because the retained response values",
            "did not vary after VIP filtering."
          )
        )
      } else {
        comp_num_eff_vip <- max(
          1,
          min(comp_num_eff, ncol(Xvip_fit), nrow(Xvip_fit) - 1)
        )
        mdl_vip <- if (isTRUE(sparse)) {
          mixOmics::spls(
            Xvip_fit,
            y_vip,
            ncomp = comp_num_eff_vip,
            keepX = rep(ncol(Xvip_fit), comp_num_eff_vip),
            scale = TRUE,
            mode = "regression"
          )
        } else {
          mixOmics::pls(
            Xvip_fit,
            y_vip,
            ncomp = comp_num_eff_vip,
            scale = TRUE,
            mode = "regression"
          )
        }
        pr_vip <- .predict_pls(mdl_vip, Xvip_fit, comp_num_eff_vip)
        yhat_vip <- as.numeric(drop(pr_vip$predict[, 1, comp_num_eff_vip]))
        r2_vip <- suppressWarnings(
          stats::cor(y_vip, yhat_vip, use = "complete.obs")^2
        )
        vip_indiv_plot <- .record_plot(.plot_indiv(
          mdl_vip,
          droplevels(factor(groups[vip_row_keep])),
          sprintf("Scores (VIP>1) R-Squared(train)=%.2f", r2_vip),
          ind_names_resolved = .subset_ind_names(lab_res, vip_row_keep)
        ))
        if (!is.null(cv_opt)) {
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
            folds_safe_vip <- max(2, min(fold_num, nrow(Xvip_fit)))
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
            K <- comp_num_eff_vip

            # Extract metrics using '$measures$METRIC$summary$mean'
            q2_vals_vip <- as.numeric(per2$measures$Q2.total$summary$mean)
            msep_vals_vip <- as.numeric(per2$measures$MSEP$summary$mean)

            # Calculate RMSEP and handle cases where perf includes a "comp 0"
            rmsep_vals_vip <- sqrt(tail(msep_vals_vip, K))
            q2_vals_vip <- tail(q2_vals_vip, K)

            # Build a tidy data frame for plotting
            df_vip_perf <- data.frame(
              Component = c(seq_along(q2_vals_vip), seq_along(rmsep_vals_vip)),
              value = c(q2_vals_vip, rmsep_vals_vip),
              metric = factor(
                c(
                  rep("Q-Squared", length(q2_vals_vip)),
                  rep("RMSEP", length(rmsep_vals_vip))
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
                    title = paste("Cross-validation (VIP > 1):", cv_title),
                    x = "Components",
                    y = NULL
                  ) +
                  ggplot2::theme_minimal(base_size = 12) +
                  ggplot2::geom_hline(
                    data = data.frame(
                      metric = factor(
                        "Q-Squared",
                        levels = c("Q-Squared", "RMSEP")
                      ),
                      yint = 0
                    ),
                    ggplot2::aes(yintercept = yint),
                    linetype = "dashed",
                    color = "gray40"
                  )
                g <- apply_font_settings_ggplot(g, resolved_fonts)
                print(g)
              } else {
                empty_plot <- ggplot2::ggplot() + ggplot2::theme_minimal()
                empty_plot <- apply_font_settings_ggplot(
                  empty_plot,
                  resolved_fonts
                )
                print(empty_plot)
              }
            })
          }
        }
      }
    }
  }

  # --- Optional PDF output (regenerate key plots for device)
  if (!is.null(output_file)) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Writing output file")
    }
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
    print(grDevices::replayPlot(pvo))
    print(grDevices::replayPlot(res_plot))
    if (!is.null(cv_plot)) {
      print(grDevices::replayPlot(cv_plot))
    }
    for (k in seq_len(comp_num_eff)) {
      do.call(
        mixOmics::plotLoadings,
        c(
          list(
            mdl,
            comp = k,
            contrib = "max",
            method = "mean",
            legend = FALSE,
            title = paste("Loadings Comp", k)
          ),
          mixomics_loadings_args
        )
      )
    }
    if (!is.null(vip_indiv_plot)) {
      print(grDevices::replayPlot(vip_indiv_plot))
    }
    if (!is.null(vip_cv_plot)) print(grDevices::replayPlot(vip_cv_plot))
  }

  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Formatting results")
  }
  # --- build metrics text ---
  metrics_text <- if (!is.null(cv_opt)) {
    paste(
      sprintf("Q2 (comp %d): %.3f", seq_along(q2_vals), q2_vals),
      sprintf("RMSEP (comp %d): %.3f", seq_along(rmsep_vals), rmsep_vals),
      if (length(q2_vals_vip)) {
        sprintf("Q2 VIP>1 (comp %d): %.3f", seq_along(q2_vals_vip), q2_vals_vip)
      },
      if (length(rmsep_vals_vip)) {
        sprintf(
          "RMSEP VIP>1 (comp %d): %.3f",
          seq_along(rmsep_vals_vip),
          rmsep_vals_vip
        )
      },
      unique(metrics_notes),
      collapse = "\n"
    )
  } else {
    paste(
      c("No cross-validation performed.", unique(metrics_notes)),
      collapse = "\n"
    )
  }
  if (!is.null(progress)) {
    progress$set(message = analysis_label, value = 1, detail = "Finished")
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
    metrics_text = metrics_text,
    pdf_file = output_file
  )
}
