#' Analyze data with Sparse Partial Least Squares Discriminant Analysis (sPLS-DA).
#'
#' @param data A matrix or data frame containing the variables. Columns not
#'   specified by \code{group_col}, \code{group_col2}, or \code{multilevel_col} are assumed to be continuous
#'   variables for analysis.
#' @param group_col A string specifying the first grouping column name that contains grouping
#'   information. If \code{group_col2} is not provided, it will be used for both
#'   grouping and treatment.
#' @param group_col2 A string specifying the second grouping column name. Default is
#'   \code{NULL}.
#' @param batch_col A string specifying the column that identifies the batch or study for each sample.
#' @param ind_names  If \code{TRUE}, the row names of the first (or second) data matrix is used as names.
#'   Default is \code{FALSE}. If a character vector is provided, these values will be used as names.
#'   If 'pch' is set this will overwrite the names as shapes. See ?mixOmics::plotIndiv for details.
#' @param splsda_colors A vector of splsda_colors for the groups or treatments. If
#'   \code{NULL}, a random palette (using \code{rainbow}) is generated based on
#'   the number of groups.
#' @param output_file A string specifying the file name for saving the PDF output.
#'                If set to NULL, the function runs in interactive mode.
#' @param ellipse Logical. Whether to draw a 95\% confidence ellipse on the figures.
#'   Default is \code{FALSE}.
#' @param bg Logical. Whether to draw the prediction background in the figures.
#'   Default is \code{FALSE}.
#' @param conf_mat Logical. Whether to print the confusion matrix for the classifications.
#'   Default is \code{FALSE}.
#' @param var_num Numeric. The number of variables to be used in the PLS-DA model.
#' @param cv_opt Character. Option for cross-validation method: either "loocv" or "Mfold".
#'   Default is \code{NULL}.
#' @param fold_num Numeric. The number of folds to use if \code{cv_opt} is "Mfold". Default is 5.
#' @param scale Character. Option for data transformation; if set to \code{"log2"}, a log2
#'   transformation is applied to the continuous variables. Default is \code{NULL}.
#' @param comp_num Numeric. The number of components to calculate in the sPLS-DA model.
#'   Default is 2.
#' @param pch_values A vector of integers specifying the plotting characters (pch values)
#'   to be used in the plots.
#' @param style Character. If set to \code{"3D"} or \code{"3d"} and \code{comp_num} equals 3,
#'   a 3D plot is generated using the \code{plot3D} package. Default is \code{NULL}.
#' @param roc Logical. Whether to compute and plot the ROC curve for the model.
#'   Default is \code{FALSE}.
#' @param multilevel_col A string specifying the column name that identifies
#'   repeated measurements (e.g., patient or sample IDs). If provided, a
#'   multilevel analysis will be performed. Default is \code{NULL}.
#'
#' @return In Download mode (output_file not NULL), a PDF file is written and the function
#'         returns NULL invisibly. In Interactive mode (output_file = NULL), a named list is
#'         returned with the following elements (in this order):
#'         1. overall_indiv_plot: Main individual classification plot
#'         2. overall_3D: Main 3D plot (if generated)
#'         3. overall_ROC: ROC curve plot for the overall model
#'         4. overall_CV: Cross-validation error plot for the overall model
#'         5. loadings: A list of loadings plots (one per component)
#'         6. vip_scores: A list of VIP score bar plots for each component
#'         7. vip_indiv_plot: Main individual plot for the VIP>1 model
#'         8. vip_3D: 3D plot for the VIP>1 model (if generated)
#'         9. vip_ROC: ROC curve plot for the VIP>1 model
#'         10. vip_CV: Cross-validation error plot for the VIP>1 model
#'         11. conf_matrix: Confusion matrix text output
#'
#' @examples
#' data_df <- ExampleData1[,-c(3)]
#' data_df <- dplyr::filter(data_df, Group != "ND", Treatment != "Unstimulated")
#'
#' cyt_splsda(data_df, output_file = NULL,
#' splsda_colors = c("black", "purple"), bg = FALSE, scale = "log2",
#' conf_mat = FALSE, var_num = 25, cv_opt = NULL, comp_num = 2,
#' pch_values = c(16, 4), style = NULL, ellipse = TRUE,
#' group_col = "Group", group_col2 = "Treatment", roc = FALSE)
#'
#' @export
#' @importFrom mixOmics splsda background.predict perf vip auroc plotIndiv plotLoadings
#' @import ggplot2
#' @importFrom plot3D scatter3D
#' @importFrom reshape2 melt
#' @importFrom caret confusionMatrix
cyt_splsda <- function(
  data,
  group_col,
  group_col2 = NULL,
  batch_col = NULL,
  ind_names = FALSE,
  multilevel_col = NULL,
  var_num,
  comp_num = 2,
  cv_opt = NULL,
  fold_num = 5,
  scale = NULL,
  ellipse = FALSE,
  bg = FALSE,
  roc = FALSE,
  conf_mat = FALSE,
  style = NULL,
  splsda_colors = NULL,
  pch_values = NULL,
  output_file = NULL,
  progress = NULL
) {
  `%notin%` <- function(x, y) !(x %in% y)

  if (!is.null(progress)) {
    progress$set(message = "Starting sPLS-DA...", value = 0)
  }

  # --- arg normalization ---------------------------------------------------
  if (is.null(group_col) && !is.null(group_col2)) {
    group_col <- group_col2
  }
  if (is.null(group_col2) && !is.null(group_col)) {
    group_col2 <- group_col
  }
  if (is.null(group_col)) {
    stop("At least one grouping column (group_col) must be provided.")
  }
  if (group_col %notin% names(data)) {
    stop("Grouping column '", group_col, "' not found.")
  }

  # --- id cols & transforms (numeric-only) --------------------------------
  id_cols <- unique(na.omit(c(
    group_col,
    group_col2,
    multilevel_col,
    batch_col
  )))
  id_cols <- id_cols[id_cols %in% names(data)]

  if (identical(scale, "log2")) {
    num_cols <- names(data)[
      vapply(data, is.numeric, logical(1)) & names(data) %notin% id_cols
    ]
    if (length(num_cols)) {
      # offset if needed
      if (any(data[, num_cols] <= 0, na.rm = TRUE)) {
        min_pos <- suppressWarnings(min(
          data[, num_cols][data[, num_cols] > 0],
          na.rm = TRUE
        ))
        off <- if (is.finite(min_pos)) min_pos / 2 else 1e-6
        data[, num_cols] <- log2(data[, num_cols] + off)
        warning(
          "Non-positive values detected; applied log2 with a small offset."
        )
      } else {
        data[, num_cols] <- log2(data[, num_cols])
      }
    }
  }

  # within-batch z-scaling (only if sensible)
  if (!is.null(batch_col) && batch_col %in% names(data)) {
    if (dplyr::n_distinct(data[[batch_col]]) > 1) {
      num_cols2 <- names(data)[
        vapply(data, is.numeric, logical(1)) & names(data) %notin% id_cols
      ]
      if (length(num_cols2)) {
        data <- dplyr::group_by(data, !!rlang::sym(batch_col)) |>
          dplyr::mutate(dplyr::across(
            dplyr::all_of(num_cols2),
            ~ {
              s <- stats::sd(., na.rm = TRUE)
              if (isTRUE(all(is.na(.))) || isTRUE(s == 0) || is.na(s)) {
                return(rep(NA_real_, length(.)))
              }
              (. - mean(., na.rm = TRUE)) / s
            }
          )) |>
          dplyr::ungroup()
      }
    }
  }

  # --- aesthetics by factor level (stable legend) -------------------------
  levs_all <- levels(droplevels(factor(data[[group_col]])))
  if (is.null(splsda_colors) || !length(splsda_colors)) {
    col_levels <- grDevices::rainbow(length(levs_all))
  } else {
    col_levels <- rep(splsda_colors, length.out = length(levs_all))
  }
  if (is.null(pch_values) || !length(pch_values)) {
    pch_levels <- rep(16, length(levs_all))
  } else {
    pch_levels <- rep(pch_values, length.out = length(levs_all))
  }
  names(col_levels) <- names(pch_levels) <- levs_all

  # helper to compute accuracy cleanly (avoids 0% bug)
  .acc_pct <- function(pred_class, ref_factor) {
    ref_factor <- droplevels(ref_factor)
    pred_factor <- factor(pred_class, levels = levels(ref_factor))
    100 * mean(pred_factor == ref_factor, na.rm = TRUE)
  }

  # helper to build predictors / groups from a subset
  .x_y <- function(df) {
    exclude <- unique(na.omit(c(
      group_col,
      group_col2,
      multilevel_col,
      batch_col
    )))
    exclude <- exclude[exclude %in% names(df)]
    X <- df[, setdiff(names(df), exclude), drop = FALSE]
    X <- X[, vapply(X, is.numeric, logical(1)), drop = FALSE]
    if (ncol(X) < 2) {
      stop("Need at least 2 numeric predictors after filtering/selection.")
    }
    Y <- droplevels(factor(df[[group_col]]))
    if (nlevels(Y) < 2) {
      stop("Grouping variable must have at least two levels.")
    }
    list(X = X, Y = Y)
  }

  # resolve labels for the current subset (TRUE, FALSE, or character vector)
  .resolve_ind_names <- function(df_subset, ind_names, parent_n, parent_rownm) {
    if (isTRUE(ind_names)) {
      return(TRUE)
    }
    if (!is.character(ind_names)) {
      return(FALSE)
    }

    # Case A: user supplied exactly the labels for this subset
    if (length(ind_names) == nrow(df_subset) && is.null(names(ind_names))) {
      return(ind_names)
    }

    # Case B: user supplied a named vector keyed by parent rownames
    rn_sub <- rownames(df_subset)
    if (!is.null(names(ind_names)) && !is.null(rn_sub)) {
      lab <- ind_names[rn_sub]
      if (sum(!is.na(lab)) >= 1) return(lab)
    }

    # Case C: user supplied full-length vector for the parent data (no names)
    if (
      length(ind_names) == parent_n &&
        !is.null(parent_rownm) &&
        !is.null(rn_sub)
    ) {
      # rely on rownames alignment of subset with parent
      return(ind_names[match(rn_sub, parent_rownm)])
    }

    warning(
      "`ind_names` provided but could not be reliably aligned; using default sample names."
    )
    TRUE
  }

  # draw indiv plot (shared by interactive and pdf)
  .plot_indiv <- function(
    model,
    groups,
    title,
    ind_names_resolved,
    bg_on = FALSE
  ) {
    bg_obj <- NULL
    if (isTRUE(bg)) {
      bg_obj <- mixOmics::background.predict(
        model,
        comp.predicted = min(2, ncol(model$variates$X)),
        dist = "max.dist",
        xlim = NULL,
        ylim = NULL,
        resolution = 300
      )
    }

    # Build args without pch/ind.names first
    args <- list(
      model,
      group = groups,
      legend = TRUE,
      col = col_levels[levels(groups)],
      title = title,
      legend.title = group_col,
      ellipse = isTRUE(ellipse),
      background = bg_obj
    )

    # Key rule: labels OR shapes, never both
    if (isTRUE(ind_names_resolved) || is.character(ind_names_resolved)) {
      args$ind.names <- ind_names_resolved
    } else {
      args$ind.names <- FALSE
      args$pch <- pch_levels[levels(groups)]
    }

    do.call(mixOmics::plotIndiv, args)
  }

  # record base graphics into replayable plot objects
  .record_plot <- function(expr) {
    if (grDevices::dev.cur() == 1) {
      grDevices::png(tempfile(fileext = ".png"))
      on.exit(grDevices::dev.off(), add = TRUE)
    }
    grDevices::dev.control(displaylist = "enable")
    force(expr)
    grDevices::recordPlot()
  }

  # main worker for one dataset (overall or by level of group_col2)
  run_one <- function(df_subset, label) {
    if (!is.null(progress)) {
      progress$inc(0.05, detail = paste("Preparing:", label))
    }
    xy <- .x_y(df_subset)
    X <- xy$X
    Y <- xy$Y
    multilevel_df <- if (!is.null(multilevel_col)) {
      data.frame(sample = df_subset[[multilevel_col]])
    } else {
      NULL
    }

    p <- ncol(X)
    var_num_eff <- max(1, min(var_num, p))
    comp_num_eff <- max(1, min(comp_num, p))

    # fit
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Fitting sPLS-DA")
    }
    args <- list(
      X = X,
      Y = Y,
      scale = TRUE,
      ncomp = comp_num_eff,
      keepX = rep(var_num_eff, comp_num_eff)
    )
    if (!is.null(multilevel_df)) {
      args$multilevel <- multilevel_df
    }
    mdl <- do.call(mixOmics::splsda, args)

    # predict + accuracy for selected component
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Predicting")
    }
    pred <- stats::predict(mdl, X, dist = "max.dist")
    pred_class <- pred$class$max.dist[, comp_num_eff]
    acc1 <- .acc_pct(pred_class, Y)

    # plots (interactive)
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Plotting")
    }
    lab_res <- .resolve_ind_names(
      df_subset,
      ind_names,
      nrow(data),
      rownames(data)
    )
    if ((isTRUE(lab_res) || is.character(lab_res)) && !is.null(pch_values)) {
      message(
        "`ind_names` is TRUE/character; ignoring `pch_values` for plotIndiv (labels and shapes are mutually exclusive)."
      )
    }

    indiv_plot <- .record_plot(.plot_indiv(
      mdl,
      Y,
      sprintf("%s With Accuracy: %s %%", label, round(acc1, 1)),
      ind_names_resolved = lab_res
    ))

    indiv_3D <- NULL
    if (!is.null(style) && tolower(style) == "3d" && comp_num_eff == 3) {
      indiv_3D <- .record_plot({
        sc <- mdl$variates$X
        idx <- as.integer(Y)
        plot3D::scatter3D(
          sc[, 1],
          sc[, 2],
          sc[, 3],
          col = col_levels[levels(Y)][idx],
          pch = pch_levels[levels(Y)][idx],
          xlab = "Component 1",
          ylab = "Component 2",
          zlab = "Component 3",
          main = paste("3D Plot:", label),
          theta = 20,
          phi = 30,
          bty = "g",
          colkey = FALSE
        )
      })
    }

    indiv_ROC <- NULL
    if (isTRUE(roc)) {
      indiv_ROC <- .record_plot({
        mixOmics::auroc(
          mdl,
          newdata = X,
          outcome.test = Y,
          plot = TRUE,
          roc.comp = comp_num_eff,
          title = paste("ROC Curve:", label),
          print = FALSE
        )
      })
    }

    # CV (optional)
    indiv_CV <- NULL
    if (!is.null(cv_opt)) {
      if (identical(cv_opt, "loocv")) {
        set.seed(123)
        cv_res <- mixOmics::perf(mdl, validation = "loo")
        er <- cv_res$error.rate$overall[, "max.dist"]
        indiv_CV <- ggplot2::ggplot(
          data.frame(Component = seq_along(er), ErrorRate = er),
          ggplot2::aes(Component, ErrorRate)
        ) +
          ggplot2::geom_line() +
          ggplot2::geom_point(size = 3) +
          ggplot2::labs(
            title = paste("LOOCV Error Rate:", label),
            x = "Components",
            y = "Error rate"
          ) +
          ggplot2::theme_minimal()
      } else if (identical(cv_opt, "Mfold")) {
        set.seed(123)
        folds_safe <- max(2, min(fold_num, nrow(X)))
        cv_res <- mixOmics::perf(
          mdl,
          validation = "Mfold",
          folds = folds_safe,
          nrepeat = 100
        )
        er <- cv_res$error.rate$overall[, "max.dist"]
        indiv_CV <- ggplot2::ggplot(
          data.frame(Component = seq_along(er), ErrorRate = er),
          ggplot2::aes(Component, ErrorRate)
        ) +
          ggplot2::geom_line() +
          ggplot2::geom_point(size = 3) +
          ggplot2::labs(
            title = paste("Mfold Error Rate:", label),
            x = "Components",
            y = "Error rate"
          ) +
          ggplot2::theme_minimal()
      }
    }

    # loadings + VIP
    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Loadings & VIP")
    }
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
          legend.color = col_levels[levels(Y)],
          title = paste("Loadings Comp", k, ":", label),
          legend = TRUE
        )
      })
    })

    vip_all <- mixOmics::vip(mdl)
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
        ) +
        ggplot2::theme_minimal()
    })

    vip_indiv_plot <- vip_loadings <- vip_3D <- vip_ROC <- vip_CV <- NULL
    vip_pred <- NULL
    vip_filter <- vip_all[, 1] > 1
    if (sum(vip_filter) > 0) {
      Xvip <- X[, vip_filter, drop = FALSE]
      args2 <- list(
        X = Xvip,
        Y = Y,
        scale = TRUE,
        ncomp = comp_num_eff,
        keepX = rep(ncol(Xvip), comp_num_eff)
      )
      if (!is.null(multilevel_df)) {
        args2$multilevel <- multilevel_df
      }
      mdl_vip <- do.call(mixOmics::splsda, args2)
      vip_pred <- stats::predict(mdl_vip, Xvip, dist = "max.dist")
      vip_class <- vip_pred$class$max.dist[, comp_num_eff]
      acc2 <- .acc_pct(vip_class, Y)

      vip_indiv_plot <- .record_plot(.plot_indiv(
        mdl_vip,
        Y,
        sprintf(
          "sPLS-DA (VIP > 1): %s With Accuracy: %s %%",
          label,
          round(acc2, 1)
        ),
        ind_names_resolved = lab_res
      ))
      vip_loadings <- lapply(seq_len(comp_num_eff), function(k) {
        .record_plot({
          mixOmics::plotLoadings(
            mdl_vip,
            comp = k,
            contrib = "max",
            method = "mean",
            size.names = 1,
            size.legend = 1,
            size.title = 1,
            legend.color = col_levels[levels(Y)],
            title = paste("Loadings (VIP>1) Comp", k, ":", label),
            legend = TRUE
          )
        })
      })
      if (!is.null(style) && tolower(style) == "3d" && comp_num_eff == 3) {
        vip_3D <- .record_plot({
          sc <- mdl_vip$variates$X
          idx <- as.integer(Y)
          plot3D::scatter3D(
            sc[, 1],
            sc[, 2],
            sc[, 3],
            col = col_levels[levels(Y)][idx],
            pch = pch_levels[levels(Y)][idx],
            xlab = "Component 1",
            ylab = "Component 2",
            zlab = "Component 3",
            main = paste("3D Plot (VIP>1):", label),
            theta = 20,
            phi = 30,
            bty = "g",
            colkey = FALSE
          )
        })
      }
      if (isTRUE(roc)) {
        vip_ROC <- .record_plot({
          mixOmics::auroc(
            mdl_vip,
            newdata = Xvip,
            outcome.test = Y,
            plot = TRUE,
            roc.comp = comp_num_eff,
            title = paste("ROC Curve (VIP>1):", label),
            print = FALSE
          )
        })
      }
      if (!is.null(cv_opt)) {
        if (identical(cv_opt, "loocv")) {
          set.seed(123)
          cv2 <- mixOmics::perf(mdl_vip, validation = "loo")
          er <- cv2$error.rate$overall[, "max.dist"]
          vip_CV <- ggplot2::ggplot(
            data.frame(Component = seq_along(er), ErrorRate = er),
            ggplot2::aes(Component, ErrorRate)
          ) +
            ggplot2::geom_line() +
            ggplot2::geom_point(size = 3) +
            ggplot2::labs(
              title = paste("LOOCV Error Rate (VIP>1):", label),
              x = "Components",
              y = "Error rate"
            ) +
            ggplot2::theme_minimal()
        } else if (identical(cv_opt, "Mfold")) {
          set.seed(123)
          folds_safe <- max(2, min(fold_num, nrow(Xvip)))
          cv2 <- mixOmics::perf(
            mdl_vip,
            validation = "Mfold",
            folds = folds_safe,
            nrepeat = 100
          )
          er <- cv2$error.rate$overall[, "max.dist"]
          vip_CV <- ggplot2::ggplot(
            data.frame(Component = seq_along(er), ErrorRate = er),
            ggplot2::aes(Component, ErrorRate)
          ) +
            ggplot2::geom_line() +
            ggplot2::geom_point(size = 3) +
            ggplot2::labs(
              title = paste("Mfold Error Rate (VIP>1):", label),
              x = "Components",
              y = "Error rate"
            ) +
            ggplot2::theme_minimal()
        }
      }
    }

    # Confusion matrices (optional)
    conf_text <- NULL
    if (isTRUE(conf_mat)) {
      conf_text_overall <- utils::capture.output({
        cat("Overall Confusion Matrix for PLS-DA Comparison\n")
        cm <- caret::confusionMatrix(
          data = factor(pred_class, levels = levels(Y)),
          reference = Y
        )
        print(cm$table)
        cat("Accuracy:", signif(cm$overall["Accuracy"], 2), "\n")
        if (nlevels(Y) == 2) {
          cat("Sensitivity:", signif(cm$byClass["Sensitivity"], 2), "\n")
          cat("Specificity:", signif(cm$byClass["Specificity"], 2), "\n")
        } else {
          cat("\nPer-Class Sensitivity:\n")
          print(signif(cm$byClass[, "Sensitivity"], 2))
          cat("\nPer-Class Specificity:\n")
          print(signif(cm$byClass[, "Specificity"], 2))
        }
      })
      conf_text_vip <- if (!is.null(vip_pred)) {
        utils::capture.output({
          cat(
            "\n\nOverall Confusion Matrix for PLS-DA Comparison with VIP Score > 1\n"
          )
          cmv <- caret::confusionMatrix(
            data = factor(
              vip_pred$class$max.dist[, comp_num_eff],
              levels = levels(Y)
            ),
            reference = Y
          )
          print(cmv$table)
          cat("Accuracy:", signif(cmv$overall["Accuracy"], 2), "\n")
          if (nlevels(Y) == 2) {
            cat("Sensitivity:", signif(cmv$byClass["Sensitivity"], 2), "\n")
            cat("Specificity:", signif(cmv$byClass["Specificity"], 2), "\n")
          } else {
            cat("\nPer-Class Sensitivity:\n")
            print(signif(cmv$byClass[, "Sensitivity"], 2))
            cat("\nPer-Class Specificity:\n")
            print(signif(cmv$byClass[, "Specificity"], 2))
          }
        })
      } else {
        ""
      }
      conf_text <- c(conf_text_overall, conf_text_vip)
    }

    # --- PDF branch (optional) --------------------------------------------
    if (!is.null(output_file)) {
      op <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(op), add = TRUE)
      grDevices::pdf(output_file, width = 10, height = 8)
      on.exit(grDevices::dev.off(), add = TRUE)

      # 2D indiv
      .plot_indiv(
        mdl,
        Y,
        sprintf("%s With Accuracy: %s %%", label, round(acc1, 1))
      )
      # 3D (if any)
      if (!is.null(style) && tolower(style) == "3d" && comp_num_eff == 3) {
        sc <- mdl$variates$X
        idx <- as.integer(Y)
        plot3D::scatter3D(
          sc[, 1],
          sc[, 2],
          sc[, 3],
          col = col_levels[levels(Y)][idx],
          pch = pch_levels[levels(Y)][idx],
          xlab = "Component 1",
          ylab = "Component 2",
          zlab = "Component 3",
          main = paste("3D Plot:", label),
          theta = 20,
          phi = 30,
          bty = "g",
          colkey = FALSE
        )
      }
      # ROC (if any)
      if (isTRUE(roc)) {
        mixOmics::auroc(
          mdl,
          newdata = X,
          outcome.test = Y,
          plot = TRUE,
          roc.comp = comp_num_eff,
          title = paste("ROC Curve:", label),
          print = FALSE
        )
      }
      # CV (if any)
      if (!is.null(indiv_CV)) {
        print(indiv_CV)
      }

      # loadings
      for (k in seq_len(comp_num_eff)) {
        mixOmics::plotLoadings(
          mdl,
          comp = k,
          contrib = "max",
          method = "mean",
          size.names = 1,
          size.legend = 1,
          size.title = 1,
          legend.color = col_levels[levels(Y)],
          title = paste("Loadings Comp", k, ":", label),
          legend = TRUE
        )
      }

      # VIP branch
      if (sum(vip_filter) > 0) {
        .plot_indiv(
          mdl_vip,
          Y,
          sprintf(
            "sPLS-DA (VIP > 1): %s With Accuracy: %s %%",
            label,
            round(acc2, 1)
          )
        )
        if (!is.null(style) && tolower(style) == "3d" && comp_num_eff == 3) {
          sc2 <- mdl_vip$variates$X
          idx <- as.integer(Y)
          plot3D::scatter3D(
            sc2[, 1],
            sc2[, 2],
            sc2[, 3],
            col = col_levels[levels(Y)][idx],
            pch = pch_levels[levels(Y)][idx],
            xlab = "Component 1",
            ylab = "Component 2",
            zlab = "Component 3",
            main = paste("3D Plot (VIP>1):", label),
            theta = 20,
            phi = 30,
            bty = "g",
            colkey = FALSE
          )
        }
        if (isTRUE(roc)) {
          mixOmics::auroc(
            mdl_vip,
            newdata = Xvip,
            outcome.test = Y,
            plot = TRUE,
            roc.comp = comp_num_eff,
            title = paste("ROC Curve (VIP>1):", label),
            print = FALSE
          )
        }
        if (!is.null(vip_CV)) {
          print(vip_CV)
        }
        for (k in seq_len(comp_num_eff)) {
          mixOmics::plotLoadings(
            mdl_vip,
            comp = k,
            contrib = "max",
            method = "mean",
            size.names = 1,
            size.legend = 1,
            size.title = 1,
            legend.color = col_levels[levels(Y)],
            title = paste("Loadings (VIP>1) Comp", k, ":", label),
            legend = TRUE
          )
        }
      }
    }

    list(
      overall_indiv_plot = indiv_plot,
      overall_3D = indiv_3D,
      overall_ROC = indiv_ROC,
      overall_CV = indiv_CV,
      loadings = loadings,
      vip_scores = vip_scores,
      vip_indiv_plot = vip_indiv_plot,
      vip_loadings = vip_loadings,
      vip_3D = vip_3D,
      vip_ROC = vip_ROC,
      vip_CV = vip_CV,
      conf_matrix = conf_text
    )
  }

  # --- overall vs split by group_col2 -------------------------------------
  if (is.null(group_col2) || identical(group_col, group_col2)) {
    res <- run_one(data, "Overall Analysis")
    if (!is.null(output_file)) {
      res$pdf_file <- output_file
    }
    return(res)
  } else {
    trts <- unique(data[[group_col2]])
    out <- lapply(trts, function(tt) {
      run_one(data[data[[group_col2]] == tt, , drop = FALSE], as.character(tt))
    })
    names(out) <- trts
    if (!is.null(output_file)) {
      attr(out, "pdf_file") <- output_file
    }
    return(out)
  }
}
