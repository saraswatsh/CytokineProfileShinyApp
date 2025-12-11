#' Analyze data with MINT Sparse Partial Least Squares Discriminant Analysis (sPLS-DA).
#'
#' This function performs a MINT (Multivariate INTegrative) sPLS-DA to handle
#' batch effects by modeling a global biological signal across different studies or batches.
#' If a second grouping column (`group_col2`) is provided, the analysis is stratified
#' and performed for each level of that column.
#'
#' @param data A matrix or data frame containing the variables. Columns not
#'   specified by \code{group_col}, \code{group_col2}, or \code{multilevel_col} are assumed to be continuous
#'   variables for analysis.
#' @param group_col A string specifying the first grouping column name that contains grouping
#'   information. If \code{group_col2} is not provided, it will be used for both
#'   grouping and treatment.
#' @param group_col2 A string specifying the second grouping column name. Default is
#'   \code{NULL}.
#' @param batch_col A string specifying the batch column name that contains batch
#'   information.
#' @param colors A vector of colors for the groups or treatments. If
#'   \code{NULL}, a random palette (using \code{rainbow}) is generated based on
#'   the number of groups.
#' @param output_file A string specifying the file name for saving the PDF output.
#'                If set to NULL, the function runs in interactive mode.
#' @param ellipse Logical. Whether to draw a 95\% confidence ellipse on the figures.
#'   Default is \code{FALSE}.
#' @param bg Logical. Whether to draw the prediction background in the figures.
#'   Default is \code{FALSE}.
#' @param var_num Numeric. The number of variables to be used in the PLS-DA model.
#' @param scale Character. Option for data transformation; if set to \code{"log2"}, a log2
#'   transformation is applied to the continuous variables. Default is \code{NULL}.
#' @param comp_num Numeric. The number of components to calculate in the sPLS-DA model.
#'   Default is 2.
#' @param cim Logical. Whether to compute and plot the Clustered Image Map (CIM) heatmap. Default is \code{FALSE}.
#' @param roc Logical. Whether to compute and plot the ROC curve for the model.
#'   Default is \code{FALSE}.
#' @param progress Optional. A Shiny \code{Progress} object for reporting progress updates.
#' @return In Download mode, a PDF file is written. In Interactive mode, a named list
#'         (`results_list`) of plots and results is returned. If `group_col2` is used,
#'         a nested list is returned, with each element corresponding to a level of `group_col2`.
#' @examples
#' # Loading ExampleData5 dataset with batch column
#' data_df <- ExampleData5[,-c(2,4)]
#' data_df <- dplyr::filter(data_df, Group != "ND")
#'
#' cyt_mint_splsda(data_df, group_col = "Group",
#'  batch_col = "Batch", colors = c("black", "purple"),
#'  ellipse = TRUE, var_num = 25, comp_num = 2,
#'  scale = "log2")
#' @export
#' @importFrom mixOmics mint.splsda perf auroc plotIndiv plotLoadings plotVar cim background.predict
#' @import ggplot2
#' @importFrom grDevices rainbow pdf dev.off recordPlot replayPlot
#' @importFrom graphics legend mtext plot.new title
#' @importFrom stats na.omit predict setNames
#' @importFrom utils tail
cyt_mint_splsda <- function(
  data,
  group_col,
  batch_col,
  group_col2 = NULL,
  colors = NULL,
  output_file = NULL,
  ellipse = TRUE,
  bg = FALSE,
  var_num = 20,
  comp_num = 2,
  cim = FALSE,
  scale = NULL,
  roc = FALSE,
  progress = NULL
) {
  # --- Helper function to run the core analysis ---
  run_mint_analysis <- function(
    data_subset,
    analysis_label = "",
    is_pdf_mode = !is.null(output_file)
  ) {
    # --- 1. Data Preparation ---
    data_subset[[group_col]] <- as.factor(data_subset[[group_col]])
    data_subset[[batch_col]] <- as.factor(data_subset[[batch_col]])
    if (nlevels(data_subset[[group_col]]) < 2) {
      message(paste(
        "Skipping '",
        analysis_label,
        "': requires at least two group levels.",
        sep = ""
      ))
      return(NULL)
    }
    if (nlevels(data_subset[[batch_col]]) < 2) {
      message(paste(
        "Skipping '",
        analysis_label,
        "': MINT requires at least two batches.",
        sep = ""
      ))
      return(NULL)
    }
    Y <- droplevels(factor(data_subset[[group_col]]))
    study <- factor(data_subset[[batch_col]])
    X <- data_subset[,
      !(names(data_subset) %in% c(group_col, group_col2, batch_col)),
      drop = FALSE
    ]
    X <- X[, sapply(X, is.numeric)]

    # --- 2. Run MINT sPLS-DA Model ---
    final_model <- mixOmics::mint.splsda(
      X = X,
      Y = Y,
      study = study,
      ncomp = comp_num,
      keepX = rep(var_num, comp_num)
    )

    # --- 3. Calculate Prediction Accuracy ---
    mint_predict <- stats::predict(
      final_model,
      X,
      study.test = study,
      dist = 'max.dist'
    )
    # Get predictions from the final component for the best accuracy
    final_predictions <- mint_predict$class$max.dist[, comp_num]
    accuracy <- sum(Y == final_predictions) / length(Y)
    acc_percent <- signif(accuracy * 100, 2)

    # --- 4. Prepare plot titles and background object ---
    title_label <- if (nzchar(analysis_label)) {
      paste("MINT sPLS-DA:", analysis_label)
    } else {
      "MINT sPLS-DA Global Plot"
    }
    main_title <- paste(title_label, "- Accuracy:", acc_percent, "%")
    bg_obj <- NULL
    if (bg) {
      try(
        {
          bg_obj <- mixOmics::background.predict(
            final_model,
            comp.predicted = 2,
            dist = "max.dist"
          )
        },
        silent = TRUE
      )
    }
    record_base_plot <- function(expr) {
      tf <- tempfile(fileext = ".png")
      grDevices::png(tf, width = 960, height = 720, res = 120)
      on.exit(grDevices::dev.off(), add = TRUE)
      grDevices::dev.control(displaylist = "enable")
      force(expr)
      grDevices::recordPlot()
    }

    # --- 5. Handle Output: PDF vs. Interactive ---
    if (is_pdf_mode) {
      mixOmics::plotIndiv(
        final_model,
        study = "global",
        group = Y,
        col = colors,
        legend = TRUE,
        legend.title = group_col,
        subtitle = main_title,
        ellipse = ellipse,
        background = bg_obj
      )
      mixOmics::plotIndiv(
        final_model,
        study = "all.partial",
        group = Y,
        col = colors,
        legend = TRUE,
        title = paste("Partial Plots:", analysis_label)
      )
      mixOmics::plotVar(
        final_model,
        var.names = FALSE,
        legend = TRUE,
        title = paste("Correlation Circle:", analysis_label)
      )
      if (cim) {
        mixOmics::cim(
          final_model,
          comp = 1,
          row.sideColors = colors[as.numeric(Y)],
          row.names = FALSE,
          title = paste("CIM (Comp 1 -", comp_num, "):", analysis_label)
        )
      }
      for (i in 1:comp_num) {
        mixOmics::plotLoadings(
          final_model,
          comp = i,
          legend.color = colors,
          study = "all.partial",
          contrib = "max",
          method = "mean",
          title = paste(
            "Partial Loadings for Component",
            i,
            "in",
            analysis_label
          )
        )
      }

      if (roc) {
        tryCatch(
          {
            mixOmics::auroc(
              object = final_model,
              newdata = X,
              outcome.test = Y,
              study.test = study,
              roc.study = "global",
              plot = TRUE,
              print = FALSE
            )
          },
          error = function(e) {
            warning(
              "Could not generate ROC curve for PDF output. Error: ",
              e$message
            )
          }
        )
      }
      return(NULL)
    } else {
      # For interactive mode, create and return the list of plot objects
      # Use lapply to create a LIST of plot objects, one for each component
      cim_obj <- NULL
      if (isTRUE(cim)) {
        cim_obj = record_base_plot({
          mixOmics::cim(
            final_model,
            comp = 1:comp_num,
            row.sideColors = colors[as.numeric(Y)],
            row.names = FALSE,
            title = paste("CIM (Comp 1 -", comp_num, "):", analysis_label)
          )
        })
      }
      studies <- levels(study)
      if (is.null(studies) || length(studies) == 0) {
        studies <- unique(as.character(study))
      }
      partial_loadings_plots <- list()
      for (i in seq_len(comp_num)) {
        for (s_idx in seq_along(studies)) {
          lbl <- paste0(
            "Comp ",
            i,
            " - ",
            deparse(substitute(batch_col)),
            ": ",
            studies[s_idx]
          )
          partial_loadings_plots[[lbl]] <- record_base_plot({
            tryCatch(
              {
                op <- graphics::par(no.readonly = TRUE)
                on.exit(graphics::par(op), add = TRUE)
                mixOmics::plotLoadings(
                  final_model,
                  comp = i,
                  legend.color = colors,
                  study = s_idx,
                  contrib = "max",
                  method = "mean",
                  title = paste(
                    "Partial Loadings for Component",
                    i,
                    "\nStudy:",
                    studies[s_idx]
                  )
                )
              },
              error = function(e) {
                plot.new()
                title(
                  main = paste(
                    "Loadings failed for Comp",
                    i,
                    "Study",
                    studies[s_idx]
                  )
                )
                mtext(e$message, side = 1, line = -1, cex = 0.8)
              }
            )
          })
        }
      }
      roc_plot <- NULL
      if (roc) {
        roc_plot <- record_base_plot({
          # The plotting command is now correctly wrapped by the recorder
          tryCatch(
            {
              mixOmics::auroc(
                object = final_model,
                newdata = X,
                outcome.test = Y,
                study.test = study,
                roc.study = "global",
                plot = TRUE,
                print = FALSE
              )
            },
            error = function(e) {
              # On error, draw a message instead of crashing
              warning("Could not generate ROC curve. Error: ", e$message)
              plot.new()
              title(main = "ROC Plot Failed to Generate", col.main = "red")
            }
          )
        })
      }
      results_list <- list(
        global_indiv_plot = record_base_plot({
          mixOmics::plotIndiv(
            final_model,
            study = "global",
            group = Y,
            col = colors,
            legend = TRUE,
            legend.title = group_col,
            subtitle = main_title,
            ellipse = ellipse,
            background = bg_obj
          )
        }),
        partial_indiv_plot = record_base_plot({
          mixOmics::plotIndiv(
            final_model,
            study = "all.partial",
            group = Y,
            col = colors,
            legend = TRUE,
            title = paste("Partial Plots:", analysis_label)
          )
        }),
        correlation_circle_plot = record_base_plot({
          mixOmics::plotVar(
            final_model,
            var.names = FALSE,
            legend = TRUE,
            title = paste("Correlation Circle:", analysis_label)
          )
        }),
        cim_obj = cim_obj, # Assign the CIM object here
        partial_loadings_plots = partial_loadings_plots, # Assign the list of plots here
        roc_plot = roc_plot # Assign the ROC plot here
      )
      return(results_list)
    }
  }

  # --- Main Execution ---
  if (!is.null(progress)) {
    progress$set(message = "Starting MINT sPLS-DA...", value = 0)
  }
  if (!is.null(scale) && scale == "log2") {
    id_cols <- unique(c(group_col, group_col2, batch_col))
    id_cols <- id_cols[!sapply(id_cols, is.null)]
    numeric_cols <- data[, !(names(data) %in% id_cols), drop = FALSE]
    data <- data.frame(data[, id_cols, drop = FALSE], log2(numeric_cols))
  }
  num_groups <- nlevels(as.factor(data[[group_col]]))
  if (is.null(colors) || length(colors) < num_groups) {
    colors <- grDevices::rainbow(num_groups)
  }
  is_pdf <- !is.null(output_file)
  if (is_pdf) {
    grDevices::pdf(file = output_file, width = 11, height = 8.5)
    on.exit(grDevices::dev.off())
  }
  if (is.null(group_col2) || group_col == group_col2) {
    results <- run_mint_analysis(
      data,
      analysis_label = "",
      is_pdf_mode = is_pdf
    )
  } else {
    treatments <- levels(as.factor(data[[group_col2]]))
    if (is_pdf) {
      for (trt in treatments) {
        run_mint_analysis(
          data[data[[group_col2]] == trt, , drop = FALSE],
          trt,
          TRUE
        )
      }
      results <- paste("Output file generated:", normalizePath(output_file))
    } else {
      results <- lapply(treatments, function(trt) {
        run_mint_analysis(
          data[data[[group_col2]] == trt, , drop = FALSE],
          trt,
          FALSE
        )
      })
      names(results) <- treatments
    }
  }
  if (!is.null(progress)) {
    progress$set(value = 1, detail = "Analysis complete.")
  }
  return(results)
}
