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
#' @param font_settings Optional named list of font sizes for supported plot
#'   text elements.
#' @param progress Optional. A Shiny \code{Progress} object for reporting progress updates.
#' @return In Download mode, a PDF file is written. In Interactive mode, a named list
#'         (`results_list`) of plots and results is returned. If `group_col2` is used,
#'         a nested list is returned, with each element corresponding to a level of `group_col2`.
#' @author Shubh Saraswat
#' @references Rohart F, Eslami A, Matigian, N, Bougeard S, Lê Cao K-A (2017).
#' MINT: A multivariate integrative approach to identify a reproducible
#' biomarker signature across multiple experiments and platforms. BMC
#' Bioinformatics 18:128.
#'
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
  font_settings = NULL,
  progress = NULL
) {
  resolved_fonts <- normalize_font_settings(
    font_settings = font_settings,
    supported_fields = c(
      "base_size", "plot_title", "x_title", "y_title", "x_text", "y_text",
      "legend_title", "legend_text", "strip_text", "variable_names", "point_labels"
    ),
    activate = !is.null(font_settings)
  )
  mixomics_indiv_args <- font_settings_mixomics_indiv_args(resolved_fonts)
  mixomics_loadings_args <- font_settings_mixomics_loadings_args(resolved_fonts)
  plotvar_args <- font_settings_plotvar_args(
    resolved_fonts,
    show_var_names = FALSE
  )
  base_font_args <- font_settings_base_graphics(resolved_fonts)

  # --- Helper function to run the core analysis ---
  run_mint_analysis <- function(
    data_subset,
    analysis_label = "",
    is_pdf_mode = !is.null(output_file),
    progress_share = 0
  ) {
    display_label <- if (nzchar(analysis_label)) analysis_label else "Overall Analysis"
    stage_weights <- list(
      prepare = 0.15,
      fit = 0.25,
      predict = 0.15,
      plots = 0.20,
      corr = 0.10,
      extra = 0.10,
      results = 0.05
    )

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
    if (!is.null(progress)) {
      progress$inc(
        progress_share * stage_weights$prepare,
        detail = paste("Preparing data for", display_label)
      )
    }

    # --- 2. Run MINT sPLS-DA Model ---
    final_model <- mixOmics::mint.splsda(
      X = X,
      Y = Y,
      study = study,
      ncomp = comp_num,
      keepX = rep(var_num, comp_num)
    )
    if (!is.null(progress)) {
      progress$inc(
        progress_share * stage_weights$fit,
        detail = paste("Fitting MINT sPLS-DA model for", display_label)
      )
    }

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
    if (!is.null(progress)) {
      progress$inc(
        progress_share * stage_weights$predict,
        detail = paste("Calculating predictions for", display_label)
      )
    }

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
    if (!is.null(progress)) {
      progress$inc(
        progress_share * stage_weights$plots,
        detail = paste("Building score plots for", display_label)
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
    draw_corr_circle_plot <- function(model, plot_title) {
      plot_obj <- do.call(
        mixOmics::plotVar,
        c(
          list(
            model,
            var.names = FALSE,
            legend = TRUE,
            title = plot_title,
            style = "ggplot2"
          ),
          plotvar_args
        )
      )

      if (inherits(plot_obj, "ggplot")) {
        plot_obj <- apply_font_settings_ggplot(plot_obj, resolved_fonts)
        print(plot_obj)
      } else {
        plot_obj
      }
    }

    # --- 5. Handle Output: PDF vs. Interactive ---
    if (is_pdf_mode) {
      do.call(
        mixOmics::plotIndiv,
        c(
          list(
            final_model,
            study = "global",
            group = Y,
            col = colors,
            legend = TRUE,
            legend.title = group_col,
            subtitle = main_title,
            ellipse = ellipse,
            background = bg_obj
          ),
          mixomics_indiv_args
        )
      )
      do.call(
        mixOmics::plotIndiv,
        c(
          list(
            final_model,
            study = "all.partial",
            group = Y,
            col = colors,
            legend = TRUE,
            title = paste("Partial Plots:", analysis_label)
          ),
          mixomics_indiv_args
        )
      )
      draw_corr_circle_plot(
        final_model,
        paste("Correlation Circle:", analysis_label)
      )
      if (!is.null(progress)) {
        progress$inc(
          progress_share * stage_weights$corr,
          detail = paste("Building correlation circle for", display_label)
        )
      }
      if (!is.null(progress)) {
        progress$inc(
          progress_share * stage_weights$extra,
          detail = paste("Building additional plots for", display_label)
        )
      }
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
        do.call(
          mixOmics::plotLoadings,
          c(
            list(
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
            ),
            mixomics_loadings_args
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
      if (!is.null(progress)) {
        progress$inc(
          progress_share * stage_weights$results,
          detail = paste("Formatting results for", display_label)
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
                do.call(
                  mixOmics::plotLoadings,
                  c(
                    list(
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
                    ),
                    mixomics_loadings_args
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
                mtext(
                  e$message,
                  side = 1,
                  line = -1,
                  cex = base_font_args$legend_cex %||% 0.8
                )
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
          do.call(
            mixOmics::plotIndiv,
            c(
              list(
                final_model,
                study = "global",
                group = Y,
                col = colors,
                legend = TRUE,
                legend.title = group_col,
                subtitle = main_title,
                ellipse = ellipse,
                background = bg_obj
              ),
              mixomics_indiv_args
            )
          )
        }),
        partial_indiv_plot = record_base_plot({
          do.call(
            mixOmics::plotIndiv,
            c(
              list(
                final_model,
                study = "all.partial",
                group = Y,
                col = colors,
                legend = TRUE,
                title = paste("Partial Plots:", analysis_label)
              ),
              mixomics_indiv_args
            )
          )
        }),
        correlation_circle_plot = record_base_plot({
          draw_corr_circle_plot(
            final_model,
            paste("Correlation Circle:", analysis_label)
          )
        }),
        cim_obj = cim_obj, # Assign the CIM object here
        partial_loadings_plots = partial_loadings_plots, # Assign the list of plots here
        roc_plot = roc_plot # Assign the ROC plot here
      )
      if (!is.null(progress)) {
        progress$inc(
          progress_share * stage_weights$corr,
          detail = paste("Building correlation circle for", display_label)
        )
        progress$inc(
          progress_share * stage_weights$extra,
          detail = paste("Building additional plots for", display_label)
        )
        progress$inc(
          progress_share * stage_weights$results,
          detail = paste("Formatting results for", display_label)
        )
      }
      return(results_list)
    }
  }

  # --- Main Execution ---
  if (!is.null(progress)) {
    progress$set(message = "Running MINT sPLS-DA...", value = 0)
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
  if (!is.null(progress)) {
    progress$inc(0.10, detail = "Preparing analysis inputs")
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
      is_pdf_mode = is_pdf,
      progress_share = 0.80
    )
  } else {
    treatments <- levels(as.factor(data[[group_col2]]))
    progress_share <- 0.80 / max(length(treatments), 1L)
    if (is_pdf) {
      for (trt in treatments) {
        run_mint_analysis(
          data[data[[group_col2]] == trt, , drop = FALSE],
          trt,
          TRUE,
          progress_share = progress_share
        )
      }
      results <- paste("Output file generated:", normalizePath(output_file))
    } else {
      results <- lapply(treatments, function(trt) {
        run_mint_analysis(
          data[data[[group_col2]] == trt, , drop = FALSE],
          trt,
          FALSE,
          progress_share = progress_share
        )
      })
      names(results) <- treatments
    }
  }
  if (!is.null(progress)) {
    progress$set(message = "Running MINT sPLS-DA...", value = 1, detail = "Finished")
  }
  return(results)
}
