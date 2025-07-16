#' Analyze data with MINT Sparse Partial Least Squares Discriminant Analysis (sPLS-DA).
#'
#' This function performs a MINT (Multivariate INTegrative) sPLS-DA to handle
#' batch effects by modeling a global biological signal across different studies or batches.
#' If a second grouping column (`group_col2`) is provided, the analysis is stratified
#' and performed for each level of that column.
#'
#' @param data A matrix or data frame containing the variables.
#' @param group_col A string specifying the primary outcome column name (e.g., "Treatment").
#' @param group_col2 A string specifying a second grouping column. If provided, the
#'   analysis will be run separately for each level of this factor. Default is `NULL`.
#' @param batch_col A string specifying the column that identifies the batch or study for each sample.
# ... (rest of params are the same)
#'
#' @return In Download mode, a PDF file is written. In Interactive mode, a named list
#'         (`results_list`) of plots and results is returned. If `group_col2` is used,
#'         a nested list is returned, with each element corresponding to a level of `group_col2`.
#'
#' @export
#' @importFrom mixOmics mint.splsda perf auroc plotIndiv plotLoadings plotVar cim background.predict
#' @import ggplot2
#' @importFrom grDevices rainbow pdf dev.off recordPlot replayPlot
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
  scale = NULL,
  pch_values = NULL,
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
    # (This section remains the same)
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
    Y <- data_subset[[group_col]]
    study <- data_subset[[batch_col]]
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
    # --- START ACCURACY FIX ---
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
    # --- END ACCURACY FIX ---

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
      if (grDevices::dev.cur() == 1) {
        grDevices::png(filename = tempfile())
      }
      grDevices::dev.control(displaylist = "enable")
      expr
      p <- grDevices::recordPlot()
      if (names(grDevices::dev.cur()) == "png") {
        grDevices::dev.off()
      }
      return(p)
    }

    # --- 5. Handle Output: PDF vs. Interactive ---
    if (is_pdf_mode) {
      mixOmics::plotIndiv(
        final_model,
        study = "global",
        group = Y,
        pch = study,
        pch.levels = levels(study),
        col.per.group = colors,
        legend = TRUE,
        legend.title = group_col,
        legend.title.pch = batch_col,
        subtitle = main_title,
        ellipse = ellipse,
        background = bg_obj
      )
      mixOmics::plotIndiv(
        final_model,
        study = "all.partial",
        group = Y,
        col.per.group = colors,
        legend = TRUE,
        title = paste("Partial Plots:", analysis_label)
      )
      mixOmics::plotVar(
        final_model,
        var.names = FALSE,
        legend = TRUE,
        title = paste("Correlation Circle:", analysis_label)
      )
      mixOmics::cim(
        final_model,
        comp = 1,
        row.sideColors = colors[as.numeric(Y)],
        row.names = FALSE,
        title = paste("CIM (Comp 1):", analysis_label)
      )
      for (i in 1:comp_num) {
        mixOmics::plotLoadings(
          final_model,
          comp = i,
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
      # --- START LOADINGS PLOT FIX (INTERACTIVE MODE) ---
      # Use lapply to create a LIST of plot objects, one for each component
      partial_loadings_plots <- lapply(1:comp_num, function(i) {
        record_base_plot({
          mixOmics::plotLoadings(
            final_model,
            comp = i, # Use loop variable 'i'
            study = "all.partial",
            contrib = "max",
            method = "mean",
            title = paste(
              "Partial Loadings for Component",
              i,
              "in",
              analysis_label
            ) # Use loop variable 'i'
          )
        })
      })
      # --- END LOADINGS PLOT FIX ---

      results_list <- list(
        global_indiv_plot = record_base_plot({
          mixOmics::plotIndiv(
            final_model,
            study = "global",
            group = Y,
            pch = study,
            pch.levels = levels(study),
            col.per.group = colors,
            legend = TRUE,
            legend.title = group_col,
            legend.title.pch = batch_col,
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
            col.per.group = colors,
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
        cim_plot = record_base_plot({
          mixOmics::cim(
            final_model,
            comp = 1,
            row.sideColors = colors[as.numeric(Y)],
            row.names = FALSE,
            title = paste("CIM (Comp 1):", analysis_label)
          )
        }),
        partial_loadings_plots = partial_loadings_plots, # Assign the list of plots here
        roc_plot = NULL
      )

      if (roc) {
        results_list$roc_plot <- record_base_plot({
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
      return(results_list)
    }
  }

  # --- Main Execution ---
  # (This section remains the same)
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
