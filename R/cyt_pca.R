#' Analyze Data with Principal Component Analysis (PCA) for Cytokines.
#'
#' This function performs PCA on cytokine data and generates several types of plots:
#' 2D individuals plot, optionally a 3D scatter plot (if style is "3d" and comp_num is 3),
#' a scree plot, loadings plots for each component, a biplot (using the default stats::biplot),
#' and a correlation circle plot.
#'
#' In PDF mode (if output_file or output_file is provided) the plots are printed to a PDF.
#' In interactive mode (if both output_file and output_file are NULL) the plots are captured using recordPlot()
#' and returned as a list for display in Shiny.
#'
#' @param data A data frame containing cytokine data.
#' @param group_col Character. The name of the column containing the grouping information.
#' @param group_col2 Character. The name of the second column containing the grouping information.
#'   If one is missing, the provided column is used for both.
#' @param pca_colors A vector of pca_colors corresponding to the groups. If NULL, a palette is generated.
#' @param output_file Optional. A file name for the PDF output. If NULL, interactive mode is assumed.
#' @param ellipse Logical. If TRUE, a 95% confidence ellipse is drawn on the individuals plot.
#' @param comp_num Numeric. Number of principal components to compute and display. Default is 2.
#' @param scale Character. If "log2", a log2 transformation is applied (excluding factor columns).
#' @param pch_values A vector of plotting symbols.
#' @param style Character. If "3d" (case insensitive) and comp_num equals 3, a 3D scatter plot is generated.
#' @param progress Optional. A progress object (e.g., from shiny::Progress) to report progress.
#'
#' @return In PDF mode, a PDF is created and the function returns NULL (invisibly).
#'         In interactive mode, a (possibly nested) list of recorded plots is returned.
#'
#' @export
#' @importFrom mixOmics pca plotIndiv plotLoadings plotVar
#' @import ggplot2
#' @importFrom plot3D scatter3D
#'
#' @examples
#' data <- ExampleData1[, -c(3,23)]
#' data_df <- dplyr::filter(data, Group != "ND" & Treatment != "Unstimulated")
#' # Run PCA analysis and save plots to a PDF file
#' cyt_pca(
#'   data = data_df,
#'   output_file = NULL,
#'   pca_colors = c("black", "red2"),
#'   scale = "log2",
#'   comp_num = 3,
#'   pch_values = c(16, 4),
#'   style = "3D",
#'   group_col = "Group",
#'   group_col2 = "Treatment",
#'   ellipse = FALSE
#' )
cyt_pca <- function(
  data,
  group_col = NULL,
  group_col2 = NULL,
  pca_colors = NULL,
  ellipse = FALSE,
  comp_num = 2,
  scale = NULL,
  pch_values = NULL,
  style = NULL,
  output_file = NULL,
  progress = NULL
) {
  # Initialize progress if provided.
  if (!is.null(progress)) {
    progress$set(message = "Starting PCA analysis...", value = 0)
  }

  # If one factor is missing, use the provided column for both grouping and treatment.
  if (is.null(group_col) && !is.null(group_col2)) {
    message(
      "Grouping column 2 not provided, using grouping column 1 as grouping variable."
    )
    group_col <- group_col2
  }
  if (is.null(group_col2) && !is.null(group_col)) {
    message(
      "Grouping column 1 not provided, using grouping column 2 as grouping variable."
    )
    group_col2 <- group_col
  }
  if (is.null(group_col) && is.null(group_col2)) {
    stop("At least one factor column must be provided.")
  }

  # Optionally apply log2 transformation only to numeric columns (excluding factor columns)
  if (!is.null(scale) && scale == "log2") {
    numeric_idx <- sapply(data, is.numeric)
    numeric_idx[names(data) %in% unique(c(group_col, group_col2))] <- FALSE
    if (sum(numeric_idx) == 0) {
      warning("No numeric columns available for log2 transformation.")
    }
    data <- data.frame(
      data[, unique(c(group_col, group_col2)), drop = FALSE],
      log2(data[, numeric_idx, drop = FALSE])
    )
    message("Results based on log2 transformation.")
  } else {
    message("Results based on no transformation.")
  }

  # Update progress after transformation.
  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Data transformation complete")
  }

  num_groups <- length(unique(data[[group_col]]))
  if (is.null(pca_colors) || length(pca_colors) == 0) {
    pca_colors <- grDevices::rainbow(num_groups)
  } else if (length(pca_colors) < num_groups) {
    pca_colors <- rep(pca_colors, length.out = num_groups)
  }

  # Helper to record a base-graphics plot.
  record_base_plot <- function(expr) {
    if (grDevices::dev.cur() > 1) {
      grDevices::dev.control(displaylist = "enable")
    }
    expr
    grDevices::recordPlot()
  }
  pdf_mode <- !is.null(output_file)
  if (pdf_mode) {
    # real PDF export
    grDevices::pdf(file = output_file, width = 8.5, height = 8)
  } else {
    # interactive mode: open one throw-away PNG device *up front*
    tmp_png <- tempfile(fileext = ".png")
    grDevices::png(tmp_png, width = 800, height = 600, res = 96)
    result_list <- list()
    # when the function exits, close & delete that temp PNG
    on.exit(
      {
        grDevices::dev.off()
        if (file.exists(tmp_png)) unlink(tmp_png)
      },
      add = TRUE
    )
  }

  # CASE 1: Single-level analysis (when group_col equals group_col2)
  if (group_col == group_col2) {
    overall_analysis <- "Overall Analysis"

    # Subset numeric data.
    the_data_df <- data[, !(names(data) %in% unique(c(group_col, group_col2)))]
    the_data_df <- the_data_df[, sapply(the_data_df, is.numeric), drop = FALSE]

    the_groups <- as.vector(data[[group_col]])
    if (length(unique(the_groups)) < 2) {
      stop("The grouping variable must have at least two levels for PCA.")
    }

    # Compute PCA using mixOmics (for other plots)...
    pca_result <- mixOmics::pca(
      the_data_df,
      ncomp = comp_num,
      center = TRUE,
      scale = TRUE
    )
    if (!is.null(progress)) {
      progress$inc(0.2, detail = "PCA computation complete")
    }
    group_factors <- seq_len(length(levels(factor(the_groups))))

    # Individuals plot.
    if (pdf_mode) {
      mixOmics::plotIndiv(
        pca_result,
        group = the_groups,
        ind.names = FALSE,
        legend = TRUE,
        col = pca_colors,
        title = paste("PCA:", overall_analysis),
        ellipse = ellipse,
        pch = pch_values
      )
    } else {
      result_list$overall_indiv_plot <- record_base_plot(
        mixOmics::plotIndiv(
          pca_result,
          group = the_groups,
          ind.names = FALSE,
          legend = TRUE,
          col = pca_colors,
          title = paste("PCA:", overall_analysis),
          ellipse = ellipse,
          pch = pch_values
        )
      )
    }
    if (!is.null(progress)) {
      progress$inc(0.2, detail = "Individuals plot generated")
    }

    # 3D plot (only if style is "3d" and comp_num == 3).
    if (!is.null(style) && comp_num == 3 && (tolower(style) == "3d")) {
      cytokine_scores <- pca_result$variates$X
      if (pdf_mode) {
        plot3D::scatter3D(
          cytokine_scores[, 1],
          cytokine_scores[, 2],
          cytokine_scores[, 3],
          pch = pch_values,
          ,
          col = pca_colors,
          xlab = "PC1",
          ylab = "PC2",
          zlab = "PC3",
          main = paste("3D Plot:", overall_analysis),
          theta = 20,
          phi = 30,
          bty = "g",
          colkey = FALSE
        )
      } else {
        result_list$overall_3D <- record_base_plot(
          plot3D::scatter3D(
            cytokine_scores[, 1],
            cytokine_scores[, 2],
            cytokine_scores[, 3],
            pch = pch_values,
            col = pca_colors,
            xlab = "PC1",
            ylab = "PC2",
            zlab = "PC3",
            main = paste("3D Plot:", overall_analysis),
            theta = 20,
            phi = 30,
            bty = "g",
            colkey = FALSE
          )
        )
      }
      if (!is.null(progress)) {
        progress$inc(0.1, detail = "3D plot generated")
      }
    }

    # Scree Plot (using ggplot2).
    variances <- pca_result$prop_expl_var$X
    cumulative_variances <- pca_result$cum.var
    scree_data <- data.frame(
      Component = 1:comp_num,
      Variance = variances,
      Cumulative = cumulative_variances
    )
    scree_plot <- ggplot2::ggplot(scree_data, ggplot2::aes(x = Component)) +
      ggplot2::geom_line(
        ggplot2::aes(y = Variance, color = "Individual"),
        linewidth = 1
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = Variance, color = "Individual"),
        size = 2
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = Cumulative, color = "Cumulative"),
        linewidth = 1,
        linetype = "dashed"
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = Cumulative, color = "Cumulative"),
        size = 2
      ) +
      ggplot2::scale_color_manual(
        values = c("Individual" = "blue", "Cumulative" = "green")
      ) +
      ggplot2::labs(
        title = paste("Scree Plot:", overall_analysis),
        x = "Principal Components",
        y = "Explained Variance",
        color = "Variance Type"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_continuous(breaks = 1:comp_num) +
      ggplot2::geom_text(
        ggplot2::aes(
          y = Variance,
          label = paste0(round(Variance * 100, 1), "%")
        ),
        vjust = -1.5,
        hjust = 0.5,
        size = 4
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          y = Cumulative,
          label = paste0(round(Cumulative * 100, 1), "%")
        ),
        vjust = 1.5,
        hjust = 0.5,
        size = 4
      )

    if (pdf_mode) {
      print(scree_plot)
    } else {
      grDevices::dev.control(displaylist = "enable")
      print(scree_plot)
      result_list$overall_scree_plot <- grDevices::recordPlot()
    }
    if (!is.null(progress)) {
      progress$inc(0.1, detail = "Scree plot generated")
    }

    # Loadings plots for each component.
    loadings_plots <- list()
    for (comp in 1:comp_num) {
      if (pdf_mode) {
        mixOmics::plotLoadings(
          pca_result,
          comp = comp,
          size.names = 1,
          size.legend = 1,
          legend.color = pca_colors,
          title = paste("Loadings for Component", comp, ":", overall_analysis),
          legend = TRUE
        )
      } else {
        loadings_plots[[comp]] <- record_base_plot(
          mixOmics::plotLoadings(
            pca_result,
            comp = comp,
            size.names = 1,
            size.legend = 1,
            legend.color = pca_colors,
            title = paste(
              "Loadings for Component",
              comp,
              ":",
              overall_analysis
            ),
            legend = TRUE
          )
        )
      }
    }
    if (!pdf_mode) {
      result_list$loadings <- loadings_plots
    }
    if (!is.null(progress)) {
      progress$inc(0.1, detail = "Loadings plots generated")
    }

    # Biplot using the default stats package.
    # Create a prcomp object from the numeric data.
    prcomp_obj <- stats::prcomp(the_data_df, center = TRUE, scale. = TRUE)
    if (pdf_mode) {
      stats::biplot(prcomp_obj, main = paste("Biplot:", overall_analysis))
    } else {
      result_list$biplot <- record_base_plot({
        graphics::plot.new()
        stats::biplot(prcomp_obj, main = paste("Biplot:", overall_analysis))
      })
    }

    # Correlation circle plot.
    if (pdf_mode) {
      mixOmics::plotVar(
        pca_result,
        comp = c(1, 2),
        var.names = TRUE,
        cex = 4,
        col = "black",
        overlap = TRUE,
        title = paste("Correlation Circle Plot:", overall_analysis),
        style = "ggplot2"
      )
    } else {
      result_list$correlation_circle <- record_base_plot(
        mixOmics::plotVar(
          pca_result,
          comp = c(1, 2),
          var.names = TRUE,
          cex = 4,
          col = "black",
          overlap = TRUE,
          title = paste("Correlation Circle Plot:", overall_analysis),
          style = "ggplot2"
        )
      )
    }
    if (!is.null(progress)) {
      progress$inc(0.1, detail = "Biplot & correlation circle generated")
    }
  } else {
    # CASE 2: Multi-level analysis when group_col != group_col2.
    result_list <- list()
    levels_vec <- unique(data[[group_col2]])

    for (lev in levels_vec) {
      current_level <- lev
      title_sub <- current_level
      condt <- data[[group_col2]] == current_level
      the_data_df <- data[
        condt,
        !(names(data) %in% unique(c(group_col, group_col2)))
      ]
      the_data_df <- the_data_df[,
        sapply(the_data_df, is.numeric),
        drop = FALSE
      ]
      the_groups <- as.vector(data[condt, group_col])

      if (length(unique(the_groups)) < 2) {
        stop("The grouping variable must have at least two levels for PCA.")
      }
      pca_result <- mixOmics::pca(
        the_data_df,
        ncomp = comp_num,
        center = TRUE,
        scale = TRUE
      )
      group_factors <- seq_len(length(levels(factor(the_groups))))

      # Create a sub-list for this treatment level.
      sublist <- list()

      # Individuals plot.
      if (pdf_mode) {
        mixOmics::plotIndiv(
          pca_result,
          group = the_groups,
          ind.names = FALSE,
          legend = TRUE,
          col = pca_colors,
          title = paste("PCA:", title_sub),
          ellipse = ellipse,
          pch = pch_values
        )
      } else {
        sublist$overall_indiv_plot <- record_base_plot(
          mixOmics::plotIndiv(
            pca_result,
            group = the_groups,
            ind.names = FALSE,
            legend = TRUE,
            col = pca_colors,
            title = paste("PCA:", title_sub),
            ellipse = ellipse,
            pch = pch_values
          )
        )
      }

      # 3D Plot.
      sublist$overall_3D <- NULL
      if (!is.null(style) && comp_num == 3 && (tolower(style) == "3d")) {
        cytokine_scores <- pca_result$variates$X
        if (pdf_mode) {
          plot3D::scatter3D(
            cytokine_scores[, 1],
            cytokine_scores[, 2],
            cytokine_scores[, 3],
            pch = pch_values,
            col = pca_colors,
            xlab = "PC1",
            ylab = "PC2",
            zlab = "PC3",
            main = paste("3D Plot:", title_sub),
            theta = 20,
            phi = 30,
            bty = "g",
            colkey = FALSE
          )
        } else {
          sublist$overall_3D <- record_base_plot(
            plot3D::scatter3D(
              cytokine_scores[, 1],
              cytokine_scores[, 2],
              cytokine_scores[, 3],
              pch = pch_values,
              col = pca_colors,
              xlab = "PC1",
              ylab = "PC2",
              zlab = "PC3",
              main = paste("3D Plot:", title_sub),
              theta = 20,
              phi = 30,
              bty = "g",
              colkey = FALSE
            )
          )
        }
      }

      # Scree Plot.
      variances <- pca_result$prop_expl_var$X
      cumulative_variances <- pca_result$cum.var
      scree_data <- data.frame(
        Component = 1:comp_num,
        Variance = variances,
        Cumulative = cumulative_variances
      )
      scree_plot <- ggplot2::ggplot(scree_data, ggplot2::aes(x = Component)) +
        ggplot2::geom_line(
          ggplot2::aes(y = Variance, color = "Individual"),
          linewidth = 1
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = Variance, color = "Individual"),
          size = 2
        ) +
        ggplot2::geom_line(
          ggplot2::aes(y = Cumulative, color = "Cumulative"),
          linewidth = 1,
          linetype = "dashed"
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = Cumulative, color = "Cumulative"),
          size = 2
        ) +
        ggplot2::scale_color_manual(
          values = c("Individual" = "blue", "Cumulative" = "green")
        ) +
        ggplot2::labs(
          title = paste("Scree Plot:", title_sub),
          x = "Principal Components",
          y = "Explained Variance",
          color = "Variance Type"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::scale_x_continuous(breaks = 1:comp_num) +
        ggplot2::geom_text(
          ggplot2::aes(
            y = Variance,
            label = paste0(round(Variance * 100, 1), "%")
          ),
          vjust = -1.5,
          hjust = 0.5,
          size = 4
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            y = Cumulative,
            label = paste0(round(Cumulative * 100, 1), "%")
          ),
          vjust = 1.5,
          hjust = 0.5,
          size = 4
        )

      if (pdf_mode) {
        print(scree_plot)
      } else {
        grDevices::dev.control(displaylist = "enable")
        print(scree_plot)
        sublist$overall_scree_plot <- grDevices::recordPlot()
      }

      # Loadings Plots.
      loadings_plots <- list()
      for (comp in 1:comp_num) {
        if (pdf_mode) {
          mixOmics::plotLoadings(
            pca_result,
            comp = comp,
            size.names = 1,
            size.legend = 1,
            legend.color = pca_colors,
            title = paste("Loadings for Component", comp, ":", title_sub),
            legend = TRUE
          )
        } else {
          loadings_plots[[comp]] <- record_base_plot(
            mixOmics::plotLoadings(
              pca_result,
              comp = comp,
              size.names = 1,
              size.legend = 1,
              legend.color = pca_colors,
              title = paste("Loadings for Component", comp, ":", title_sub),
              legend = TRUE
            )
          )
        }
      }
      sublist$loadings <- loadings_plots

      # Biplot using the default stats package.
      prcomp_obj <- stats::prcomp(the_data_df, center = TRUE, scale. = TRUE)
      if (pdf_mode) {
        stats::biplot(prcomp_obj, main = paste("Biplot:", title_sub))
      } else {
        sublist$biplot <- record_base_plot({
          graphics::plot.new()
          stats::biplot(prcomp_obj, main = paste("Biplot:", title_sub))
        })
      }

      # Correlation circle.
      if (pdf_mode) {
        mixOmics::plotVar(
          pca_result,
          comp = c(1, 2),
          var.names = TRUE,
          cex = 4,
          col = "black",
          overlap = TRUE,
          title = paste("Correlation Circle Plot:", title_sub),
          style = "ggplot2"
        )
      } else {
        sublist$correlation_circle <- record_base_plot(
          mixOmics::plotVar(
            pca_result,
            comp = c(1, 2),
            var.names = TRUE,
            cex = 4,
            col = "black",
            overlap = TRUE,
            title = paste("Correlation Circle Plot:", title_sub),
            style = "ggplot2"
          )
        )
      }

      result_list[[as.character(current_level)]] <- sublist
    }
  }

  if (pdf_mode) {
    if (grDevices::dev.cur() > 1) {
      grDevices::dev.off()
    }
    invisible(NULL)
  } else {
    return(result_list)
  }
}
