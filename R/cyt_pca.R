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
#' @param scale Character. Optional transformation applied to numeric columns
#'   used in PCA. Supported values are \code{NULL} (default; no
#'   transformation), \code{"none"}, \code{"log2"}, \code{"log10"},
#'   \code{"zscore"}, or \code{"custom"}.
#' @param pch_values A vector of plotting symbols.
#' @param style Character. If "3d" (case insensitive) and comp_num equals 3, a 3D scatter plot is generated.
#' @param font_settings Optional named list of font sizes for supported plot
#'   text elements.
#' @param progress Optional. A Shiny \code{Progress} object for reporting progress updates.
#' @param custom_fn Optional transformation function used when
#'   \code{scale = "custom"}.
#'
#' @return In PDF mode, a PDF is created and the function returns NULL (invisibly).
#'         In interactive mode, a (possibly nested) list of recorded plots is returned.
#'
#' @export
#' @importFrom mixOmics pca plotIndiv plotLoadings plotVar
#' @import ggplot2
#' @importFrom plot3D scatter3D
#' @author Shubh Saraswat
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
  font_settings = NULL,
  progress = NULL,
  custom_fn = NULL
) {
  # Initialize progress if provided.
  if (!is.null(progress)) {
    progress$set(message = "Running PCA...", value = 0)
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
      "annotation_text",
      "variable_names",
      "point_labels"
    ),
    activate = !is.null(font_settings)
  )

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

  transform_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  transform_cols <- setdiff(transform_cols, unique(c(group_col, group_col2)))
  if (!is.null(scale)) {
    if (length(transform_cols) == 0L) {
      warning("No numeric columns available for the requested transformation.")
    } else {
      data <- apply_scale(
        data = data,
        columns = transform_cols,
        scale = scale,
        custom_fn = custom_fn
      )
    }
  }

  # Update progress after transformation.
  if (!is.null(progress)) {
    progress$inc(0.1, detail = "Preparing data")
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
  mixomics_indiv_args <- font_settings_mixomics_indiv_args(resolved_fonts)
  mixomics_loadings_args <- font_settings_mixomics_loadings_args(resolved_fonts)
  plotvar_args <- font_settings_plotvar_args(
    resolved_fonts,
    show_var_names = TRUE
  )
  base_font_args <- font_settings_base_graphics(resolved_fonts)
  scree_label_size <- if (is.null(resolved_fonts)) {
    4
  } else {
    font_settings_ggplot_text_size(
      resolved_fonts$annotation_text,
      default_size = 4
    )
  }

  draw_indiv_plot <- function(model, groups, plot_title) {
    plot_args <- c(
      list(
        model,
        group = groups,
        ind.names = FALSE,
        legend = TRUE,
        col = pca_colors,
        title = plot_title,
        ellipse = ellipse,
        pch = pch_values
      ),
      mixomics_indiv_args
    )
    do.call(mixOmics::plotIndiv, plot_args)
  }

  draw_3d_plot <- function(scores, plot_title) {
    plot_args <- list(
      scores[, 1],
      scores[, 2],
      scores[, 3],
      pch = pch_values,
      col = pca_colors,
      xlab = "PC1",
      ylab = "PC2",
      zlab = "PC3",
      main = plot_title,
      theta = 20,
      phi = 30,
      bty = "g",
      colkey = FALSE
    )

    if (!is.null(resolved_fonts)) {
      plot_args <- c(
        plot_args,
        list(
          cex = base_font_args$point_cex,
          cex.lab = base_font_args$cex.lab,
          cex.axis = base_font_args$cex.axis,
          cex.main = base_font_args$cex.main
        )
      )
    }

    do.call(plot3D::scatter3D, plot_args)
  }

  build_scree_plot <- function(variances, cumulative_variances, plot_title) {
    scree_data <- data.frame(
      Component = seq_along(variances),
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
        title = plot_title,
        x = "Principal Components",
        y = "Explained Variance",
        color = "Variance Type"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_continuous(breaks = seq_along(variances)) +
      ggplot2::geom_text(
        ggplot2::aes(
          y = Variance,
          label = paste0(round(Variance * 100, 1), "%")
        ),
        vjust = -1.5,
        hjust = 0.5,
        size = scree_label_size
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          y = Cumulative,
          label = paste0(round(Cumulative * 100, 1), "%")
        ),
        vjust = 1.5,
        hjust = 0.5,
        size = scree_label_size
      )

    apply_font_settings_ggplot(scree_plot, resolved_fonts)
  }

  draw_loadings_plot <- function(model, component, plot_title) {
    plot_args <- c(
      list(
        model,
        comp = component,
        legend.color = pca_colors,
        title = plot_title,
        legend = TRUE
      ),
      mixomics_loadings_args
    )
    do.call(mixOmics::plotLoadings, plot_args)
  }

  draw_biplot <- function(prcomp_obj, plot_title) {
    op <- NULL
    if (!is.null(resolved_fonts)) {
      op <- graphics::par(
        cex = base_font_args$cex,
        cex.main = base_font_args$cex.main,
        cex.lab = base_font_args$cex.lab,
        cex.axis = base_font_args$cex.axis
      )
      on.exit(graphics::par(op), add = TRUE)
    }

    stats::biplot(prcomp_obj, main = plot_title)
  }

  draw_corr_circle <- function(model, plot_title) {
    plot_obj <- do.call(
      mixOmics::plotVar,
      c(
        list(
          model,
          comp = c(1, 2),
          var.names = TRUE,
          col = "black",
          overlap = TRUE,
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
    analysis_label <- resolve_analysis_display_label(group_col)

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
      progress$inc(0.2, detail = "Fitting PCA model")
    }
    group_factors <- seq_len(length(levels(factor(the_groups))))

    # Individuals plot.
    if (pdf_mode) {
      draw_indiv_plot(pca_result, the_groups, paste("PCA:", analysis_label))
    } else {
      result_list$overall_indiv_plot <- record_base_plot(
        draw_indiv_plot(pca_result, the_groups, paste("PCA:", analysis_label))
      )
    }
    if (!is.null(progress)) {
      progress$inc(0.2, detail = "Building individuals plot")
    }

    # 3D plot (only if style is "3d" and comp_num == 3).
    if (!is.null(style) && comp_num == 3 && (tolower(style) == "3d")) {
      cytokine_scores <- pca_result$variates$X
      if (pdf_mode) {
        draw_3d_plot(cytokine_scores, paste("3D Plot:", analysis_label))
      } else {
        result_list$overall_3D <- record_base_plot(
          draw_3d_plot(cytokine_scores, paste("3D Plot:", analysis_label))
        )
      }
      if (!is.null(progress)) {
        progress$inc(0.1, detail = "Building 3D plot")
      }
    }

    # Scree Plot (using ggplot2).
    variances <- pca_result$prop_expl_var$X
    cumulative_variances <- pca_result$cum.var
    scree_plot <- build_scree_plot(
      variances = variances,
      cumulative_variances = cumulative_variances,
      plot_title = paste("Scree Plot:", analysis_label)
    )

    if (pdf_mode) {
      print(scree_plot)
    } else {
      grDevices::dev.control(displaylist = "enable")
      print(scree_plot)
      result_list$overall_scree_plot <- grDevices::recordPlot()
    }
    if (!is.null(progress)) {
      progress$inc(0.1, detail = "Building scree plot")
    }

    # Loadings plots for each component.
    loadings_plots <- list()
    for (comp in 1:comp_num) {
      if (pdf_mode) {
        draw_loadings_plot(
          pca_result,
          component = comp,
          plot_title = paste(
            "Loadings for Component",
            comp,
            ":",
            analysis_label
          )
        )
      } else {
        loadings_plots[[comp]] <- record_base_plot(
          draw_loadings_plot(
            pca_result,
            component = comp,
            plot_title = paste(
              "Loadings for Component",
              comp,
              ":",
              analysis_label
            )
          )
        )
      }
    }
    if (!pdf_mode) {
      result_list$loadings <- loadings_plots
    }
    if (!is.null(progress)) {
      progress$inc(0.1, detail = "Building loadings plots")
    }

    # Biplot using the default stats package.
    # Create a prcomp object from the numeric data.
    prcomp_obj <- stats::prcomp(the_data_df, center = TRUE, scale. = TRUE)
    if (pdf_mode) {
      draw_biplot(prcomp_obj, paste("Biplot:", analysis_label))
    } else {
      result_list$biplot <- record_base_plot({
        graphics::plot.new()
        draw_biplot(prcomp_obj, paste("Biplot:", analysis_label))
      })
    }

    # Correlation circle plot.
    if (pdf_mode) {
      draw_corr_circle(
        pca_result,
        paste("Correlation Circle Plot:", analysis_label)
      )
    } else {
      result_list$correlation_circle <- record_base_plot(
        draw_corr_circle(
          pca_result,
          paste("Correlation Circle Plot:", analysis_label)
        )
      )
    }
    if (!is.null(progress)) {
      progress$inc(0.1, detail = "Building biplot and correlation circle")
    }
  } else {
    # CASE 2: Multi-level analysis when group_col != group_col2.
    result_list <- list()
    levels_vec <- unique(data[[group_col2]])
    level_inc <- if (length(levels_vec) > 0L) 0.80 / length(levels_vec) else 0

    for (lev_idx in seq_along(levels_vec)) {
      lev <- levels_vec[[lev_idx]]
      current_level <- lev
      title_sub <- resolve_analysis_display_label(group_col, current_level)
      if (!is.null(progress)) {
        progress$inc(
          level_inc,
          detail = paste(
            "Processing subset",
            lev_idx,
            "of",
            length(levels_vec),
            ":",
            title_sub
          )
        )
      }
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
        draw_indiv_plot(pca_result, the_groups, paste("PCA:", title_sub))
      } else {
        sublist$overall_indiv_plot <- record_base_plot(
          draw_indiv_plot(pca_result, the_groups, paste("PCA:", title_sub))
        )
      }

      # 3D Plot.
      sublist$overall_3D <- NULL
      if (!is.null(style) && comp_num == 3 && (tolower(style) == "3d")) {
        cytokine_scores <- pca_result$variates$X
        if (pdf_mode) {
          draw_3d_plot(cytokine_scores, paste("3D Plot:", title_sub))
        } else {
          sublist$overall_3D <- record_base_plot(
            draw_3d_plot(cytokine_scores, paste("3D Plot:", title_sub))
          )
        }
      }

      # Scree Plot.
      variances <- pca_result$prop_expl_var$X
      cumulative_variances <- pca_result$cum.var
      scree_plot <- build_scree_plot(
        variances = variances,
        cumulative_variances = cumulative_variances,
        plot_title = paste("Scree Plot:", title_sub)
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
          draw_loadings_plot(
            pca_result,
            component = comp,
            plot_title = paste("Loadings for Component", comp, ":", title_sub)
          )
        } else {
          loadings_plots[[comp]] <- record_base_plot(
            draw_loadings_plot(
              pca_result,
              component = comp,
              plot_title = paste("Loadings for Component", comp, ":", title_sub)
            )
          )
        }
      }
      sublist$loadings <- loadings_plots

      # Biplot using the default stats package.
      prcomp_obj <- stats::prcomp(the_data_df, center = TRUE, scale. = TRUE)
      if (pdf_mode) {
        draw_biplot(prcomp_obj, paste("Biplot:", title_sub))
      } else {
        sublist$biplot <- record_base_plot({
          graphics::plot.new()
          draw_biplot(prcomp_obj, paste("Biplot:", title_sub))
        })
      }

      # Correlation circle.
      if (pdf_mode) {
        draw_corr_circle(
          pca_result,
          paste("Correlation Circle Plot:", title_sub)
        )
      } else {
        sublist$correlation_circle <- record_base_plot(
          draw_corr_circle(
            pca_result,
            paste("Correlation Circle Plot:", title_sub)
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
    if (!is.null(progress)) {
      progress$set(message = "Running PCA...", value = 1, detail = "Finished")
    }
    invisible(NULL)
  } else {
    if (!is.null(progress)) {
      progress$set(message = "Running PCA...", value = 1, detail = "Finished")
    }
    return(result_list)
  }
}
