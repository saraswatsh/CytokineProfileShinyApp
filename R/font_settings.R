# Shared font helpers for plotting functions and Shiny controls.

font_settings_known_fields <- function() {
  c(
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
    "row_names",
    "col_names",
    "cell_text",
    "variable_names",
    "point_labels"
  )
}

font_settings_defaults <- function(base_size = 11) {
  list(
    base_size = base_size,
    plot_title = base_size + 1,
    x_title = base_size,
    y_title = base_size,
    x_text = base_size - 1,
    y_text = base_size - 1,
    legend_title = base_size,
    legend_text = base_size - 1,
    strip_text = base_size,
    annotation_text = base_size - 1,
    row_names = base_size - 1,
    col_names = base_size - 1,
    cell_text = base_size - 1,
    variable_names = base_size - 1,
    point_labels = base_size - 1
  )
}

font_settings_has_values <- function(x) {
  is.list(x) && length(x) > 0L && any(!vapply(x, is.null, logical(1)))
}

validate_font_setting_scalar <- function(value, field) {
  if (is.null(value)) {
    return(NULL)
  }

  if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
    stop(
      sprintf("font_settings$%s must be a single positive numeric value.", field),
      call. = FALSE
    )
  }

  as.numeric(value)
}

normalize_font_settings <- function(
  font_settings = NULL,
  supported_fields = NULL,
  legacy = NULL,
  activate = !is.null(font_settings)
) {
  known_fields <- font_settings_known_fields()

  if (!is.null(font_settings)) {
    if (!is.list(font_settings) || is.null(names(font_settings))) {
      stop("font_settings must be a named list.", call. = FALSE)
    }

    unknown_fields <- setdiff(names(font_settings), known_fields)
    if (length(unknown_fields) > 0L) {
      stop(
        sprintf(
          "Unknown font_settings field(s): %s.",
          paste(unknown_fields, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (is.null(legacy)) {
    legacy <- list()
  }
  if (!is.list(legacy)) {
    stop("legacy font settings must be supplied as a named list.", call. = FALSE)
  }

  if (!isTRUE(activate)) {
    return(NULL)
  }

  explicit_fields <- if (is.null(font_settings)) character() else names(font_settings)
  base_candidate <- NULL
  if (!is.null(font_settings) && "base_size" %in% explicit_fields) {
    base_candidate <- font_settings$base_size
  } else if ("base_size" %in% names(legacy)) {
    base_candidate <- legacy$base_size
  }

  base_size <- validate_font_setting_scalar(base_candidate %||% 11, "base_size")
  resolved <- font_settings_defaults(base_size)

  for (field in names(legacy)) {
    if (!(field %in% known_fields)) {
      next
    }
    if (field %in% explicit_fields) {
      next
    }
    value <- validate_font_setting_scalar(legacy[[field]], field)
    if (!is.null(value)) {
      resolved[[field]] <- value
    }
  }

  if (!is.null(font_settings)) {
    for (field in explicit_fields) {
      value <- validate_font_setting_scalar(font_settings[[field]], field)
      if (!is.null(value)) {
        resolved[[field]] <- value
      }
    }
  }

  if (!is.null(supported_fields)) {
    supported_fields <- intersect(
      unique(c("base_size", supported_fields)),
      known_fields
    )
    resolved <- resolved[supported_fields]
  }

  resolved
}

apply_font_settings_ggplot <- function(plot, font_settings = NULL) {
  if (is.null(font_settings)) {
    return(plot)
  }

  theme_args <- list(
    text = ggplot2::element_text(size = font_settings$base_size)
  )

  if ("plot_title" %in% names(font_settings)) {
    theme_args$plot.title <- ggplot2::element_text(size = font_settings$plot_title)
  }
  if ("x_title" %in% names(font_settings)) {
    theme_args$axis.title.x <- ggplot2::element_text(size = font_settings$x_title)
  }
  if ("y_title" %in% names(font_settings)) {
    theme_args$axis.title.y <- ggplot2::element_text(size = font_settings$y_title)
  }
  if ("x_text" %in% names(font_settings)) {
    theme_args$axis.text.x <- ggplot2::element_text(size = font_settings$x_text)
  }
  if ("y_text" %in% names(font_settings)) {
    theme_args$axis.text.y <- ggplot2::element_text(size = font_settings$y_text)
  }
  if ("legend_title" %in% names(font_settings)) {
    theme_args$legend.title <- ggplot2::element_text(size = font_settings$legend_title)
  }
  if ("legend_text" %in% names(font_settings)) {
    theme_args$legend.text <- ggplot2::element_text(size = font_settings$legend_text)
  }
  if ("strip_text" %in% names(font_settings)) {
    theme_args$strip.text <- ggplot2::element_text(size = font_settings$strip_text)
  }

  plot + do.call(ggplot2::theme, theme_args)
}

font_settings_ggplot_text_size <- function(size_points, reference_points = 10, default_size = 4) {
  if (is.null(size_points) || is.na(size_points) || size_points <= 0) {
    return(default_size)
  }

  default_size * (as.numeric(size_points) / reference_points)
}

font_settings_mixomics_text_scale <- function(size_points, reference_points = 11, default_scale = 1) {
  if (is.null(size_points) || is.na(size_points) || size_points <= 0) {
    return(default_scale)
  }

  as.numeric(size_points) / reference_points
}

font_settings_mixomics_indiv_cex <- function(
  size_points,
  reference_points = 11,
  default_cex = 3
) {
  if (is.null(size_points) || is.na(size_points) || size_points <= 0) {
    return(default_cex)
  }

  default_cex * (as.numeric(size_points) / reference_points)
}

font_settings_mixomics_scale <- function(font_settings = NULL, default_scale = 1) {
  if (is.null(font_settings) || is.null(font_settings$base_size)) {
    return(default_scale)
  }

  as.numeric(font_settings$base_size) / 11
}

font_settings_mixomics_indiv_args <- function(font_settings = NULL) {
  if (is.null(font_settings)) {
    return(list())
  }

  list(
    size.title = font_settings$plot_title,
    size.subtitle = font_settings$strip_text,
    size.xlabel = font_settings$x_title,
    size.ylabel = font_settings$y_title,
    size.axis = mean(c(font_settings$x_text, font_settings$y_text)),
    size.legend = font_settings$legend_text,
    size.legend.title = font_settings$legend_title,
    cex = font_settings_mixomics_indiv_cex(font_settings$point_labels)
  )
}

font_settings_mixomics_loadings_args <- function(font_settings = NULL) {
  if (is.null(font_settings)) {
    return(list())
  }

  list(
    size.name = font_settings$variable_names / 10,
    size.legend = font_settings_mixomics_text_scale(font_settings$legend_text),
    size.title = font_settings_mixomics_text_scale(font_settings$plot_title),
    size.axis = font_settings_mixomics_text_scale(
      mean(c(font_settings$x_text, font_settings$y_text))
    ),
    size.labs = font_settings_mixomics_text_scale(
      mean(c(font_settings$x_title, font_settings$y_title))
    )
  )
}

font_settings_plotvar_cex <- function(
  size_points,
  reference_points = 10,
  default_cex = 4
) {
  if (is.null(size_points) || is.na(size_points) || size_points <= 0) {
    return(default_cex)
  }

  default_cex * (as.numeric(size_points) / reference_points)
}

font_settings_plotvar_args <- function(
  font_settings = NULL,
  show_var_names = TRUE
) {
  if (is.null(font_settings) || !isTRUE(show_var_names)) {
    return(list())
  }

  list(
    cex = font_settings_plotvar_cex(font_settings$variable_names)
  )
}

font_settings_base_graphics <- function(font_settings = NULL) {
  if (is.null(font_settings)) {
    return(list())
  }

  list(
    cex = font_settings$base_size / 11,
    cex.main = font_settings$plot_title / 12,
    cex.lab = mean(c(font_settings$x_title, font_settings$y_title)) / 11,
    cex.axis = mean(c(font_settings$x_text, font_settings$y_text)) / 10,
    legend_cex = font_settings$legend_text / 10,
    legend_title_cex = font_settings$legend_title / 11,
    point_cex = font_settings$point_labels / 10,
    variable_cex = font_settings$variable_names / 10
  )
}

font_settings_heatmap_args <- function(font_settings = NULL) {
  if (is.null(font_settings)) {
    return(list())
  }

  list(
    fontsize = font_settings$base_size,
    fontsize_row = font_settings$row_names %||% (font_settings$base_size - 1),
    fontsize_col = font_settings$col_names %||% (font_settings$base_size - 1),
    fontsize_number = font_settings$cell_text %||% (font_settings$base_size - 1)
  )
}
