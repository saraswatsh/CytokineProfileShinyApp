#' Generic export function for Cytokine plots
#'
#' The purpose of this helper is to save a collection of plots
#' generated throughout the CytoProfile package to a user specified
#' format.  It is designed to handle objects created with `ggplot2`
#' (or other grid based plots), recorded base plots (see
#' `grDevices::recordPlot()`), or custom drawing functions stored
#' as closures.  If a multi-page PDF is requested, all plots are
#' written into a single file; for raster formats (PNG, JPEG and
#' TIFF), each element of the plot list is saved to its own file
#' with a numbered suffix.  Nothing is returned except invisibly.
#'
#' @param plots A single plot object or a list of plots.  Each
#'   element may be a `ggplot`/grid object, a `recordedplot`, or a
#'   function with no arguments that when invoked produces the plot.
#' @param filename Base file name (without extension).  The
#'   appropriate extension is added automatically depending on
#'   `format`.  A full path can be supplied to save into a
#'   particular directory.  When saving raster formats the file
#'   names will include an index (e.g. `"myplot_001.png"`).
#' @param format Character string giving the desired output format.
#'   One of "pdf", "png", "jpeg" or "tiff".  Partial matching is
#'   performed.  The default is "pdf".
#' @param width,height Width and height of the device in inches.
#'   These arguments are passed directly to the graphics device.
#' @param dpi Resolution in dots per inch used for raster formats
#'   (ignored for PDF).
#' @param which Optional character vector naming the subset of
#'   `plots` to save.  If provided, only the named plots are
#'   exported; otherwise all elements are saved in the order they
#'   appear.
#' @export
#' @author Shubh Saraswat
#' @examples
#' # Example usage within CytoProfile:
#' # res <- cyt_splsda(...)
#' # cyt_export(res$plots, filename = "splsda", format = "png")
#'
cyt_export <- function(
  plots,
  filename = "cyto_output",
  format = c("pdf", "png", "jpeg", "tiff", "svg"),
  width = 10,
  height = 8,
  dpi = 300,
  which = NULL
) {
  format <- match.arg(tolower(format), c("pdf", "png", "jpeg", "tiff", "svg"))
  # Normalize plots to a list
  if (
    !is.list(plots) ||
      inherits(plots, c("gg", "ggplot", "grob", "gtable", "recordedplot")) ||
      is.function(plots)
  ) {
    plots <- list(plots)
  }
  # Subset if names provided
  if (!is.null(which)) {
    # Accept both numeric indices and character names
    if (is.character(which)) {
      missing_names <- setdiff(which, names(plots))
      if (length(missing_names) > 0) {
        stop(
          "The following plot names were not found: ",
          paste(missing_names, collapse = ", ")
        )
      }
      plots <- plots[which]
    } else if (is.numeric(which)) {
      plots <- plots[which]
    } else {
      stop("Argument 'which' must be NULL, numeric or character vector.")
    }
  }
  # Helper to determine plot type
  is_gridish <- function(x) {
    inherits(x, c("gg", "ggplot", "grob", "gtable"))
  }
  is_recorded <- function(x) {
    inherits(x, "recordedplot")
  }
  is_plotter <- function(x) {
    is.function(x)
  }
  # PDF: one file multi-page
  if (format == "pdf") {
    pdf_path <- paste0(filename, ".pdf")
    grDevices::pdf(file = pdf_path, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
    for (p in plots) {
      if (is_gridish(p)) {
        print(p)
      } else if (is_recorded(p)) {
        grDevices::replayPlot(p)
      } else if (is_plotter(p)) {
        p()
      } else {
        stop(
          "Unsupported plot type detected. Each element must be a ggplot, recordedplot or plot function."
        )
      }
    }
    invisible(NULL)
  } else {
    # Raster formats: separate files
    ext <- switch(
      format,
      png = "png",
      jpeg = "jpeg",
      tiff = "tiff",
      svg = "svg"
    )
    # Determine appropriate device function
    dev_fun <- switch(
      ext,
      png = grDevices::png,
      jpeg = grDevices::jpeg,
      tiff = grDevices::tiff,
      svg = grDevices::svg
    )
    for (i in seq_along(plots)) {
      p <- plots[[i]]
      # skip NULLs silently
      if (is.null(p)) {
        next
      }
      # flatten one level of nesting (e.g. loadings_list is a list of closures)
      if (
        is.list(p) &&
          !inherits(p, c("gg", "ggplot", "grob", "gtable", "recordedplot"))
      ) {
        warning(
          "Skipping nested list element at index ",
          i,
          " - pass a flat list of plot objects to cyt_export."
        )
        next
      }
      file_index <- sprintf("%03d", i)
      file_path <- paste0(filename, "_", file_index, ".", ext)
      if (format == "svg") {
        dev_fun(
          filename = file_path,
          width = width,
          height = height
        )
      } else {
        dev_fun(
          filename = file_path,
          width = width,
          height = height,
          units = "in",
          res = dpi
        )
      }
      tryCatch(
        {
          if (is_gridish(p)) {
            print(p)
          } else if (is_recorded(p)) {
            grDevices::replayPlot(p)
          } else if (is_plotter(p)) {
            p()
          } else {
            stop(
              "Unsupported plot type. Each element must be a ggplot, recordedplot or plot function."
            )
          }
        },
        finally = {
          grDevices::dev.off()
        }
      )
    }
    invisible(NULL)
  }
}
