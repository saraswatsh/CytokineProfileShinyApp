# Generic export function for Cytokine plots

The purpose of this helper is to save a collection of plots generated
throughout the CytoProfile package to a user specified format. It is
designed to handle objects created with `ggplot2` (or other grid based
plots), recorded base plots (see
[`grDevices::recordPlot()`](https://rdrr.io/r/grDevices/recordplot.html)),
or custom drawing functions stored as closures. If a multi-page PDF is
requested, all plots are written into a single file; for raster formats
(PNG, JPEG and TIFF), each element of the plot list is saved to its own
file with a numbered suffix. Nothing is returned except invisibly.

## Usage

``` r
cyt_export(
  plots,
  filename = "cyto_output",
  format = c("pdf", "png", "jpeg", "tiff", "svg"),
  width = 10,
  height = 8,
  dpi = 300,
  which = NULL
)
```

## Arguments

- plots:

  A single plot object or a list of plots. Each element may be a
  `ggplot`/grid object, a `recordedplot`, or a function with no
  arguments that when invoked produces the plot.

- filename:

  Base file name (without extension). The appropriate extension is added
  automatically depending on `format`. A full path can be supplied to
  save into a particular directory. When saving raster formats the file
  names will include an index (e.g. `"myplot_001.png"`).

- format:

  Character string giving the desired output format. One of "pdf",
  "png", "jpeg" or "tiff". Partial matching is performed. The default is
  "pdf".

- width, height:

  Width and height of the device in inches. These arguments are passed
  directly to the graphics device.

- dpi:

  Resolution in dots per inch used for raster formats (ignored for PDF).

- which:

  Optional character vector naming the subset of `plots` to save. If
  provided, only the named plots are exported; otherwise all elements
  are saved in the order they appear.

## Author

Shubh Saraswat

## Examples

``` r
# Example usage within CytoProfile:
# res <- cyt_splsda(...)
# cyt_export(res$plots, filename = "splsda", format = "png")
```
