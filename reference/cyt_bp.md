# Boxplots for Overall Comparisons by Continuous Variables.

This function creates boxplots for the continuous variables in the
provided data. If the number of columns in `data` exceeds `bin_size`,
the plots are split across multiple chunks. If an `output_file` is
provided, the function writes the plots to that PDF file; otherwise, it
returns a list of ggplot2 objects.

## Usage

``` r
cyt_bp(
  data,
  output_file = NULL,
  bin_size = 25,
  mf_row = c(1, 1),
  y_lim = NULL,
  scale = NULL,
  progress = NULL
)
```

## Arguments

- data:

  A matrix or data frame containing the raw data to be plotted.

- output_file:

  Optional. A file path to save the plots as a PDF file. If NULL, the
  function returns a list of ggplot2 objects.

- bin_size:

  An integer specifying the maximum number of box plots to display per
  chunk.

- mf_row:

  A numeric vector of length two specifying the layout (rows and
  columns) in the PDF output. (Not used when returning ggplot2 objects.)

- y_lim:

  An optional numeric vector defining the y-axis limits for the plots.

- scale:

  An optional character string. If set to "log2", numeric columns are
  log2-transformed.

- progress:

  Optional. A Shiny `Progress` object for reporting progress updates.

## Value

If `output_file` is NULL, a list of ggplot2 objects; otherwise, writes a
PDF and returns NULL.

## Examples

``` r
data_df <- ExampleData1

cyt_bp(data_df[,-c(1:3)], output_file = NULL, scale = "log2")
#> [[1]]

#> 
```
