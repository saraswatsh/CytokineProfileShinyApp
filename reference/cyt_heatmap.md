# Heat Map.

Heat Map.

## Usage

``` r
cyt_heatmap(
  data,
  scale = c(NULL, "log2", "row_zscore", "col_zscore"),
  annotation_col = NULL,
  annotation_side = c("auto", "row", "col"),
  progress = NULL
)
```

## Arguments

- data:

  A data frame containing the input data. Only numeric columns will be
  used.

- scale:

  Character. Optional scaling method. One of NULL (no scaling), "log2"
  (log2 transformation), "row_zscore" (z-score normalization by row), or
  "col_zscore" (z-score normalization by column). Default is NULL.

- annotation_col:

  Optional. A character string specifying a column name in `data` or a
  vector of annotations matching the number of rows or columns in `data`
  for adding colored annotations to the heatmap.

- annotation_side:

  Character. Specifies whether the annotation should be applied to rows
  or columns. One of "auto" (automatically determine based on length),
  "row", or "col". Default is "auto".

- progress:

  Optional. A Shiny `Progress` object for reporting progress updates.

## Value

A pheatmap object representing the generated heatmap.

## Examples

``` r
# Load sample data
data("ExampleData1")
data_df <- ExampleData1
# Generate a heatmap with log2 scaling and annotation based on
# the "Group" column
cyt_heatmap(
  data = data_df[, -c(2:3)],
  scale = "log2",  # Optional scaling
  annotation_col = "Group"
)

```
