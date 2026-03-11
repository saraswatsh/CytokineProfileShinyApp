# Apply a scale transformation to numeric columns

This helper function applies a chosen scaling or transformation to
specified numeric columns in a data frame. Supported built‑in
transformations include no transformation ("none"), log2, log10, and
z‑score scaling. A custom function can also be supplied to perform
arbitrary transformations.

## Usage

``` r
apply_scale(
  data,
  columns = NULL,
  scale = c("none", "log2", "log10", "zscore", "custom"),
  custom_fn = NULL
)
```

## Arguments

- data:

  A data.frame or matrix containing the data to be transformed.

- columns:

  A character vector of column names to transform. If NULL (default) all
  numeric columns will be transformed.

- scale:

  A character string specifying the transformation to apply. Possible
  values are "none", "log2", "log10", "zscore", or "custom". When set to
  "custom" the function specified in `custom_fn` will be applied to the
  columns.

- custom_fn:

  A function that takes a numeric vector and returns a transformed
  numeric vector. Only used when `scale = "custom"`.

## Value

A data.frame with the same dimensions as `data` with transformed numeric
columns.
