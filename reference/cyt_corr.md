# Calculate correlations between a target variable and all other numeric variables.

This function computes Pearson or Spearman correlation coefficients
between a specified target variable and all other numeric variables in
the input data frame. It also calculates p-values and adjusted p-values
(Bonferroni and Benjamini-Hochberg). Optionally, it can perform
group-wise correlation analysis and compare correlations between two
groups using Fisher's z-transformation.

## Usage

``` r
cyt_corr(
  data,
  target,
  methods = c("spearman", "pearson"),
  group_var = NULL,
  compare_groups = FALSE,
  plot = FALSE,
  progress = NULL
)
```

## Arguments

- data:

  A data frame containing the variables.

- target:

  A character string specifying the name of the target variable. Must be
  a numeric column in `data`.

- methods:

  A character string indicating the correlation method to be used. Can
  be "pearson" (default) or "spearman".

- group_var:

  An optional character string specifying the name of a grouping
  variable in `data`. If provided, group-wise correlations will be
  calculated.

- compare_groups:

  Logical. If `TRUE` and `group_var` is provided with at least two
  levels, the function will compare the correlations between the first
  two levels of `group_var`.

- plot:

  Logical. If `TRUE`, the function will generate and return a
  correlation plot.

## Value

A list containing:

- results:

  A data frame with overall correlation results, including variable
  names, correlation coefficients (`r`), p-values (`p`), sample size
  (`n`), method, and adjusted p-values (`p_bonf`, `p_bh`), sorted by
  absolute correlation coefficient in descending order.

- results_groupwise:

  A data frame with group-wise correlation results, if `group_var` is
  provided. Includes similar columns as `results`, plus the `group`
  identifier.

- results_diff:

  A data frame with results of correlation comparison between the first
  two groups, if `compare_groups` is `TRUE` and `group_var` has at least
  two levels. Includes `variable`, `r_diff` (difference in
  correlations), `z` (Fisher's z-score), `p_diff` (p-value for the
  difference), and adjusted p-values (`p_diff_bonf`, `p_diff_bh`).

- plot:

  A correlation plot, if `plot` is `TRUE`.
