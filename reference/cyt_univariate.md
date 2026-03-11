# Pairwise Univariate Tests Between Two Groups

`cyt_univariate` supports additional scaling options and explicit choice
of statistical test. For each categorical predictor with exactly two
levels and each numeric outcome, a two‑sample t‑test or Wilcoxon
rank–sum test is performed. Results are returned either as a list of
test objects or, if `format_output = TRUE`, as a tidy data frame with
one row per comparison.

## Usage

``` r
cyt_univariate(
  data,
  scale = NULL,
  method = c("auto", "ttest", "wilcox"),
  verbose = TRUE,
  format_output = FALSE,
  custom_fn = NULL,
  p_adjust_method = NULL,
  progress = NULL
)
```

## Arguments

- data:

  A data frame or matrix containing both categorical and numeric
  variables.

- scale:

  A character specifying a transformation to apply to numeric variables
  prior to testing. Choices are `NULL` (no transformation), "log2",
  "log10", "zscore", or "custom". When set to "custom", supply a
  function via `custom_fn`.

- method:

  Character specifying the test to perform. Use "auto" (default) to
  select between t‑test and Wilcoxon based on Shapiro–Wilk normality
  tests for each outcome; "ttest" to always use Student's t‑test; or
  "wilcox" to always use the Wilcoxon rank–sum test.

- verbose:

  Logical indicating whether to return the results. Provided for
  backward compatibility but has no effect on printing.

- format_output:

  Logical. If `TRUE`, returns the results as a tidy data frame; if
  `FALSE` (default), returns a list of test objects similar to the
  original function.

- custom_fn:

  A function to apply when `scale = "custom"`.

- p_adjust_method:

  Character or `NULL`. Method passed to
  [`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for correcting
  p-values across all comparisons (e.g., `"BH"` for Benjamini-Hochberg).
  If `NULL` (default) no adjustment is performed.

- progress:

  Optional. A Shiny `Progress` object for reporting progress updates
  during the analysis.

## Value

If `format_output = FALSE`, a named list of test objects keyed by
"Outcome_Categorical". If `format_output = TRUE`, a data frame with
columns `Outcome`, `Categorical`, `Comparison`, `Test`, `Estimate`,
`Statistic`, and `P_value`.

## Author

Shubh Saraswat

## Examples

``` r
data_df <- ExampleData1[, -c(3)]
data_df <- dplyr::filter(data_df, Group != "ND", Treatment != "Unstimulated")
cyt_univariate(data_df[, c(1:2, 5:6)], scale = "log2",
               method = "auto", format_output = TRUE)
#>   Outcome Categorical      Comparison
#> 1   IFN.G       Group   PreT2D vs T2D
#> 2   IL.10       Group   PreT2D vs T2D
#> 3   IFN.G   Treatment CD3/CD28 vs LPS
#> 4   IL.10   Treatment CD3/CD28 vs LPS
#>                                                Test Estimate Statistic P_value
#> 1 Wilcoxon rank sum test with continuity correction   -2.463    1599.0   0.008
#> 2 Wilcoxon rank sum test with continuity correction   -0.956    1625.0   0.012
#> 3 Wilcoxon rank sum test with continuity correction    9.024    4132.5   0.000
#> 4 Wilcoxon rank sum test with continuity correction    1.690    3091.0   0.000
```
