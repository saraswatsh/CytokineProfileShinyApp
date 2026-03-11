# Adjust p-values using a specified method

A thin wrapper around
[`stats::p.adjust`](https://rdrr.io/r/stats/p.adjust.html) that defaults
to the Benjamini–Hochberg procedure. Useful for unifying multiple
testing adjustments across the package.

## Usage

``` r
adjust_p(p_values, method = "BH")
```

## Arguments

- p_values:

  A numeric vector of raw p-values.

- method:

  A character string specifying the p‑value adjustment method. Passed
  directly to `p.adjust`. Defaults to "BH" (Benjamini–Hochberg). See
  `p.adjust.methods` for other options.

## Value

A numeric vector of adjusted p-values of the same length as `p_values`.
