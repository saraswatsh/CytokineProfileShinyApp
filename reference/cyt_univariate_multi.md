# Univariate Tests for Multi‑Level Categorical Predictors

`cyt_univariate_multi`provides univariate statistical testing for
categorical predictors with more than two levels. For each categorical
predictor and numeric outcome pair, a global test is performed, followed
by pairwise comparisons if the global test is significant. Users may
choose between two methods, classical ANOVA with Tukey's Honest
Significant Difference (HSD) or a non‑parametric Kruskal–Wallis test
followed by pairwise Wilcoxon rank–sum tests. The return format can
either be a list of adjusted p‑values for each outcome–predictor pair
or, if `format_output = TRUE`, a tidy data frame summarizing all
pairwise comparisons.

## Usage

``` r
cyt_univariate_multi(
  data,
  method = c("anova", "kruskal"),
  cat_vars = NULL,
  cont_vars = NULL,
  p_adjust_method = "BH",
  format_output = FALSE,
  progress = NULL
)
```

## Arguments

- data:

  A data frame or matrix containing both categorical and continuous
  variables. Character columns will be converted to factors.

- method:

  Character specifying the type of global test to perform. Use "anova"
  (default) for one‑way ANOVA with Tukey HSD or "kruskal" for
  Kruskal–Wallis with pairwise Wilcoxon tests.

- cat_vars:

  Optional character vector of predictor column names. When `NULL`, all
  factor or character columns in `data` are used.

- cont_vars:

  Optional character vector of numeric outcome variable names. When
  `NULL`, all numeric columns in `data` are used.

- p_adjust_method:

  Character string specifying the method for p‑value adjustment across
  pairwise comparisons. Passed to `p.adjust`. Default is "BH".

- format_output:

  Logical. If `TRUE`, returns a tidy data frame; otherwise (default)
  returns a list of numeric vectors keyed by "Outcome_Categorical". Each
  numeric vector contains adjusted p‑values for the pairwise
  comparisons.

- progress:

  Optional. A Shiny `Progress` object for reporting progress updates
  during the analysis.

## Value

Either a list (if `format_output = FALSE`) or a data frame (if
`format_output = TRUE`).

## Author

Shubh Saraswat

## Examples

``` r
data("ExampleData1")
cyt_univariate_multi(ExampleData1[, c(1:2, 5:6)], method = "kruskal",
                     format_output = TRUE)
#> $results
#>   Outcome Categorical            Comparison  P_adj
#> 1  GM.CSF   Treatment          LPS-CD3/CD28 0.0000
#> 2  GM.CSF   Treatment Unstimulated-CD3/CD28 0.0000
#> 3  GM.CSF   Treatment      Unstimulated-LPS 0.0000
#> 4   IFN.G       Group             PreT2D-ND 0.0247
#> 5   IFN.G       Group                T2D-ND 0.5516
#> 6   IFN.G       Group            T2D-PreT2D 0.0247
#> 7   IFN.G   Treatment          LPS-CD3/CD28 0.0000
#> 8   IFN.G   Treatment Unstimulated-CD3/CD28 0.0000
#> 9   IFN.G   Treatment      Unstimulated-LPS 0.0000
#> 
#> $assumptions
#> NULL
#> 
```
