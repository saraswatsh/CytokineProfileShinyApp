# Univariate Tests for Multi-Level Categorical Predictors

`cyt_univariate_multi` provides univariate statistical testing for
categorical predictors with more than two levels. For each categorical
predictor and numeric outcome pair, a global test is performed, followed
by pairwise comparisons when appropriate. Users may choose between two
methods: classical ANOVA with Tukey's Honest Significant Difference
(HSD) or a non-parametric Kruskal-Wallis test followed by pairwise
Wilcoxon rank-sum tests. The return format can either be a list of
adjusted p-values for each outcome-predictor pair or, if
`format_output = TRUE`, a list containing separate global and pairwise
summary tables.

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

  Character specifying the type of global test to perform. Use `"anova"`
  (default) for one-way ANOVA with Tukey HSD or `"kruskal"` for
  Kruskal-Wallis with pairwise Wilcoxon tests.

- cat_vars:

  Optional character vector of predictor column names. When `NULL`, all
  factor or character columns in `data` are used.

- cont_vars:

  Optional character vector of numeric outcome variable names. When
  `NULL`, all numeric columns in `data` are used.

- p_adjust_method:

  Character string specifying the method for p-value adjustment across
  pairwise Kruskal-Wallis follow-up comparisons. Passed to `p.adjust`.
  Default is `"BH"`.

- format_output:

  Logical. If `TRUE`, returns a list with global results, pairwise
  results, and assumption summaries; otherwise (default) returns a list
  of numeric vectors keyed by `"Outcome_Categorical"`. Each numeric
  vector contains adjusted p-values for the pairwise comparisons.

- progress:

  Optional. A Shiny `Progress` object for reporting progress updates
  during the analysis.

## Value

Either a list of adjusted pairwise p-values (if `format_output = FALSE`)
or a list with `results`, `pairwise`, and `assumptions` data frames (if
`format_output = TRUE`).

## Author

Shubh Saraswat

## Examples

``` r
data("ExampleData1")
cyt_univariate_multi(ExampleData1[, c(1:2, 5:6)], method = "kruskal",
  format_output = TRUE
)
#> $results
#>   Outcome Categorical Comparison           Test Statistic P_value
#> 1  GM.CSF       Group    Overall Kruskal-Wallis     0.300   0.861
#> 2   IFN.G       Group    Overall Kruskal-Wallis     8.127   0.017
#> 3  GM.CSF   Treatment    Overall Kruskal-Wallis   198.241   0.000
#> 4   IFN.G   Treatment    Overall Kruskal-Wallis   191.334   0.000
#> 
#> $pairwise
#>                    Outcome Categorical               Comparison
#> IFN.G_Group.1        IFN.G       Group             ND vs PreT2D
#> IFN.G_Group.2        IFN.G       Group                ND vs T2D
#> IFN.G_Group.3        IFN.G       Group            PreT2D vs T2D
#> GM.CSF_Treatment.1  GM.CSF   Treatment          CD3/CD28 vs LPS
#> GM.CSF_Treatment.2  GM.CSF   Treatment CD3/CD28 vs Unstimulated
#> GM.CSF_Treatment.3  GM.CSF   Treatment      LPS vs Unstimulated
#> IFN.G_Treatment.1    IFN.G   Treatment          CD3/CD28 vs LPS
#> IFN.G_Treatment.2    IFN.G   Treatment CD3/CD28 vs Unstimulated
#> IFN.G_Treatment.3    IFN.G   Treatment      LPS vs Unstimulated
#>                                 Test Estimate P_adj
#> IFN.G_Group.1      Pairwise Wilcoxon    36.35 0.025
#> IFN.G_Group.2      Pairwise Wilcoxon   -16.63 0.552
#> IFN.G_Group.3      Pairwise Wilcoxon   -65.43 0.025
#> GM.CSF_Treatment.1 Pairwise Wilcoxon     3.73 0.000
#> GM.CSF_Treatment.2 Pairwise Wilcoxon     4.53 0.000
#> GM.CSF_Treatment.3 Pairwise Wilcoxon     0.59 0.000
#> IFN.G_Treatment.1  Pairwise Wilcoxon 45492.85 0.000
#> IFN.G_Treatment.2  Pairwise Wilcoxon 46319.12 0.000
#> IFN.G_Treatment.3  Pairwise Wilcoxon    77.53 0.000
#> 
#> $assumptions
#> NULL
#> 
```
