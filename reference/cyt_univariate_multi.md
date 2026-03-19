# Univariate Tests for Multi-Level Categorical Predictors

`cyt_univariate_multi` provides univariate statistical testing for
categorical predictors with more than two levels. For one-way designs,
each categorical predictor and numeric outcome pair receives a global
test followed by pairwise comparisons when appropriate. Users may choose
between classical ANOVA with Tukey's Honest Significant Difference (HSD)
or a non-parametric Kruskal-Wallis test followed by pairwise Wilcoxon
rank-sum tests. The function also supports explicit two-way ANOVA and
ANCOVA designs with optional `primary:secondary`, `primary:covariate`,
and `secondary:covariate` interaction terms.

## Usage

``` r
cyt_univariate_multi(
  data,
  method = c("anova", "kruskal"),
  design = c("one_way", "two_way", "ancova"),
  cat_vars = NULL,
  cont_vars = NULL,
  p_adjust_method = "BH",
  primary_cat_var = NULL,
  secondary_cat_var = NULL,
  covariate_col = NULL,
  include_primary_secondary_interaction = FALSE,
  include_primary_covariate_interaction = FALSE,
  include_secondary_covariate_interaction = FALSE,
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
  Kruskal-Wallis with pairwise Wilcoxon tests. `"kruskal"` is only
  supported when `design = "one_way"`.

- design:

  Character specifying the model design. Use `"one_way"` (default) to
  preserve the existing multi-level ANOVA/Kruskal-Wallis workflow,
  `"two_way"` for two-way ANOVA, or `"ancova"` for ANCOVA.

- cat_vars:

  Optional character vector of predictor column names used by the
  one-way workflow. When `NULL`, all factor or character columns in
  `data` are used.

- cont_vars:

  Optional character vector of numeric outcome variable names. When
  `NULL`, all numeric columns in `data` are used, excluding
  `covariate_col` for ANCOVA.

- p_adjust_method:

  Character string specifying the method for p-value adjustment across
  pairwise Kruskal-Wallis follow-up comparisons. Passed to `adjust_p`.
  Default is `"BH"`.

- primary_cat_var:

  Optional primary categorical predictor used by the two-way ANOVA and
  ANCOVA workflows.

- secondary_cat_var:

  Optional secondary categorical predictor used by the two-way ANOVA and
  ANCOVA workflows.

- covariate_col:

  Optional numeric covariate used by the ANCOVA workflow.

- include_primary_secondary_interaction:

  Logical. Whether to include the `primary:secondary` interaction term
  when `secondary_cat_var` is supplied.

- include_primary_covariate_interaction:

  Logical. Whether to include the `primary:covariate` interaction term
  when `covariate_col` is supplied.

- include_secondary_covariate_interaction:

  Logical. Whether to include the `secondary:covariate` interaction term
  when both `secondary_cat_var` and `covariate_col` are supplied.

- format_output:

  Logical. If `TRUE`, returns a list with global results, pairwise
  results, and assumption summaries; otherwise (default) returns a list
  of numeric vectors keyed by `"Outcome_Categorical"` for the one-way
  workflow. Two-way ANOVA and ANCOVA return raw model bundles keyed by
  outcome when `format_output = FALSE`.

- progress:

  Optional. A Shiny `Progress` object for reporting progress updates
  during the analysis.

## Value

Either a list of adjusted pairwise p-values (if `format_output = FALSE`
and `design = "one_way"`) or a list with `results`, `pairwise`, and
`assumptions` data frames (if `format_output = TRUE`).

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
