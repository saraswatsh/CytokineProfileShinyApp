# ANOVA Analysis on Continuous Variables.

This function performs an analysis of variance (ANOVA) for each
continuous variable against every categorical predictor in the input
data. Character columns are automatically converted to factors; all
factor columns are used as predictors while numeric columns are used as
continuous outcomes. For each valid predictor (i.e., with more than one
level and no more than 10 levels), Tukey's Honest Significant Difference
(HSD) test is conducted and the adjusted p-values for pairwise
comparisons are extracted.

## Usage

``` r
cyt_anova(data, format_output = FALSE, progress = NULL)
```

## Arguments

- data:

  A data frame or matrix containing both categorical and continuous
  variables. Character columns will be converted to factors and used as
  predictors, while numeric columns will be used as continuous outcomes.

- format_output:

  Logical. If TRUE, returns the results as a tidy data frame instead of
  a list. Default is FALSE.

- progress:

  Optional. A Shiny `Progress` object for reporting progress updates.

## Value

If `format_output` is FALSE (default), a list of adjusted p-values from
Tukey's HSD tests for each combination of continuous outcome and
categorical predictor. If `format_output` is TRUE, a data frame with
columns "Outcome", "Categorical", "Comparison", and "P_adj".

## Examples

``` r
data("ExampleData1")
cyt_anova(ExampleData1[, c(1:2, 5:6)], format_output = TRUE)
#> $out_df
#>    Outcome Categorical            Comparison P_adj
#> 1   GM.CSF       Group             PreT2D-ND 0.773
#> 2   GM.CSF       Group                T2D-ND 0.537
#> 3   GM.CSF       Group            T2D-PreT2D 0.189
#> 4    IFN.G       Group             PreT2D-ND 0.088
#> 5    IFN.G       Group                T2D-ND 0.978
#> 6    IFN.G       Group            T2D-PreT2D 0.055
#> 7   GM.CSF   Treatment          LPS-CD3/CD28 0.000
#> 8   GM.CSF   Treatment Unstimulated-CD3/CD28 0.000
#> 9   GM.CSF   Treatment      Unstimulated-LPS 0.348
#> 10   IFN.G   Treatment          LPS-CD3/CD28 0.000
#> 11   IFN.G   Treatment Unstimulated-CD3/CD28 0.000
#> 12   IFN.G   Treatment      Unstimulated-LPS 0.999
#> 
#> $tukey_list
#> $tukey_list$GM.CSF_Group
#>  PreT2D-ND     T2D-ND T2D-PreT2D 
#>  0.7730980  0.5373287  0.1893654 
#> 
#> $tukey_list$IFN.G_Group
#>  PreT2D-ND     T2D-ND T2D-PreT2D 
#> 0.08833557 0.97787433 0.05495262 
#> 
#> $tukey_list$GM.CSF_Treatment
#>          LPS-CD3/CD28 Unstimulated-CD3/CD28      Unstimulated-LPS 
#>          7.183143e-13          6.974421e-13          3.481621e-01 
#> 
#> $tukey_list$IFN.G_Treatment
#>          LPS-CD3/CD28 Unstimulated-CD3/CD28      Unstimulated-LPS 
#>          7.445156e-13          7.394085e-13          9.987624e-01 
#> 
#> 
#> $message
#> [1] ""
#> 
```
