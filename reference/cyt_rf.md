# Run Random Forest Classification on Cytokine Data

This function trains and evaluates a Random Forest classification model
on cytokine data. It includes variable importance visualization,
cross-validation for feature selection, and performance metrics such as
accuracy, sensitivity, and specificity. For binary classification the
function can also plot the ROC curve and compute the AUC.

## Usage

``` r
cyt_rf(
  data,
  group_col,
  ntree = 500,
  mtry = 5,
  train_fraction = 0.7,
  plot_roc = FALSE,
  k_folds = 5,
  step = 0.5,
  run_rfcv = TRUE,
  verbose = FALSE,
  seed = 123,
  cv = FALSE,
  cv_folds = 5,
  scale = c("none", "log2", "log10", "zscore", "custom"),
  custom_fn = NULL,
  output_file = NULL,
  progress = NULL
)
```

## Arguments

- data:

  A data frame containing the cytokine measurements. One column should
  correspond to the grouping variable (the outcome) and the remaining
  columns should be numeric predictors.

- group_col:

  A string naming the column in `data` that contains the grouping
  variable.

- ntree:

  Integer specifying the number of trees to grow. Default is `500`.

- mtry:

  Integer specifying the number of variables randomly sampled at each
  split. Default is `5`.

- train_fraction:

  Numeric between 0 and 1 giving the proportion of data used for
  training. Default is `0.7`.

- plot_roc:

  Logical. If `TRUE` and the problem is binary, an ROC curve and AUC are
  computed and returned. Default is `FALSE`.

- k_folds:

  Integer specifying the number of folds for `rfcv` when
  `run_rfcv = TRUE`. Default is `5`.

- step:

  Numeric specifying the fraction of variables removed at each step
  during `rfcv`. Default is `0.5`.

- run_rfcv:

  Logical indicating whether to run Random Forest cross-validation for
  feature selection. Default is `TRUE`.

- verbose:

  Logical. When `TRUE`, training and test performance metrics, confusion
  matrices, and cross-validation details are printed. Default is
  `FALSE`.

- seed:

  Optional integer seed for reproducibility. Default is `123`.

- cv:

  Logical indicating whether to perform a separate k-fold classification
  cross-validation using `caret`. Default is `FALSE`.

- cv_folds:

  Integer specifying the number of folds for classification
  cross-validation when `cv = TRUE`. Default is `5`.

- scale:

  Character string specifying a transformation to apply to numeric
  predictor columns prior to model fitting. One of `"none"` (default),
  `"log2"`, `"log10"`, `"zscore"`, or `"custom"`.

- custom_fn:

  A custom transformation function used when `scale = "custom"`.

- output_file:

  Optional. A file path to save outputs as a PDF. If `NULL` (default), a
  named list is returned for interactive display.

- progress:

  Optional. A Shiny `Progress` object for reporting progress updates.

## Value

When `output_file` is `NULL`, an invisible named list with elements
`summary_text`, `vip_plot`, `roc_plot`, `rfcv_plot`, `rfcv_data`,
`importance_data`, and `cv_results`. When `output_file` is provided, a
PDF is written and the function returns `NULL` invisibly.

## Author

Shubh Saraswat and Xiaohua Douglas Zhang

## Examples

``` r
data.df0 <- ExampleData1
data.df  <- data.frame(data.df0[, 1:3], log2(data.df0[, -c(1:3)]))
data.df  <- data.df[, -c(2:3)]
data.df  <- dplyr::filter(data.df, Group != "ND")
cyt_rf(data = data.df, group_col = "Group", k_folds = 5, ntree = 1000,
       mtry = 4, run_rfcv = TRUE, plot_roc = TRUE, verbose = FALSE)
```
