# Run XGBoost Classification on Cytokine Data

This function trains and evaluates an XGBoost classification model on
cytokine data. It allows for hyperparameter tuning, cross-validation,
and visualizes feature importance.

## Usage

``` r
cyt_xgb(
  data,
  group_col,
  train_fraction = 0.7,
  nrounds = 500,
  max_depth = 6,
  learning_rate = 0.1,
  nfold = 5,
  cv = FALSE,
  objective = "multi:softprob",
  early_stopping_rounds = NULL,
  eval_metric = "mlogloss",
  min_split_loss = 0,
  colsample_bytree = 1,
  subsample = 1,
  min_child_weight = 1,
  top_n_features = 10,
  verbose = 0,
  plot_roc = FALSE,
  print_results = FALSE,
  seed = 123,
  scale = c("none", "log2", "log10", "zscore", "custom"),
  custom_fn = NULL,
  output_file = NULL,
  progress = NULL
)
```

## Arguments

- data:

  A data frame containing numeric predictor variables and one grouping
  column. Non-numeric predictor columns will be coerced to numeric if
  possible.

- group_col:

  Character string naming the column that contains the class labels
  (target variable). Required.

- train_fraction:

  Numeric between 0 and 1 specifying the proportion of samples used for
  model training. Default is `0.7`.

- nrounds:

  Integer specifying the number of boosting rounds. Default is `500`.

- max_depth:

  Integer specifying the maximum depth of each tree. Default is `6`.

- learning_rate:

  Numeric specifying the learning rate. Default is `0.1`.

- nfold:

  Integer specifying the number of folds for cross-validation when
  `cv = TRUE`. Default is `5`.

- cv:

  Logical indicating whether to perform cross-validation using `xgb.cv`.
  Default is `FALSE`.

- objective:

  Character string specifying the objective function. Default is
  `"multi:softprob"`.

- early_stopping_rounds:

  Integer. Number of rounds without improvement before training stops
  early. Default is `NULL`.

- eval_metric:

  Character specifying the evaluation metric. Default is `"mlogloss"`.

- min_split_loss:

  Numeric. Minimum loss reduction required to make a further partition.
  Default is `0`.

- colsample_bytree:

  Numeric. Subsample ratio of columns per tree. Default is `1`.

- subsample:

  Numeric. Subsample ratio of training instances. Default is `1`.

- min_child_weight:

  Numeric. Minimum sum of instance weight in a child. Default is `1`.

- top_n_features:

  Integer. Number of top features to display in the importance plot.
  Default is `10`.

- verbose:

  Integer (0, 1, or 2) controlling verbosity of `xgb.train`. Default is
  `0`.

- plot_roc:

  Logical. Whether to plot the ROC curve and compute AUC for binary
  classification. Default is `FALSE`.

- print_results:

  Logical. If `TRUE`, prints the confusion matrix, top features, and
  cross-validation metrics to the console. Default is `FALSE`.

- seed:

  Optional integer seed for reproducibility. Default is `123`.

- scale:

  Character string specifying a transformation to apply to numeric
  predictors prior to model fitting. One of `"none"` (default),
  `"log2"`, `"log10"`, `"zscore"`, or `"custom"`.

- custom_fn:

  A custom transformation function used when `scale = "custom"`. Ignored
  otherwise.

- output_file:

  Optional. A file path to save outputs as a PDF. If `NULL` (default), a
  named list is returned for interactive display.

- progress:

  Optional. A Shiny `Progress` object for reporting progress updates.

## Value

When `output_file` is `NULL`, an invisible named list with elements
`summary_text`, `model`, `confusion_matrix`, `importance`,
`class_mapping`, `cv_results`, `importance_plot`, and `roc_plot`. When
`output_file` is provided, a PDF is written and the function returns
`NULL` invisibly.

## Author

Shubh Saraswat and Xiaohua Douglas Zhang

## Examples

``` r
data_df0 <- ExampleData1
data_df  <- data.frame(data_df0[, 1:3], log2(data_df0[, -c(1:3)]))
data_df  <- data_df[, -c(2:3)]
data_df  <- dplyr::filter(data_df, Group != "ND")
cyt_xgb(data = data_df, group_col = "Group", nrounds = 250,
        max_depth = 4, learning_rate = 0.05, nfold = 5,
        cv = FALSE, objective = "multi:softprob",
        eval_metric = "auc", plot_roc = TRUE, print_results = FALSE)
```
