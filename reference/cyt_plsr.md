# Perform Partial Least Squares Regression (PLSR) or sparse PLSR (sPLSR) analysis

This function wraps the mixOmics `pls()` and `spls()` functions to
perform regression of one or more response variables on a set of
predictors. It automatically handles optional log2 scaling, extraction
of numeric columns and grouping information, and returns a small list of
recorded plots for display in Shiny. The first plot (`indiv_plot`) is a
score plot of the first two latent components colored by a grouping
variable (if provided). The second plot (`pred_plot`) is a scatterplot
of the observed vs predicted response values for the first response
variable. Additional plots could easily be added (e.g. loadings), but
these two provide basic diagnostics.

## Usage

``` r
cyt_plsr(
  data,
  response_col,
  group_col = NULL,
  ind_names = FALSE,
  comp_num = 2,
  sparse = FALSE,
  var_num = NULL,
  cv_opt = NULL,
  fold_num = 5,
  scale = NULL,
  ellipse = FALSE,
  pls_colors = NULL,
  output_file = NULL,
  progress = NULL
)
```

## Arguments

- data:

  A data frame containing the predictor and response variables.

- response_col:

  The name of the column containing the response variable. Must be
  numeric.

- group_col:

  The name of the column to use for grouping samples in the score plot.
  If `NULL`, no grouping is applied.

- ind_names:

  Logical, or a character vector. If `TRUE`, sample names are displayed
  on the plot. If a character vector, these names are used.

- comp_num:

  The number of components to use in the PLSR model.

- sparse:

  Logical. If `TRUE`, performs sparse PLSR (sPLSR).

- var_num:

  The number of variables to keep per component for sPLSR. Required if
  `sparse = TRUE`.

- cv_opt:

  Character string specifying the cross-validation option: "None",
  "LOOCV" (Leave-One-Out Cross-Validation), or "Mfold" (M-fold
  Cross-Validation).

- fold_num:

  The number of folds for M-fold cross-validation. Only applicable if
  `cv_opt = "Mfold"`.

- scale:

  Character string. If "log2", applies log2 transformation to numeric
  predictor columns.

- ellipse:

  Logical. If `TRUE`, draws 95% confidence ellipses on the score plot.

- pls_colors:

  A character vector of colors to use for grouping.

- output_file:

  Optional. A file path to save the plots as a PDF.

## Value

A list containing:

- scores_plot:

  A recorded plot of the PLSR scores.

- pred_vs_obs:

  A recorded plot of predicted vs. observed response values.

- residuals_plot:

  A recorded plot of residuals vs. fitted values.

- cv_plot:

  A recorded plot of cross-validation performance (Q2 and RMSEP), if
  `cv_opt` is not "None".

- loadings:

  A list of recorded plots for loadings of each component.

- vip_scores:

  A list of `ggplot` objects of VIP scores for each component.

- vip_scores_indiv:

  A recorded plot of the PLSR scores for variables with VIP \> 1, if
  applicable.

- vip_cv_plot:

  A recorded plot of cross-validation performance for variables with VIP
  \> 1, if applicable.

- metrics_text:

  Text summary of Q2 values, if available.

- pdf_file:

  The path to the generated PDF file, if `output_file` was provided.
