# Understanding PLSR

## When to use PLSR

Partial Least Squares Regression (PLSR) is useful when your goal is
prediction of one numeric outcome from many cytokines at the same time.
In CytokineProfile Shiny, it is the right choice when you want to answer
questions such as:

- Which cytokines best predict a continuous response?
- Does the overall cytokine profile explain variation in a clinical
  score, concentration, or severity measure?
- Can I reduce many correlated predictors into a smaller number of
  components without losing the prediction goal?

PLSR is especially helpful when the predictors are numerous and
correlated, which is common in cytokine datasets.

## When not to use PLSR

PLSR is usually not the best first choice when:

- your outcome is categorical rather than numeric, in which case
  `Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)` or a
  classifier is a better fit
- your goal is unsupervised structure rather than prediction, in which
  case `Principal Component Analysis (PCA)` is a better first look
- you mainly want cytokine-by-cytokine significance testing rather than
  multivariable prediction

## Example context

A typical use case is predicting a numeric inflammatory or clinical
outcome from a panel of cytokines after Step 2 filtering has limited the
dataset to the cohort and variables of interest.

## What the app is showing

The PLSR workflow combines prediction-oriented and
interpretation-oriented outputs:

- `Scores Plot`
- `Predicted vs Observed`
- `Residuals vs Fitted`
- `Loadings Plots`
- `VIP Scores`
- optional `Cross-Validation`
- optional `VIP > 1: Scores`
- optional `VIP > 1: Cross-Validation`

These views should be read together. A model can produce visually
interesting components without actually predicting the outcome well.

## Which Step 4 arguments matter most

The highest-value controls are:

- `Response Column`: the numeric outcome the model is trying to predict.
- `Predictor Columns`: the cytokines or other numeric predictors
  included in the model.
- `Number of Components`: how many latent components the model extracts.
- `Sparse PLSR`: whether the model keeps all predictors or enforces
  variable selection.
- `Number of Variables`: how many predictors are retained per component
  when `Sparse PLSR` is turned on.
- `Cross-validation` and `Number of Folds`: whether the app estimates
  generalization performance.
- `Grouping Column` and `Ellipse`: visual aids for the score plot only.

In practice, `Response Column`, `Predictor Columns`, and
`Number of Components` define the model, while `Sparse PLSR` changes how
simple or dense the predictor set remains.

## How to read the main outputs

### Scores Plot

This plot shows the samples in the reduced component space. It is most
useful for asking whether samples with similar response behavior also
occupy similar positions in the latent space.

Interpretation checklist:

1.  Check whether samples with similar biology or known groups cluster
    loosely together.
2.  Look for extreme outliers that might dominate the model.
3.  Do not treat this plot alone as evidence of good prediction.

### Predicted vs Observed

This is one of the most important plots because it shows how closely the
fitted values track the true response.

- points close to the diagonal suggest stronger predictive agreement
- wide scatter suggests weaker predictive value
- systematic curvature or separation can suggest the model is missing
  structure

If this plot looks weak, then attractive scores or loadings plots should
be interpreted cautiously.

### Residuals vs Fitted

This plot helps you judge model misfit.

- a roughly patternless cloud is more reassuring
- strong trends or funnels suggest the model error changes across the
  fitted range
- a few extreme residuals can indicate influential samples

### Loadings Plots

These plots show which cytokines contribute most strongly to each
component.

- larger absolute loadings indicate stronger contribution
- the sign is directional, not a p-value
- if the same predictors recur across components, they may be especially
  influential

### VIP Scores

VIP scores summarize predictor importance across the model. They are
useful for prioritization rather than formal hypothesis testing.

A common rule of thumb is that predictors above 1 are more influential,
which is why the app can also show `VIP > 1` views.

## Common cautions

Important limits to remember:

- good component separation does not automatically mean good prediction
- too many components can start modeling noise
- sparse models are easier to interpret, but too much sparsity can throw
  away useful signal
- correlated biomarkers can trade importance with one another, so
  variable rankings are not absolute
- PLSR is not a replacement for external validation

## How to reproduce the result in the app

1.  Filter the dataset to the samples and cytokines you want to model.
2.  Choose `Partial Least Squares Regression (PLSR)`.
3.  Set `Response Column` and confirm the `Predictor Columns`.
4.  Choose `Number of Components`.
5.  Turn on `Sparse PLSR` only if you want a smaller selected predictor
    set.
6.  Add `Cross-validation` if you want a better sense of model
    stability.
7.  Read `Predicted vs Observed`, `Residuals vs Fitted`,
    `Loadings Plots`, and `VIP Scores` together.

## What to read next

Related articles:

- [Understanding
  PCA](https://shinyinfo.cytokineprofile.org/articles/Understanding-PCA.md)
  if your goal is unsupervised structure rather than prediction.
- [Understanding
  (s)PLS-DA](https://shinyinfo.cytokineprofile.org/articles/Understanding-sPLS-DA.md)
  if the outcome is categorical rather than numeric.
- [Understanding MINT
  sPLS-DA](https://shinyinfo.cytokineprofile.org/articles/Understanding-MINT-sPLS-DA.md)
  if the study spans multiple batches or cohorts.

------------------------------------------------------------------------

*Last updated:* April 03, 2026
