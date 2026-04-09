# Understanding MINT sPLS-DA

## When to use MINT sPLS-DA

MINT sPLS-DA is useful when you need supervised class separation across
multiple studies, batches, cohorts, or platforms. In CytokineProfile
Shiny, this is the method to use when:

- the outcome is categorical
- the same biological question was measured in more than one batch or
  study
- you want a model that integrates those sources while accounting for
  their separate origins

This makes MINT sPLS-DA more appropriate than ordinary sPLS-DA when
batch structure is not a nuisance detail but a defining part of the
study design.

## When not to use MINT sPLS-DA

MINT sPLS-DA is usually not the best choice when:

- all samples come from a single study or batch
- there is no reliable study or batch identifier column
- your outcome is numeric rather than categorical
- you only want unsupervised exploration rather than supervised
  discrimination

If you do not actually have multiple study sources, standard
`Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)` is
usually simpler and easier to interpret.

## Example context

A common use case is integrating cytokine measurements collected from
separate cohorts, recruitment waves, laboratories, or assay runs while
still asking whether disease groups can be discriminated reproducibly
across those sources.

## What the app is showing

The MINT workflow can return:

- `Global Sample Plot`
- `Partial Sample Plots`
- `Variable Loadings`
- `Correlation Circle`
- optional `Heatmap (CIM)`
- optional `ROC Curve`

If a `MINT sPLS-DA Stratification Column` is selected, the app can also
repeat the analysis within each subgroup and show nested result tabs.

## Which Step 4 arguments matter most

The controls that most affect interpretation are:

- `MINT sPLS-DA Comparison Column`: the classes the model is trying to
  separate.
- `Batch Column`: the study, batch, cohort, or platform identifier used
  for integration.
- `MINT sPLS-DA Stratification Column`: optional subgrouping for
  repeated analyses.
- `Number of Variables to Select`: how many predictors are kept on each
  component.
- `Number of Components`: how much latent structure is modeled.
- `Draw a Clustered Image Map?`: adds a selected-feature heatmap.
- `Draw Ellipse`, `Plot ROC Curve`, and `Draw Background`: optional aids
  for interpretation and performance checking.

For most users, `MINT sPLS-DA Comparison Column`, `Batch Column`, and
`Number of Variables to Select` are the settings that matter most.

## How to read the main outputs

### Global Sample Plot

This plot summarizes how the integrated model separates the biological
classes across all batches together.

Ask:

- do the classes separate at all after batch-aware integration?
- is the separation consistent or still dominated by overlap?
- do a few samples drive the appearance of separation?

### Partial Sample Plots

These plots help you judge batch-by-batch consistency. They are
especially valuable because a model can look strong globally while
working unevenly across studies.

A good sign is when the same class structure appears repeatedly across
the partial views rather than only in one batch.

### Variable Loadings and Correlation Circle

These outputs identify which cytokines drive the integrated components.

- larger absolute loadings indicate stronger contribution
- repeated appearance of the same cytokines across integrated views
  increases confidence
- highly batch-specific signals deserve caution, even if they look
  important

### Heatmap (CIM)

When enabled, the clustered image map helps you see whether the selected
variables show recognizable group patterning.

This is useful for communication, but it should support the model
interpretation rather than replace it.

### ROC Curve

If the ROC view is available, use it as a check on discrimination
quality rather than trusting the score plots alone.

## Common cautions

Important cautions include:

- MINT is only as good as the `Batch Column` you provide
- apparent separation can still reflect residual batch structure if the
  study sources are very different
- selected features are model-dependent, not final biomarkers by
  themselves
- small batches can make integrated discrimination look less stable than
  a single pooled plot suggests
- if one batch dominates the signal, reproducibility across studies may
  still be weak

## How to reproduce the result in the app

1.  Filter the dataset to the biological groups and samples you want to
    compare.
2.  Choose
    `Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)`.
3.  Set `MINT sPLS-DA Comparison Column`.
4.  Set `Batch Column` to the study or cohort identifier.
5.  Choose `Number of Variables to Select` and `Number of Components`.
6.  Optionally add `MINT sPLS-DA Stratification Column`,
    `Draw a Clustered Image Map?`, `Plot ROC Curve`, or `Draw Ellipse`.
7.  Read the global and partial plots together before trusting the
    selected variables.

## What to read next

Related articles:

- [Understanding
  (s)PLS-DA](https://shinyinfo.cytokineprofile.org/articles/Understanding-sPLS-DA.md)
  for supervised class separation without explicit multi-study
  integration.
- [Understanding
  PLSR](https://shinyinfo.cytokineprofile.org/articles/Understanding-PLSR.md)
  if your response is numeric instead of categorical.
- [Understanding
  PCA](https://shinyinfo.cytokineprofile.org/articles/Understanding-PCA.md)
  for a simpler unsupervised first look at structure.

------------------------------------------------------------------------

*Last updated:* April 09, 2026
