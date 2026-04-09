# Understanding PCA

## When to use PCA

Principal Component Analysis (PCA) is an unsupervised multivariate
method. It is useful when you want to understand broad structure in the
cytokine data without forcing the samples to separate by known group
labels.

That makes PCA a good first multivariate step when your question is
exploratory:

- Do samples cluster naturally?
- Are some groups partially separated even before using a supervised
  model?
- Which cytokines appear to drive the strongest sources of variation?

## When not to use PCA

PCA is usually not the best first choice when:

- your main goal is supervised class separation
- the outcome is a numeric response you want to predict
- you need formal group-wise statistical testing rather than exploratory
  structure

## What outputs matter most

The PCA workflow in CytokineProfile Shiny can produce several
complementary views:

- individuals plot
- scree plot
- loadings plots
- biplot
- correlation circle

These outputs answer different questions, so it is best to read them
together rather than relying on only one figure.

### Individuals plot

This is usually the first PCA figure to inspect. Each point is a sample
projected into the reduced component space.

Interpret it by asking:

- Do samples from the same group tend to cluster together?
- Do groups overlap heavily, or is there partial separation?
- Are there obvious outliers that may deserve follow-up?

Because PCA is unsupervised, any separation you see here can be
especially informative. The method did not use the class labels to force
that pattern.

### Scree plot

The scree plot shows how much variance is explained by each principal
component.

Use it to decide whether:

- the first two components already capture a meaningful amount of
  structure
- a third component is worth inspecting
- the data are spread across many components rather than dominated by
  one or two major axes

If the first few components explain only a small fraction of the total
variance, then a 2D PCA plot may be visually convenient but biologically
incomplete.

### Loadings plots and correlation circle

These outputs help answer which cytokines are driving the components.

In general:

- larger absolute loadings indicate stronger contribution to a component
- cytokines far from the center of a correlation circle contribute more
  strongly to the displayed components
- cytokines pointing in similar directions tend to vary together
- cytokines pointing in opposite directions tend to vary in opposing
  patterns

## Which app arguments matter most

The most important PCA settings are:

- `PCA Comparison Column`: decides how the main sample groups are
  colored or separated visually.
- `PCA Stratification Column`: adds a second visual grouping layer when
  you want shapes within colors.
- `Number of Components`: determines how many components are computed
  and emphasized.
- `Select Colors for PCA Plot (Optional)`: lets you override the
  automatic colors.
- `Draw Ellipse`: adds visual confidence regions to the individuals
  plot.
- `Plot Style`: enables 2D or 3D plotting.
- `Plotting Symbols`: helps distinguish subgroup structure when you use
  a second grouping column.

For most app users, one of the biggest decisions still happens earlier
in Step 2. The preprocessing or transformation applied there can change
which cytokines dominate the PCA, even though it is not chosen again on
the PCA settings screen.

## Common cautions

Keep these limits in mind:

- PCA does not test for statistical significance between groups.
- Separation in PCA can be influenced by scaling choices and outliers.
- A clear individuals plot does not automatically mean the components
  are easy to interpret biologically.
- If group labels are your main outcome of interest, PCA should often be
  followed by a supervised method rather than treated as the final
  answer.

## How to reproduce the result in the app

1.  Filter the dataset to the samples and cytokines you want to explore.
2.  Choose `Principal Component Analysis (PCA)`.
3.  Set `PCA Comparison Column`, `PCA Stratification Column`, and
    `Number of Components`.
4.  Decide whether to turn on `Draw Ellipse`, switch `Plot Style`, or
    adjust `Plotting Symbols`.
5.  Read the individuals plot together with the scree and loadings
    views.

## What to read next

Related articles:

- [Understanding
  PLSR](https://shinyinfo.cytokineprofile.org/articles/Understanding-PLSR.md)
  if the goal is numeric prediction rather than unsupervised structure.
- [Understanding
  (s)PLS-DA](https://shinyinfo.cytokineprofile.org/articles/Understanding-sPLS-DA.md)
  if class separation is the main question.
- [Understanding
  Heatmaps](https://shinyinfo.cytokineprofile.org/articles/Understanding-Heatmaps.md)
  for broader matrix-style pattern discovery.

------------------------------------------------------------------------

*Last updated:* April 09, 2026
