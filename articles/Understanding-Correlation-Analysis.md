# Understanding Correlation Analysis

## When to use correlation analysis

Correlation analysis is useful when your question is about coordinated
behavior rather than group differences. In CytokineProfile Shiny, it
helps answer questions such as:

- Which cytokines track most strongly with a target marker?
- Do those relationships look linear or only monotonic?
- Do correlations change between two biological groups?

## When not to use correlation analysis

Correlation analysis is usually not the best first choice when:

- your main goal is comparing group averages rather than associations
- the response of interest is categorical rather than numeric
- you need prediction or classification rather than association
  screening

## What the app is showing

The correlation workflow centers on one numeric response variable and
compares it with all other numeric variables in the dataset.

Depending on the settings, the app can return:

- overall correlation tables
- Pearson and Spearman correlation views
- optional subgroup-specific heatmaps
- a heatmap-style plot of the correlation structure

## Which app arguments matter most

The most important settings are:

- `Response Variable`: the numeric measurement you want to compare
  against the rest of the dataset.
- `Stratification Variable`: the optional grouping column used to split
  the correlations into subgroups.
- `Per-group Heatmaps`: whether the app should render separate
  subgroup-specific heatmaps.

### Choosing Pearson versus Spearman

The app shows both Pearson and Spearman results, so it helps to read
them together:

- Pearson correlation is best when the relationship is approximately
  linear and sensitive to magnitude.
- Spearman correlation is best when the relationship is monotonic but
  not necessarily linear, or when ranking is more reliable than raw
  scale.

For many biological datasets with skewness or outliers, Spearman is
often a safer first look.

## How to interpret the results

A good reading order is:

1.  Identify the strongest positive and negative correlations in the
    overall table.
2.  Compare the Pearson and Spearman views to see whether the
    relationship looks consistently strong across methods.
3.  If a `Stratification Variable` is selected, check whether those same
    relationships are consistent across subgroups.

Interpretation tips:

- A large absolute correlation coefficient suggests a stronger
  association.
- A small p-value suggests the observed association is less consistent
  with no correlation.
- Group differences in correlation can be biologically meaningful even
  when the overall correlation looks modest.

The subgroup-specific view is especially valuable because it can reveal
relationships that are masked when all samples are pooled together.

## Common cautions

Important cautions include:

- Correlation does not imply causation.
- Strong correlations can be driven by outliers or narrow sample ranges.
- Pooling heterogeneous groups can create misleading correlations.
- Multiple testing still matters when screening one target against many
  variables.

## How to reproduce the result in the app

1.  Choose a numeric target variable with biological interest.
2.  Choose `Correlation Plots`.
3.  Set `Response Variable`.
4.  Add a `Stratification Variable` and turn on `Per-group Heatmaps` if
    you want to compare biological subgroups.
5.  Use the table and the plot together to decide which associations
    deserve follow-up.

## What to read next

Related articles:

- [Understanding
  Heatmaps](https://shinyinfo.cytokineprofile.org/articles/Understanding-Heatmaps.md)
  for broader matrix-style clustering views.
- [Understanding
  PCA](https://shinyinfo.cytokineprofile.org/articles/Understanding-PCA.md)
  for unsupervised multivariate structure.
- [Understanding Boxplots and Violin
  Plots](https://shinyinfo.cytokineprofile.org/articles/Understanding-Boxplots-and-Violin-Plots.md)
  if you need to inspect the raw distributions behind a strong
  correlation.

------------------------------------------------------------------------

*Last updated:* April 02, 2026
