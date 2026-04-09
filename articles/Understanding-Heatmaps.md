# Understanding Heatmaps

## When to use a heatmap

A heatmap is useful when you want to compare many cytokines across many
samples at once and look for broader expression patterns, clusters, or
subgroup structure.

In CytokineProfile Shiny, heatmaps are especially helpful for:

- seeing whether samples cluster by group
- spotting cytokines that co-vary across the dataset
- summarizing many features in one compact figure

## When not to use a heatmap

A heatmap is usually not the best stand-alone choice when:

- your main goal is formal testing rather than pattern discovery
- you need an explicit predictive model
- the audience must see raw distributions rather than color-coded
  summaries

## What the app is showing

A heatmap converts the numeric cytokine matrix into a color-coded grid:

- rows or columns represent samples and cytokines
- color intensity represents relative abundance
- clustering dendrograms show similarity patterns when clustering is
  enabled

The exact interpretation depends heavily on scaling. That is one of the
most important choices in this workflow.

## Which app arguments matter most

The settings that usually matter most are:

- `Annotation Column`
- `Annotation Side`

### Why scaling matters

Heatmap interpretation also depends strongly on the preprocessing chosen
earlier in Step 2:

- a log-style transformation is often useful when cytokine values are
  highly skewed
- z-score style preprocessing emphasizes relative patterns rather than
  raw magnitude
- leaving the data closer to the original scale preserves absolute size
  differences more clearly

For app users, the most important question is whether you want to
preserve the original magnitude relationships or emphasize relative
pattern differences.

## How to interpret the figure

Read a heatmap in layers:

1.  Look for large group-level blocks of similar color.
2.  Check whether clustering puts samples from the same group near one
    another.
3.  Identify cytokine clusters that appear to rise or fall together.
4.  Use annotations to see whether those patterns align with biology,
    treatment, or batch structure.

If the color pattern changes dramatically when you switch scaling
methods, that is not necessarily a problem. It usually means the figure
is sensitive to whether you are emphasizing absolute magnitude or
relative patterning.

## Common cautions

Important cautions for heatmaps are:

- Strong clustering can sometimes be driven by only a few dominant
  cytokines.
- Z-scoring improves pattern visibility but removes the original units.
- A visually striking cluster is not automatically a statistically
  validated subgroup.
- Annotation labels are essential when the study design includes
  treatment arms, batches, or repeated structures.

## How to reproduce the result in the app

1.  Filter the dataset to the relevant samples and cytokines.
2.  Choose `Heatmap`.
3.  Decide on any Step 2 preprocessing before running the heatmap.
4.  Set `Annotation Column` and `Annotation Side` so the clustering can
    be interpreted in study context.
5.  Inspect the figure both with and without clustering if you want to
    separate raw ordering from similarity structure.

## What to read next

Related articles:

- [Understanding Correlation
  Analysis](https://shinyinfo.cytokineprofile.org/articles/Understanding-Correlation-Analysis.md)
  for targeted association screening.
- [Understanding
  PCA](https://shinyinfo.cytokineprofile.org/articles/Understanding-PCA.md)
  for a low-dimensional view of structure.
- [Understanding Boxplots and Violin
  Plots](https://shinyinfo.cytokineprofile.org/articles/Understanding-Boxplots-and-Violin-Plots.md)
  when a cluster needs raw-distribution follow-up.

------------------------------------------------------------------------

*Last updated:* April 09, 2026
