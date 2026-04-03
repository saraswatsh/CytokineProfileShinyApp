# Understanding Univariate Test Selection

## When to use this guide

This guide is for the point in the workflow where you know you want
formal statistical testing, but you are not yet sure which univariate
route in the app best matches the study design.

Use it when your next question is:

- is this a simple two-group comparison?
- do I have more than two groups?
- do I need two-way ANOVA or ANCOVA because the design is more complex?

## When not to use this guide

This guide is not the best starting point when:

- you are still deciding whether the data need exploratory plots first
- your goal is multivariate profile structure rather than
  cytokine-by-cytokine testing
- your main outcome is classification or prediction

## What the app is offering

The main univariate branches in CytokineProfile Shiny are:

- `Univariate Tests (T-test, Wilcoxon)` for two-group comparisons
- `Multi-level Univariate Tests (Anova, Kruskal-Wallis)` for one factor
  with more than two groups
- `Two-way ANOVA` for two categorical factors
- `ANCOVA` when a categorical effect needs adjustment for a continuous
  covariate

The best method depends more on the study design than on which result
table looks most familiar.

## Which app choice fits which question

### `Univariate Tests (T-test, Wilcoxon)`

Use this when:

- there are exactly two groups being compared
- the goal is cytokine-by-cytokine testing
- you want the app to choose between a parametric and non-parametric
  two-group test, or you already know which one you want

### `Multi-level Univariate Tests (Anova, Kruskal-Wallis)`

Use this when:

- there is one grouping factor with more than two levels
- the main question is whether at least one group differs from the
  others
- follow-up pairwise comparisons may be needed after the global test

### `Two-way ANOVA`

Use this when:

- there are two categorical design factors
- you want to estimate both main effects and their interaction
- the study question depends on whether one factor changes the effect of
  the other

### `ANCOVA`

Use this when:

- the outcome is numeric
- there is at least one categorical factor of interest
- a continuous covariate should be adjusted for rather than ignored

## How to read the outputs

A good reading order for any univariate workflow is:

1.  Start with the global test table.
2.  Check pairwise results only when the design and global evidence
    justify it.
3.  Read assumption checks before over-trusting borderline findings.
4.  Compare statistical significance with effect size and biological
    relevance.

Interpretation checklist:

- a low p-value does not automatically mean a large or important
  biological change
- many tested cytokines increase the importance of p-value adjustment
- violations of assumptions may matter as much as the p-value itself

## Common cautions

Important cautions include:

- choosing a test because it is familiar rather than because it matches
  the design
- ignoring repeated measures, covariates, or interactions that belong in
  the model
- treating pairwise comparisons as primary when the design question is
  global
- forgetting that statistical significance is only one part of
  interpretation

## How to reproduce the result in the app

1.  Use Step 2 to define the exact groups and variables you want to
    test.
2.  In Step 3, choose the branch that matches the design:
    `Univariate Tests (T-test, Wilcoxon)`,
    `Multi-level Univariate Tests (Anova, Kruskal-Wallis)`,
    `Two-way ANOVA`, or `ANCOVA`.
3.  In Step 4, confirm the grouping variables, factors, or covariates.
4.  Run the analysis and read the main table together with any pairwise
    or assumption outputs.

## What to read next

Related articles:

- [Understanding Multi-Level Univariate
  Analysis](https://shinyinfo.cytokineprofile.org/articles/Understanding-Multi-Level-Univariate-Analysis.md)
  for the more detailed one-way, two-way, and ANCOVA workflows.
- [Understanding Error-Bar
  Plots](https://shinyinfo.cytokineprofile.org/articles/Understanding-Error-Bar-Plot.md)
  for a compact visual summary of group comparisons.
- [Understanding Boxplots and Violin
  Plots](https://shinyinfo.cytokineprofile.org/articles/Understanding-Boxplots-and-Violin-Plots.md)
  if you want to inspect the distributions before committing to a test.

------------------------------------------------------------------------

*Last updated:* April 03, 2026
