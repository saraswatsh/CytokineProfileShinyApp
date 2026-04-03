# Understanding Error-Bar Plots

## When to use an error-bar plot

Error-bar plots are useful when you want a compact summary of
group-level values for many cytokines at once. In CytokineProfile Shiny,
they are most helpful when your question is:

- how do group summaries compare across many cytokines?
- do the group means or medians move in a consistent direction?
- which cytokines look most different before deeper follow-up?

This method is best when a clean summary view is more useful than
showing every raw point.

## When not to use an error-bar plot

Error-bar plots are usually not the best first choice when:

- the raw distributions are important and should be shown directly
- the dataset is small enough that hiding individual points would lose
  too much context
- you need a formal differential-summary method such as a volcano plot

If you want to see full distribution shape, [Understanding Boxplots and
Violin
Plots](https://shinyinfo.cytokineprofile.org/articles/Understanding-Boxplots-and-Violin-Plots.md)
is usually a better companion.

## Example context

A typical use case is comparing treatment groups or responder groups
across many cytokines after Step 2 filtering has defined the cohort and
comparison variable.

## What the app is showing

The error-bar workflow summarizes each cytokine within each group using
a selected center and uncertainty metric, then optionally adds
significance annotations across groups.

The figure answers a summary question first: how do the group-level
values compare across the measured variables?

## Which Step 4 arguments matter most

The highest-value controls are:

- `Comparison Column`: the grouping variable being compared.
- `Statistic`: whether the bar height represents the mean or median.
- `Error Metric`: what the error bars represent.
- `Statistical Test`: how the significance annotations are computed.
- `Multiple-Testing Correction`: whether p-values are adjusted across
  many comparisons.
- `Number of Columns`: how many cytokines are shown per page.

Interpret these settings carefully:

- `Statistic = Mean` pairs naturally with `Standard error`,
  `Standard deviation`, or `95% CI`.
- `Statistic = Median` is often easier to trust when the data are skewed
  or contain outliers.
- `95% CI` and `Standard error` describe uncertainty more than raw
  spread.
- `Standard deviation` and `MAD` describe variability in the
  measurements themselves.

## How to read the main output

A useful reading order is:

1.  Identify the cytokines where group summaries are farthest apart.
2.  Check whether the chosen `Error Metric` suggests stable or noisy
    group estimates.
3.  Read any statistical annotations only after remembering which
    `Statistical Test` and correction were used.

Interpretation checklist:

- large bar differences are more persuasive when the uncertainty bars
  are not overwhelmingly wide
- overlapping error bars do not automatically mean “not significant”
- non-overlapping error bars do not automatically mean “significant”
- significance markers should be read as summaries of the selected test,
  not as replacements for distribution inspection

## Common cautions

Important cautions include:

- error bars are often misread, especially when users forget whether
  they show spread or uncertainty
- raw data shape can be hidden, including skewness, bimodality, and
  outliers
- significance annotations can look stronger than the underlying effect
  size really is
- many side-by-side cytokines can make the page visually dense, so
  `Number of Columns` matters

## How to reproduce the result in the app

1.  Filter the dataset to the groups and cytokines you want to
    summarize.
2.  Choose `Error-Bar Plot`.
3.  Set `Comparison Column`.
4.  Choose `Statistic` and `Error Metric`.
5.  Review `Statistical Test` and `Multiple-Testing Correction` if you
    want annotations.
6.  Adjust `Number of Columns` if the figure feels crowded.
7.  Use boxplots or violin plots as a follow-up when the summary bars
    are not enough.

## What to read next

Related articles:

- [Understanding Boxplots and Violin
  Plots](https://shinyinfo.cytokineprofile.org/articles/Understanding-Boxplots-and-Violin-Plots.md)
  for fuller distribution views.
- [Understanding Volcano
  Plot](https://shinyinfo.cytokineprofile.org/articles/Understanding-Volcano-Plot.md)
  for two-group effect size plus significance screening.
- [Understanding Univariate Test
  Selection](https://shinyinfo.cytokineprofile.org/articles/Understanding-Univariate-Test-Selection.md)
  for choosing the underlying statistical workflow more deliberately.

------------------------------------------------------------------------

*Last updated:* April 03, 2026
