# Understanding Multi-Level Univariate Analysis

## When to use multi-level univariate analysis

This workflow is useful when you want cytokine-by-cytokine inference but
the design is more complex than a simple two-group comparison.

In CytokineProfile Shiny, this article covers the higher-value workflows
available in the app:

- one-way comparisons across more than two groups
- two-way ANOVA
- ANCOVA

This is the right workflow when you want interpretable per-cytokine
inference while still respecting study design.

## When not to use multi-level univariate analysis

This workflow is usually not the best fit when:

- the study is only a simple two-group comparison
- the main question is multivariate patterning rather than per-cytokine
  inference
- the outcome is classification or prediction instead of hypothesis
  testing

## What the app is doing

The app fits one model per cytokine, then reports global results,
pairwise follow-up results when appropriate, and assumption summaries.

That means the analysis remains univariate at the cytokine level, but it
can still handle richer designs than a basic two-sample comparison.

## Design 1: one-way analysis

Use one-way analysis when one categorical variable has more than two
levels.

Examples:

- ND vs PreT2D vs T2D
- three treatment conditions
- multiple response categories

The app can support:

- ANOVA with Tukey follow-up comparisons
- Kruskal-Wallis with pairwise Wilcoxon follow-up comparisons

Use ANOVA when the assumptions are reasonably met. Use the
Kruskal-Wallis option when the distributions or sample conditions make a
rank-based approach more appropriate.

## Design 2: two-way ANOVA

Use two-way ANOVA when you want to estimate the effect of two
categorical variables on each cytokine.

Examples:

- disease group plus treatment
- treatment plus sex
- group plus stimulation condition

This design is especially useful when you need to know whether:

- one factor has a main effect
- the second factor has a main effect
- the effect of one factor depends on the other factor

If the interaction term is important, interpretation should focus on
that interaction first rather than on the main effects alone.

## Design 3: ANCOVA

Use ANCOVA when you want to compare groups while adjusting for a
continuous covariate.

Examples:

- group differences adjusted for age
- treatment differences adjusted for baseline biomarker level

ANCOVA is useful when you suspect that a continuous variable explains
part of the cytokine variation and you do not want group comparisons to
ignore that structure.

When covariate interactions are included, interpretation becomes more
specific: you are no longer just asking whether groups differ on
average, but whether the relationship with the covariate changes across
groups.

## Which app arguments matter most

The most important settings are:

- In `Multi-level Univariate Tests (Anova, Kruskal-Wallis)`,
  `Global Test Method` controls whether the overall test is ANOVA or
  Kruskal-Wallis. If you choose Kruskal-Wallis,
  `Pairwise P-Value Adjustment` controls the follow-up multiple-testing
  correction.
- In `Two-way ANOVA`, `Primary Factor`, `Secondary Factor`, and
  `Include primary:secondary interaction` define the fitted design.
- In `ANCOVA`, `Primary Factor`, `Secondary Factor (Optional)`, and
  `Covariate` define the adjusted model.
- In `ANCOVA`, `Include primary:secondary interaction`,
  `Include primary:covariate interaction`, and
  `Include secondary:covariate interaction` determine whether the model
  includes effect-modification terms.

These are not cosmetic settings. They change the scientific question the
app is answering for each cytokine.

## How to interpret the outputs

A practical reading order is:

1.  Start with the global results table.
2.  If the global test is meaningful, move to the pairwise or contrast
    results.
3.  Review the assumption summary before over-interpreting marginal
    p-values.

Interpretation reminders:

- In one-way analysis, the global result tells you whether any level
  differs before you focus on pairwise follow-ups.
- In two-way ANOVA, interaction terms can change the meaning of the main
  effects.
- In ANCOVA, the covariate-adjusted result is often more relevant than
  the raw group mean difference.

## Common cautions

Important cautions for these workflows are:

- Each cytokine is modeled separately, so multiple-testing context still
  matters.
- Significant interactions should not be ignored in favor of easier
  main-effect summaries.
- Sparse cells or very unbalanced designs can weaken interpretation.
- Assumption diagnostics matter more as the design becomes more complex.

## How to reproduce the result in the app

1.  Filter the dataset to the part of the study you want to analyze.
2.  Choose `Multi-level Univariate Tests (Anova, Kruskal-Wallis)`,
    `Two-way ANOVA`, or `ANCOVA`, depending on the study question.
3.  Set the exact factor and covariate fields for the workflow you
    selected.
4.  Turn the interaction checkboxes on only when the scientific question
    really requires them.
5.  Review global results, pairwise follow-ups, and assumption summaries
    together.

## What to read next

Related articles:

- [Understanding Univariate Test
  Selection](https://shinyinfo.cytokineprofile.org/articles/Understanding-Univariate-Test-Selection.md)
  for deciding which testing branch fits the design.
- [Understanding Error-Bar
  Plots](https://shinyinfo.cytokineprofile.org/articles/Understanding-Error-Bar-Plot.md)
  for a compact summary figure after testing.
- [Understanding Boxplots and Violin
  Plots](https://shinyinfo.cytokineprofile.org/articles/Understanding-Boxplots-and-Violin-Plots.md)
  for inspecting assumptions and raw distributions.

------------------------------------------------------------------------

*Last updated:* April 06, 2026
