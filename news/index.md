# Changelog

## CytokineProfileShinyApp 0.0.0.9000

This development build focuses on making the app easier to use, easier
to read, and more reliable.

### What’s New

- You can now run `Two-way ANOVA` and `ANCOVA` directly in the app.
- The app now gives clearer warnings when interaction effects may make
  results harder to interpret on their own.
- Many figure-based analyses now include easier text controls, so you
  can adjust titles, axis labels, legends, and other plot text without
  digging into code.

### Easier to Use

- Re-uploading a file now refreshes the saved copy right away, even if
  the file has the same name as before.
- Excel imports are more reliable. If a previously chosen sheet is no
  longer available, the app now switches safely instead of failing.
- When you upload a new file, saved editor choices from the previous
  file are cleared more cleanly.
- Columns that look like numbers, even when they include symbols or
  common missing-value markers, are now more likely to be recognized
  correctly.
- Column selections now update more smoothly, and related defaults stay
  in sync better as you change your data choices.
- The app now starts more reliably in different R workflows.

### Better Figures

- Plot text is now more customizable across many figure types.
- PCA, PLSR, sPLS-DA, and MINT sPLS-DA figures now use more readable
  default text sizes.
- Correlation circle plots in PCA and MINT sPLS-DA now behave more
  reliably.
- Heatmaps now display more consistently during interactive use.

### Bug Fixes

- Some result tables are now labeled more accurately when column names
  contain underscores.
- The app handles empty analysis results more gracefully instead of
  returning confusing output.
- The XGBoost workflow now chooses the best training step more reliably
  across different scoring methods.
- Editing data in the built-in editor now preserves values more
  consistently.

### Behind the Scenes

- Several parts of the app were cleaned up to reduce small startup and
  package-loading problems.
