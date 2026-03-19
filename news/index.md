# Changelog

## CytokineProfileShinyApp 0.0.0.9000

This development build focuses on making the app easier to use, easier
to read, and more reliable.

### What’s New

- You can now run `Two-way ANOVA` and `ANCOVA` directly in the app.
- ANCOVA now supports optional `secondary:covariate` interactions, with
  clearer slope checks and more appropriate follow-up comparisons when
  factor-by-covariate effects are included.
- The app now gives clearer warnings when interaction effects may make
  results harder to interpret on their own.
- Many figure-based analyses now include easier text controls, so you
  can adjust titles, axis labels, legends, and other plot text without
  digging into code.
- Random Forest and XGBoost outputs now report model setup details more
  clearly, including split settings, class balance, cross-validation
  status, and key tuning choices.

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
- Step 2 now lets you force selected columns back to numeric, and any
  leftover non-numeric entries are converted to missing values instead
  of blocking numeric workflows.
- Column selections now update more smoothly, and related defaults stay
  in sync better as you change your data choices.
- The missing-value help now gives clearer guidance on when to use mean,
  median, mode, and the two nearest-neighbor options, and the popup is
  easier to read.
- Two-group testing controls now label `Welch t-test` more explicitly,
  so the manual choice matches the app’s underlying behavior more
  clearly.
- PLSR now handles partially missing predictor data more gracefully by
  keeping usable rows, dropping only unusable predictors, and giving
  clearer guidance when sparse columns may need missing-value treatment
  first.
- If your uploaded data includes out-of-range values, the app now warns
  you when you click `Save & Use` and explains what was adjusted in
  plain language.
- The app now starts more reliably in different R workflows.
- Progress notifications are easier to read during longer analyses, with
  cleaner spacing and clearer separation between the main task and
  detail text.

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
- The missing-value help popup now opens more cleanly and is less likely
  to be cut off inside the app window.
- The missing-value method selector now keeps its nearest-neighbor
  options in sync more reliably instead of dropping or mismatching those
  controls.
- PLSR is now more stable when only one component can be fit, and the
  VIP\>1 follow-up preview now skips safely instead of failing when too
  few predictors remain.
- Deselecting all categorical columns in Step 2 now stays deselected
  instead of snapping back to all selected.

### Behind the Scenes

- Several parts of the app were cleaned up to reduce small startup and
  package-loading problems.
- Internal helper code and UI regression checks were cleaned up so
  package checks behave more consistently across local testing and
  staged installs.
