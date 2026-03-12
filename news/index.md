# Changelog

## CytokineProfileShinyApp 0.0.0.9000

The development build contains several quality-of-life fixes and
reliability improvements focused on data upload, type handling, editor
state, and initial performance profiling. Key changes:

- Upload & file handling
  - Uploaded files are now always copied to the app upload directory
    with `overwrite = TRUE`, so re-uploading a file (even with the same
    filename) refreshes the stored copy immediately.
  - Excel sheet selection is now robust: the app validates
    `input$sheet_name` against the workbook’s actual sheets and falls
    back to the first sheet when a previously selected sheet is not
    present, avoiding “Sheet ‘NA’ not found” errors.
  - When a new file is uploaded the Bio‑Plex editor persisted state and
    related caches are cleared and a short notification is shown.
- Missing values & type coercion
  - Character columns that are numeric-like (for example containing NA
    tokens, asterisks, OOR markers, or commas) are now heuristically
    cleaned and coerced back to numeric where appropriate using the
    existing cleaning helper. This prevents numeric columns from being
    mis-classified as character and improves downstream analyses.
- Reactive state & UI sync
  - `userState$selected_columns` is updated immediately when Step 2
    checkbox groups change, so downstream reactives (including
    variable-count defaults) reflect current selections without needing
    to navigate forward.
  - Defaults for `plsr_keepX`, `splsda_var_num`, and
    `mint_splsda_var_num` are recomputed and pushed to the UI when
    column selections change, but manual user overrides are respected
    via `*_manual` flags.
- Data editor robustness
  - The data editor now clears stale `sheet_name` selections on new
    uploads to prevent mismatches between previously selected sheet
    names and the newly uploaded workbook.
  - The editor’s AutoFill and cell-edit path preserve numeric coercion
    logic for edited cells.
- Bug fixes & code quality (PR
  [\#1](https://github.com/saraswatsh/CytokineProfileShinyApp/issues/1)
  review)
  - [`cyt_heatmap()`](https://shinyinfo.cytokineprofile.org/reference/cyt_heatmap.md):
    the heatmap is now drawn on the active graphics device in
    interactive use. Previously `silent = TRUE` was unconditional,
    suppressing all on-screen rendering; it is now only set when saving
    to a file.
  - [`cyt_univariate()`](https://shinyinfo.cytokineprofile.org/reference/cyt_univariate.md)
    and
    [`cyt_univariate_multi()`](https://shinyinfo.cytokineprofile.org/reference/cyt_univariate_multi.md):
    fixed a key-parsing bug where column names containing underscores
    would produce mis-labelled `Outcome` and `Categorical` columns in
    formatted output. Results are now assembled by iterating over the
    original variable vectors directly rather than splitting the
    composite key string. Both functions also now return a consistent
    type (empty [`list()`](https://rdrr.io/r/base/list.html) or typed
    empty [`data.frame()`](https://rdrr.io/r/base/data.frame.html) with
    a [`warning()`](https://rdrr.io/r/base/warning.html)) when no valid
    tests can be performed, instead of a bare character string.
  - [`cyt_xgb()`](https://shinyinfo.cytokineprofile.org/reference/cyt_xgb.md):
    the best cross-validation iteration is now chosen correctly for all
    evaluation metrics. When early stopping is enabled,
    `xgb_cv$best_iteration` is used directly; otherwise the selection
    switches between
    [`which.max()`](https://rdrr.io/r/base/which.min.html) and
    [`which.min()`](https://rdrr.io/r/base/which.min.html) depending on
    whether the metric is higher-is-better (e.g. `auc`, `aucpr`, `map`,
    `ndcg`) or lower-is-better (e.g. `logloss`, `rmse`).
  - [`run_app()`](https://shinyinfo.cytokineprofile.org/reference/run_app.md):
    the function now resolves the app directory using
    `dirname(system.file("app.R", ...))`, which works correctly with
    both
    [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
    and a standard package installation.
  - `NAMESPACE`:
    [`rlang::.data`](https://rlang.r-lib.org/reference/dot-data.html)
    and `rlang::%||%` are now formally imported via
    `CytokineProfileShinyApp-package.R`, preventing potential “object
    not found” errors when tidy-eval pronouns or the null-coalescing
    operator are used in package context.
