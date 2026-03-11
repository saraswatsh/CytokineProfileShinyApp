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
