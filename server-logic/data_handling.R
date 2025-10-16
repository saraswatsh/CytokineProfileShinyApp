## ---------------------------
## Data Upload and Built-in Data Option
## ---------------------------
# remember the checkbox
shiny::observeEvent(
  input$use_builtin,
  {
    userState$use_builtin <- input$use_builtin
  },
  ignoreNULL = FALSE
)

# remember which built‑in data they picked
shiny::observeEvent(
  input$built_in_choice,
  {
    userState$built_in_choice <- input$built_in_choice
  },
  ignoreNULL = FALSE
)
apply_stamp <- shiny::reactiveVal(0)
shiny::observeEvent(
  input$apply_types,
  {
    apply_stamp(apply_stamp() + 1)
  },
  ignoreInit = TRUE
)

userData <- shiny::reactive({
  if (isTRUE(bioplex$active) && !is.null(bioplex$final)) {
    df <- bioplex$final
  } else if (isTRUE(input$use_builtin)) {
    req(input$built_in_choice)
    df <- get(input$built_in_choice)
    dest <- file.path(builtins_dir, paste0(input$built_in_choice, ".rds"))
    if (!file.exists(dest)) saveRDS(df, dest)
  } else {
    req(input$datafile)
    dest <- file.path(upload_dir, input$datafile$name)
    if (!file.exists(dest)) {
      file.copy(input$datafile$datapath, dest, overwrite = TRUE)
    }
    ext <- tolower(tools::file_ext(dest))
    if (ext == "csv") {
      df <- read.csv(dest, stringsAsFactors = FALSE)
    } else if (ext == "txt") {
      df <- read.table(
        dest,
        header = TRUE,
        sep = "\t",
        stringsAsFactors = FALSE
      )
    } else if (ext %in% c("xls", "xlsx")) {
      all_sheets <- readxl::excel_sheets(dest)
      sheet_to_read <- if (
        !is.null(input$sheet_name) && length(input$sheet_name) > 0
      ) {
        intersect(input$sheet_name, all_sheets)[1]
      } else {
        all_sheets[1]
      }
      df <- readxl::read_excel(dest, sheet = sheet_to_read) %>%
        as.data.frame()
    } else {
      stop("Unsupported file type.")
    }
  }
  df$..cyto_id.. <- 1:nrow(df)
  # --- apply user-chosen types when the button is pressed ---
  if (apply_stamp() > 0) {
    fc <- isolate(input$factor_cols)
    if (length(fc)) {
      keep <- intersect(fc, names(df))
      df[keep] <- lapply(df[keep], function(x) factor(as.character(x)))
    }
    if (
      isTRUE(isolate(input$factor_order_enable)) &&
        isTruthy(isolate(input$factor_order_col)) &&
        isTruthy(isolate(input$factor_levels_csv))
    ) {
      target <- isolate(input$factor_order_col)
      if (target %in% names(df)) {
        levs <- trimws(strsplit(isolate(input$factor_levels_csv), ",")[[1]])
        if (length(levs)) {
          df[[target]] <- factor(as.character(df[[target]]), levels = levs)
        }
      }
    }
  }
  df
})
# Hide the internal ID from any UI choices
safe_names <- function(df) setdiff(names(df), "..cyto_id..")

# Convenience: only numeric or only categorical user-facing cols
safe_num <- function(df) {
  cn <- safe_names(df)
  cn[sapply(df[cn], is.numeric)]
}
safe_cat <- function(df) {
  cn <- safe_names(df)
  cn[!sapply(df[cn], is.numeric)]
}

## ---------------------------
## UI for Sheet Selector and Built-in Data Choice
## ---------------------------
output$sheet_selector <- shiny::renderUI({
  req(input$datafile)
  ext <- tolower(tools::file_ext(input$datafile$name))
  if (!ext %in% c("xls", "xlsx")) {
    return(NULL)
  }

  # Read sheet names directly from the uploaded temp file
  sheets <- tryCatch(
    readxl::excel_sheets(input$datafile$datapath),
    error = function(e) character()
  )

  if (length(sheets) == 0) {
    return(NULL)
  }

  selectizeInput(
    inputId = "sheet_name",
    label = "Select the desired sheets:",
    multiple = TRUE,
    choices = sheets,
    options = list(
      placeholder = "Select sheets..."
    ),
    selected = isolate(userState$sheet_name) %||% sheets[1]
  )
})

output$built_in_selector <- shiny::renderUI({
  if (!isTRUE(input$use_builtin)) {
    return(NULL)
  }

  # Use radioButtons to display all options inside the card
  radioButtons(
    inputId = "built_in_choice",
    label = "Select a built-in dataset:",
    choices = c(
      "ExampleData1",
      "ExampleData2",
      "ExampleData3",
      "ExampleData4",
      "ExampleData5"
    ), # Using example names
    selected = isolate(userState$built_in_choice) %||% "ExampleData1"
  )
})
# ----- Bio-Plex: multi-sheet picker -----
# flag for conditionalPanel
output$bioplex_on <- shiny::reactive({
  isTruthy(input$bioplex_file) && isTruthy(input$bioplex_file$datapath)
})
shiny::outputOptions(output, "bioplex_on", suspendWhenHidden = FALSE)

shiny::observeEvent(input$bioplex_file, {
  # only proceed for Excel uploads
  req(isTruthy(input$bioplex_file$datapath))
  req(grepl("\\.xlsx?$", input$bioplex_file$name, ignore.case = TRUE))

  sh <- tryCatch(
    readxl::excel_sheets(input$bioplex_file$datapath),
    error = function(e) character(0)
  )
  # update the choices (safe even if the input was just created)
  shiny::updateSelectizeInput(
    session,
    "bioplex_sheets",
    choices = sh,
    selected = head(sh, 1),
    server = TRUE
  )
})

output$bioplex_sheet_selector <- shiny::renderUI({
  req(input$bioplex_file)
  sheets <- tryCatch(
    readxl::excel_sheets(input$bioplex_file$datapath),
    error = function(e) character()
  )
  if (!length(sheets)) {
    return(NULL)
  }
  shiny::selectizeInput(
    "bioplex_sheets",
    "Choose up to two sheets:",
    choices = sheets,
    multiple = TRUE,
    options = list(maxItems = 2, placeholder = "Select up to two sheets")
  )
})

# ----- Bio-Plex: build working table from selected sheet(s) -----
bioplex_build_df <- shiny::reactive({
  req(isTruthy(input$bioplex_file$datapath))
  req(length(input$bioplex_sheets) >= 1) # at least one sheet picked

  dfs <- lapply(input$bioplex_sheets, function(sh) {
    x <- readxl::read_excel(
      input$bioplex_file$datapath,
      sheet = sh,
      col_names = FALSE
    )
    if (!nrow(x)) {
      return(NULL)
    } # nothing to use
    hdr <- as.character(unlist(x[1, ], use.names = FALSE))
    x <- x[-1, , drop = FALSE]
    names(x) <- make.unique(make.names(hdr)) # valid, unique
    x
  })
  dfs <- Filter(Negate(is.null), dfs)
  req(length(dfs) >= 1) # ensure at least one non-empty sheet
  dplyr::bind_rows(dfs) # bind by names, fill NAs as needed
})

# Keep working copy in reactiveValues so we can rename columns & delete rows before import
shiny::observeEvent(
  bioplex_build_df(),
  {
    req(bioplex_build_df())
    bioplex$df <- bioplex_build_df()
    bioplex$deleted_idx <- integer(0)
    bioplex$active <- FALSE
  },
  ignoreInit = TRUE
)
# Per-sheet data frames (row 1 -> column names)
bioplex_per_sheet <- shiny::reactive({
  # Prefer Option A (new flow)
  if (isTruthy(input$datafile) && length(input$sheet_name) >= 1) {
    path <- input$datafile$datapath
    sh <- input$sheet_name
    # Fallback: legacy Bio-Plex inputs (if you ever re-enable Option C)
  } else if (
    isTruthy(input$bioplex_file) && length(input$bioplex_sheets) >= 1
  ) {
    path <- input$bioplex_file$datapath
    sh <- input$bioplex_sheets
  } else {
    return(NULL)
  }

  out <- setNames(
    lapply(sh, function(s) {
      x <- readxl::read_excel(path, sheet = s, col_names = FALSE)
      if (!nrow(x)) {
        return(NULL)
      }
      hdr <- as.character(unlist(x[1, ], use.names = FALSE))
      x <- x[-1, , drop = FALSE]
      names(x) <- make.unique(make.names(hdr))
      x
    }),
    sh
  )
  Filter(Negate(is.null), out)
})


# Combined
bioplex_combined <- shiny::reactive({
  req(bioplex_per_sheet())
  dplyr::bind_rows(bioplex_per_sheet())
})
bioplex_combined_filtered <- shiny::reactive({
  ps <- bioplex_per_sheet()
  req(length(ps) >= 1)
  dels <- bioplex$deleted_by_sheet %||% list()

  filtered <- lapply(names(ps), function(nm) {
    df <- ps[[nm]]
    del <- dels[[nm]] %||% integer(0)
    keep <- setdiff(seq_len(nrow(df)), del)
    df[keep, , drop = FALSE]
  })
  dplyr::bind_rows(filtered)
})
# shows the button only when a file is uploaded
output$open_editor_btn <- shiny::renderUI({
  req(input$datafile) # waits until a file is selected/uploaded
  div(
    class = "mt-2",
    actionButton(
      "open_editor",
      "Open Data Editor",
      class = "btn-primary btn-sm ms-2"
    )
  )
})
shiny::observeEvent(input$open_editor, {
  req(input$datafile)
  dest <- file.path(upload_dir, input$datafile$name)
  if (!file.exists(dest)) {
    file.copy(input$datafile$datapath, dest, overwrite = TRUE)
  }
  ext <- tolower(tools::file_ext(dest))

  if (ext %in% c("xls", "xlsx") && length(input$sheet_name) >= 1) {
    # SHEETS MODE → tabs appear
    bioplex$editor_mode <- "sheets"
    ps <- bioplex_per_sheet() # now points at Option A inputs
    req(length(ps) >= 1)
    bioplex$deleted_by_sheet <- setNames(
      replicate(length(ps), integer(0), simplify = FALSE),
      names(ps)
    )
    bioplex$df <- dplyr::bind_rows(ps) # combined working table (no .id column)
  } else {
    # PERSISTED MODE for csv/txt or single-sheet Excel
    df <- switch(
      ext,
      "csv" = read.csv(dest, stringsAsFactors = FALSE),
      "txt" = read.table(
        dest,
        header = TRUE,
        sep = "\t",
        stringsAsFactors = FALSE
      ),
      "xls" = as.data.frame(readxl::read_excel(
        dest,
        sheet = input$sheet_name %||% 1L
      )),
      "xlsx" = as.data.frame(readxl::read_excel(
        dest,
        sheet = input$sheet_name %||% 1L
      )),
      stop("Unsupported file type.")
    )
    bioplex$editor_mode <- "persisted"
    bioplex$df <- df
  }

  bioplex$deleted_idx <- integer(0)
  show_data_editor_modal()
})


output$bioplex_modal_tabs <- shiny::renderUI({
  req(bioplex$df)

  mk_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

  sheet_tabs <- if (identical(bioplex$editor_mode, "sheets")) {
    lapply(names(bioplex_per_sheet()), function(nm) {
      stem <- mk_id(nm)
      tbl_id <- paste0("bp_tbl_", stem)
      del_id <- paste0("bp_del_", stem)
      rst_id <- paste0("bp_rst_", stem)

      # render the per-sheet table (filtered by this sheet's deletes)
      local({
        nm_local <- nm
        output[[tbl_id]] <- DT::renderDT(
          {
            df <- bioplex_per_sheet()[[nm_local]]
            del <- bioplex$deleted_by_sheet[[nm_local]] %||% integer(0)
            keep <- setdiff(seq_len(nrow(df)), del)
            DT::datatable(
              df[keep, , drop = FALSE],
              selection = "multiple",
              options = list(
                scrollX = TRUE,
                scrollY = "55vh",
                paging = FALSE
              ),
              rownames = FALSE
            )
          },
          server = FALSE
        )

        # DELETE rows for this sheet
        observeEvent(
          input[[del_id]],
          {
            df <- bioplex_per_sheet()[[nm_local]]
            del <- bioplex$deleted_by_sheet[[nm_local]] %||% integer(0)
            keep <- setdiff(seq_len(nrow(df)), del)
            sel <- input[[paste0(tbl_id, "_rows_selected")]]
            if (length(sel)) {
              bioplex$deleted_by_sheet[[nm_local]] <- sort(unique(c(
                del,
                keep[sel]
              )))
              # keep Combined in sync while preserving any custom column names
              old_names <- names(bioplex$df)
              bioplex$df <- bioplex_combined_filtered()
              if (
                !is.null(old_names) && length(old_names) == ncol(bioplex$df)
              ) {
                names(bioplex$df) <- old_names
              }
              bioplex$deleted_idx <- integer(0) # reset combined-level deletes (indices changed)
            }
          },
          ignoreInit = TRUE
        )

        # RESTORE all rows for this sheet
        observeEvent(
          input[[rst_id]],
          {
            bioplex$deleted_by_sheet[[nm_local]] <- integer(0)
            old_names <- names(bioplex$df)
            bioplex$df <- bioplex_combined_filtered()
            if (!is.null(old_names) && length(old_names) == ncol(bioplex$df)) {
              names(bioplex$df) <- old_names
            }
            bioplex$deleted_idx <- integer(0)
          },
          ignoreInit = TRUE
        )
      })
      tabPanel(
        nm,
        DT::DTOutput(tbl_id)
      )
    })
  } else {
    list()
  }

  tabsetPanel(
    id = "bioplex_tabs",
    tabPanel(
      "Data to be Imported",
      # (optional) banner to make mode obvious
      if (identical(bioplex$editor_mode, "persisted")) {
        div(
          class = "alert alert-info mb-2",
          "Editing previously saved data."
        )
      },
      tags$label("Column names"),
      tags$div(
        style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:.5rem;",
        lapply(seq_along(names(bioplex$df)), function(i) {
          textInput(
            paste0("bioplex_colname_", i),
            NULL,
            names(bioplex$df)[i],
            width = "100%"
          )
        })
      ),
      actionButton(
        "bioplex_apply_names_modal",
        "Apply Column Names",
        icon = icon("fas fa-check"),
        class = "btn-primary btn-sm mt-2"
      ),
      actionButton(
        "bioplex_set_header",
        "Use Selected Row as Column Names",
        icon = icon("fas fa-check"),
        class = "btn-primary btn-sm mt-2"
      ),
      actionButton(
        "bioplex_add_col",
        "Create New Column",
        icon = icon("fas fa-plus"),
        class = "btn-secondary btn-sm mt-2 ms-2"
      ),
      hr(),
      div(
        class = "d-flex justify-content-between align-items-center mb-2",
        tags$div(
          actionButton(
            "bioplex_modal_delete_selected",
            "Delete Selected Rows",
            icon = icon("trash"),
            class = "btn-danger btn-sm"
          ),
          actionButton(
            "bioplex_modal_restore_all",
            "Restore All",
            icon = icon("undo"),
            class = "btn-secondary btn-sm ms-2"
          )
        )
      ),
      DT::DTOutput("bioplex_modal_table_combined")
    ),
    !!!sheet_tabs
  )
})

output$bioplex_modal_table_combined <- DT::renderDT(
  {
    combined_now <- if (identical(bioplex$editor_mode, "persisted")) {
      req(bioplex$df)
      bioplex$df
    } else {
      bioplex_combined_filtered()
    }
    req(nrow(combined_now) > 0)
    keep <- setdiff(seq_len(nrow(combined_now)), bioplex$deleted_idx)

    # show current names (after any Apply Column Names)
    if (!is.null(bioplex$df) && ncol(bioplex$df) == ncol(combined_now)) {
      names(combined_now) <- names(bioplex$df)
    }

    DT::datatable(
      combined_now[keep, , drop = FALSE],
      selection = "multiple",
      editable = TRUE,
      extensions = "AutoFill",
      options = list(
        scrollX = TRUE,
        scrollY = "55vh",
        paging = FALSE,
        autoWidth = TRUE,
        autoFill = TRUE
      ),
      rownames = FALSE,
      callback = DT::JS(
        "
  var tbl = $(table.table().node());
  var id  = tbl.closest('.datatables').attr('id');   // will be 'bioplex_modal_table_combined'
  table.on('autoFill.dt', function(e, datatable, cells) {
    var out = [];
    for (var i = 0; i < cells.length; ++i) {
      var cells_i = cells[i];
      for (var j = 0; j < cells_i.length; ++j) {
        var c = cells_i[j];
        var value = (c.set === null) ? '' : c.set;  // normalize nulls
        out.push({ row: c.index.row + 1,   // 1-based for R
                   col: c.index.column,    // keep as provided; editData handles it
                   value: value });
      }
    }
    Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out, {priority: 'event'});
    table.rows().invalidate();  // fix column type if needed
  });
"
      )
    )
  }
)
# Helper to ensure column names are unique
.bioplex_unique_name <- function(nm, existing) {
  nm <- trimws(nm)
  if (nm == "") {
    return(NULL)
  }
  if (!(nm %in% existing)) {
    return(nm)
  }
  base <- nm
  k <- 1
  while (paste0(base, "_", k) %in% existing) {
    k <- k + 1
  }
  paste0(base, "_", k)
}

shiny::observeEvent(input$bioplex_add_col, {
  showModal(modalDialog(
    title = "Create New Column",
    easyClose = FALSE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("bioplex_newcol_confirm", "Add", class = "btn-primary")
    ),
    textInput("bioplex_newcol_name", "New column name", value = "")
  ))
})
shiny::observeEvent(input$bioplex_set_header, {
  req(bioplex$df)

  # Rebuild view and map selection -> absolute row
  combined_now <- if (identical(bioplex$editor_mode, "persisted")) {
    bioplex$df
  } else {
    bioplex_combined_filtered()
  }
  keep <- setdiff(seq_len(nrow(combined_now)), bioplex$deleted_idx)

  sel <- input$bioplex_modal_table_combined_rows_selected
  if (length(sel) != 1) {
    showNotification(
      "Please select exactly one row to use as column names.",
      type = "error"
    )
    return()
  }
  abs_row <- keep[sel]

  # Persist view if needed
  if (!identical(bioplex$editor_mode, "persisted")) {
    bioplex$df <- combined_now
    bioplex$editor_mode <- "persisted"
  }

  # Promote the selected row to headers (safe and unique)
  header <- as.character(unlist(bioplex$df[abs_row, , drop = TRUE]))
  header[!nzchar(header)] <- paste0("V", which(!nzchar(header))) # optional fill
  new_names <- make.unique(make.names(header))

  bioplex$df <- bioplex$df[-abs_row, , drop = FALSE]
  names(bioplex$df) <- new_names

  # Sync UI + table
  for (i in seq_along(new_names)) {
    updateTextInput(
      session,
      paste0("bioplex_colname_", i),
      value = new_names[i]
    )
  }
  DT::replaceData(
    DT::dataTableProxy("bioplex_modal_table_combined"),
    bioplex$df,
    resetPaging = FALSE,
    clearSelection = "none"
  )
})


shiny::observeEvent(input$bioplex_newcol_confirm, {
  req(bioplex$df)
  nm <- .bioplex_unique_name(input$bioplex_newcol_name, names(bioplex$df))
  if (is.null(nm)) {
    showNotification(
      "Please enter a non-empty column name.",
      type = "warning"
    )
    return()
  }

  # Add new (character) column to the working dataset
  bioplex$df[[nm]] <- rep(NA_character_, nrow(bioplex$df))
  bioplex$user_columns <- unique(c(bioplex$user_columns, nm))

  # Ensure the Combined tab renders from the working df (so the new column is visible)
  bioplex$editor_mode <- "persisted"

  # Close the small 'name prompt' and immediately reopen the main editor
  removeModal()
  show_bioplex_editor_modal()
})
# Observing autofill
shiny::observeEvent(input$bioplex_modal_table_combined_cells_filled, {
  x <- input$bioplex_modal_table_combined_cells_filled
  req(x)

  # 1) Coerce to data.frame (DT.cellInfo should already be one, but be defensive)
  if (is.list(x) && !is.data.frame(x)) {
    # list of lists -> bind rows
    x <- do.call(
      rbind,
      lapply(x, function(z) as.data.frame(z, stringsAsFactors = FALSE))
    )
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  req(nrow(x) > 0, all(c("row", "col", "value") %in% names(x)))

  # 2) Rebuild the displayed data to map displayed row -> absolute row
  combined_now <- if (identical(bioplex$editor_mode, "persisted")) {
    bioplex$df
  } else {
    bioplex_combined_filtered()
  }
  req(nrow(combined_now) > 0)
  keep <- setdiff(seq_len(nrow(combined_now)), bioplex$deleted_idx)

  # 3) If we're in non-persisted view, persist now so indices/columns align
  if (!identical(bioplex$editor_mode, "persisted")) {
    bioplex$df <- combined_now
    bioplex$editor_mode <- "persisted"
  }

  # 4) Apply each change (note: AutoFill col index is 0-based; add 1 for R)
  for (k in seq_len(nrow(x))) {
    r_disp <- as.integer(x$row[k]) # displayed row (1-based from our JS)
    c_disp0 <- as.integer(x$col[k]) # 0-based DT col index
    val <- x$value[k]

    if (!is.finite(r_disp) || r_disp < 1 || r_disp > length(keep)) {
      next
    }
    abs_r <- keep[r_disp]

    c <- c_disp0 + 1L
    if (c < 1 || c > ncol(bioplex$df)) {
      next
    }
    col_nm <- colnames(bioplex$df)[c]

    if (is.numeric(bioplex$df[[col_nm]])) {
      bioplex$df[[col_nm]][abs_r] <- suppressWarnings(as.numeric(val))
    } else {
      bioplex$df[[col_nm]][abs_r] <- val
    }
  }

  # 5) Push the updated data back to the widget
  DT::replaceData(
    DT::dataTableProxy("bioplex_modal_table_combined"),
    bioplex$df,
    resetPaging = FALSE,
    clearSelection = "none"
  )
})

# Column-name editor (appears once df exists)
output$bioplex_modal_colname_editor <- shiny::renderUI({
  req(bioplex$df)
  tagList(
    tags$label("Column names"),
    tags$div(
      style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:.5rem;",
      lapply(seq_along(names(bioplex$df)), function(i) {
        textInput(
          paste0("bioplex_colname_", i),
          NULL,
          names(bioplex$df)[i],
          width = "100%"
        )
      })
    ),
    actionButton(
      "bioplex_apply_names_modal",
      "Apply Column Names",
      icon = icon("fas fa-check"),
      class = "btn-primary btn-sm mt-2"
    )
  )
})

shiny::observeEvent(input$bioplex_apply_names_modal, {
  req(bioplex$df)
  new_names <- vapply(
    seq_along(bioplex$df),
    function(i) {
      val <- input[[paste0("bioplex_colname_", i)]]
      if (is.null(val) || !nzchar(val)) names(bioplex$df)[i] else val
    },
    character(1)
  )
  new_names <- make.unique(make.names(new_names))
  names(bioplex$df) <- new_names

  # update column names in the read-only per-sheet views where columns overlap
  ps <- bioplex_per_sheet()
  for (nm in names(ps)) {
    common <- intersect(names(ps[[nm]]), names(bioplex$df))
    names(ps[[nm]])[match(common, names(ps[[nm]]))] <- common
  }
})
shiny::observeEvent(input$bioplex_modal_table_combined_cell_edit, {
  info <- input$bioplex_modal_table_combined_cell_edit

  # Reconstruct the currently displayed data (same as in your renderDT)
  combined_now <- if (identical(bioplex$editor_mode, "persisted")) {
    req(bioplex$df)
    bioplex$df
  } else {
    bioplex_combined_filtered()
  }
  req(nrow(combined_now) > 0)

  keep <- setdiff(seq_len(nrow(combined_now)), bioplex$deleted_idx)

  # Map displayed row index -> absolute row index in combined_now
  abs_row <- keep[info$row]
  j <- info$col + 1L
  req(j >= 1, j <= ncol(combined_now))
  colname <- colnames(combined_now)[[j]]
  newval <- info$value

  # Write back into the *working* dataset used by the editor
  # (bioplex$df is the working copy in both modes)
  if (is.numeric(bioplex$df[[colname]])) {
    bioplex$df[[colname]][abs_row] <- suppressWarnings(as.numeric(newval))
  } else {
    bioplex$df[[colname]][abs_row] <- newval
  }
})
show_bioplex_editor_modal <- function() {
  shiny::showModal(
    modalDialog(
      title = "Bio-Plex Editor",
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Close"),
        actionButton(
          "bioplex_confirm_modal",
          "Save & Use",
          class = "btn-primary"
        )
      ),
      uiOutput("bioplex_modal_tabs")
    )
  )
}
show_data_editor_modal <- function(title = "Data Editor") {
  shiny::showModal(
    modalDialog(
      title = title,
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Close"),
        actionButton(
          "bioplex_confirm_modal",
          "Save & Use",
          class = "btn-primary"
        )
      ),
      uiOutput("bioplex_modal_tabs")
    )
  )
}

shiny::observeEvent(input$bioplex_modal_delete_selected, {
  combined_now <- if (identical(bioplex$editor_mode, "persisted")) {
    req(bioplex$df)
    bioplex$df
  } else {
    bioplex_combined_filtered()
  }
  req(nrow(combined_now) > 0)
  sel <- input$bioplex_modal_table_combined_rows_selected
  if (length(sel)) {
    current <- setdiff(seq_len(nrow(combined_now)), bioplex$deleted_idx)
    bioplex$deleted_idx <- sort(unique(c(bioplex$deleted_idx, current[sel])))
  }
})

shiny::observeEvent(input$bioplex_modal_restore_all, {
  bioplex$deleted_idx <- integer(0)
})

# Helper: clean * and OOR for ONE column, using that column's min/max
.clean_bioplex_column <- function(v) {
  if (is.numeric(v)) {
    return(v)
  } # already numeric
  v_chr <- as.character(v)

  # Detect OOR tokens (case-insensitive)
  oor_gt <- grepl("(?i)\\bOOR\\s*>", v_chr, perl = TRUE)
  oor_lt <- grepl("(?i)\\bOOR\\s*<", v_chr, perl = TRUE)

  # Remove asterisks and commas; strip OOR tokens so we can parse numbers
  base <- v_chr
  base <- gsub("\\*", "", base) # strip asterisks
  base <- gsub(",", "", base) # strip thousands sep
  base <- gsub("(?i)\\bOOR\\s*[<>]", "", base, perl = TRUE) # drop OOR tokens before parsing

  # Parse numeric (keep digits, sign, decimal, exponent)
  # If you prefer strict parse: suppressWarnings(as.numeric(base))
  num <- suppressWarnings(as.numeric(gsub("[^0-9eE+\\-\\.]", "", base)))

  # Reference values for min/max (exclude rows that were OOR and failed parse)
  ref <- num[!(oor_gt | oor_lt) & !is.na(num)]
  if (!length(ref)) {
    return(v)
  } # nothing to anchor to; leave as-is

  mn <- min(ref)
  mx <- max(ref)

  # Apply OOR rules
  if (any(oor_gt)) {
    num[oor_gt] <- round(((mx - mn) / 100) + mx, 2)
  }
  if (any(oor_lt)) {
    if (isTRUE(mn > 0)) {
      num[oor_lt] <- round(mn + (mn / 10), 2)
    } else {
      # if min is not positive, leave at mn (explicitly *not* adding 10%)
      num[oor_lt] <- mn
    }
  }
  num
}
# Heuristic: should this column be treated as numeric?
# Heuristic: should this column be treated as numeric?
.is_numeric_like <- function(v) {
  if (is.numeric(v)) {
    return(TRUE)
  }

  v_chr <- as.character(v)

  # tokens like "OOR >", "OOR<", with or without spaces
  oor_token <- grepl("(?i)\\bOOR\\s*[<>]", v_chr, perl = TRUE)

  # pre-clean the obvious noise so we can probe numeric-ness
  base <- gsub("\\*|,", "", v_chr)
  base <- gsub("(?i)\\bOOR\\s*[<>]", "", base, perl = TRUE)

  num <- suppressWarnings(as.numeric(gsub("[^0-9eE+\\-\\.]", "", base)))

  # After removing OOR, check if there are still letters left.
  # This correctly ignores the letters in "OOR" itself.
  v_no_oor <- gsub("(?i)\\bOOR\\b", "", v_chr)
  prop_letters <- mean(grepl("[A-Za-z]", v_no_oor))

  prop_numeric <- mean(!is.na(num) & nzchar(trimws(v_chr)))
  prop_oor <- mean(oor_token) # count OOR as “numeric-like”

  # Numeric-like if (numbers + OOR) dominate and column isn't mostly other text
  isTRUE((prop_numeric + prop_oor) >= 0.7 && prop_letters < 0.4)
}

# Columns to never coerce (common label/meta names; case-insensitive)
.always_categorical <- function(nms) {
  deny <- c(
    "type",
    "well",
    "desc",
    "description",
    "sample",
    "id",
    "group",
    "plate",
    "file",
    "reader",
    "serial",
    "target",
    "matrix",
    "bead",
    "count"
  )
  nms[tolower(nms) %in% deny]
}

# Apply cleaner only to numeric-like columns; keep others as character
bioplex_clean_numeric_only <- function(df) {
  out <- df
  deny <- .always_categorical(names(out))
  for (nm in names(out)) {
    if (nm %in% deny) {
      out[[nm]] <- as.character(out[[nm]])
    } else if (.is_numeric_like(out[[nm]])) {
      out[[nm]] <- .clean_bioplex_column(out[[nm]])
    } else {
      out[[nm]] <- as.character(out[[nm]])
    }
  }
  out
}
# Confirm import (activate for Step 2)
shiny::observeEvent(input$bioplex_confirm_modal, {
  combined_now <- if (identical(bioplex$editor_mode, "persisted")) {
    req(bioplex$df)
    bioplex$df
  } else {
    bioplex_combined_filtered()
  }
  req(nrow(combined_now) > 0)

  # preserve column names if the user renamed them
  if (!is.null(bioplex$df) && ncol(bioplex$df) == ncol(combined_now)) {
    names(combined_now) <- names(bioplex$df)
  }

  # apply any deletions done on the Combined tab itself
  keep <- setdiff(seq_len(nrow(combined_now)), bioplex$deleted_idx)
  req(length(keep) > 0)
  final_raw <- combined_now[keep, , drop = FALSE]

  # Clean asterisks + OOR (your existing helper)
  final <- bioplex_clean_numeric_only(final_raw)

  # Persist & activate
  bioplex$final <- final
  bioplex$active <- TRUE
  bioplex$editor_mode <- "persisted"

  removeModal()
  showNotification(
    "Bio-Plex data cleaned and staged. Click 'Next Step' to continue.",
    type = "message"
  )
})

# REACTIVE & OUTPUT to track if data is loaded
output$data_is_loaded <- shiny::reactive({
  isTRUE(input$use_builtin) || isTRUE(bioplex$active)
})
shiny::outputOptions(output, "data_is_loaded", suspendWhenHidden = FALSE)

# Summary stats
output$data_summary <- shiny::renderUI({
  req(userData())
  df <- userData()
  fluidRow(
    column(4, tags$b("Rows:"), nrow(df)),
    column(4, tags$b("Columns:"), ncol(df)),
    column(
      4,
      tags$b("Missing %:"),
      paste0(
        round(100 * sum(is.na(df)) / (nrow(df) * ncol(df)), 1),
        "%"
      )
    )
  )
})
output$preview_ui <- shiny::renderUI({
  req(input$use_builtin || isTRUE(bioplex$active))
  DT::dataTableOutput("data_preview")
})
output$data_preview <- DT::renderDT(
  {
    # Hide the internal ID column from the user
    df <- userData()
    df$..cyto_id.. <- NULL
    df
  },
  options = list(
    pageLength = 5, # initial page size
    lengthMenu = c(5, 10, 25, 50, 100, nrow(df)),
    scrollX = TRUE,
    scrollY = TRUE
  )
)
output$summary_stats_table <- DT::renderDT({
  req(userData())
  df <- userData()
  df$..cyto_id.. <- NULL # Hide internal ID

  # build a “wide” skim table
  wide <- skimr::skim(df) %>%
    dplyr::select(
      -character.min,
      -character.max,
      -character.empty,
      -character.whitespace
    ) %>%
    dplyr::rename(
      'Variable Type' = skim_type,
      Variable = skim_variable,
      'Number of Missing' = n_missing,
      'Completion Rate' = complete_rate,
      'Number of Unique Levels' = character.n_unique,
      Min = numeric.p0,
      '25th Percentile' = numeric.p25,
      Median = numeric.p50,
      '75th Percentile' = numeric.p75,
      Max = numeric.p100,
      Mean = numeric.mean,
      SD = numeric.sd,
      Histogram = numeric.hist
    ) %>%
    dplyr::rename_with(
      ~ tools::toTitleCase(gsub("\\.", " ", .x)),
      .cols = dplyr::everything()
    )

  DT::datatable(
    wide,
    rownames = FALSE,
    options = list(
      pageLength = 5, # show 5 rows per page by default
      lengthMenu = c(5, 10, 25, 50, 100, nrow(df)),
      scrollX = TRUE
    )
  )
})
# Simple validations & warnings
shiny::observeEvent(userData(), {
  # Reset deleted rows when new data is loaded
  userState$deleted_row_ids <- NULL

  df <- userData()
  # Example: if no numeric column, warn
  if (all(!sapply(df, is.numeric))) {
    feedbackWarning(
      "datafile",
      TRUE,
      "No numeric columns found. Some analyses require numeric data."
    )
  } else {
    hideFeedback("datafile")
  }

  # Example: too many missing
  pct_missing <- sum(is.na(df)) / (nrow(df) * ncol(df))
  if (pct_missing > 0.05) {
    feedbackWarning(
      "datafile",
      TRUE,
      "Over 5% of cells are missing."
    )
  }
})
