app_stage_init <- function(app_ctx, stage_env = parent.frame()) {
  if (!identical(parent.env(stage_env), app_ctx)) {
    parent.env(stage_env) <- app_ctx
  }
  invisible(stage_env)
}

app_stage_commit <- function(
  app_ctx,
  stage_env = parent.frame(),
  exclude = c("input", "output", "session", "app_ctx", "stage_env")
) {
  logic_names <- setdiff(ls(stage_env, all.names = TRUE), exclude)
  for (nm in logic_names) {
    assign(
      nm,
      base::get(nm, envir = stage_env, inherits = FALSE),
      envir = app_ctx
    )
  }
  invisible(app_ctx)
}

init_theme_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  ## ---------------------------
  ## Theme Toggle (per-session, per-browser via localStorage)
  ## ---------------------------

  base_theme <- bslib::bs_theme(
    base_font = bslib::font_google("Inter"),
    code_font = bslib::font_google("Roboto Mono")
  )

  system_theme <- shiny::reactive({
    if (is.null(input$system_theme)) "flatly" else input$system_theme
  })

  theme_choice_rv <- shiny::reactiveVal("auto")

  # IMPORTANT: do NOT ignore init; we want the initial value on refresh
  shiny::observeEvent(
    input$theme_choice,
    {
      shiny::req(input$theme_choice)
      theme_choice_rv(input$theme_choice)

      # persist per-browser
      shinyjs::runjs(sprintf(
        "localStorage.setItem('user_theme', '%s');",
        input$theme_choice
      ))
    },
    ignoreInit = FALSE
  )

  # Apply theme whenever either theme_choice_rv() or system_theme() changes
  shiny::observe({
    choice <- theme_choice_rv()
    bootswatch <- if (identical(choice, "auto")) system_theme() else choice

    session$setCurrentTheme(
      bslib::bs_theme_update(base_theme, bootswatch = bootswatch)
    )
  })

  invisible(app_stage_commit(app_ctx, stage_env))
}

init_wizard_step_control_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  ## ---------------------------
  ## Wizard Step Control
  ## ---------------------------
  currentStep <- shiny::reactiveVal(1)
  output$currentStep <- shiny::reactive({
    currentStep()
  })
  shiny::outputOptions(output, "currentStep", suspendWhenHidden = FALSE)

  invisible(app_stage_commit(app_ctx, stage_env))
}

init_persistent_state_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  ## ---------------------------
  ## Persistent State: Create a shiny::reactiveValues object
  ## ---------------------------
  selected_function <- shiny::reactiveVal(NULL)
  deleted_row_ids <- shiny::reactiveVal(character())
  imputed_data <- shiny::reactiveVal(NULL)

  userState <- shiny::reactiveValues(
    # General state
    selected_columns = NULL,
    selected_categorical_cols = NULL,
    selected_numerical_cols = NULL,
    # Built-in data tracking:
    use_builtin = FALSE,
    built_in_choice = NULL,

    # Bioplex sheet tracking
    sheet_name = NULL,
    # Imputation
    impute_meta = NULL,

    # Step 2 preprocessing
    step2_scale = "none",
    step2_factor_cols = NULL,
    step2_numeric_override_cols = NULL,
    step2_factor_order_enable = FALSE,
    step2_factor_order_col = NULL,
    step2_factor_levels_csv = NULL,
    step2_applied_factor_cols = NULL,
    step2_applied_numeric_override_cols = NULL,
    step2_applied_factor_order_enable = FALSE,
    step2_applied_factor_order_col = NULL,
    step2_applied_factor_levels_csv = NULL,

    # Boxplots options
    bp_bin_size = NULL,
    bp_group_by = NULL,
    bp_y_lim = NULL,
    bp_font_settings = NULL,

    # Violin Plot options
    vio_group_by = NULL,
    vio_bin_size = NULL,
    vio_y_lim = NULL,
    vio_boxplot_overlay = NULL,
    vio_font_settings = NULL,

    # Univariate Tests (T-test, Wilcoxon)
    uv2_method = NULL,
    uv2_p_adjust_method = NULL,

    # Univariate Tests (ANOVA, Kruskal-Wallis)
    uvm_method = NULL,
    uvm_p_adjust_method = NULL,
    twa_primary_cat_var = NULL,
    twa_secondary_cat_var = NULL,
    twa_include_primary_secondary_interaction = NULL,
    anc_primary_cat_var = NULL,
    anc_secondary_cat_var = NULL,
    anc_covariate_col = NULL,
    anc_include_primary_secondary_interaction = NULL,
    anc_include_primary_covariate_interaction = NULL,
    anc_include_secondary_covariate_interaction = NULL,

    # Error-Bar Plot
    eb_group_col = NULL,
    eb_p_lab = NULL,
    eb_es_lab = NULL,
    eb_class_symbol = NULL,
    eb_x_lab = NULL,
    eb_y_lab = NULL,
    eb_title = NULL,
    eb_stat = NULL,
    eb_error = NULL,
    eb_method = NULL,
    eb_p_adjust_method = NULL,
    eb_n_col = NULL,
    eb_fill_palette = NULL,
    eb_font_settings = NULL,

    # Dual-Flashlight Plot options
    df_group_var = NULL,
    df_cond1 = NULL,
    df_cond2 = NULL,
    df_ssmd_thresh = NULL,
    df_log2fc_thresh = NULL,
    df_top_labels = NULL,
    df_font_settings = NULL,

    # Heatmap options
    hm_annotation = NULL,
    hm_ann_side = NULL,
    hm_font_settings = NULL,

    # PCA options
    pca_group_col = NULL,
    pca_group_col2 = NULL,
    pca_comp_num = NULL,
    pca_ellipse = NULL,
    pca_style = NULL,
    pca_pch = NULL,
    pca_colors = NULL,
    pca_font_settings = NULL,

    # Random Forest options
    rf_group_col = NULL,
    rf_ntree = NULL,
    rf_mtry = NULL,
    rf_train_fraction = NULL,
    rf_plot_roc = NULL,
    rf_run_rfcv = NULL,
    rf_k_folds = NULL,
    rf_step = NULL,
    rf_font_settings = NULL,

    # PLSR options
    plsr_group_col = NULL,
    plsr_response_col = NULL,
    plsr_predictor_cols = NULL,
    plsr_comp_num = NULL,
    plsr_keepX = NULL,
    plsr_keepX_manual = FALSE,
    plsr_sparse = FALSE,
    plsr_cv_opt = NULL,
    plsr_fold_num = NULL,
    plsr_ellipse = NULL,
    plsr_colors = NULL,
    plsr_font_settings = NULL,

    # Skewness/Kurtosis options
    skku_group_cols = NULL,
    skku_print_raw = NULL,
    skku_print_log = NULL,
    skku_font_settings = NULL,

    # sPLS-DA options
    splsda_group_col = NULL,
    splsda_group_col2 = NULL,
    splsda_batch_col = NULL,
    splsda_var_num = NULL,
    splsda_var_num_manual = FALSE,
    splsda_cv_opt = NULL,
    splsda_fold_num = NULL,
    splsda_comp_num = NULL,
    splsda_pch = NULL,
    splsda_ind_names_mode = NULL,
    splsda_ind_names_col = NULL,
    splsda_style = NULL,
    splsda_roc = NULL,
    splsda_ellipse = NULL,
    splsda_bg = NULL,
    splsda_conf_mat = NULL,
    splsda_colors = NULL,
    splsda_multilevel = NULL,
    splsda_use_batch_corr = FALSE,
    splsda_batch_col = NULL,
    splsda_font_settings = NULL,

    # MINT sPLS-DA options
    mint_splsda_group_col = NULL,
    mint_splsda_group_col2 = NULL,
    mint_splsda_batch_col = NULL,
    mint_splsda_var_num = NULL,
    mint_splsda_var_num_manual = FALSE,
    mint_splsda_comp_num = NULL,
    mint_splsda_cim = NULL,
    mint_splsda_ellipse = NULL,
    mint_splsda_bg = NULL,
    mint_splsda_roc = NULL,
    mint_splsda_colors = NULL,
    mint_splsda_font_settings = NULL,

    # Volcano Plot options
    volc_group_col = NULL,
    volc_cond1 = NULL,
    volc_cond2 = NULL,
    volc_fold_change_thresh = NULL,
    volc_p_value_thresh = NULL,
    volc_top_labels = NULL,
    volc_font_settings = NULL,

    # XGBoost options
    xgb_group_col = NULL,
    xgb_train_fraction = NULL,
    xgb_nrounds = NULL,
    xgb_max_depth = NULL,
    xgb_eta = NULL,
    xgb_nfold = NULL,
    xgb_cv = NULL,
    xgb_eval_metric = NULL,
    xgb_top_n_features = NULL,
    xgb_plot_roc = NULL,
    xgb_font_settings = NULL,

    # Correlation options
    corr_target = NULL,
    corr_group_col = NULL,
    corr_by_group = FALSE,
    corr_font_settings = NULL
  )
  # --- Bio-Plex import state ---
  bioplex <- shiny::reactiveValues(
    active = FALSE, # when TRUE, userData() will use the confirmed Bio-Plex data
    df = NULL, # working table (while editing)
    final = NULL, # persisted dataset after "Save & Use"
    editor_mode = "sheets", # "sheets" or "persisted"
    deleted_idx = integer(),
    deleted_by_sheet = list(),
    user_columns = character(0)
  )

  # Reactive values to store the selection from our new custom buttons
  selected_stat_func <- shiny::reactiveVal(
    "Univariate Tests (T-test, Wilcoxon)"
  )
  selected_exploratory_func <- shiny::reactiveVal("Boxplots")
  selected_multivariate_func <- shiny::reactiveVal(
    "Principal Component Analysis (PCA)"
  )
  selected_ml_func <- shiny::reactiveVal("Random Forest")

  # Has the development notice modal been shown this session?
  dev_notice_shown <- shiny::reactiveVal(FALSE)

  invisible(app_stage_commit(app_ctx, stage_env))
}

init_data_handling_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  userState <- app_ctx$userState
  bioplex <- app_ctx$bioplex
  builtins_dir <- app_ctx$builtins_dir
  upload_dir <- app_ctx$upload_dir
  builtInList <- app_ctx$builtInList
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

  # remember which built-in data they picked
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
      factor_cols <- shiny::isolate(input$factor_cols) %||% character(0)
      numeric_cols <- shiny::isolate(input$numeric_override_cols) %||%
        character(0)
      factor_order_enable <- isTRUE(shiny::isolate(input$factor_order_enable))
      factor_order_col <- shiny::isolate(input$factor_order_col)
      factor_levels_csv <- shiny::isolate(input$factor_levels_csv) %||% ""
      overlap <- step2_conflicting_type_cols(factor_cols, numeric_cols)

      if (length(overlap)) {
        shiny::showNotification(
          paste(
            "Choose only one type override per column.",
            "Remove these overlaps:",
            paste(overlap, collapse = ", ")
          ),
          type = "error"
        )
        return()
      }

      userState$step2_factor_cols <- factor_cols
      userState$step2_numeric_override_cols <- numeric_cols
      userState$step2_factor_order_enable <- factor_order_enable
      userState$step2_factor_order_col <- factor_order_col
      userState$step2_factor_levels_csv <- factor_levels_csv
      userState$step2_applied_factor_cols <- factor_cols
      userState$step2_applied_numeric_override_cols <- numeric_cols
      userState$step2_applied_factor_order_enable <- factor_order_enable
      userState$step2_applied_factor_order_col <- factor_order_col
      userState$step2_applied_factor_levels_csv <- factor_levels_csv
      apply_stamp(apply_stamp() + 1)
    },
    ignoreInit = TRUE
  )

  # cache for loaded Excel sheets
  excel_cache <- shiny::reactiveVal(list())

  # If the user uploads a new main data file or a new bioplex file, clear any
  # previously 'saved' Bio-Plex dataset so the editor will read the newly
  # uploaded file (instead of returning the persisted `bioplex$final`).
  shiny::observeEvent(
    input$datafile,
    {
      # New upload -> clear persisted bioplex state so editor uses uploaded file
      bioplex$active <- FALSE
      bioplex$final <- NULL
      bioplex$df <- NULL
      bioplex$editor_mode <- "sheets"
      bioplex$deleted_idx <- integer(0)
      bioplex$deleted_by_sheet <- list()
      bioplex$user_columns <- character(0)
      # Also clear any excel cache entries related to this datapath so sheets re-read
      excel_cache(list())

      # Clear stored sheet selection (prevents mismatched sheet names when
      # uploading a different workbook that doesn't contain the previously
      # selected sheet). Also clear the saved userState sheet name so the UI
      # doesn't re-populate with an invalid value.
      userState$sheet_name <- NULL
      userState$selected_columns <- NULL
      userState$selected_categorical_cols <- NULL
      userState$selected_numerical_cols <- NULL
      userState$step2_scale <- "none"
      userState$step2_factor_cols <- NULL
      userState$step2_numeric_override_cols <- NULL
      userState$step2_factor_order_enable <- FALSE
      userState$step2_factor_order_col <- NULL
      userState$step2_factor_levels_csv <- NULL
      userState$step2_applied_factor_cols <- NULL
      userState$step2_applied_numeric_override_cols <- NULL
      userState$step2_applied_factor_order_enable <- FALSE
      userState$step2_applied_factor_order_col <- NULL
      userState$step2_applied_factor_levels_csv <- NULL
      try(
        shiny::updateSelectizeInput(
          session,
          "sheet_name",
          selected = character(0)
        ),
        silent = TRUE
      )

      # Notify the user that previous editor state was cleared due to new upload
      shiny::showNotification(
        "New upload detected - previous data-editor state cleared.",
        type = "message",
        duration = 4
      )
    },
    ignoreNULL = TRUE
  )

  shiny::observeEvent(
    input$bioplex_file,
    {
      bioplex$active <- FALSE
      bioplex$final <- NULL
      bioplex$df <- NULL
      bioplex$editor_mode <- "sheets"
      bioplex$deleted_idx <- integer(0)
      bioplex$deleted_by_sheet <- list()
      bioplex$user_columns <- character(0)
      excel_cache(list())
      userState$sheet_name <- NULL
      userState$selected_columns <- NULL
      userState$selected_categorical_cols <- NULL
      userState$selected_numerical_cols <- NULL
      userState$step2_scale <- "none"
      userState$step2_factor_cols <- NULL
      userState$step2_numeric_override_cols <- NULL
      userState$step2_factor_order_enable <- FALSE
      userState$step2_factor_order_col <- NULL
      userState$step2_factor_levels_csv <- NULL
      userState$step2_applied_factor_cols <- NULL
      userState$step2_applied_numeric_override_cols <- NULL
      userState$step2_applied_factor_order_enable <- FALSE
      userState$step2_applied_factor_order_col <- NULL
      userState$step2_applied_factor_levels_csv <- NULL
      try(
        shiny::updateSelectizeInput(
          session,
          "sheet_name",
          selected = character(0)
        ),
        silent = TRUE
      )
      shiny::showNotification(
        "New Bioplex file detected - previous data-editor state cleared.",
        type = "message",
        duration = 4
      )
    },
    ignoreNULL = TRUE
  )

  userData <- shiny::reactive({
    if (isTRUE(bioplex$active) && !is.null(bioplex$final)) {
      df <- bioplex$final
    } else if (isTRUE(input$use_builtin)) {
      shiny::req(input$built_in_choice)
      df <- app_builtin_dataset(input$built_in_choice)
      dest <- file.path(builtins_dir, paste0(input$built_in_choice, ".rds"))
      if (!file.exists(dest)) saveRDS(df, dest)
    } else {
      shiny::req(input$datafile)
      safe_name <- basename(input$datafile$name)
      ext <- tolower(tools::file_ext(safe_name))
      if (ext %in% c("csv", "txt")) {
        df <- read_uploaded_flat_file(input$datafile$datapath, ext)
      } else if (ext %in% c("xls", "xlsx")) {
        dest <- file.path(upload_dir, safe_name)
        # Always copy Excel uploads into our upload dir, overwriting previous
        # copies with the same filename so re-uploading a wrong file refreshes
        # the stored file immediately.
        file.copy(input$datafile$datapath, dest, overwrite = TRUE)
        all_sheets <- tryCatch(readxl::excel_sheets(dest), error = function(e) {
          character()
        })
        if (length(all_sheets) == 0) {
          stop("No sheets found in uploaded Excel file.")
        }
        # Safely choose the requested sheet(s). If the previously selected
        # sheet(s) don't exist in this workbook, fall back to the first sheet.
        sheet_to_read <- NULL
        if (!is.null(input$sheet_name) && length(input$sheet_name) > 0) {
          valid <- intersect(input$sheet_name, all_sheets)
          if (length(valid) >= 1) sheet_to_read <- valid[1]
        }
        if (is.null(sheet_to_read)) {
          sheet_to_read <- all_sheets[1]
        }
        df <- readxl::read_excel(dest, sheet = sheet_to_read) |>
          as.data.frame()
      } else {
        stop("Unsupported file type.")
      }
    }
    df$..cyto_id.. <- 1:nrow(df)

    # Coerce character columns that are actually numeric-like back to numeric.
    # This prevents columns containing NA tokens or minor noise from being
    # treated as character and breaking downstream numeric-only flows.
    for (nm in names(df)) {
      if (!is.factor(df[[nm]]) && !is.numeric(df[[nm]])) {
        # Never coerce obvious metadata/label columns
        if (nm %in% .always_categorical(names(df))) {
          df[[nm]] <- as.character(df[[nm]])
          next
        }

        # Only coerce when the column looks numeric-like by heuristic.
        # This avoids turning categorical codes (e.g. "1","2") or other
        # mostly-text columns into numeric types unintentionally.
        if (.is_numeric_like(df[[nm]])) {
          cleaned <- tryCatch(
            .clean_bioplex_column(df[[nm]]),
            error = function(e) {
              step2_parse_numeric_values(df[[nm]])
            }
          )
          if (is.numeric(cleaned)) {
            df[[nm]] <- as.numeric(cleaned)
          } else {
            df[[nm]] <- as.character(df[[nm]])
          }
        } else {
          df[[nm]] <- as.character(df[[nm]])
        }
      }
    }
    df
  })

  step2_applied_type_state <- shiny::reactive({
    apply_stamp()

    list(
      factor_cols = userState$step2_applied_factor_cols %||% character(0),
      numeric_cols = userState$step2_applied_numeric_override_cols %||%
        character(0),
      factor_order_enable = isTRUE(userState$step2_applied_factor_order_enable),
      factor_order_col = userState$step2_applied_factor_order_col,
      factor_levels_csv = userState$step2_applied_factor_levels_csv %||% ""
    )
  })

  step2_typed_data <- shiny::reactive({
    df <- userData()
    state <- step2_applied_type_state()

    step2_apply_type_overrides(
      df = df,
      factor_cols = state$factor_cols,
      numeric_cols = state$numeric_cols,
      factor_order_enable = state$factor_order_enable,
      factor_order_col = state$factor_order_col,
      factor_levels_csv = state$factor_levels_csv
    )
  })

  step2_typed_col_info <- shiny::reactive({
    step2_classify_columns(step2_typed_data())
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
    shiny::req(input$datafile)
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

    shiny::selectizeInput(
      inputId = "sheet_name",
      label = "Select the desired sheets:",
      multiple = TRUE,
      choices = sheets,
      options = list(
        placeholder = "Select sheets..."
      ),
      selected = shiny::isolate(userState$sheet_name) %||% sheets[1]
    )
  })

  output$built_in_selector <- shiny::renderUI({
    if (!isTRUE(input$use_builtin)) {
      return(NULL)
    }

    # Use radioButtons to display all options inside the card
    shiny::radioButtons(
      inputId = "built_in_choice",
      label = "Select a built-in dataset:",
      choices = builtInList,
      selected = shiny::isolate(userState$built_in_choice) %||% builtInList[1]
    )
  })
  # ----- Bio-Plex: multi-sheet picker -----
  # flag for conditionalPanel
  output$bioplex_on <- shiny::reactive({
    shiny::isTruthy(input$bioplex_file) &&
      shiny::isTruthy(input$bioplex_file$datapath)
  })
  shiny::outputOptions(output, "bioplex_on", suspendWhenHidden = FALSE)

  shiny::observeEvent(input$bioplex_file, {
    # only proceed for Excel uploads
    shiny::req(shiny::isTruthy(input$bioplex_file$datapath))
    shiny::req(grepl("\\.xlsx?$", input$bioplex_file$name, ignore.case = TRUE))

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
    shiny::req(input$bioplex_file)
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
    shiny::req(shiny::isTruthy(input$bioplex_file$datapath))
    shiny::req(length(input$bioplex_sheets) >= 1) # at least one sheet picked

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
    shiny::req(length(dfs) >= 1) # ensure at least one non-empty sheet
    dplyr::bind_rows(dfs) # bind by names, fill NAs as needed
  })

  # Keep working copy in reactiveValues so we can rename columns & delete rows before import
  shiny::observeEvent(
    bioplex_build_df(),
    {
      shiny::req(bioplex_build_df())
      bioplex$df <- bioplex_build_df()
      bioplex$deleted_idx <- integer(0)
      bioplex$active <- FALSE
    },
    ignoreInit = TRUE
  )
  # Per-sheet data frames (row 1 -> column names)
  bioplex_per_sheet <- shiny::reactive({
    if (shiny::isTruthy(input$datafile) && length(input$sheet_name) >= 1) {
      path <- input$datafile$datapath
      # Ensure we only attempt to read sheets that exist in this workbook
      all_sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) {
        character()
      })
      if (length(all_sheets) == 0) {
        return(NULL)
      }
      sh <- intersect(input$sheet_name, all_sheets)
      if (!length(sh)) sh <- all_sheets[1]
    } else if (
      shiny::isTruthy(input$bioplex_file) && length(input$bioplex_sheets) >= 1
    ) {
      path <- input$bioplex_file$datapath
      all_sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) {
        character()
      })
      if (length(all_sheets) == 0) {
        return(NULL)
      }
      sh <- intersect(input$bioplex_sheets, all_sheets)
      if (!length(sh)) sh <- all_sheets[1]
    } else {
      return(NULL)
    }

    cache <- excel_cache()
    result <- list()
    for (s in sh) {
      key <- paste(path, s, sep = "::")
      if (!is.null(cache[[key]])) {
        # use cached version
        result[[s]] <- cache[[key]]
      } else {
        x <- readxl::read_excel(path, sheet = s, col_names = FALSE)
        if (nrow(x) > 0) {
          hdr <- as.character(unlist(x[1, ], use.names = FALSE))
          x <- x[-1, , drop = FALSE]
          names(x) <- make.unique(make.names(hdr))
          result[[s]] <- x
          cache[[key]] <- x # update cache
        }
      }
    }
    excel_cache(cache) # save updated cache
    Filter(Negate(is.null), result)
  })

  # Combined
  bioplex_combined <- shiny::reactive({
    shiny::req(bioplex_per_sheet())
    dplyr::bind_rows(bioplex_per_sheet())
  })
  bioplex_combined_filtered <- shiny::reactive({
    ps <- bioplex_per_sheet()
    shiny::req(length(ps) >= 1)
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
    shiny::req(input$datafile) # waits until a file is selected/uploaded
    shiny::div(
      class = "mt-2",
      shiny::actionButton(
        "open_editor",
        "Open Data Editor",
        class = "btn-primary btn-sm ms-2"
      )
    )
  })
  shiny::observeEvent(input$open_editor, {
    if (isTRUE(bioplex$active) && !is.null(bioplex$final)) {
      bioplex$editor_mode <- "persisted"
      bioplex$df <- bioplex$final
      bioplex$deleted_idx <- integer(0)
      bioplex$cols_master <- NULL
      show_data_editor_modal()
      return() # <- important: don't run the file-reading path
    }
    shiny::req(input$datafile)
    safe_name <- basename(input$datafile$name)
    ext <- tolower(tools::file_ext(safe_name))

    if (ext %in% c("xls", "xlsx") && length(input$sheet_name) >= 1) {
      # SHEETS MODE a?' tabs appear
      dest <- file.path(upload_dir, safe_name)
      # Always overwrite the stored Excel file when opening the editor so that
      # if the user re-uploads a workbook with the same name the latest upload
      # is used for downstream persisted reads.
      file.copy(input$datafile$datapath, dest, overwrite = TRUE)
      bioplex$editor_mode <- "sheets"
      ps <- bioplex_per_sheet() # now points at Option A inputs
      shiny::req(length(ps) >= 1)
      bioplex$deleted_by_sheet <- setNames(
        replicate(length(ps), integer(0), simplify = FALSE),
        names(ps)
      )
      bioplex$df <- dplyr::bind_rows(ps) # combined working table (no .id column)
    } else {
      # PERSISTED MODE for csv/txt or single-sheet Excel
      df <- switch(
        ext,
        "csv" = read_uploaded_flat_file(input$datafile$datapath, ext),
        "txt" = read_uploaded_flat_file(input$datafile$datapath, ext),
        "xls" = {
          dest <- file.path(upload_dir, safe_name)
          file.copy(input$datafile$datapath, dest, overwrite = TRUE)
          all_sheets <- tryCatch(
            readxl::excel_sheets(dest),
            error = function(e) {
              character()
            }
          )
          sheet_choice <- NULL
          if (!is.null(input$sheet_name) && length(input$sheet_name) > 0) {
            valid <- intersect(input$sheet_name, all_sheets)
            if (length(valid) >= 1) sheet_choice <- valid[1]
          }
          if (is.null(sheet_choice) || length(all_sheets) == 0) {
            sheet_choice <- 1L
          }
          as.data.frame(readxl::read_excel(dest, sheet = sheet_choice))
        },
        "xlsx" = {
          dest <- file.path(upload_dir, safe_name)
          file.copy(input$datafile$datapath, dest, overwrite = TRUE)
          all_sheets <- tryCatch(
            readxl::excel_sheets(dest),
            error = function(e) {
              character()
            }
          )
          sheet_choice <- NULL
          if (!is.null(input$sheet_name) && length(input$sheet_name) > 0) {
            valid <- intersect(input$sheet_name, all_sheets)
            if (length(valid) >= 1) sheet_choice <- valid[1]
          }
          if (is.null(sheet_choice) || length(all_sheets) == 0) {
            sheet_choice <- 1L
          }
          as.data.frame(readxl::read_excel(dest, sheet = sheet_choice))
        },
        stop("Unsupported file type.")
      )
      bioplex$editor_mode <- "persisted"
      bioplex$df <- df
    }

    bioplex$deleted_idx <- integer(0)
    show_data_editor_modal()
  })

  output$bioplex_modal_tabs <- shiny::renderUI({
    shiny::req(bioplex$df)

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
          shiny::observeEvent(
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
          shiny::observeEvent(
            input[[rst_id]],
            {
              bioplex$deleted_by_sheet[[nm_local]] <- integer(0)
              old_names <- names(bioplex$df)
              bioplex$df <- bioplex_combined_filtered()
              if (
                !is.null(old_names) && length(old_names) == ncol(bioplex$df)
              ) {
                names(bioplex$df) <- old_names
              }
              bioplex$deleted_idx <- integer(0)
            },
            ignoreInit = TRUE
          )
        })
        shiny::tabPanel(
          nm,
          DT::DTOutput(tbl_id)
        )
      })
    } else {
      list()
    }

    shiny::tabsetPanel(
      id = "bioplex_tabs",
      shiny::tabPanel(
        "Data to be Imported",
        # (optional) banner to make mode obvious
        if (identical(bioplex$editor_mode, "persisted")) {
          shiny::div(
            class = "alert alert-info mb-2",
            "Editing previously saved data."
          )
        },
        shiny::tags$label("Column names"),
        shiny::tags$div(
          style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:.5rem;",
          lapply(seq_along(names(bioplex$df)), function(i) {
            shiny::textInput(
              paste0("bioplex_colname_", i),
              NULL,
              names(bioplex$df)[i],
              width = "100%"
            )
          })
        ),
        shiny::actionButton(
          "bioplex_apply_names_modal",
          "Apply Column Names",
          icon = shiny::icon("fas fa-check"),
          class = "btn-primary btn-sm mt-2"
        ),
        shiny::actionButton(
          "bioplex_set_header",
          "Use Selected Row as Column Names",
          icon = shiny::icon("fas fa-check"),
          class = "btn-primary btn-sm mt-2"
        ),
        shiny::actionButton(
          "bioplex_add_col",
          "Create New Column",
          icon = shiny::icon("fas fa-plus"),
          class = "btn-secondary btn-sm mt-2 ms-2"
        ),
        shiny::hr(),
        shiny::div(
          class = "d-flex justify-content-between align-items-center mb-2",
          shiny::tags$div(
            shiny::actionButton(
              "bioplex_modal_delete_selected",
              "Delete Selected Rows",
              icon = shiny::icon("trash"),
              class = "btn-danger btn-sm"
            ),
            shiny::actionButton(
              "bioplex_modal_restore_all",
              "Restore All",
              icon = shiny::icon("undo"),
              class = "btn-secondary btn-sm ms-2"
            ),
            shiny::actionButton(
              "bioplex_modal_delete_cols",
              "Delete Selected Columns",
              icon = shiny::icon("trash"),
              class = "btn-danger btn-sm ms-2"
            ),
            shiny::actionButton(
              "bioplex_modal_restore_cols",
              "Restore All Columns",
              icon = shiny::icon("undo"),
              class = "btn-secondary btn-sm ms-2"
            )
          )
        ),
        DT::DTOutput("bioplex_modal_table_combined")
      ),
      !!!sheet_tabs
    )
  })

  output$bioplex_modal_table_combined <- DT::renderDT({
    shiny::req(bioplex$df)
    df <- bioplex$df
    keep <- setdiff(seq_len(nrow(df)), bioplex$deleted_idx)

    # build a container with thead + tfoot so footer clicks select columns
    cols <- names(df)
    sketch <- htmltools::tags$table(
      class = "display nowrap",
      htmltools::tags$thead(
        htmltools::tags$tr(lapply(cols, htmltools::tags$th))
      ),
      htmltools::tags$tfoot(
        htmltools::tags$tr(lapply(cols, htmltools::tags$th))
      )
    )

    DT::datatable(
      df[keep, , drop = FALSE],
      container = sketch,
      selection = list(mode = "multiple", target = "row+column"),
      editable = TRUE,
      extensions = c("Buttons", "AutoFill"),
      options = list(
        dom = "Bfrtip",
        buttons = list("colvis"),
        scrollX = TRUE,
        scrollY = "55vh",
        paging = FALSE,
        autoWidth = TRUE,
        autoFill = TRUE
      ),
      rownames = FALSE,
      callback = DT::JS(
        "
      var $container = $(table.table().container());
      var id   = $container.closest('.datatables').attr('id');
      var selectedDataIdx = [];

      function redrawHighlights(){
        $(table.cells().nodes()).removeClass('col-selected');
        $container.find('.dataTables_scrollHeadInner th').removeClass('col-selected');
        $container.find('.dataTables_scrollFootInner th').removeClass('col-selected');
        selectedDataIdx.forEach(function(di){
          $(table.cells(null, di).nodes()).addClass('col-selected');
          var vis = table.column(di).index('visible');
          if (vis != null && vis >= 0){
            $container.find('.dataTables_scrollHeadInner th').eq(vis).addClass('col-selected');
            $container.find('.dataTables_scrollFootInner th').eq(vis).addClass('col-selected');
          }
        });
      }

      // Footer click toggles column (DT: row+column uses footer)
      $container.on('click', 'div.dataTables_scrollFootInner th', function(){
        var visIdx  = $(this).index();
        var dataIdx = table.column.index('fromVisible', visIdx); // convert visible -> data
        if (dataIdx == null) return;
        var pos = selectedDataIdx.indexOf(dataIdx);
        if (pos === -1) selectedDataIdx.push(dataIdx); else selectedDataIdx.splice(pos,1);
        Shiny.setInputValue(id + '_columns_selected_data', selectedDataIdx, {priority:'event'});
        redrawHighlights();
      });

      table.on('draw.dt column-visibility.dt', function(){ redrawHighlights(); });

      // your existing AutoFill handler (unchanged)
      table.on('autoFill.dt', function(e, datatable, cells) {
        var out = [];
        for (var i = 0; i < cells.length; ++i) {
          var cells_i = cells[i];
          for (var j = 0; j < cells_i.length; ++j) {
            var c = cells_i[j];
            var value = (c.set === null) ? '' : c.set;
            out.push({ row: c.index.row + 1, col: c.index.column, value: value });
          }
        }
        Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out, {priority: 'event'});
        table.rows().invalidate();
      });
    "
      )
    )
  })

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
    shiny::showModal(shiny::modalDialog(
      title = "Create New Column",
      easyClose = FALSE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(
          "bioplex_newcol_confirm",
          "Add",
          class = "btn-primary"
        )
      ),
      shiny::textInput("bioplex_newcol_name", "New column name", value = "")
    ))
  })
  shiny::observeEvent(input$bioplex_set_header, {
    shiny::req(bioplex$df)

    # Rebuild view and map selection -> absolute row
    combined_now <- if (identical(bioplex$editor_mode, "persisted")) {
      bioplex$df
    } else {
      bioplex_combined_filtered()
    }
    keep <- setdiff(seq_len(nrow(combined_now)), bioplex$deleted_idx)

    sel <- input$bioplex_modal_table_combined_rows_selected
    if (length(sel) != 1) {
      shiny::showNotification(
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
      shiny::updateTextInput(
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
    shiny::req(bioplex$df)
    nm <- .bioplex_unique_name(input$bioplex_newcol_name, names(bioplex$df))
    if (is.null(nm)) {
      shiny::showNotification(
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
    shiny::removeModal()
    show_bioplex_editor_modal()
  })
  # Observing autofill
  shiny::observeEvent(input$bioplex_modal_table_combined_cells_filled, {
    x <- input$bioplex_modal_table_combined_cells_filled
    shiny::req(x)

    # 1) Coerce to data.frame (DT.cellInfo should already be one, but be defensive)
    if (is.list(x) && !is.data.frame(x)) {
      # list of lists -> bind rows
      x <- do.call(
        rbind,
        lapply(x, function(z) as.data.frame(z, stringsAsFactors = FALSE))
      )
    }
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    shiny::req(nrow(x) > 0, all(c("row", "col", "value") %in% names(x)))

    # 2) Rebuild the displayed data to map displayed row -> absolute row
    combined_now <- if (identical(bioplex$editor_mode, "persisted")) {
      bioplex$df
    } else {
      bioplex_combined_filtered()
    }
    shiny::req(nrow(combined_now) > 0)
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
    shiny::req(bioplex$df)
    shiny::tagList(
      shiny::tags$label("Column names"),
      shiny::tags$div(
        style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:.5rem;",
        lapply(seq_along(names(bioplex$df)), function(i) {
          shiny::textInput(
            paste0("bioplex_colname_", i),
            NULL,
            names(bioplex$df)[i],
            width = "100%"
          )
        })
      ),
      shiny::actionButton(
        "bioplex_apply_names_modal",
        "Apply Column Names",
        icon = shiny::icon("fas fa-check"),
        class = "btn-primary btn-sm mt-2"
      )
    )
  })

  shiny::observeEvent(input$bioplex_apply_names_modal, {
    shiny::req(bioplex$df)
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
      shiny::req(bioplex$df)
      bioplex$df
    } else {
      bioplex_combined_filtered()
    }
    shiny::req(nrow(combined_now) > 0)

    keep <- setdiff(seq_len(nrow(combined_now)), bioplex$deleted_idx)

    # Map displayed row index -> absolute row index in combined_now
    abs_row <- keep[info$row]
    j <- info$col + 1L
    shiny::req(j >= 1, j <= ncol(combined_now))
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
      shiny::modalDialog(
        title = "Bio-Plex Editor",
        size = "l",
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::modalButton("Close"),
          shiny::actionButton(
            "bioplex_confirm_modal",
            "Save & Use",
            class = "btn-primary"
          )
        ),
        shiny::uiOutput("bioplex_modal_tabs")
      )
    )
  }
  show_data_editor_modal <- function(title = "Data Editor") {
    shiny::showModal(
      shiny::modalDialog(
        title = title,
        size = "l",
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::modalButton("Close"),
          shiny::actionButton(
            "bioplex_confirm_modal",
            "Save & Use",
            class = "btn-primary"
          )
        ),
        shiny::uiOutput("bioplex_modal_tabs")
      )
    )
  }

  shiny::observeEvent(input$bioplex_modal_delete_selected, {
    combined_now <- if (identical(bioplex$editor_mode, "persisted")) {
      shiny::req(bioplex$df)
      bioplex$df
    } else {
      bioplex_combined_filtered()
    }
    shiny::req(nrow(combined_now) > 0)
    sel <- input$bioplex_modal_table_combined_rows_selected
    if (length(sel)) {
      current <- setdiff(seq_len(nrow(combined_now)), bioplex$deleted_idx)
      bioplex$deleted_idx <- sort(unique(c(bioplex$deleted_idx, current[sel])))
    }
  })

  shiny::observeEvent(input$bioplex_modal_restore_all, {
    bioplex$deleted_idx <- integer(0)
  })
  shiny::observeEvent(input$bioplex_modal_delete_cols, {
    shiny::req(bioplex$df)

    idx0 <- input$bioplex_modal_table_combined_columns_selected_data %||%
      input$bioplex_modal_table_combined_columns_selected
    shiny::validate(shiny::need(
      length(idx0) > 0,
      "Click footer cells to select column(s), then Delete."
    ))

    drop_idx <- as.integer(idx0) + 1L
    drop_idx <- drop_idx[drop_idx >= 1 & drop_idx <= ncol(bioplex$df)]
    shiny::validate(shiny::need(
      length(drop_idx) > 0,
      "No valid columns to delete."
    ))

    if (is.null(bioplex$cols_master)) {
      bioplex$cols_master <- bioplex$df
    }
    keep_names <- setdiff(names(bioplex$df), names(bioplex$df)[drop_idx])
    bioplex$df <- bioplex$df[, keep_names, drop = FALSE]
    bioplex$deleted_idx <- integer(0) # reset row deletes on combined view
  })

  # Helper: clean * and OOR for ONE column, using that column's min/max
  .clean_bioplex_column <- function(v) {
    if (is.numeric(v)) {
      return(v)
    } # already numeric
    v_chr <- step2_normalize_missing_tokens(v)

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
    step2_is_numeric_like(v)
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

  .detect_oor_tokens <- function(df) {
    if (is.null(df) || !nrow(df) || !ncol(df)) {
      return(list(total = 0L, gt = 0L, lt = 0L, columns = character(0)))
    }

    deny <- .always_categorical(names(df))
    gt_total <- 0L
    lt_total <- 0L
    affected_cols <- character(0)

    for (nm in names(df)) {
      if (nm %in% deny || !.is_numeric_like(df[[nm]])) {
        next
      }

      values_chr <- as.character(df[[nm]])
      oor_gt <- grepl("(?i)\\bOOR\\s*>", values_chr, perl = TRUE)
      oor_lt <- grepl("(?i)\\bOOR\\s*<", values_chr, perl = TRUE)

      if (any(oor_gt | oor_lt, na.rm = TRUE)) {
        gt_total <- gt_total + sum(oor_gt, na.rm = TRUE)
        lt_total <- lt_total + sum(oor_lt, na.rm = TRUE)
        affected_cols <- c(affected_cols, nm)
      }
    }

    list(
      total = gt_total + lt_total,
      gt = gt_total,
      lt = lt_total,
      columns = unique(affected_cols)
    )
  }

  .format_oor_columns <- function(columns, limit = 8L) {
    columns <- unique(columns)
    if (!length(columns)) {
      return("None")
    }
    if (length(columns) <= limit) {
      return(paste(columns, collapse = ", "))
    }
    shown <- columns[seq_len(limit)]
    paste(
      c(shown, paste0("+", length(columns) - limit, " more")),
      collapse = ", "
    )
  }

  # Confirm import (activate for Step 2)
  shiny::observeEvent(input$bioplex_confirm_modal, {
    combined_now <- if (identical(bioplex$editor_mode, "persisted")) {
      shiny::req(bioplex$df)
      bioplex$df
    } else {
      bioplex_combined_filtered()
    }
    shiny::req(nrow(combined_now) > 0)

    # preserve column names if the user renamed them
    if (!is.null(bioplex$df) && ncol(bioplex$df) == ncol(combined_now)) {
      names(combined_now) <- names(bioplex$df)
    }

    # apply any deletions done on the Combined tab itself
    keep <- setdiff(seq_len(nrow(combined_now)), bioplex$deleted_idx)
    shiny::req(length(keep) > 0)
    final_raw <- combined_now[keep, , drop = FALSE]
    oor_summary <- .detect_oor_tokens(final_raw)

    # Clean asterisks + OOR (your existing helper)
    final <- bioplex_clean_numeric_only(final_raw)

    # Persist & activate
    bioplex$final <- final
    bioplex$active <- TRUE
    bioplex$editor_mode <- "persisted"

    shiny::removeModal()
    if (oor_summary$total > 0) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Out-of-range values were adjusted",
          easyClose = FALSE,
          footer = shiny::actionButton(
            "oor_warning_ok",
            "Continue",
            class = "btn-primary"
          ),
          shiny::p(
            sprintf(
              "The uploaded data included %d out-of-range value%s. %d were above range and %d were below range.",
              oor_summary$total,
              if (oor_summary$total == 1) "" else "s",
              oor_summary$gt,
              oor_summary$lt
            )
          ),
          shiny::p(
            shiny::tags$b("Affected columns:"),
            paste0(" ", .format_oor_columns(oor_summary$columns))
          ),
          shiny::p(
            "These values were handled automatically during data cleaning before the dataset was staged for analysis."
          ),
          shiny::p(
            "For each affected column, the app uses the non-out-of-range numeric values in that same column as the reference for the replacement values."
          ),
          shiny::tags$ul(
            shiny::tags$li(
              "Values marked above range are replaced with the column maximum plus one percent of that column's observed range."
            ),
            shiny::tags$li(
              "Values marked below range are replaced with the column minimum plus ten percent of that minimum when the column minimum is positive."
            ),
            shiny::tags$li(
              "If the column minimum is zero or negative, values marked below range are replaced with that minimum."
            )
          ),
          shiny::p(
            "Your cleaned data has already been saved for use in the app. Click Continue to proceed."
          )
        )
      )
    } else {
      shiny::showNotification(
        "Bio-Plex data cleaned and staged. Click 'Next Step' to continue.",
        type = "message"
      )
    }
  })

  shiny::observeEvent(input$oor_warning_ok, {
    shiny::removeModal()
  })

  # REACTIVE & OUTPUT to track if data is loaded
  output$data_is_loaded <- shiny::reactive({
    isTRUE(input$use_builtin) || isTRUE(bioplex$active)
  })
  shiny::outputOptions(output, "data_is_loaded", suspendWhenHidden = FALSE)

  # Summary stats
  output$data_summary <- shiny::renderUI({
    shiny::req(userData())
    df <- userData()
    shiny::fluidRow(
      shiny::column(4, shiny::tags$b("Rows:"), nrow(df)),
      shiny::column(4, shiny::tags$b("Columns:"), ncol(df)),
      shiny::column(
        4,
        shiny::tags$b("Missing %:"),
        paste0(
          round(100 * sum(is.na(df)) / (nrow(df) * ncol(df)), 1),
          "%"
        )
      )
    )
  })
  output$preview_ui <- shiny::renderUI({
    shiny::req(input$use_builtin || isTRUE(bioplex$active))
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
    shiny::req(userData())
    df <- userData()
    df$..cyto_id.. <- NULL # Hide internal ID

    # build a a?owidea?? skim table
    wide <- skimr::skim(df) |>
      dplyr::select(
        -character.min,
        -character.max,
        -character.empty,
        -character.whitespace
      ) |>
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
      ) |>
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
      shinyFeedback::feedbackWarning(
        "datafile",
        TRUE,
        "No numeric columns found. Some analyses require numeric data."
      )
    } else {
      shinyFeedback::hideFeedback("datafile")
    }

    # Example: too many missing
    pct_missing <- sum(is.na(df)) / (nrow(df) * ncol(df))
    if (pct_missing > 0.05) {
      shinyFeedback::feedbackWarning(
        "datafile",
        TRUE,
        "Over 5% of cells are missing."
      )
    }
  })

  invisible(app_stage_commit(app_ctx, stage_env))
}

init_data_filtering_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  userState <- app_ctx$userState
  step2_typed_data <- app_ctx$step2_typed_data
  step2_typed_col_info <- app_ctx$step2_typed_col_info
  safe_names <- app_ctx$safe_names
  deleted_row_ids <- app_ctx$deleted_row_ids
  imputed_data <- app_ctx$imputed_data
  ## ---------------------------
  ## Data Filtering and Column Selection
  ## ---------------------------

  # Decoupled UI generation to prevent reactive loops
  output$filter_ui <- shiny::renderUI({
    df <- step2_typed_data()
    shiny::req(df, input$selected_categorical_cols)
    col_info <- step2_typed_col_info()

    # Only create filters for the categorical columns the user has selected
    factor_cols <- intersect(
      col_info$categorical,
      input$selected_categorical_cols
    )

    if (length(factor_cols) == 0) {
      return(NULL)
    }

    lapply(factor_cols, function(col) {
      all_levels <- sort(unique(df[[col]]))

      # Use isolate() to read the input value without creating a dependency
      # On first load, default to all levels being selected
      selected_now <- shiny::isolate(input[[paste0("filter_", col)]]) %||%
        all_levels

      shiny::selectizeInput(
        inputId = paste0("filter_", col),
        label = paste("Filter", col, "(select levels)"),
        choices = all_levels,
        selected = selected_now,
        multiple = TRUE,
        options = list(plugins = c("remove_button", "restore_on_backspace"))
      )
    })
  })

  # ---- Step 2: Type controls ----
  output$step2_type_override_ui <- shiny::renderUI({
    df <- step2_typed_data()
    shiny::req(df)
    cols <- safe_names(df)
    selected_factor_cols <- intersect(
      shiny::isolate(input$factor_cols) %||%
        userState$step2_factor_cols %||%
        character(0),
      cols
    )
    selected_numeric_cols <- intersect(
      shiny::isolate(input$numeric_override_cols) %||%
        userState$step2_numeric_override_cols %||%
        character(0),
      cols
    )
    factor_order_choices <- selected_factor_cols
    factor_order_selected <- shiny::isolate(input$factor_order_col) %||%
      userState$step2_factor_order_col
    factor_order_selected <- intersect(
      factor_order_selected %||% character(0),
      factor_order_choices
    )
    if (!length(factor_order_selected) && length(factor_order_choices)) {
      factor_order_selected <- factor_order_choices[1]
    }
    factor_order_enabled <- isTRUE(
      shiny::isolate(input$factor_order_enable) %||%
        userState$step2_factor_order_enable
    )
    factor_levels_value <- shiny::isolate(input$factor_levels_csv) %||%
      userState$step2_factor_levels_csv %||%
      ""

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectizeInput(
            "factor_cols",
            "Treat these columns as categorical:",
            choices = cols,
            selected = selected_factor_cols,
            multiple = TRUE,
            options = list(plugins = "remove_button")
          )
        ),
        shiny::column(
          6,
          shiny::selectizeInput(
            "numeric_override_cols",
            "Treat these columns as numeric:",
            choices = cols,
            selected = selected_numeric_cols,
            multiple = TRUE,
            options = list(plugins = "remove_button")
          )
        )
      ),
      shiny::checkboxInput(
        "factor_order_enable",
        "Specify level order for one selected categorical column (optional)",
        value = factor_order_enabled
      ),
      shiny::conditionalPanel(
        "input.factor_order_enable",
        shiny::selectInput(
          "factor_order_col",
          "Column to order:",
          choices = factor_order_choices,
          selected = factor_order_selected
        ),
        shiny::textInput(
          "factor_levels_csv",
          "Level order (comma-separated):",
          value = factor_levels_value,
          placeholder = "e.g. Control, Case, Unknown"
        )
      ),
      shiny::helpText(
        "Values that cannot be parsed as numbers will become missing (NA) while the column stays numeric."
      ),
      shiny::actionButton("apply_types", "Apply types")
    )
  })

  invisible(app_stage_commit(app_ctx, stage_env))
}

init_options_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  userState <- app_ctx$userState
  selected_function <- app_ctx$selected_function
  filteredData <- app_ctx$filteredData
  safe_names <- app_ctx$safe_names
  ## ---------------------------
  ## Function Options UI (Step 3)
  ## ---------------------------
  allowed_colors <- c(
    "red",
    "blue",
    "green",
    "orange",
    "purple",
    "brown",
    "pink",
    "yellow",
    "gray",
    "lightgray",
    "black"
  )

  p_adjust_choices <- c(
    "None" = "",
    "Bonferroni" = "bonferroni",
    "Holm" = "holm",
    "Hochberg" = "hochberg",
    "Hommel" = "hommel",
    "Benjamini-Hochberg" = "BH",
    "Benjamini-Yekutieli" = "BY"
  )
  p_adjust_default <- "BH"

  p_adjust_help <- shiny::HTML(paste(
    "Use this when you are testing many cytokines or features at the same time.<br><br>",
    "Adjustment makes the reported p-values more conservative so fewer findings look significant just by chance.<br><br>",
    "<b>None</b>: keep the raw p-values exactly as calculated.<br>",
    "<b>Bonferroni</b>: very strict; best when you want to minimize any false positives.<br>",
    "<b>Holm</b>: still strict, but usually a little less harsh than Bonferroni.<br>",
    "<b>Hochberg</b>: another family-wise error method that can be a bit more powerful when assumptions fit.<br>",
    "<b>Hommel</b>: a more advanced family-wise error method; useful but less commonly chosen by non-specialists.<br>",
    "<b>Benjamini-Hochberg</b>: controls the expected false discovery rate and is a common choice for exploratory biology data.<br>",
    "<b>Benjamini-Yekutieli</b>: similar to Benjamini-Hochberg, but more conservative when tests may be strongly related."
  ))

  # PCH Values
  pch_choices <- c(
    "Square" = 0,
    "Circle" = 1,
    "Triangle Up" = 2,
    "Plus" = 3,
    "Cross" = 4,
    "Diamond" = 5,
    "Triangle Down" = 6,
    "Square Cross" = 7,
    "Star" = 8,
    "Diamond Plus" = 9,
    "Circle Plus" = 10,
    "Triangles Up/Down" = 11,
    "Square Plus" = 12,
    "Circle Plus" = 13,
    "Square" = 14,
    "Square Filled" = 15,
    "Circle Filled" = 16,
    "Triangle Up Filled" = 17,
    "Diamond Filled" = 18,
    "Circle Filled" = 19,
    "Bullet" = 20,
    "Circle w/ Border" = 21,
    "Square w/ Border" = 22,
    "Diamond w/ Border" = 23,
    "Triangle Up w/ Border" = 24,
    "Triangle Down w/ Border" = 25
  )

  core_font_fields <- c(
    "base_size",
    "plot_title",
    "x_title",
    "y_title",
    "x_text",
    "y_text",
    "legend_title",
    "legend_text",
    "strip_text"
  )
  core_font_slider_fields <- setdiff(core_font_fields, "base_size")

  font_field_specs <- list(
    base_size = list(label = "Base Font Size", min = 8, max = 24, step = 1),
    plot_title = list(label = "Plot Title", min = 8, max = 30, step = 1),
    x_title = list(label = "X-Axis Title", min = 8, max = 28, step = 1),
    y_title = list(label = "Y-Axis Title", min = 8, max = 28, step = 1),
    x_text = list(label = "X-Axis Text", min = 6, max = 24, step = 1),
    y_text = list(label = "Y-Axis Text", min = 6, max = 24, step = 1),
    legend_title = list(label = "Legend Title", min = 6, max = 24, step = 1),
    legend_text = list(label = "Legend Text", min = 6, max = 24, step = 1),
    strip_text = list(label = "Facet Strip Text", min = 6, max = 24, step = 1),
    annotation_text = list(
      label = "Annotation Text",
      min = 6,
      max = 24,
      step = 1
    ),
    row_names = list(label = "Row Names", min = 6, max = 24, step = 1),
    col_names = list(label = "Column Names", min = 6, max = 24, step = 1),
    cell_text = list(label = "Cell Labels", min = 6, max = 24, step = 1),
    variable_names = list(
      label = "Variable Labels",
      min = 6,
      max = 24,
      step = 1
    ),
    point_labels = list(label = "Point Labels", min = 6, max = 24, step = 1)
  )

  analysis_font_specs <- list(
    "Boxplots" = list(
      prefix = "bp",
      state_key = "bp_font_settings",
      supported_fields = core_font_slider_fields,
      default_font_settings = font_settings_defaults(11)
    ),
    "Violin Plots" = list(
      prefix = "vio",
      state_key = "vio_font_settings",
      supported_fields = core_font_slider_fields,
      default_font_settings = font_settings_defaults(11)
    ),
    "Error-Bar Plot" = list(
      prefix = "eb",
      state_key = "eb_font_settings",
      supported_fields = c(core_font_slider_fields, "annotation_text"),
      default_font_settings = font_settings_defaults(11)
    ),
    "Dual-Flashlight Plot" = list(
      prefix = "df",
      state_key = "df_font_settings",
      supported_fields = c(core_font_slider_fields, "annotation_text"),
      default_font_settings = font_settings_defaults(11)
    ),
    "Heatmap" = list(
      prefix = "hm",
      state_key = "hm_font_settings",
      supported_fields = c("row_names", "col_names"),
      default_font_settings = font_settings_defaults(10)
    ),
    "Correlation Plots" = list(
      prefix = "corr",
      state_key = "corr_font_settings",
      supported_fields = c(
        "plot_title",
        "x_text",
        "y_text",
        "legend_title",
        "legend_text",
        "cell_text"
      ),
      default_font_settings = font_settings_defaults(11)
    ),
    "Skewness/Kurtosis" = list(
      prefix = "skku",
      state_key = "skku_font_settings",
      supported_fields = core_font_slider_fields,
      default_font_settings = font_settings_defaults(11)
    ),
    "Volcano Plot" = list(
      prefix = "volc",
      state_key = "volc_font_settings",
      supported_fields = c(core_font_slider_fields, "annotation_text"),
      default_font_settings = font_settings_defaults(11)
    ),
    "Principal Component Analysis (PCA)" = list(
      prefix = "pca",
      state_key = "pca_font_settings",
      supported_fields = c(
        core_font_slider_fields,
        "annotation_text",
        "variable_names",
        "point_labels"
      ),
      default_font_settings = utils::modifyList(
        font_settings_defaults(14),
        list(
          plot_title = 18,
          x_title = 16,
          y_title = 16,
          x_text = 14,
          y_text = 14,
          legend_title = 16,
          legend_text = 14,
          annotation_text = 13,
          variable_names = 12,
          point_labels = 11
        )
      )
    ),
    "Partial Least Squares Regression (PLSR)" = list(
      prefix = "plsr",
      state_key = "plsr_font_settings",
      supported_fields = c(
        core_font_slider_fields,
        "variable_names",
        "point_labels"
      ),
      default_font_settings = utils::modifyList(
        font_settings_defaults(14),
        list(
          plot_title = 18,
          x_title = 16,
          y_title = 16,
          x_text = 14,
          y_text = 14,
          legend_title = 16,
          legend_text = 14,
          annotation_text = 13,
          variable_names = 12,
          point_labels = 11
        )
      )
    ),
    "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = list(
      prefix = "splsda",
      state_key = "splsda_font_settings",
      supported_fields = c(
        core_font_slider_fields,
        "variable_names",
        "point_labels"
      ),
      default_font_settings = utils::modifyList(
        font_settings_defaults(14),
        list(
          plot_title = 18,
          x_title = 16,
          y_title = 16,
          x_text = 14,
          y_text = 14,
          legend_title = 16,
          legend_text = 14,
          annotation_text = 13,
          variable_names = 12,
          point_labels = 11
        )
      )
    ),
    "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)" = list(
      prefix = "mint_splsda",
      state_key = "mint_splsda_font_settings",
      supported_fields = c(
        core_font_slider_fields,
        "variable_names",
        "point_labels"
      ),
      default_font_settings = utils::modifyList(
        font_settings_defaults(14),
        list(
          plot_title = 18,
          x_title = 16,
          y_title = 16,
          x_text = 14,
          y_text = 14,
          legend_title = 16,
          legend_text = 14,
          annotation_text = 13,
          variable_names = 12,
          point_labels = 11
        )
      )
    ),
    "Random Forest" = list(
      prefix = "rf",
      state_key = "rf_font_settings",
      supported_fields = core_font_slider_fields,
      default_font_settings = font_settings_defaults(11)
    ),
    "Extreme Gradient Boosting (XGBoost)" = list(
      prefix = "xgb",
      state_key = "xgb_font_settings",
      supported_fields = core_font_slider_fields,
      default_font_settings = font_settings_defaults(11)
    )
  )

  get_analysis_font_spec <- function(func_name) {
    analysis_font_specs[[func_name]]
  }

  font_settings_state_from_inputs <- function(
    input,
    prefix,
    supported_fields,
    default_font_settings = font_settings_defaults(11)
  ) {
    use_custom_id <- paste0(prefix, "_font_use_custom")
    custom_ids <- paste0(prefix, "_font_", supported_fields)
    defaults <- normalize_font_settings(
      font_settings = default_font_settings,
      supported_fields = unique(c("base_size", supported_fields)),
      activate = TRUE
    )

    if (
      is.null(input[[use_custom_id]]) &&
        all(vapply(custom_ids, function(id) is.null(input[[id]]), logical(1)))
    ) {
      return(NULL)
    }

    state <- list(
      use_custom = isTRUE(input[[use_custom_id]])
    )

    for (field in supported_fields) {
      state[[field]] <- input[[paste0(prefix, "_font_", field)]] %||%
        defaults[[field]]
    }

    state
  }

  font_settings_state_to_backend <- function(
    state,
    default_font_settings = font_settings_defaults(11)
  ) {
    defaults <- normalize_font_settings(
      font_settings = default_font_settings,
      activate = TRUE
    )

    if (is.null(state)) {
      return(defaults)
    }

    if (!isTRUE(state$use_custom)) {
      return(defaults)
    }

    for (field in setdiff(names(state), "use_custom")) {
      defaults[[field]] <- state[[field]]
    }

    defaults
  }

  restore_font_settings_inputs <- function(
    session,
    prefix,
    supported_fields,
    state = NULL,
    default_font_settings = font_settings_defaults(11)
  ) {
    defaults <- font_settings_state_to_backend(
      state = state,
      default_font_settings = default_font_settings
    )

    try(
      shiny::updateCheckboxInput(
        session,
        paste0(prefix, "_font_use_custom"),
        value = isTRUE(state$use_custom)
      ),
      silent = TRUE
    )

    for (field in supported_fields) {
      try(
        shiny::updateSliderInput(
          session,
          paste0(prefix, "_font_", field),
          value = state[[field]] %||% defaults[[field]]
        ),
        silent = TRUE
      )
    }
  }
  # --- Helper: expand/normalize PCH to match number of levels ---
  expand_pch <- function(selected, needed, pool = 0:25) {
    sel <- as.integer(selected)
    sel <- sel[!is.na(sel)]
    if (!length(sel)) {
      sel <- c(16, 17)
    } # sensible defaults if none selected
    sel <- unique(sel)

    if (needed <= length(sel)) {
      return(sel[seq_len(needed)])
    }

    add_n <- needed - length(sel)
    remaining <- setdiff(pool, sel)

    if (length(remaining) >= add_n) {
      # fill with unique, non-duplicate shapes
      extra <- sample(remaining, add_n, replace = FALSE)
    } else {
      # pool exhausted: take all remaining, then sample with replacement
      extra <- c(
        remaining,
        sample(pool, add_n - length(remaining), replace = TRUE)
      )
    }
    c(sel, extra)
  }

  output$function_options_ui <- shiny::renderUI({
    func <- selected_function()
    shiny::req(func)

    userState$selected_function <- func
    func_name <- func
    helper_color <- if (input$theme_choice %in% c("darkly", "cyborg")) {
      "red"
    } else {
      "blue"
    }
    helper_label <- function(
      text,
      title,
      content,
      icon = "fas fa-question-circle"
    ) {
      shinyhelper::helper(
        type = "inline",
        title = title,
        icon = icon,
        shiny_tag = shiny::HTML(
          sprintf("<span style='margin-right: 15px;'>%s</span>", text)
        ),
        content = content,
        colour = helper_color
      )
    }
    build_univariate_formula_preview <- function(
      design,
      primary = NULL,
      secondary = NULL,
      covariate = NULL,
      include_ps = FALSE,
      include_pc = FALSE,
      include_sc = FALSE
    ) {
      if (is.null(primary) || !nzchar(primary)) {
        return("outcome ~ <choose predictors>")
      }

      rhs_terms <- primary
      if (identical(design, "two_way")) {
        if (is.null(secondary) || !nzchar(secondary)) {
          return("outcome ~ <choose predictors>")
        }
        rhs_terms <- c(rhs_terms, secondary)
        if (isTRUE(include_ps)) {
          rhs_terms <- c(rhs_terms, paste(primary, secondary, sep = ":"))
        }
      } else {
        if (!is.null(secondary) && nzchar(secondary)) {
          rhs_terms <- c(rhs_terms, secondary)
        }
        if (is.null(covariate) || !nzchar(covariate)) {
          return("outcome ~ <choose predictors>")
        }
        rhs_terms <- c(rhs_terms, covariate)
        if (isTRUE(include_ps) && !is.null(secondary) && nzchar(secondary)) {
          rhs_terms <- c(rhs_terms, paste(primary, secondary, sep = ":"))
        }
        if (isTRUE(include_pc)) {
          rhs_terms <- c(rhs_terms, paste(primary, covariate, sep = ":"))
        }
        if (isTRUE(include_sc) && !is.null(secondary) && nzchar(secondary)) {
          rhs_terms <- c(rhs_terms, paste(secondary, covariate, sep = ":"))
        }
      }

      paste("outcome ~", paste(rhs_terms, collapse = " + "))
    }
    formula_preview_ui <- function(formula_text, note = NULL) {
      shiny::div(
        class = "border rounded p-2 mt-2",
        shiny::strong("Model formula preview"),
        shiny::tags$pre(style = "margin:0.5rem 0 0;", formula_text),
        if (!is.null(note)) {
          shiny::helpText(note)
        }
      )
    }
    build_font_controls_ui <- function(func_name) {
      spec <- get_analysis_font_spec(func_name)
      if (is.null(spec)) {
        return(NULL)
      }

      state <- userState[[spec$state_key]]
      defaults <- font_settings_state_to_backend(
        state = state,
        default_font_settings = spec$default_font_settings
      )
      custom_fields <- spec$supported_fields

      slider_ui <- function(field) {
        field_spec <- font_field_specs[[field]]
        slider_label <- field_spec$label

        if (
          identical(field, "point_labels") &&
            func_name %in%
              c(
                "Principal Component Analysis (PCA)",
                "Partial Least Squares Regression (PLSR)",
                "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)",
                "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
              )
        ) {
          slider_label <- helper_label(
            "Point Labels",
            "mixOmics Point / Label Size",
            "For mixOmics individual plots, this control sets point size when sample names are hidden and label size when sample names are shown."
          )
        }

        shiny::sliderInput(
          paste0(spec$prefix, "_font_", field),
          label = slider_label,
          min = field_spec$min,
          max = field_spec$max,
          step = field_spec$step,
          value = state[[field]] %||% defaults[[field]]
        )
      }

      custom_rows <- lapply(
        split(custom_fields, ceiling(seq_along(custom_fields) / 2)),
        function(fields) {
          shiny::fluidRow(
            lapply(fields, function(field) {
              shiny::column(6, slider_ui(field))
            })
          )
        }
      )

      shiny::tagList(
        shiny::tags$details(
          class = "mt-2 border rounded p-2",
          open = isTRUE(state$use_custom),
          shiny::tags$summary(shiny::strong("Text & Fonts")),
          shiny::div(
            class = "mt-2",
            shiny::checkboxInput(
              paste0(spec$prefix, "_font_use_custom"),
              label = helper_label(
                "Use Custom Text Sizes",
                "Custom Font Controls",
                "Turn this on to customize the supported text elements for this figure, including titles, axes, legends, labels, and other plot-specific text."
              ),
              value = isTRUE(state$use_custom)
            ),
            shiny::conditionalPanel(
              condition = sprintf(
                "input.%s == true",
                paste0(spec$prefix, "_font_use_custom")
              ),
              do.call(shiny::tagList, custom_rows)
            )
          )
        )
      )
    }
    ui_list <- list()
    switch(
      func_name,
      # ------------------------
      # Univariate Tests (T-test, Wilcoxon)
      # ------------------------
      "Univariate Tests (T-test, Wilcoxon)" = {
        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "uv2_method",
                label = helper_label(
                  "Test Method",
                  "Two-Level Test Method",
                  ui_two_group_univariate_help_text()
                ),
                choices = ui_two_group_test_choices(),
                selected = shiny::isolate(userState$uv2_method) %||% "auto"
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "uv2_p_adjust_method",
                label = helper_label(
                  "P-Value Adjustment",
                  "Multiple-Testing Correction",
                  p_adjust_help
                ),
                choices = p_adjust_choices,
                selected = shiny::isolate(userState$uv2_p_adjust_method) %||%
                  p_adjust_default
              )
            )
          )
        )
      },
      # ------------------------
      # Univariate Tests (ANOVA, Kruskal-Wallis)
      # ------------------------
      "Multi-level Univariate Tests (Anova, Kruskal-Wallis)" = {
        method_selected <- input$uvm_method %||%
          shiny::isolate(userState$uvm_method) %||%
          "anova"
        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "uvm_method",
                label = helper_label(
                  "Global Test Method",
                  "Multi-Level Test Method",
                  "Choose the overall test used when one categorical variable has more than two groups. ANOVA is the standard choice when group means are a sensible summary and the data are reasonably well behaved. Kruskal-Wallis is a safer rank-based option when the data are non-normal or contain outliers."
                ),
                choices = c("ANOVA" = "anova", "Kruskal-Wallis" = "kruskal"),
                selected = shiny::isolate(userState$uvm_method) %||% "anova"
              )
            ),
            shiny::column(
              6,
              if (identical(method_selected, "kruskal")) {
                shiny::selectInput(
                  "uvm_p_adjust_method",
                  label = helper_label(
                    "Pairwise P-Value Adjustment",
                    "Pairwise Multiple-Testing Correction",
                    p_adjust_help
                  ),
                  choices = p_adjust_choices,
                  selected = input$uvm_p_adjust_method %||%
                    shiny::isolate(userState$uvm_p_adjust_method) %||%
                    p_adjust_default
                )
              }
            )
          )
        )
      },
      # ------------------------
      # Two-way ANOVA
      # ------------------------
      "Two-way ANOVA" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)
        cat_cols <- cols[vapply(
          df[cols],
          function(x) !is.numeric(x),
          logical(1)
        )]
        twa_primary_selected <- input$twa_primary_cat_var %||%
          shiny::isolate(userState$twa_primary_cat_var) %||%
          if (length(cat_cols)) cat_cols[1] else NULL
        twa_secondary_choices <- setdiff(cat_cols, twa_primary_selected %||% "")
        if (length(twa_secondary_choices) == 0L) {
          twa_secondary_choices <- cat_cols
        }
        preview_formula <- build_univariate_formula_preview(
          design = "two_way",
          primary = twa_primary_selected,
          secondary = input$twa_secondary_cat_var %||%
            shiny::isolate(userState$twa_secondary_cat_var),
          include_ps = isTRUE(
            input$twa_include_primary_secondary_interaction %||%
              shiny::isolate(
                userState$twa_include_primary_secondary_interaction
              )
          )
        )

        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "twa_primary_cat_var",
                label = helper_label(
                  "Primary Factor",
                  "Two-way ANOVA Primary Factor",
                  "Choose the main categorical factor whose mean differences you want to evaluate across outcomes."
                ),
                choices = cat_cols,
                selected = twa_primary_selected
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "twa_secondary_cat_var",
                label = helper_label(
                  "Secondary Factor",
                  "Two-way ANOVA Secondary Factor",
                  "Choose the second categorical factor that defines the two-way ANOVA design."
                ),
                choices = twa_secondary_choices,
                selected = shiny::isolate(userState$twa_secondary_cat_var) %||%
                  if (length(twa_secondary_choices)) {
                    twa_secondary_choices[1]
                  } else {
                    NULL
                  }
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::checkboxInput(
                "twa_include_primary_secondary_interaction",
                label = helper_label(
                  "Include primary:secondary interaction",
                  "Two-way ANOVA Interaction",
                  "Turn this on to include the primary:secondary interaction term in the fitted model."
                ),
                value = shiny::isolate(
                  userState$twa_include_primary_secondary_interaction
                ) %||%
                  FALSE
              ),
              formula_preview_ui(preview_formula)
            )
          )
        )
      },
      # ------------------------
      # ANCOVA
      # ------------------------
      "ANCOVA" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)
        cat_cols <- cols[vapply(
          df[cols],
          function(x) !is.numeric(x),
          logical(1)
        )]
        num_cols <- cols[vapply(df[cols], is.numeric, logical(1))]
        anc_primary_selected <- input$anc_primary_cat_var %||%
          shiny::isolate(userState$anc_primary_cat_var) %||%
          if (length(cat_cols)) cat_cols[1] else NULL
        anc_secondary_base <- setdiff(cat_cols, anc_primary_selected %||% "")
        secondary_choices <- c(
          "None" = "",
          stats::setNames(anc_secondary_base, anc_secondary_base)
        )
        preview_formula <- build_univariate_formula_preview(
          design = "ancova",
          primary = anc_primary_selected,
          secondary = input$anc_secondary_cat_var %||%
            shiny::isolate(userState$anc_secondary_cat_var),
          covariate = input$anc_covariate_col %||%
            shiny::isolate(userState$anc_covariate_col),
          include_ps = isTRUE(
            input$anc_include_primary_secondary_interaction %||%
              shiny::isolate(
                userState$anc_include_primary_secondary_interaction
              )
          ),
          include_pc = isTRUE(
            input$anc_include_primary_covariate_interaction %||%
              shiny::isolate(
                userState$anc_include_primary_covariate_interaction
              )
          ),
          include_sc = isTRUE(
            input$anc_include_secondary_covariate_interaction %||%
              shiny::isolate(
                userState$anc_include_secondary_covariate_interaction
              )
          )
        )

        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::selectInput(
                "anc_primary_cat_var",
                label = helper_label(
                  "Primary Factor",
                  "ANCOVA Primary Factor",
                  "Choose the main categorical factor whose adjusted mean differences you want to test."
                ),
                choices = cat_cols,
                selected = anc_primary_selected
              )
            ),
            shiny::column(
              4,
              shiny::selectInput(
                "anc_secondary_cat_var",
                label = helper_label(
                  "Secondary Factor (Optional)",
                  "ANCOVA Secondary Factor",
                  "Optionally add a second categorical factor. Leave this as None to fit a single-factor ANCOVA."
                ),
                choices = secondary_choices,
                selected = shiny::isolate(userState$anc_secondary_cat_var) %||%
                  ""
              )
            ),
            shiny::column(
              4,
              shiny::selectInput(
                "anc_covariate_col",
                label = helper_label(
                  "Covariate",
                  "ANCOVA Covariate",
                  "Choose the numeric covariate that should be adjusted for in the ANCOVA model."
                ),
                choices = num_cols,
                selected = shiny::isolate(userState$anc_covariate_col) %||%
                  if (length(num_cols)) num_cols[1] else NULL
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "anc_include_primary_secondary_interaction",
                label = helper_label(
                  "Include primary:secondary interaction",
                  "ANCOVA Factor Interaction",
                  "Turn this on to include the primary:secondary interaction term when a secondary factor is selected."
                ),
                value = shiny::isolate(
                  userState$anc_include_primary_secondary_interaction
                ) %||%
                  FALSE
              )
            ),
            shiny::column(
              4,
              shiny::checkboxInput(
                "anc_include_primary_covariate_interaction",
                label = helper_label(
                  "Include primary:covariate interaction",
                  "ANCOVA Slope Interaction",
                  "Turn this on to model a primary:covariate interaction directly instead of treating slope heterogeneity as an assumption check."
                ),
                value = shiny::isolate(
                  userState$anc_include_primary_covariate_interaction
                ) %||%
                  FALSE
              )
            ),
            shiny::column(
              4,
              shiny::checkboxInput(
                "anc_include_secondary_covariate_interaction",
                label = helper_label(
                  "Include secondary:covariate interaction",
                  "ANCOVA Secondary Slope Interaction",
                  "Turn this on to model a secondary:covariate interaction directly when a secondary factor is selected. Leave it off to keep the secondary effect additive and assess secondary slope homogeneity in the assumptions table."
                ),
                value = shiny::isolate(
                  userState$anc_include_secondary_covariate_interaction
                ) %||%
                  FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              formula_preview_ui(
                preview_formula,
                note = "Supported here: primary:secondary, primary:covariate, and secondary:covariate. Out of scope in this version: primary:secondary:covariate and factor-by-factor simple effects."
              )
            )
          )
        )
      },
      # ------------------------
      # Boxplots
      # ------------------------
      "Boxplots" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        group_choices <- names(df)[!sapply(df, is.numeric)]
        group_choices <- setdiff(group_choices, "..cyto_id..")

        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::numericInput(
                "bp_bin_size",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Bin size for boxplots",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Bin Size</span>"
                  ),
                  content = "Choose how many numeric variables are shown on one page of boxplots at a time. Smaller values are easier to read, while larger values show more variables per page but can make the figure feel crowded.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$bp_bin_size) %||% 25,
                min = 1
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectizeInput(
                "bp_group_by",
                label = helper_label(
                  "Grouping Columns (Optional)",
                  "Boxplot Grouping Columns",
                  "Pick one or more categorical columns if you want separate boxplots for groups such as treatment, sex, cohort, or timepoint. Leave this blank to show one overall boxplot per numeric variable."
                ),
                choices = group_choices,
                selected = shiny::isolate(userState$bp_group_by),
                multiple = TRUE,
                options = list(
                  placeholder = "Leave blank for ungrouped boxplots",
                  plugins = c("remove_button", "restore_on_backspace")
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::textInput(
                "bp_y_lim",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Y-axis Limits",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Y-axis Limits</span><br>(min, max; comma-separated; leave blank for auto)"
                  ),
                  content = "Set the vertical range shown on the plot by entering two numbers separated by a comma, such as 0,100. Leave this blank to let the app choose the range automatically from your data. Set it manually only when you want multiple plots to use the same scale for easier comparison.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$bp_y_lim) %||% ""
              )
            )
          )
        )
      },
      # ------------------------
      # Violin Plots
      # ------------------------
      "Violin Plots" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        group_choices <- names(df)[!sapply(df, is.numeric)]
        group_choices <- setdiff(group_choices, "..cyto_id..")

        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::numericInput(
                "vio_bin_size",
                label = helper_label(
                  "Bin Size",
                  "Violin Plot Bin Size",
                  "Choose how many numeric variables are shown on one violin-plot page at a time. Smaller values are easier to read, while larger values show more variables per page but can make the page busier."
                ),
                value = shiny::isolate(userState$vio_bin_size) %||% 25,
                min = 1
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectizeInput(
                "vio_group_by",
                label = helper_label(
                  "Grouping Columns (Optional)",
                  "Violin Plot Grouping",
                  "Pick one or more categorical columns if you want the violins split into groups, such as treatment, cohort, or timepoint. Leave this blank if you only want the overall distribution for each numeric variable."
                ),
                choices = group_choices,
                selected = shiny::isolate(userState$vio_group_by),
                multiple = TRUE,
                options = list(
                  placeholder = "Leave blank for ungrouped violin plots",
                  plugins = c("remove_button", "restore_on_backspace")
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::textInput(
                "vio_y_lim",
                label = helper_label(
                  "Y-Axis Limits",
                  "Violin Plot Y-Axis Limits",
                  "Set the minimum and maximum y-axis values as two numbers separated by a comma, for example 0,100. Leave this blank for automatic scaling. Set it manually when you want several plots to use the same vertical scale."
                ),
                value = shiny::isolate(userState$vio_y_lim) %||% ""
              )
            ),
            shiny::column(
              width = 6,
              shiny::checkboxInput(
                "vio_boxplot_overlay",
                label = helper_label(
                  "Show Boxplot Overlay",
                  "Violin Plot Overlay",
                  "Add a small boxplot inside each violin so you can see the median and middle spread more clearly. Turn this on when the violin shape alone feels hard to interpret."
                ),
                value = shiny::isolate(userState$vio_boxplot_overlay) %||% FALSE
              )
            )
          )
        )
      },
      # ------------------------
      # Error-Bar Plot
      # ------------------------
      "Error-Bar Plot" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cat_vars <- names(df)[sapply(df, function(x) {
          is.factor(x) || is.character(x)
        })]
        eb_fill_palette_selected <- shiny::isolate(
          userState$eb_fill_palette
        ) %||%
          "gray"
        if (identical(eb_fill_palette_selected, "grey")) {
          eb_fill_palette_selected <- "gray"
        }

        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "eb_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Comparison Column</span>"
                  ),
                  content = "Choose the categorical column that defines the groups you want to compare, such as treatment, responder status, or timepoint. The app will summarize each numeric outcome within the levels of this column and use those groups for the statistical annotations.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cat_vars,
                selected = shiny::isolate(userState$eb_group_col) %||%
                  cat_vars[1]
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "eb_method",
                label = helper_label(
                  "Test Method",
                  "Error-Bar Statistical Test",
                  ui_error_bar_test_help_text()
                ),
                choices = ui_two_group_test_choices(),
                selected = shiny::isolate(userState$eb_method) %||% "auto"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::checkboxInput(
                "eb_p_lab",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "P-Value Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>P-Value Label</span>"
                  ),
                  content = "Show p-value annotations directly on the plot. Turn this on if you want each comparison labeled with statistical significance instead of reading the values only from a separate results table.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_p_lab) %||% FALSE
              )
            ),
            shiny::column(
              3,
              shiny::checkboxInput(
                "eb_es_lab",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Effect-Size Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Effect-Size Label</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Show an effect-size label above each comparison so you can judge how large the difference is, not just whether it is statistically significant.<br><br>",
                    "This plot uses SSMD (strictly standardized mean difference), which is helpful for screening-style comparisons because it emphasizes the size and direction of the separation.<br><br>",
                    "Learn more about SSMD at ",
                    "<a href='https://doi.org/10.1177/1087057111405851' target='_blank' rel='noopener noreferrer'>Zhang (2011)</a>",
                    " and ",
                    "<a href='https://doi.org/10.1177/1087057107300645' target='_blank' rel='noopener noreferrer'>Zhang (2007)</a>."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_es_lab) %||% FALSE
              )
            ),
            shiny::column(
              3,
              shiny::checkboxInput(
                "eb_class_symbol",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Class Symbol",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Class Symbol</span>"
                  ),
                  content = "Replace the full annotation text with short symbol-style categories. Use this if you want a cleaner plot with less text. Leave it off if you prefer the exact p-value or effect-size wording to stay visible.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_class_symbol) %||% FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::textInput(
                "eb_y_lab",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Y-Axis Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Y-Axis Label</span>"
                  ),
                  content = "Text shown on the vertical axis. Use a plain description of the measurement units or quantity being plotted, such as Concentration, Signal, or Normalized Value.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_y_lab) %||% "Concentrations"
              )
            ),
            shiny::column(
              3,
              shiny::textInput(
                "eb_x_lab",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "X-Axis Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>X-Axis Label</span>"
                  ),
                  content = "Text shown on the horizontal axis. Use this to describe what each bar represents, such as Cytokine, Marker, or Feature.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_x_lab) %||% "Cytokines"
              )
            ),
            shiny::column(
              3,
              shiny::textInput(
                "eb_title",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot Title",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Plot Title</span>"
                  ),
                  content = "Main title displayed above the plot. Use a short description that tells the viewer what comparison or data subset they are looking at.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$eb_title) %||% "Error-Bar Plot"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              3,
              shiny::selectInput(
                "eb_stat",
                label = helper_label(
                  "Statistic",
                  "Error-Bar Summary Statistic",
                  "Choose what the height of each bar represents for every group. Mean is the usual average. Median is the middle value and is often easier to trust when the data contain outliers or are strongly skewed."
                ),
                choices = c("Mean" = "mean", "Median" = "median"),
                selected = shiny::isolate(userState$eb_stat) %||% "mean"
              )
            ),
            shiny::column(
              3,
              shiny::selectInput(
                "eb_error",
                label = helper_label(
                  "Error Metric",
                  "Error-Bar Metric",
                  "Choose what the error bars show around each bar. Standard deviation shows spread in the raw values, standard error shows uncertainty in the mean, MAD is a robust spread measure, and 95% CI shows an estimated range for the group summary."
                ),
                choices = c(
                  "Standard error" = "se",
                  "Standard deviation" = "sd",
                  "MAD" = "mad",
                  "95% CI" = "ci"
                ),
                selected = shiny::isolate(userState$eb_error) %||% "se"
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "eb_p_adjust_method",
                label = helper_label(
                  "P-Value Adjustment",
                  "Error-Bar Multiple-Testing Correction",
                  p_adjust_help
                ),
                choices = p_adjust_choices,
                selected = shiny::isolate(userState$eb_p_adjust_method) %||%
                  p_adjust_default
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "eb_n_col",
                label = helper_label(
                  "Columns",
                  "Facet Grid Columns",
                  "Choose how many facet columns are used when the plot is split across many cytokines or features. More columns create a wider plot with fewer rows. Fewer columns create a taller plot that may be easier to read."
                ),
                value = shiny::isolate(userState$eb_n_col) %||% 3,
                min = 1,
                max = 10,
                step = 1
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "eb_fill_palette",
                label = helper_label(
                  "Bar Color",
                  "Bar Fill Palette",
                  "Choose how the bars are colored for each group. Gray keeps the plot simple. The named palettes make group differences easier to follow when you have several levels in the comparison column."
                ),
                choices = c(
                  "Gray (default)" = "gray",
                  "Tableau 10" = "tableau",
                  "Colorblind-safe" = "colorblind",
                  "Pastel" = "pastel"
                ),
                selected = eb_fill_palette_selected
              )
            )
          )
        )
      },

      # ------------------------
      # Dual-Flashlight Plot
      # ------------------------
      "Dual-Flashlight Plot" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::selectInput(
                "df_group_var",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Comparison Column</span>"
                  ),
                  content = "Choose the categorical column that defines the two groups you want to compare in the dual-flashlight plot. Typical examples are treatment, disease status, or responder group. The condition selectors below will use levels from this column.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$df_group_var) %||% cols[1]
              )
            ),
            shiny::column(
              4,
              shiny::uiOutput("df_conditions_ui")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::numericInput(
                "df_ssmd_thresh",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "SSMD Threshold",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>SSMD Threshold</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Set the minimum SSMD effect size needed for a feature to stand out in the plot.<br><br>",
                    "Higher values make the plot stricter by highlighting only stronger differences between the two groups. Lower values show more features but include weaker effects.<br><br>",
                    "Learn more about SSMD at ",
                    "<a href='https://doi.org/10.1177/1087057111405851' target='_blank' rel='noopener noreferrer'>Zhang (2011)</a>",
                    " and ",
                    "<a href='https://doi.org/10.1177/1087057107300645' target='_blank' rel='noopener noreferrer'>Zhang (2007)</a>."
                  )),

                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$df_ssmd_thresh) %||% 1
              )
            ),
            shiny::column(
              4,
              shiny::numericInput(
                "df_log2fc_thresh",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Log2 FC Threshold",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Log2 Fold Change Threshold</span>"
                  ),
                  content = "Set the minimum absolute log2 fold change needed for a feature to count as meaningfully changed. A value of 1 means about a two-fold change between groups. Raise this cutoff to focus on bigger changes.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$df_log2fc_thresh) %||% 1
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::numericInput(
                "df_top_labels",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Top Labels",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Top Labels</span>"
                  ),
                  content = "Choose how many of the most extreme features receive text labels on the plot. Higher values label more points but can make the figure busier. Lower values keep the plot cleaner.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$df_top_labels) %||% 15
              )
            )
          )
        )
      },
      # ------------------------
      # Heatmap
      # ------------------------
      "Heatmap" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }

        cols <- safe_names(df)
        ann_choices <- c("None" = "", cols)

        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectizeInput(
                "hm_annotation",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Annotation Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Annotation Column</span>"
                  ),
                  content = "Choose a categorical column to add a side annotation bar to the heatmap. This is useful when you want samples or features colored by a known grouping variable, such as treatment or batch. Choose None if you want a plain heatmap.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = ann_choices,
                multiple = FALSE,
                selected = shiny::isolate(userState$hm_annotation) %||% ""
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                "hm_ann_side",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Annotation Side",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Annotation Side</span>"
                  ),
                  content = "Choose where the annotation bar is attached. Auto lets the app decide whether the annotation belongs on rows or columns based on the data shape. Override it only if the annotation is being placed on the wrong side.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("Auto" = "auto", "Row" = "row", "Column" = "col"),
                selected = shiny::isolate(userState$hm_ann_side) %||% "auto"
              )
            )
          )
        )
      },
      # a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      # Correlation Plots
      # a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      "Correlation Plots" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }

        cols <- safe_names(df)
        cols <- cols[cols != "..cyto_id.."]
        num_cols <- cols[sapply(df[cols], is.numeric)]
        none_choice <- c("None (no grouping)" = "")

        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "corr_target",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Response Variable",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Response Variable</span>"
                  ),
                  content = "Choose the numeric variable that will be compared with every other numeric feature. Use the measurement you care most about as the response, then the plot will show which other variables move with it.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = num_cols,
                selected = shiny::isolate(userState$corr_target) %||%
                  num_cols[1]
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "corr_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Stratification (optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Stratification Variable</span>"
                  ),
                  content = "Optionally choose a grouping column if you want correlations calculated within separate subgroups. This helps answer questions like whether a correlation looks different in treated versus untreated samples.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c(none_choice, setNames(cols, cols)),
                selected = shiny::isolate(userState$corr_group_col) %||% ""
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "corr_by_group",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Per-group Heatmaps",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Per-group Heatmaps</span>"
                  ),
                  content = "Show a separate correlation heatmap for each level of the selected grouping column. Turn this on only when you want subgroup-specific patterns. Leave it off for one overall correlation view.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$corr_by_group) %||% FALSE
              )
            )
          )
        )
      },

      # a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      # PCA
      # a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      "Principal Component Analysis (PCA)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)
        ui_list <- shiny::tagList(
          # Row 1: grouping columns
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "pca_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "PCA Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>PCA Comparison Column</span>"
                  ),
                  content = "Choose the main categorical column used to color or separate samples in the PCA plots. This does not change the PCA calculation itself, but it helps you see whether known groups cluster apart after dimension reduction.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$pca_group_col) %||% cols[1]
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "pca_group_col2",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "PCA Stratification Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>PCA Stratification Column</span>"
                  ),
                  content = "Choose an optional second grouping column if you want another visual layer, such as shapes within colors. Use the same column as the main comparison column if you do not want a second level of labeling.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$pca_group_col2) %||% cols[1]
              )
            )
          ),

          # Row 2: components & colors
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "pca_comp_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Choose how many principal components the PCA should calculate. For a standard 2D score plot, 2 components are enough. Choose at least 3 if you want a 3D plot or want to inspect more than the first two major patterns in the data.<br><br>",
                    "Method reference: <a href='https://doi.org/10.1016/0098-3004(93)90090-R' target='_blank' rel='noopener noreferrer'>Mackiewicz and Ratajczak (1993)</a>."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$pca_comp_num) %||% 2,
                min = 2
              )
            ),
            shiny::column(
              6,
              shiny::selectizeInput(
                "pca_colors",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Select Colors for PCA Plot (Optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Select Colors for PCA Plot (Optional)</span>"
                  ),
                  content = "Choose custom colors for the PCA groups. If you leave this empty, the app will generate colors automatically. If you set your own colors, try to provide enough for all levels in the main grouping column.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = allowed_colors,
                multiple = TRUE,
                options = list(
                  placeholder = "Select Colors (Optional)",
                  plugins = c("remove_button", "restore_on_backspace")
                )
              )
            )
          ),

          # Row 3: transformations & ellipse
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "pca_ellipse",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Draw Ellipse",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Draw Ellipse</span>"
                  ),
                  content = "Draw a 95% ellipse around each group in the PCA score plot. This gives a quick visual summary of where most samples in a group fall and how much the groups overlap.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$pca_ellipse) %||% FALSE
              )
            )
          ),

          # Row 4: style & symbols
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "pca_style",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot Style",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plot Style</span>"
                  ),
                  content = "Choose whether the PCA score plot is shown in 2D or 3D. Use 2D for a simpler figure. Use 3D only if you need to see separation along a third component and have calculated at least 3 components.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("2D", "3D"),
                selected = shiny::isolate(userState$pca_style) %||% "2D"
              )
            ),
            shiny::column(
              6,
              shiny::selectizeInput(
                "pca_pch",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plotting Symbols",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plotting Symbols</span>"
                  ),
                  content = "Choose the point shapes used in the PCA score plot. This matters most when you use a second grouping column, because different shapes help distinguish subgroups within the same color.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = pch_choices,
                selected = shiny::isolate(
                  userState$pca_pch %||% pch_choices[c(17, 5)]
                ),
                multiple = TRUE,
                options = list(
                  placeholder = "Select Symbols",
                  plugins = c("remove_button", "restore_on_backspace"),
                  create = TRUE
                )
              )
            )
          )
        )
      },

      # a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      # Partial Least Squares Regression
      # a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      "Partial Least Squares Regression (PLSR)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)
        cols <- cols[cols != "..cyto_id.."]

        # Adding a None option
        none_choice <- c("None (no grouping)" = "") # empty-string sentinel
        grp_choices <- c(none_choice, setNames(cols, cols)) # prepend None

        num_cols <- names(df)[sapply(df, is.numeric)]
        num_cols <- num_cols[num_cols != "..cyto_id.."]
        default_num_vars <- sum(sapply(df, is.numeric))
        if (!isTRUE(userState$plsr_keepX_manual)) {
          userState$plsr_keepX <- default_num_vars
        }
        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "plsr_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Grouping Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Grouping Column</span>"
                  ),
                  content = "Optionally choose a categorical column to color samples in the PLSR score plot. This is for visual interpretation only and helps you see whether known groups line up with the model structure.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = grp_choices,
                selected = shiny::isolate(userState$plsr_group_col) %||% ""
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "plsr_response_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Response Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Response Column</span>"
                  ),
                  content = "Choose the numeric outcome you want the PLSR model to predict. This should be the main response variable of interest, such as a concentration, score, or clinical measurement.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = num_cols,
                selected = shiny::isolate(userState$plsr_response_col) %||%
                  num_cols[1]
              )
            )
          ),
          shiny::fluidRow(shiny::column(
            12,
            shiny::selectizeInput(
              "plsr_predictor_cols",
              label = shinyhelper::helper(
                type = "inline",
                title = "Predictor Columns",
                icon = "fas fa-question-circle",
                shiny_tag = shiny::HTML(
                  "<span style='margin-right: 15px;'>Predictor Columns</span>"
                ),
                content = "Choose which numeric variables are used as predictors in the model. Any numeric column you do not select here will be excluded. Leave the full default selection in place if you want the model to use all eligible numeric predictors except the response column.",
                colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                  "red"
                } else {
                  "blue"
                }
              ),
              choices = num_cols,
              selected = shiny::isolate(userState$plsr_predictor_cols) %||%
                num_cols,
              multiple = TRUE,
              options = list(
                placeholder = "Select predictors (default: all numeric)",
                plugins = c("remove_button", "restore_on_backspace")
              )
            )
          )),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "plsr_comp_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = "Choose how many latent components the PLSR model should extract. More components let the model capture more structure, but too many can make the result harder to interpret and may start modeling noise.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$plsr_comp_num) %||% 2,
                min = 2
              )
            ),
            # Add a checkbox for sparse PLSR
            shiny::column(
              6,
              shiny::checkboxInput(
                "plsr_sparse",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Sparse PLSR",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Sparse PLSR</span>"
                  ),
                  content = "Turn this on to build a sparse PLSR model that keeps only a subset of predictors on each component. This is useful when you want a smaller, easier-to-interpret variable set instead of using every predictor.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$plsr_sparse) %||% FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              # Conditional to show keepX only if sparse is checked
              shiny::conditionalPanel(
                condition = "input.plsr_sparse == true",
                shiny::numericInput(
                  "plsr_keepX",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Number of Variables",
                    icon = "fas fa-exclamation-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Number of Variables</span>"
                    ),
                    content = "Choose how many predictors are kept on each component when sparse PLSR is enabled. Lower values force stronger variable selection and produce a simpler model. Higher values keep more predictors and act more like standard PLSR.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = shiny::isolate(userState$plsr_keepX),
                  1
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "plsr_ellipse",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Ellipse",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Ellipse</span>"
                  ),
                  content = "Draw 95% ellipses around groups in the PLSR score plot when a grouping column is selected. This helps you see whether groups occupy distinct regions of the plot or strongly overlap.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$plsr_ellipse) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::selectizeInput(
                "plsr_colors",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Colors (optionnal)",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Colors (optional)</span>"
                  ),
                  content = "Choose custom colors for the PLSR score plot groups. If you leave this empty, the app will assign colors automatically.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = allowed_colors, # already defined globally
                selected = shiny::isolate(userState$plsr_colors),
                multiple = TRUE,
                options = list(placeholder = "Pick colors (optional)")
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::tagList(
                shiny::radioButtons(
                  "plsr_cv_opt",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Cross-validation",
                    icon = "fas fa-exclamation-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Cross-validation</span>"
                    ),
                    content = "Choose whether to estimate model stability with cross-validation. LOOCV uses almost all samples for training each time and is useful for small datasets. Mfold splits the data into a smaller number of repeated train/test parts and is often faster.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = c("None", "LOOCV", "Mfold"),
                  selected = shiny::isolate(userState$plsr_cv_opt) %||% "None",
                  inline = TRUE
                ),
                shiny::conditionalPanel(
                  condition = "input.plsr_cv_opt == 'Mfold'",
                  shiny::numericInput(
                    "plsr_fold_num",
                    label = helper_label(
                      "Number of Folds",
                      "PLSR Cross-Validation Folds",
                      "Choose how many parts the data are split into when Mfold cross-validation is selected. More folds use more data for training in each run but take longer. Five or ten folds are common starting points."
                    ),
                    value = shiny::isolate(userState$plsr_fold_num) %||% 5,
                    min = 2
                  )
                )
              )
            )
          )
        )
      },
      # a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      # Random Forest
      # a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      "Random Forest" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        ui_list <- shiny::tagList(
          # Row 1: grouping & ntrees
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "rf_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Grouping Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Grouping Column</span>"
                  ),
                  content = "Choose the categorical column the Random Forest model should learn to predict. This is the outcome or class label, such as case versus control or treatment group.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$rf_group_col) %||% cols[1]
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "rf_ntree",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Trees",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Number of Trees</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Choose how many trees are grown in the Random Forest. More trees usually make the model more stable, but they also increase runtime. The default is a sensible starting point for most datasets.<br><br>",
                    "Method reference: <a href='https://doi.org/10.1023/A:1010933404324' target='_blank' rel='noopener noreferrer'>Breiman (2001)</a>."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$rf_ntree) %||% 500,
                min = 1
              )
            )
          ),

          # Row 2: mtry & train fraction
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "rf_mtry",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Variables to Split",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Number of Variables to Split</span>"
                  ),
                  content = "Choose how many predictors the model is allowed to consider at each split inside each tree. Smaller values add more randomness between trees. Larger values let each split consider more variables and can sometimes improve accuracy, but they also reduce the diversity of the forest.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$rf_mtry) %||% 5,
                min = 1
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "rf_train_fraction",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Train Fraction",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Train Fraction</span>"
                  ),
                  content = "Choose what fraction of the samples is used to train the model. The remaining samples are held back for testing. Larger training fractions give the model more data to learn from, while smaller fractions leave more data for an honest holdout check.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$rf_train_fraction) %||% 0.7,
                min = 0.1,
                max = 0.9,
                step = 0.1
              )
            )
          ),

          # Row 3: ROC & RFCV toggle
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "rf_plot_roc",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Plot ROC (Binary Comparison Only)</span>"
                  ),
                  content = "Show an ROC curve for model performance. This is only meaningful for two-class problems and helps you see how well the model separates the two groups across different thresholds.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$rf_plot_roc) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "rf_run_rfcv",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Run RFCV?",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Run RFCV</span>"
                  ),
                  content = "Run recursive feature elimination with cross-validation to see how performance changes as the model uses fewer predictors. Turn this on if you want to explore whether a smaller feature set performs almost as well as the full model.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$rf_run_rfcv) %||% FALSE
              )
            )
          ),

          # Row 4: RFCV parameters (conditional)
          shiny::conditionalPanel(
            condition = "input.rf_run_rfcv == true",
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::numericInput(
                  "rf_k_folds",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Number of Folds",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right:15px;'>Number of Folds</span>"
                    ),
                    content = "Choose how many folds are used in the RFCV feature-selection procedure. More folds generally give a more stable estimate, but they also increase runtime.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = shiny::isolate(userState$rf_k_folds) %||% 5,
                  min = 2
                )
              ),
              shiny::column(
                6,
                shiny::numericInput(
                  "rf_step",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Step Size",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right:15px;'>Step Size</span>"
                    ),
                    content = "Choose how aggressively predictors are removed during RFCV. Smaller step sizes remove features more gradually and give a finer search, while larger step sizes are faster but coarser.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = shiny::isolate(userState$rf_step) %||% 0.5,
                  min = 0.1,
                  max = 0.9,
                  step = 0.1
                )
              )
            )
          )
        )
      },
      # ------------------------
      # Skewness/Kurtosis
      # ------------------------
      "Skewness/Kurtosis" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "skku_group_cols",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Grouping Columns",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Grouping Columns</span>"
                  ),
                  content = "Choose one or more categorical columns if you want skewness and kurtosis summarized within groups. Leave this as one grouping variable for a simple split, or add more if you need a more detailed breakdown.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$skku_group_cols),
                multiple = TRUE
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "skku_print_raw",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Print Raw Results",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Print Raw Results</span>"
                  ),
                  content = "Show skewness and kurtosis calculated from the raw values. Turn this on if you want to understand the shape of the data before any log transformation.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$skku_print_raw) %||% FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "skku_print_log",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Print Log-Transformed Results",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Print Log-Transformed Results</span>"
                  ),
                  content = "Show skewness and kurtosis after log transformation. This is useful when you want to check whether logging the data makes the distributions look more symmetric.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$skku_print_log) %||% FALSE
              )
            ),
            shiny::column(6) # placeholder
          )
        )
      },
      # ------------------------
      # sPLS-DA
      # ------------------------
      "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        default_num_vars <- sum(sapply(df, is.numeric))
        if (!isTRUE(userState$splsda_var_num_manual)) {
          userState$splsda_var_num <- default_num_vars
        }
        cols <- safe_names(df)
        ui_list <- shiny::tagList(
          # Row 1: grouping columns
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "splsda_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "sPLS-DA Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>sPLS-DA Comparison Column</span>"
                  ),
                  content = "Choose the categorical column that defines the classes the sPLS-DA model should separate. This is the main outcome or grouping variable, such as treatment group or disease status.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$splsda_group_col) %||%
                  cols[1]
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "splsda_group_col2",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "sPLS-DA Stratification Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>sPLS-DA Stratification Column</span>"
                  ),
                  content = "Choose an optional second grouping column if you want extra visual separation, such as shapes within colors. If you do not need that extra layer, use the same column as the main comparison column.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$splsda_group_col2) %||%
                  cols[1]
              )
            )
          ),

          # batch_col column based on a conditional check to do batch correction similar to multilevel
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_use_batch_corr",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Perform Batch Correction?",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Perform Batch Correction?</span>"
                  ),
                  content = "Turn this on if your samples come from different batches, plates, studies, or experimental runs and you want to reduce those unwanted technical differences. Use it only when the batch label is known and meaningful.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_use_batch_corr) %||%
                  FALSE
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "input.splsda_use_batch_corr == true",
                shiny::selectInput(
                  "splsda_batch_col",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Multilevel Column",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Select Batch Column</span>"
                    ),
                    content = "Choose the column that records the batch, study, run, or plate for each sample. The model uses this column only when batch correction is enabled.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = cols,
                  selected = shiny::isolate(userState$splsda_batch_col) %||%
                    cols[1]
                )
              )
            )
          ),
          # Row 2: multilevel toggle + selector
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_use_multilevel",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Perform Multilevel Analysis?",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Perform Multilevel Analysis?</span>"
                  ),
                  content = "Turn this on when the same subject or experimental unit was measured more than once. This helps the model focus on within-subject changes instead of treating repeated measurements as fully independent samples.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_use_multilevel) %||%
                  FALSE
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "input.splsda_use_multilevel == true",
                shiny::selectInput(
                  "splsda_multilevel",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Multilevel Column",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Select Repeated Measures Column</span>"
                    ),
                    content = "Choose the column that identifies repeated measurements from the same subject or unit, such as Patient ID or Animal ID. Use this only when multilevel analysis is turned on.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = cols,
                  selected = shiny::isolate(userState$splsda_multilevel) %||%
                    cols[1]
                )
              )
            )
          ),

          # Row 3: sparsity & colors
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "splsda_var_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Variables",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Number of Variables</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Choose how many predictors are kept on each component. Lower values create a smaller feature set that is easier to interpret, while higher values keep more variables.<br><br>",
                    "Method reference: <a href='https://doi.org/10.1186/1471-2105-12-253' target='_blank' rel='noopener noreferrer'>Le Cao, Boitard, and Besse (2011)</a>."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_var_num),
                min = 1
              )
            ),
            shiny::column(
              6,
              shiny::selectizeInput(
                "splsda_colors",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Select Colors (Optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Select Colors for sPLS-DA Plot (Optional)</span>"
                  ),
                  content = "Choose custom colors for the sPLS-DA groups. If you leave this empty, the app will assign colors automatically. If you set colors manually, try to supply enough for every class.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = allowed_colors,
                multiple = TRUE,
                options = list(
                  placeholder = "Select Colors (Optional)",
                  plugins = c("remove_button", "restore_on_backspace")
                )
              )
            )
          ),

          # Row 4: CV options & fold number
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "splsda_cv_opt",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Cross-Validation Option",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Cross-Validation Option</span>"
                  ),
                  content = "Choose whether to estimate how stable the classifier is with cross-validation. LOOCV is useful for small datasets. Mfold is often faster and splits the data into a chosen number of parts.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("None", "LOOCV", "Mfold"),
                selected = shiny::isolate(userState$splsda_cv_opt) %||% "None"
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "input.splsda_cv_opt == 'Mfold'",
                shiny::numericInput(
                  "splsda_fold_num",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Number of Folds",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Number of Folds</span>"
                    ),
                    content = "Choose how many parts the data are split into when Mfold cross-validation is used. More folds can give a more stable estimate, but they also take longer to run.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = shiny::isolate(userState$splsda_fold_num) %||% 5,
                  min = 2
                )
              )
            )
          ),

          # Row 5: log2 & components
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "splsda_comp_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = "Choose how many components the model should calculate. Two components are enough for most 2D plots. Use three or more only if you need more model structure or a 3D view.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_comp_num) %||% 2,
                min = 2
              )
            ),
            shiny::column(
              6,
              shiny::radioButtons(
                "splsda_ind_names_mode",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot Individual Names?",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plot Individual Names?</span>"
                  ),
                  content = "Choose whether individual samples should be labeled on the score plot. Leave this off for a cleaner plot. Use row names or a selected column when you need to identify specific samples.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c(
                  "Off" = "off",
                  "Row names" = "rownames",
                  "Column" = "column"
                ),
                selected = shiny::isolate(userState$splsda_ind_names_mode) %||%
                  "off",
                inline = TRUE
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "input.splsda_ind_names_mode === 'column'",
                shiny::selectInput(
                  "splsda_ind_names_col",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Label Column",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Label Column</span>"
                    ),
                    content = "Choose the column whose values should be used as point labels. Pick something short and recognizable, such as sample ID.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = cols,
                  selected = shiny::isolate(userState$splsda_ind_names_col) %||%
                    NULL
                )
              )
            )
          ),
          # Row 6: symbols & plot style
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectizeInput(
                "splsda_pch",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plotting Symbols",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plotting Symbols</span>"
                  ),
                  content = "Choose the point shapes used in the sPLS-DA plot. Shapes are most helpful when you use a second grouping column and want a visual cue beyond color.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = pch_choices,
                selected = shiny::isolate(
                  userState$splsda_pch %||% pch_choices[c(17, 5)]
                ),
                multiple = TRUE,
                options = list(
                  placeholder = "Select PCH Values",
                  plugins = c("remove_button", "restore_on_backspace"),
                  create = TRUE
                )
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "splsda_style",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot Style",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plot Style</span>"
                  ),
                  content = "Choose whether the score plot is drawn in 2D or 3D. Use 2D for the clearest default view. Use 3D only when you have at least 3 components and need to inspect separation along the third one.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("2D", "3D"),
                selected = shiny::isolate(userState$splsda_style) %||% "2D"
              )
            )
          ),

          # Row 7: ROC & ellipse
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_roc",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plot ROC</span>"
                  ),
                  content = "Show ROC curves to summarize classification performance. This is most useful for two-class problems, where it helps you judge how well the model separates the groups.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_roc) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_ellipse",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Draw Ellipse",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Draw Ellipse</span>"
                  ),
                  content = "Draw a 95% ellipse around each group in the score plot. This gives a quick visual summary of group spread and overlap.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_ellipse) %||% FALSE
              )
            )
          ),

          # Row 8: background & confusion matrix
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_bg",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Shaded Background",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Shaded Background Prediction</span>"
                  ),
                  content = "Draw shaded background regions showing the model's predicted class areas. This can make the decision boundaries easier to see, but it also adds visual complexity.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_bg) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "splsda_conf_mat",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Confusion Matrix",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Confusion Matrix</span>"
                  ),
                  content = "Show a confusion matrix comparing the true class of each sample with the class predicted by the model. Use this when you want a simple summary of which groups are classified well and which ones are commonly confused.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$splsda_conf_mat) %||% FALSE
              )
            )
          ),
          NULL
        )
      },
      "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        default_num_vars <- sum(sapply(df, is.numeric))
        if (!isTRUE(userState$mint_splsda_var_num_manual)) {
          userState$mint_splsda_var_num <- default_num_vars
        }

        ui_list <- shiny::tagList(
          # Row 1: Grouping Columns
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "mint_splsda_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "MINT sPLS-DA Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>MINT sPLS-DA Comparison Column</span>"
                  ),
                  content = "Choose the categorical column that defines the classes the MINT sPLS-DA model should separate. This is the main outcome or grouping variable you want the model to learn.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$mint_splsda_group_col) %||%
                  cols[1]
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "mint_splsda_group_col2",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "MINT sPLS-DA Stratification Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>MINT sPLS-DA Stratification Column</span>"
                  ),
                  content = "Choose an optional second grouping column if you want the analysis repeated within each level of another variable. Use this when you want separate MINT results by subgroup. If you do not need that, leave it at None.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("None" = NULL, cols),
                selected = shiny::isolate(userState$mint_splsda_group_col2) %||%
                  NULL
              )
            )
          ),
          # Row 2: Batch and Variable Number
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "mint_splsda_batch_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Batch Column (Study)",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Batch Column</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Choose the column that identifies the study, batch, cohort, or platform each sample came from. MINT uses this information to integrate multiple experiments while keeping those sources separate during modeling.<br><br>",
                    "Method reference: Rohart, Eslami, Matigian, Bougeard, and Le Cao (2017), <i>BMC Bioinformatics</i> 18:128."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$mint_splsda_batch_col) %||%
                  cols[2]
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "mint_splsda_var_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Variables",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Number of Variables to Select</span>"
                  ),
                  content = "Choose how many predictors are kept on each component. Lower values produce a smaller and easier-to-interpret biomarker set. Higher values keep more variables in the model.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_var_num),
                min = 1
              )
            )
          ),
          # Row 3: Components and Colors
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "mint_splsda_comp_num",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = "Choose how many components the model should calculate. Two components are enough for most standard score plots. Use more only when you need to explore additional structure.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_comp_num) %||% 2,
                min = 2
              )
            ),
            shiny::column(
              6,
              shiny::selectizeInput(
                "mint_splsda_colors",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Select Colors (Optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Select Colors for Groups</span>"
                  ),
                  content = "Choose custom colors for the groups shown in the MINT plots. If you leave this empty, the app will assign colors automatically. If you set colors yourself, provide enough for all groups.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = allowed_colors,
                multiple = TRUE,
                options = list(
                  placeholder = "Select Colors (Optional)",
                  plugins = c("remove_button", "restore_on_backspace")
                )
              )
            )
          ),
          # Row 4: PCH and log2
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "mint_splsda_cim",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Draw a Clustered Image Map (CIM)?",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Draw a Clustered Image Map?</span>"
                  ),
                  content = "Show a clustered heatmap of the selected features. Turn this on when you want to see whether samples and variables group together visually beyond the score plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_cim) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "mint_splsda_ellipse",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Draw Ellipse",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Draw Ellipse</span>"
                  ),
                  content = "Draw a 95% ellipse around each group in the sample plots. This gives a quick visual summary of group spread and overlap.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_ellipse) %||% FALSE
              )
            )
          ),
          # Row 5: Final Toggles
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "mint_splsda_roc",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Plot ROC Curve</span>"
                  ),
                  content = "Show ROC curves to summarize classification performance. This is most informative for two-class problems, where it helps you judge how well the model separates the groups.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_roc) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "mint_splsda_bg",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Draw Prediction Background",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right: 15px;'>Draw Background</span>"
                  ),
                  content = "Draw shaded background regions that show the model's predicted class areas. Use this when you want an easier visual sense of the decision regions, and leave it off for a cleaner plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$mint_splsda_bg) %||% FALSE
              )
            )
          )
        )
      },
      # ------------------------
      # Volcano Plot
      # ------------------------
      "Volcano Plot" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        ui_list <- shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "volc_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Comparison Column</span>"
                  ),
                  content = "Choose the categorical column that defines the two groups compared in the volcano plot. The condition selectors below will let you pick which two levels of this column are compared.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$volc_group_col) %||% cols[1]
              )
            ),
            shiny::column(
              6,
              shiny::uiOutput(
                "volc_conditions_ui"
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "volc_fold_change_thresh",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Log2 FC Threshold",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Log2 Fold Change Threshold</span>"
                  ),
                  content = "Set the minimum absolute log2 fold change needed for a feature to be highlighted. A value of 1 means about a two-fold change between groups. Higher values focus on larger shifts.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$volc_fold_change_thresh) %||% 1
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "volc_p_value_thresh",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "P-Value Threshold",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>P-Value Threshold</span>"
                  ),
                  content = "Set the adjusted p-value cutoff used to flag statistically significant features. Smaller values are stricter. The common starting point is 0.05.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$volc_p_value_thresh) %||% 0.05
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "volc_top_labels",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Top Labels",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Top Labels</span>"
                  ),
                  content = "Choose how many of the most prominent features receive text labels on the plot. More labels show more names but can clutter the figure. Fewer labels keep the plot cleaner.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$volc_top_labels) %||% 15
              )
            )
          )
        )
      },
      # a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      # Extreme Gradient Boosting (XGBoost)
      # a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"a?"
      "Extreme Gradient Boosting (XGBoost)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- safe_names(df)

        ui_list <- shiny::tagList(
          # Row 1: grouping & train frac
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                "xgb_group_col",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Grouping Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Grouping Column</span>"
                  ),
                  content = "Choose the categorical column the XGBoost model should learn to predict. This is the outcome or class label, such as case versus control or treatment group.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = shiny::isolate(userState$xgb_group_col) %||% cols[1]
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "xgb_train_fraction",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Train Fraction",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Train Fraction</span>"
                  ),
                  content = "Choose what fraction of the samples is used to train the model. The remaining samples are held back for testing. Larger fractions give the model more training data, while smaller fractions leave more data for the holdout check.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_train_fraction) %||% 0.7,
                min = 0.1,
                max = 0.9,
                step = 0.1
              )
            )
          ),
          # Row 2: rounds & max_depth
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "xgb_nrounds",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Number of Rounds",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Number of Rounds</span>"
                  ),
                  content = shiny::HTML(paste0(
                    "Choose how many boosting rounds the model runs. More rounds can improve performance, but they also increase runtime and can overfit if set too high.<br><br>",
                    "Method reference: <a href='https://doi.org/10.1145/2939672.2939785' target='_blank' rel='noopener noreferrer'>Chen and Guestrin (2016)</a>."
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_nrounds) %||% 500,
                min = 100
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                "xgb_max_depth",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Maximum Depth",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Maximum Depth</span>"
                  ),
                  content = "Choose how deep each tree is allowed to grow. Shallower trees are simpler and less likely to overfit. Deeper trees can capture more complex patterns but may fit noise.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_max_depth) %||% 6,
                min = 1
              )
            )
          ),
          # Row 3: eta & eval metric
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "xgb_eta",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Learning Rate",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Learning Rate</span>"
                  ),
                  content = "Choose how quickly the model updates itself at each boosting round. Smaller values learn more cautiously and often need more rounds. Larger values learn faster but can overshoot good solutions.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_eta) %||% 0.1,
                min = 0,
                step = 0.01
              )
            ),
            shiny::column(
              6,
              shiny::selectInput(
                "xgb_eval_metric",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Evaluation Metric",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Evaluation Metric</span>"
                  ),
                  content = "Choose the metric used to judge model performance during training. Use mlogloss for general probabilistic classification, especially with more than two classes. Use AUC when you specifically care about binary-class separation.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("mlogloss", "auc"),
                selected = shiny::isolate(userState$xgb_eval_metric) %||%
                  "mlogloss"
              )
            )
          ),
          # Row 4: top features & ROC plot
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                "xgb_top_n_features",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Top Number of Features",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Top Number of Features</span>"
                  ),
                  content = "Choose how many of the most important predictors are shown in the feature-importance output. Higher values show more of the ranking but can make the display busier.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_top_n_features) %||% 10,
                min = 1
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                "xgb_plot_roc",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Plot ROC (Binary Only)</span>"
                  ),
                  content = "Show an ROC curve for the fitted XGBoost model. This is only meaningful for two-class problems and helps summarize how well the model separates the two groups.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_plot_roc) %||% FALSE
              )
            )
          ),
          # Row 5: CV toggle & folds (conditional)
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                "xgb_cv",
                label = shinyhelper::helper(
                  type = "inline",
                  title = "Cross-Validation",
                  icon = "fas fa-question-circle",
                  shiny_tag = shiny::HTML(
                    "<span style='margin-right:15px;'>Cross-Validation</span>"
                  ),
                  content = "Turn this on to estimate model performance with cross-validation instead of relying only on one train/test split. This gives a more stable performance estimate, but it also takes longer to run.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = shiny::isolate(userState$xgb_cv) %||% FALSE
              )
            ),
            shiny::column(
              6,
              shiny::conditionalPanel(
                condition = "input.xgb_cv == true",
                shiny::numericInput(
                  "xgb_nfold",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Number of Folds",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right:15px;'>Number of Folds</span>"
                    ),
                    content = "Choose how many parts the data are split into during cross-validation. More folds usually give a more stable estimate, but they increase runtime.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = shiny::isolate(userState$xgb_nfold) %||% 5,
                  min = 2
                )
              )
            )
          )
        )
      }
    )
    font_controls <- build_font_controls_ui(func_name)
    if (!is.null(font_controls)) {
      ui_list <- append(ui_list, list(font_controls))
    }
    do.call(shiny::tagList, ui_list)
  })

  # 2) Auto-sync in Step 3/4 based purely on what was checked in Step 2
  shiny::observe({
    # Grab exactly what the user checked in Step 2
    cols_checked <- input$selected_numerical_cols %||% character(0)
    default_num_vars <- length(cols_checked)

    # If they haven't manually typed a value, update it
    if (!isTRUE(userState$splsda_var_num_manual)) {
      shiny::updateNumericInput(
        session,
        "splsda_var_num",
        value = default_num_vars
      )
    }
    if (!isTRUE(userState$mint_splsda_var_num_manual)) {
      shiny::updateNumericInput(
        session,
        "mint_splsda_var_num",
        value = default_num_vars
      )
    }
    if (!isTRUE(userState$plsr_keepX_manual)) {
      shiny::updateNumericInput(
        session,
        "plsr_keepX",
        value = default_num_vars
      )
    }
  })

  # 3) As soon as they type their own number, stop auto-syncing
  shiny::observeEvent(
    input$splsda_var_num,
    {
      userState$splsda_var_num_manual <- TRUE
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$mint_splsda_var_num,
    {
      userState$mint_splsda_var_num_manual <- TRUE
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(
    input$plsr_keepX,
    {
      userState$plsr_keepX_manual <- TRUE
    },
    ignoreInit = TRUE
  )

  output$df_conditions_ui <- shiny::renderUI({
    shiny::req(filteredData())
    shiny::req(input$df_group_var)
    df <- filteredData()
    choices <- unique(df[[input$df_group_var]])
    if (length(choices) < 2) {
      return(shiny::helpText("Not enough unique levels in grouping column"))
    }

    # Use shiny::fluidRow and column to place inputs side-by-side
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::selectInput(
          "df_cond1",
          label = shinyhelper::helper(
            type = "inline",
            title = "Condition 1",
            icon = "fas fa-question-circle",
            shiny_tag = shiny::HTML(
              "<span style='margin-right: 15px;'>Condition 1</span>"
            ),
            content = "Choose the first group level used in the comparison. The plot will compare this level against Condition 2.",
            colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
              "red"
            } else {
              "blue"
            }
          ),
          choices = choices,
          selected = shiny::isolate(userState$df_cond1) %||% choices[1]
        ),
        shiny::selectInput(
          "df_cond2",
          label = shinyhelper::helper(
            type = "inline",
            title = "Condition 2",
            icon = "fas fa-question-circle",
            shiny_tag = shiny::HTML(
              "<span style='margin-right: 15px;'>Condition 2</span>"
            ),
            content = "Choose the second group level used in the comparison. The plot will compare this level against Condition 1.",
            colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
              "red"
            } else {
              "blue"
            }
          ),
          choices = choices,
          selected = shiny::isolate(userState$df_cond2) %||% choices[2]
        )
      )
    )
  })

  output$volc_conditions_ui <- shiny::renderUI({
    shiny::req(filteredData())
    shiny::req(input$volc_group_col)
    df <- filteredData()
    choices <- unique(df[[input$volc_group_col]])
    if (length(choices) >= 2) {
      # Use shiny::fluidRow and column to place inputs side-by-side
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::selectInput(
            "volc_cond1",
            label = shinyhelper::helper(
              type = "inline",
              title = "Condition 1",
              icon = "fas fa-question-circle",
              shiny_tag = shiny::HTML(
                "<span style='margin-right: 15px;'>Condition 1</span>"
              ),
              content = "Choose the first group level used in the volcano comparison. The plot will compare this level against Condition 2.",
              colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                "red"
              } else {
                "blue"
              }
            ),
            choices = choices,
            selected = shiny::isolate(userState$volc_cond1) %||% choices[1]
          ),
          shiny::selectInput(
            "volc_cond2",
            label = shinyhelper::helper(
              type = "inline",
              title = "Condition 2",
              icon = "fas fa-question-circle",
              shiny_tag = shiny::HTML(
                "<span style='margin-right: 15px;'>Condition 2</span>"
              ),
              content = "Choose the second group level used in the volcano comparison. The plot will compare this level against Condition 1.",
              colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                "red"
              } else {
                "blue"
              }
            ),
            choices = choices,
            selected = shiny::isolate(userState$volc_cond2) %||% choices[2]
          )
        )
      )
    } else {
      shiny::helpText(
        "Selected comparison variable does not have at least two unique values."
      )
    }
  })

  invisible(app_stage_commit(app_ctx, stage_env))
}

init_navigation_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  userState <- app_ctx$userState
  bioplex <- app_ctx$bioplex
  analysis_font_specs <- app_ctx$analysis_font_specs
  currentStep <- app_ctx$currentStep
  selected_function <- app_ctx$selected_function
  selected_stat_func <- app_ctx$selected_stat_func
  selected_exploratory_func <- app_ctx$selected_exploratory_func
  selected_multivariate_func <- app_ctx$selected_multivariate_func
  selected_ml_func <- app_ctx$selected_ml_func
  dev_notice_shown <- app_ctx$dev_notice_shown
  userData <- app_ctx$userData
  step2_typed_data <- app_ctx$step2_typed_data
  step2_typed_col_info <- app_ctx$step2_typed_col_info
  imputed_data <- app_ctx$imputed_data
  get_analysis_font_spec <- app_ctx$get_analysis_font_spec
  font_settings_state_from_inputs <- app_ctx$font_settings_state_from_inputs
  ## ---------------------------
  ## Sidebar Navigation
  ## ---------------------------
  # 1) Track which page is currently active.
  currentPage <- shiny::reactiveVal("home")
  shiny::observe({
    # clear all
    for (id in c(
      "nav_home",
      "nav_tutorials",
      "nav_start",
      "nav_upload",
      "nav_filter",
      "nav_options",
      "nav_args",
      "nav_results",
      "nav_news",
      "nav_contact"
    )) {
      shinyjs::removeClass(id, "active")
    }
    # add to the one by name
    page <- currentPage()
    btn <- switch(
      page,
      "home" = "nav_home",
      "tutorials" = "nav_tutorials",
      "step1" = "nav_upload", # Upload Data
      "step2" = "nav_filter", # Select & Filter
      "step3" = "nav_options", # Analysis Options
      "step4" = "nav_args", # Analysis Arguments
      "step5" = "nav_results", # Results
      "news" = "nav_news",
      "contact" = "nav_contact",
      NULL
    )
    if (!is.null(btn)) shinyjs::addClass(btn, "active")
  })
  # 2) Wire sidebar buttons into it:
  shiny::observeEvent(input$nav_home, {
    currentPage("home")
  })
  shiny::observeEvent(input$nav_tutorials, {
    currentPage("tutorials")
  })
  shiny::observeEvent(input$nav_start_home, {
    currentPage("step1")
    shinyjs::toggle("nav_submenu")
  })
  shiny::observeEvent(input$nav_start, {
    currentPage("step1")
    shinyjs::toggle("nav_submenu")
  })
  # then observe the real nav buttons as you already do
  shiny::observeEvent(input$nav_upload, {
    currentPage("step1")
    currentStep(1)
  })
  shiny::observeEvent(input$nav_filter, {
    currentPage("step2")
    currentStep(2)
  })
  shiny::observeEvent(input$nav_options, {
    currentPage("step3")
    currentStep(3)
  })
  shiny::observeEvent(input$nav_args, {
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$nav_results, {
    currentPage("step5")
    currentStep(5)
  })
  shiny::observe({
    if (currentPage() %in% paste0("step", 1:5)) {
      shinyjs::show("nav_submenu")
    } else {
      shinyjs::hide("nav_submenu")
    }
  })

  shiny::observeEvent(input$nav_news, {
    currentPage("news")
  })
  shiny::observeEvent(input$nav_contact, {
    currentPage("contact")
  })

  shiny::observeEvent(input$back2, {
    currentPage("step1")
    currentStep(1)
  })
  shiny::observeEvent(
    input$next2,
    {
      currentPage("step3")
      currentStep(3)
      userState$selected_columns <- selected_columns_combined()
    }
  )
  shiny::observeEvent(input$back3, {
    currentPage("step2")
    currentStep(2)
  })

  shiny::observeEvent(input$back4, {
    currentPage("step3")
    currentStep(3)
  })
  shiny::observeEvent(input$next4, {
    currentPage("step5")
    currentStep(5)
  })
  shiny::observeEvent(input$back5, {
    currentPage("step4")
    currentStep(4)
  })

  resetState <- function() {
    # General state
    userState$selected_columns = NULL
    userState$selected_categorical_cols = NULL
    userState$selected_numerical_cols = NULL

    # Built-in data tracking:
    userState$use_builtin = FALSE
    userState$built_in_choice = NULL

    # Step 2 preprocessing
    userState$step2_scale = "none"
    userState$step2_factor_cols = NULL
    userState$step2_numeric_override_cols = NULL
    userState$step2_factor_order_enable = FALSE
    userState$step2_factor_order_col = NULL
    userState$step2_factor_levels_csv = NULL
    userState$step2_applied_factor_cols = NULL
    userState$step2_applied_numeric_override_cols = NULL
    userState$step2_applied_factor_order_enable = FALSE
    userState$step2_applied_factor_order_col = NULL
    userState$step2_applied_factor_levels_csv = NULL

    # Boxplots options
    userState$bp_bin_size = NULL
    userState$bp_group_by = NULL
    userState$bp_y_lim = NULL

    # Violin Plot options
    userState$vio_group_by = NULL
    userState$vio_bin_size = NULL
    userState$vio_y_lim = NULL
    userState$vio_boxplot_overlay = NULL

    # Univariate test options
    userState$uv2_method = NULL
    userState$uv2_p_adjust_method = NULL
    userState$uvm_method = NULL
    userState$uvm_p_adjust_method = NULL
    userState$twa_primary_cat_var = NULL
    userState$twa_secondary_cat_var = NULL
    userState$twa_include_primary_secondary_interaction = NULL
    userState$anc_primary_cat_var = NULL
    userState$anc_secondary_cat_var = NULL
    userState$anc_covariate_col = NULL
    userState$anc_include_primary_secondary_interaction = NULL
    userState$anc_include_primary_covariate_interaction = NULL
    userState$anc_include_secondary_covariate_interaction = NULL

    # Error-Bar Plot
    userState$eb_group_col = NULL
    userState$eb_p_lab = NULL
    userState$eb_es_lab = NULL
    userState$eb_class_symbol = NULL
    userState$eb_x_lab = NULL
    userState$eb_y_lab = NULL
    userState$eb_title = NULL
    userState$eb_stat = NULL
    userState$eb_error = NULL
    userState$eb_method = NULL
    userState$eb_p_adjust_method = NULL
    # Dual-Flashlight Plot options
    userState$df_group_var = NULL
    userState$df_cond1 = NULL
    userState$df_cond2 = NULL
    userState$df_ssmd_thresh = NULL
    userState$df_log2fc_thresh = NULL
    userState$df_top_labels = NULL

    # Heatmap options
    userState$hm_annotation = NULL
    userState$hm_ann_side = NULL

    # Correlation options
    userState$corr_target = NULL
    userState$corr_group_col = NULL
    userState$corr_by_group = FALSE

    # PCA options
    userState$pca_group_col = NULL
    userState$pca_group_col2 = NULL
    userState$pca_comp_num = NULL
    userState$pca_ellipse = NULL
    userState$pca_style = NULL
    userState$pca_pch = NULL
    userState$pca_colors = NULL

    # PLSR options
    userState$plsr_group_col = NULL
    userState$plsr_response_col = NULL
    userState$plsr_predictor_cols = NULL
    userState$plsr_comp_num = NULL
    userState$plsr_keepX = NULL
    userState$plsr_keepX_manual = FALSE
    userState$plsr_sparse = FALSE
    userState$plsr_cv_opt = NULL
    userState$plsr_fold_num = NULL
    userState$plsr_ellipse = FALSE
    userState$plsr_colors = NULL

    # Random Forest options
    userState$rf_group_col = NULL
    userState$rf_ntree = NULL
    userState$rf_mtry = NULL
    userState$rf_train_fraction = NULL
    userState$rf_plot_roc = NULL
    userState$rf_run_rfcv = NULL
    userState$rf_k_folds = NULL
    userState$rf_step = NULL

    # Skewness/Kurtosis options
    userState$skku_group_cols = NULL
    userState$skku_print_raw = NULL
    userState$skku_print_log = NULL

    # sPLS-DA options
    userState$splsda_group_col = NULL
    userState$splsda_group_col2 = NULL
    userState$splsda_batch_col = NULL
    userState$splsda_var_num = NULL
    userState$splsda_var_num_manual = FALSE
    userState$splsda_cv_opt = NULL
    userState$splsda_fold_num = NULL
    userState$splsda_comp_num = NULL
    userState$splsda_pch = NULL
    userState$splsda_ind_names_mode = NULL
    userState$splsda_ind_names_col = NULL
    userState$splsda_style = NULL
    userState$splsda_roc = NULL
    userState$splsda_ellipse = NULL
    userState$splsda_bg = NULL
    userState$splsda_conf_mat = NULL
    userState$splsda_colors = NULL
    userState$splsda_use_multilevel = FALSE
    userState$splsda_multilevel = NULL
    userState$splsda_use_batch_corr = FALSE
    userState$splsda_batch_col = NULL
    # MINT sPLS-DA options
    userState$mint_splsda_group_col = NULL
    userState$mint_splsda_group_col2 = NULL
    userState$mint_splsda_batch_col = NULL
    userState$mint_splsda_var_num = NULL
    userState$mint_splsda_var_num_manual = FALSE
    userState$mint_splsda_comp_num = NULL
    userState$mint_splsda_cim = NULL
    userState$mint_splsda_ellipse = NULL
    userState$mint_splsda_bg = NULL
    userState$mint_splsda_roc = NULL
    userState$mint_splsda_colors = NULL

    # Volcano Plot options
    userState$volc_group_col = NULL
    userState$volc_cond1 = NULL
    userState$volc_cond2 = NULL
    userState$volc_fold_change_thresh = NULL
    userState$volc_p_value_thresh = NULL
    userState$volc_top_labels = NULL

    # XGBoost options
    userState$xgb_group_col = NULL
    userState$xgb_train_fraction = NULL
    userState$xgb_nrounds = NULL
    userState$xgb_max_depth = NULL
    userState$xgb_eta = NULL
    userState$xgb_nfold = NULL
    userState$xgb_cv = NULL
    userState$xgb_eval_metric = NULL
    userState$xgb_top_n_features = NULL
    userState$xgb_plot_roc = NULL

    lapply(analysis_font_specs, function(spec) {
      userState[[spec$state_key]] <- NULL
    })
  }
  shiny::observeEvent(input$new_fresh, {
    session$reload()
  })
  shiny::observeEvent(input$fresh_start, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Start fresh?",
        "This will clear uploaded/built-in data and all saved options.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            "confirm_fresh_start",
            "Start fresh",
            class = "btn-danger"
          )
        ),
        easyClose = FALSE
      )
    )
  })

  shiny::observeEvent(input$confirm_fresh_start, {
    shiny::removeModal()
    session$reload()
  })
  shiny::observeEvent(input$new_reuse, {
    shiny::isolate({
      # Boxplots options
      userState$bp_bin_size = NULL
      userState$bp_group_by = NULL
      userState$bp_y_lim = NULL

      # Violin Plot options
      userState$vio_group_by = NULL
      userState$vio_bin_size = NULL
      userState$vio_y_lim = NULL
      userState$vio_boxplot_overlay = NULL

      # Univariate test options
      userState$uv2_method = NULL
      userState$uv2_p_adjust_method = NULL
      userState$uvm_method = NULL
      userState$uvm_p_adjust_method = NULL
      userState$twa_primary_cat_var = NULL
      userState$twa_secondary_cat_var = NULL
      userState$twa_include_primary_secondary_interaction = NULL
      userState$anc_primary_cat_var = NULL
      userState$anc_secondary_cat_var = NULL
      userState$anc_covariate_col = NULL
      userState$anc_include_primary_secondary_interaction = NULL
      userState$anc_include_primary_covariate_interaction = NULL
      userState$anc_include_secondary_covariate_interaction = NULL

      # Error-Bar Plot
      userState$eb_group_col = NULL
      userState$eb_p_lab = NULL
      userState$eb_es_lab = NULL
      userState$eb_class_symbol = NULL
      userState$eb_x_lab = NULL
      userState$eb_y_lab = NULL
      userState$eb_title = NULL
      userState$eb_stat = NULL
      userState$eb_error = NULL
      userState$eb_method = NULL
      userState$eb_p_adjust_method = NULL
      userState$eb_n_col = NULL
      userState$eb_fill_palette = NULL

      # Dual-Flashlight Plot options
      userState$df_group_var = NULL
      userState$df_cond1 = NULL
      userState$df_cond2 = NULL
      userState$df_ssmd_thresh = NULL
      userState$df_log2fc_thresh = NULL
      userState$df_top_labels = NULL

      # Heatmap options
      userState$hm_annotation = NULL
      userState$hm_ann_side = NULL

      # PCA options
      userState$pca_group_col = NULL
      userState$pca_group_col2 = NULL
      userState$pca_comp_num = NULL
      userState$pca_ellipse = NULL
      userState$pca_style = NULL
      userState$pca_pch = NULL
      userState$pca_colors = NULL

      # PLSR options
      userState$plsr_group_col = NULL
      userState$plsr_response_col = NULL
      userState$plsr_predictor_cols = NULL
      userState$plsr_comp_num = NULL
      userState$plsr_keepX = NULL
      userState$plsr_keepX_manual = FALSE
      userState$plsr_sparse = FALSE
      userState$plsr_cv_opt = NULL
      userState$plsr_fold_num = NULL
      userState$plsr_ellipse = FALSE
      userState$plsr_colors = NULL

      # Random Forest options
      userState$rf_group_col = NULL
      userState$rf_ntree = NULL
      userState$rf_mtry = NULL
      userState$rf_train_fraction = NULL
      userState$rf_plot_roc = NULL
      userState$rf_run_rfcv = NULL
      userState$rf_k_folds = NULL
      userState$rf_step = NULL

      # Skewness/Kurtosis options
      userState$skku_group_cols = NULL
      userState$skku_print_raw = NULL
      userState$skku_print_log = NULL

      # sPLS-DA options
      userState$splsda_group_col = NULL
      userState$splsda_group_col2 = NULL
      userState$splsda_batch_col = NULL
      userState$splsda_var_num = NULL
      userState$splsda_var_num_manual = FALSE
      userState$splsda_cv_opt = NULL
      userState$splsda_fold_num = NULL
      userState$splsda_comp_num = NULL
      userState$splsda_pch = NULL
      userState$splsda_ind_names_mode = NULL
      userState$splsda_ind_names_col = NULL
      userState$splsda_style = NULL
      userState$splsda_roc = NULL
      userState$splsda_ellipse = NULL
      userState$splsda_bg = NULL
      userState$splsda_conf_mat = NULL
      userState$splsda_colors = NULL
      userState$splsda_multilevel = NULL
      userState$splsda_use_batch_corr = FALSE
      userState$splsda_batch_col = NULL
      userState$splsda_use_multilevel = FALSE
      # MINT sPLS-DA options
      userState$mint_splsda_group_col = NULL
      userState$mint_splsda_group_col2 = NULL
      userState$mint_splsda_batch_col = NULL
      userState$mint_splsda_var_num = NULL
      userState$mint_splsda_var_num_manual = FALSE
      userState$mint_splsda_comp_num = NULL
      userState$mint_splsda_cim = NULL
      userState$mint_splsda_ellipse = NULL
      userState$mint_splsda_bg = NULL
      userState$mint_splsda_roc = NULL
      userState$mint_splsda_colors = NULL

      # Volcano Plot options
      userState$volc_group_col = NULL
      userState$volc_cond1 = NULL
      userState$volc_cond2 = NULL
      userState$volc_fold_change_thresh = NULL
      userState$volc_p_value_thresh = NULL
      userState$volc_top_labels = NULL

      # XGBoost options
      userState$xgb_group_col = NULL
      userState$xgb_train_fraction = NULL
      userState$xgb_nrounds = NULL
      userState$xgb_max_depth = NULL
      userState$xgb_eta = NULL
      userState$xgb_nfold = NULL
      userState$xgb_cv = NULL
      userState$xgb_eval_metric = NULL
      userState$xgb_top_n_features = NULL
      userState$xgb_plot_roc = NULL

      # Correlation options
      userState$corr_target = NULL
      userState$corr_group_col = NULL
      userState$corr_by_group = FALSE

      lapply(analysis_font_specs, function(spec) {
        userState[[spec$state_key]] <- NULL
      })
    })
    currentPage("step2")
    currentStep(2)
  })

  output$page_content <- shiny::renderUI({
    switch(
      currentPage(),
      "home" = homeUI(), # Landing page with the main intro CTA.
      "tutorials" = tutorialUI(), # Simple link out to docs.
      "step1" = step1UI(), # Existing upload-data card + Next button.
      "step2" = step2UI(), # Existing select-cols/filters + Next/Back.
      "step3" = step3UI(), # Existing analysis-options grid + Back.
      "step4" = step4UI(), # Existing function-args form + Run/Back.
      "step5" = resultsUI(), # existing results page
      "news" = newsUI(), # Simple news page
      "contact" = contactUI(), # Simple contact page
      homeUI() # Fallback
    )
  })

  # Development-notice modal: show once per session when user lands on Home.
  shiny::observe({
    cfg <- tryCatch(config::get(), error = function(e) list(build_type = NULL))
    is_devel <- identical(cfg$build_type, "development") ||
      identical(cfg$build_type, "devel")

    if (
      is_devel &&
        identical(currentPage(), "home") &&
        !isTRUE(dev_notice_shown())
    ) {
      shiny::showModal(
        shiny::modalDialog(
          title = NULL,
          # Warning-styled content
          shiny::div(
            class = "alert alert-warning",
            role = "alert",
            shiny::tags$h4("Development Build", class = "alert-heading"),
            shiny::tags$p(
              "This is a development build of CytokineProfile. Some features may be incomplete or unstable."
            ),
            shiny::tags$p("Press OK to continue to the app.")
          ),
          footer = shiny::tagList(
            shiny::actionButton("dev_notice_ok", "OK", class = "btn-primary")
          ),
          size = "m",
          easyClose = FALSE
        )
      )

      # Override the global wide-modal CSS just for this shown dialog so it appears centered
      # and medium width. Requires shinyjs, which is loaded by app_ui().
      try(
        {
          shinyjs::runjs(
            "$('#shiny-modal .modal-dialog').css({'max-width':'640px','width':'640px','margin':'auto'});"
          )
        },
        silent = TRUE
      )
    }
  })

  shiny::observeEvent(input$dev_notice_ok, {
    shiny::removeModal()
    dev_notice_shown(TRUE)
  })
  totalPages <- 5
  stepHeader <- function(step) {
    pct <- round((step - 1) / (totalPages - 1) * 100)
    shiny::tagList(
      shiny::div(
        class = "step-title",
        shiny::h3(switch(
          as.character(step),
          "1" = "Step 1: Upload Data",
          "2" = "Step 2: Select Columns & Apply Filters",
          "3" = "Step 3: Analysis Choices",
          "4" = paste0("Step 4: Options for ", selected_function()),
          "5" = "Analysis Results"
        ))
      ),
      shiny::div(
        class = "progress-wrapper",
        shinyWidgets::progressBar(
          id = "wizard_pb",
          value = pct,
          display_pct = TRUE,
          striped = TRUE,
          size = "xs",
          status = "info"
        )
      )
    )
  }
  homeUI <- function() {
    shiny::tagList(
      shiny::h1("Welcome to CytokineProfile", style = "font-weight:300;"),
      shiny::p(shiny::HTML(paste0(
        "CytokineProfile is an R Shiny Application based on the CytoProfile R package available at ",
        "<a href='https://cran.r-project.org/package=CytoProfile'>CRAN</a>. ",
        "This application is designed for advanced cytokine data analysis. ",
        "It provides a comprehensive suite of functions for exploratory, univariate, ",
        "and multivariate analysis as well as machine learning methods tailored to your data."
      ))),
      shiny::tags$h3("Features we offer:", style = "margin-top:2rem;"),
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::div(
            class = "card h-100",
            shiny::div(
              class = "card-header bg-primary text-white",
              "Univariate Analysis"
            ),
            shiny::div(
              class = "card-body",
              shiny::tags$ul(
                shiny::tags$li("Univariate Tests (T-test, Wilcoxon)"),
                shiny::tags$li(
                  "Multi-level Univariate Tests (Anova, Kruskal-Wallis)"
                ),
                shiny::tags$li("Two-way ANOVA"),
                shiny::tags$li("ANCOVA")
              )
            )
          )
        ),
        shiny::column(
          width = 3,
          shiny::div(
            class = "card h-100",
            shiny::div(
              class = "card-header bg-primary text-white",
              "Exploratory Analysis"
            ),
            shiny::div(
              class = "card-body",
              shiny::tags$ul(
                shiny::tags$li("Boxplots"),
                shiny::tags$li("Violin Plots"),
                shiny::tags$li("Correlation Plots"),
                shiny::tags$li("Error-Bar Plot"),
                shiny::tags$li("Dual-Flashlight Plot"),
                shiny::tags$li("Heatmap"),
                shiny::tags$li("Skewness/Kurtosis Plots"),
                shiny::tags$li("Volcano Plot")
              )
            )
          )
        ),
        shiny::column(
          width = 3,
          shiny::div(
            class = "card h-100",
            shiny::div(
              class = "card-header bg-primary text-white",
              "Multivariate Analysis"
            ),
            shiny::div(
              class = "card-body",
              shiny::tags$ul(
                shiny::tags$li("Principal Component Analysis (PCA)"),
                shiny::tags$li("Partial Least Squares Regression (PLSR)"),
                shiny::tags$li(
                  "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
                ),
                shiny::tags$li(
                  "Multivariate INTegrative Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
                )
              )
            )
          )
        ),
        shiny::column(
          width = 3,
          shiny::div(
            class = "card h-100",
            shiny::div(
              class = "card-header bg-primary text-white",
              "Machine Learning Methods"
            ),
            shiny::div(
              class = "card-body",
              shiny::tags$ul(
                shiny::tags$li("Random Forest"),
                shiny::tags$li("Extreme Gradient Boosting (XGBoost)")
              )
            )
          )
        )
      ),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          align = "center",
          shiny::actionButton(
            "nav_start_home",
            "Let's get started!",
            icon = shiny::icon("arrow-right"),
            class = "btn-primary btn-lg"
          )
        )
      )
    )
  }

  tutorialUI <- function() {
    shiny::tagList(
      shiny::includeMarkdown("TUTORIALS.md")
    )
  }
  newsUI <- function() {
    shiny::tagList(
      shiny::includeMarkdown("NEWS.md")
    )
  }
  contactUI <- function() {
    shiny::fluidPage(
      shiny::h1("About Us"),
      shiny::fluidRow(
        ## Column 1 ##
        shiny::column(
          width = 6,
          shiny::h2("Shubh Saraswat"),
          shiny::p(shiny::em(
            "Maintainer, Co-Creator, and Author of CytokineProfile"
          )),
          shiny::p("Biomedical Data Scientist"),
          shiny::p("PhD Student in Epidemiology & Biostatistics"),
          shiny::p("University of Kentucky"),
          shiny::tags$a(
            href = "mailto:shubh.saraswat@uky.edu",
            class = "btn btn-primary me-2",
            shiny::icon("envelope"),
            "Email"
          ),
          shiny::tags$a(
            href = "https://www.linkedin.com/in/ssaraswat22",
            class = "btn btn-info me-2",
            shiny::icon("linkedin"),
            "LinkedIn"
          ),
          shiny::tags$a(
            href = "https://github.com/saraswatsh",
            class = "btn btn-dark me-2",
            shiny::icon("github"),
            "GitHub"
          ),
          shiny::tags$a(
            href = "https://orcid.org/0009-0009-2359-1484",
            class = "btn btn-link",
            shiny::icon("orcid"),
            "ORCID"
          )
        ),

        ## Column 2 ##
        shiny::column(
          width = 6,
          shiny::h2("Xiaohua Douglas Zhang"),
          shiny::p(shiny::em("Co-Creator and Author of CytokineProfile")),
          shiny::p("Professor, Department of Biostatistics"),
          shiny::p("University of Kentucky"),
          shiny::tags$a(
            href = "mailto:xiaohua.zhang@uky.edu",
            class = "btn btn-primary me-2",
            shiny::icon("envelope"),
            "Email"
          ),
          shiny::tags$a(
            href = "https://orcid.org/0000-0002-2486-7931",
            class = "btn btn-link",
            shiny::icon("orcid"),
            "ORCID"
          )
        )
      ),
      shiny::br(),
      shiny::fluidRow(
        ## Column 3 ##
        shiny::column(
          width = 6,
          shiny::h2("Bira Arumndari Nurrahma"),
          shiny::p(shiny::em("Author of CytokineProfile")),
          shiny::p("PhD Student in Nutritional Sciences"),
          shiny::p("University of Kentucky"),
          shiny::tags$a(
            href = "mailto:biraarum@uky.edu",
            class = "btn btn-primary me-2",
            shiny::icon("envelope"),
            "Email"
          )
        )
      ),
      # Add a note about who to contact for application issues
      shiny::column(
        width = 12,
        shiny::br(),
        shiny::p(
          "For issues related to the application, submit an issue at the ",
          shiny::tags$a(
            href = "https://github.com/saraswatsh/CytokineProfileShinyApp/issues",
            "GitHub repository."
          ),
          "For additional questions or concerns, contact the maintainer Shubh Saraswat with the provided email above."
        )
      )
    )
  }

  step1UI <- function() {
    shiny::tagList(
      stepHeader(currentStep()),

      shiny::fluidRow(
        # --- Left Column: Upload Controls ---
        shiny::column(
          width = 5,
          bslib::card(
            bslib::card_header(shiny::h4(
              shiny::icon("upload"),
              "Step 1: Provide Your Data"
            )),
            bslib::card_body(
              shiny::tags$h5("Option A: Upload a File"),
              shiny::fileInput(
                "datafile",
                label = NULL,
                accept = c(".csv", ".txt", ".xls", ".xlsx")
              ),
              shiny::helpText(
                "Accepted Formats: '.csv', '.txt', '.xls', '.xlsx'"
              ),
              shiny::uiOutput("sheet_selector"),
              # Conditional panel to show the editor button once data is uploaded
              shiny::uiOutput("open_editor_btn"),
              shiny::hr(),
              shiny::tags$h5("Option B: Use Built-in Data"),
              shiny::checkboxInput(
                "use_builtin",
                "Use a built-in dataset?",
                value = shiny::isolate(userState$use_builtin) %||% FALSE
              ),
              shiny::uiOutput("built_in_selector"),
              shiny::uiOutput("step1_bottom_block")
            ),
            bslib::card_footer(
              shiny::div(
                style = "text-align: right;",
                shiny::actionButton(
                  "next1",
                  "Next Step",
                  icon = shiny::icon("arrow-right"),
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # --- Right Column: Data Preview & Summary ---
        shiny::column(
          width = 7,
          shiny::conditionalPanel(
            condition = "output.data_is_loaded == true",
            bslib::navset_card_tab(
              id = "step1_data_tabs",
              bslib::nav_panel(
                "Data Preview",
                shiny::conditionalPanel(
                  condition = "input.view_data",
                  shiny::uiOutput("data_summary"),
                  shinycssloaders::withSpinner(
                    shiny::uiOutput("preview_ui"),
                    type = 8
                  )
                ),
                shiny::conditionalPanel(
                  condition = "!input.view_data",
                  shiny::p(
                    style = "padding: 1rem;",
                    "Check 'View Data Loaded?' to see a preview of your data here."
                  )
                )
              ),
              bslib::nav_panel(
                "Summary Statistics",
                shiny::conditionalPanel(
                  condition = "input.show_summary",
                  shinycssloaders::withSpinner(
                    DT::DTOutput("summary_stats_table"),
                    type = 8
                  )
                ),
                shiny::conditionalPanel(
                  condition = "!input.show_summary",
                  shiny::p(
                    style = "padding: 1rem;",
                    "Check 'Show summary statistics' to see a summary here."
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  step2UI <- function() {
    {
      df <- step2_typed_data()
      col_info <- step2_typed_col_info()
      numeric_cols <- col_info$numerical
      categorical_cols <- col_info$categorical
      selected_cat <- step2_restore_bucket_selection(
        userState$selected_columns,
        categorical_cols
      )
      selected_num <- step2_restore_bucket_selection(
        userState$selected_columns,
        numeric_cols
      )

      shiny::tagList(
        stepHeader(currentStep()),
        shiny::fluidRow(
          # -- Left Column: Selections & Filters --
          shiny::column(
            width = 5,
            # 1) Categorical selector
            bslib::card(
              bslib::card_header(
                class = "bg-info",
                "1. Select Categorical Columns"
              ),
              bslib::card_body(
                shiny::div(
                  style = "margin-bottom: 10px;",
                  shiny::actionButton(
                    "select_all_cat",
                    "Select All",
                    class = "btn-sm"
                  ),
                  shiny::actionButton(
                    "deselect_all_cat",
                    "Deselect All",
                    class = "btn-sm"
                  )
                ),
                shiny::div(
                  class = "scrollable-checkbox-group",
                  shiny::checkboxGroupInput(
                    inputId = "selected_categorical_cols",
                    label = NULL,
                    choices = categorical_cols,
                    selected = selected_cat,
                    inline = TRUE
                  )
                )
              )
            ),

            # 2) Numerical selector
            bslib::card(
              bslib::card_header(
                class = "bg-info",
                "2. Select Numerical Columns"
              ),
              bslib::card_body(
                shiny::div(
                  style = "margin-bottom: 6px;",
                  shiny::actionButton(
                    "select_all_num",
                    "Select All",
                    class = "btn-sm"
                  ),
                  shiny::actionButton(
                    "deselect_all_num",
                    "Deselect All",
                    class = "btn-sm"
                  )
                ),
                shiny::div(
                  class = "scrollable-checkbox-group",
                  shiny::checkboxGroupInput(
                    inputId = "selected_numerical_cols",
                    label = NULL,
                    choices = numeric_cols,
                    selected = selected_num,
                    inline = TRUE
                  )
                )
              )
            ),
            bslib::card(
              class = "mb-3", # nice spacing below
              bslib::card_header(
                class = "bg-info",
                "2a. Override Column Types"
              ),
              bslib::card_body(
                shiny::uiOutput("step2_type_override_ui")
              )
            ),
            # 3) Optional: data transformation
            bslib::card(
              bslib::card_header(
                class = "bg-info",
                "3. Optional: Data Transformation"
              ),
              bslib::card_body(
                style = "overflow: visible; min-height: 8rem;",
                shiny::selectInput(
                  "step2_scale",
                  label = shinyhelper::helper(
                    type = "inline",
                    title = "Data Transformation",
                    icon = "fas fa-question-circle",
                    shiny_tag = shiny::HTML(
                      "<span style='margin-right: 15px;'>Apply a preprocessing method to selected numerical columns</span>"
                    ),
                    content = "Apply one preprocessing method to the selected numerical columns before imputation and downstream analysis. Log transforms require all non-missing selected values to be finite and greater than zero.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = c(
                    "None" = "none",
                    "log2" = "log2",
                    "log10" = "log10",
                    "Z-score" = "zscore"
                  ),
                  selected = shiny::isolate(userState$step2_scale) %||% "none",
                  selectize = FALSE,
                  width = "100%"
                )
              )
            ),
            # 4) Conditional filters UI
            shiny::uiOutput("conditional_filter_ui")
          ),

          # -- Right Column: Data Preview & Deletion --
          shiny::column(
            width = 7,
            bslib::card(
              style = "height: 40vh; display: flex; flex-direction: column;",
              bslib::card_header(shiny::h4(
                shiny::icon("table"),
                "Filtered Data Explorer"
              )),
              bslib::card_body(
                style = "flex: 1 1 auto; overflow-y: auto; padding: 1rem;",
                shiny::div(
                  style = "max-height: 40vh; overflow-y: auto; padding: 1rem;",
                  DT::DTOutput("filtered_data_preview")
                )
              ),
              bslib::card_footer(
                style = "
                           display: flex;
                          justify-content: center;
                          padding: 0.75rem;
                          border-top: 1px solid #444;
                           background: inherit;
                        ",
                shiny::actionButton(
                  "delete_selected_rows",
                  "Delete Selected",
                  icon = shiny::icon("trash"),
                  class = "btn-danger me-2"
                ),
                shiny::actionButton(
                  "expand_filtered",
                  "Enlarge Window",
                  icon = shiny::icon("expand"),
                  class = "btn-secondary"
                )
              )
            ),

            # wrap deleteda??samples in a card too
            bslib::card(
              style = "display: flex; flex-direction: column;",
              bslib::card_header(shiny::h4(
                shiny::icon("table"),
                "Deleted Samples"
              )),
              bslib::card_body(
                style = "flex: 1 1 auto; overflow-y: auto; padding: 1rem;",
                shiny::div(
                  style = "max-height: 40vh; overflow-y: auto; padding: 1rem;",
                  DT::DTOutput("deleted_data_preview")
                )
              ),
              bslib::card_footer(
                style = "display: flex;
                       justify-content: center;
                       padding: 0.75rem;
                       border-top: 1px solid #444;
                       background: inherit;",
                shiny::actionButton(
                  "restore_selected_rows",
                  "Restore Selected",
                  icon = shiny::icon("undo"),
                  class = "btn-secondary me-2"
                ),
                shiny::actionButton(
                  "restore_all_rows",
                  "Restore All",
                  icon = shiny::icon("undo"),
                  class = "btn-secondary me-2"
                ),
                # NEW: enlarge button for deleted samples
                shiny::actionButton(
                  "expand_deleted",
                  "Enlarge Window",
                  icon = shiny::icon("expand"),
                  class = "btn-secondary"
                )
              )
            )
          )
        ),
        # -- Navigation --
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::div(
              style = "margin-top:0.5rem;",
              shiny::actionButton(
                "back2",
                "Back",
                icon = shiny::icon("arrow-left")
              ),
              shiny::conditionalPanel(
                "input.step2_scale && input.step2_scale !== 'none'",
                shiny::actionButton(
                  "preview_transform",
                  "Preview Transformation",
                  icon = shiny::icon("magnifying-glass"),
                  class = "btn-secondary"
                )
              ),
              shiny::actionButton(
                "open_impute_modal",
                "Treat missing values",
                icon = shiny::icon("fas fa-eraser"),
                class = "btn-secondary"
              ),
              shiny::actionButton(
                "next2",
                "Next",
                icon = shiny::icon("arrow-right"),
                class = "btn-primary"
              )
            )
          )
        )
      )
    }
  }

  step3UI <- function() {
    shiny::tagList(
      stepHeader(currentStep()),
      shiny::fluidRow(
        # Statistical Tests
        shiny::column(
          width = 3,
          bslib::card(
            bslib::card_header("Univariate Analysis", class = "bg-info"),
            bslib::card_body(
              shiny::actionButton(
                "menu_univariate_2lvl",
                "Univariate Tests (T-test, Wilcoxon)",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_univariate_multi",
                "Multi-level Univariate Tests (Anova, Kruskal-Wallis)",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_two_way_anova",
                "Two-way ANOVA",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_ancova",
                "ANCOVA",
                class = "menu-card"
              ),
            )
          )
        ),
        # Exploratory Analysis
        shiny::column(
          width = 3,
          bslib::card(
            bslib::card_header("Exploratory Analysis", class = "bg-info"),
            bslib::card_body(
              shiny::actionButton(
                "menu_boxplots",
                "Boxplots",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_violin",
                "Violin Plots",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_correlation",
                "Correlation Plots",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_skewkurt",
                "Skewness/Kurtosis",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_errorbp",
                "Error-Bar Plot",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_dualflash",
                "Dual-Flashlight Plot",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_heatmap",
                "Heatmap",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_volcano",
                "Volcano Plot",
                class = "menu-card"
              )
            )
          )
        ),
        # Multivariate
        shiny::column(
          width = 3,
          bslib::card(
            bslib::card_header("Multivariate Analysis", class = "bg-info"),
            bslib::card_body(
              shiny::actionButton(
                "menu_PCA",
                "Principal Component Analysis (PCA)",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_PLSR",
                "Partial Least Squares Regression (PLSR)",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_splsda",
                "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_mint_splsda",
                "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)",
                class = "menu-card"
              )
            )
          )
        ),
        # Machine Learning
        shiny::column(
          width = 3,
          bslib::card(
            bslib::card_header("Machine Learning Methods", class = "bg-info"),
            bslib::card_body(
              shiny::actionButton(
                "menu_rf",
                "Random Forest",
                class = "menu-card"
              ),
              shiny::actionButton(
                "menu_xgb",
                "Extreme Gradient Boosting (XGBoost)",
                class = "menu-card"
              )
            )
          )
        ),

        # # Back/Next buttons for the wizard
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::div(
              style = "margin-top: 1rem; display: flex; justify-content: flex-start;",
              shiny::actionButton(
                "back3",
                "Back",
                icon = shiny::icon("arrow-left"),
                class = "btn-secondary"
              )
            )
          )
        )
      )
    )
  }

  step4UI <- function() {
    shiny::tagList(
      stepHeader(currentStep()),
      shiny::uiOutput("function_options_ui"),
      shiny::div(
        style = "display: flex; justify-content: space-between; margin-top: 1rem;",
        shiny::actionButton(
          "back4",
          "Back",
          icon = shiny::icon("arrow-left"),
          class = "btn-secondary"
        ),
        shiny::actionButton(
          "next4",
          "Run Analysis",
          icon = shiny::icon("play"),
          class = "btn-success"
        )
      )
    )
  }

  resultsUI <- function() {
    shiny::tagList(
      stepHeader(currentStep()),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::uiOutput("result_display")
        )
      ),
      # Add the download UI output here
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::uiOutput("download_ui") # Placeholder for download button
        )
      ),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::div(
            style = "display:flex; align-items:center; margin-top:1rem;",
            shiny::actionButton(
              "back5",
              "Back",
              icon = shiny::icon("arrow-left"),
              class = "btn-secondary"
            ),
            shiny::div(
              style = "margin-left:auto; display:flex; align-items:center; gap:.5rem;",
              shiny::actionButton(
                "new_fresh",
                "Start New (fresh)",
                icon = shiny::icon("play"),
                class = "btn-primary"
              ),
              shiny::actionButton(
                "new_reuse",
                "Start New (reuse data)",
                icon = shiny::icon("repeat"),
                class = "btn-secondary"
              )
            )
          )
        )
      )
    )
  }

  # --- render the deleted table (read-only; select rows to restore)
  output$deleted_data_preview <- DT::renderDT({
    all <- userData()
    deleted_ids <- userState$deleted_row_ids %||% integer(0)
    df_del <- all[all$..cyto_id.. %in% deleted_ids, , drop = FALSE]
    df_del$..cyto_id.. <- NULL # Remove the internal ID column for display
    DT::datatable(
      df_del,
      selection = "multiple",
      options = list(scrollX = TRUE, pageLength = 5)
    )
  })

  # --- Enlarge Deleted Samples Explorer ---
  shiny::observeEvent(input$expand_deleted, {
    shiny::showModal(shiny::modalDialog(
      title = "Deleted Samples - Full View",
      DT::DTOutput("deleted_data_preview_modal"),
      size = "l",
      easyClose = TRUE,
      footer = shiny::div(
        class = "d-flex justify-content-center gap-2 flex-wrap w-100",
        shiny::actionButton(
          "restore_selected_rows_modal",
          "Restore Selected",
          icon = shiny::icon("undo"),
          class = "btn-secondary"
        ),
        shiny::actionButton(
          "restore_all_rows_modal",
          "Restore All",
          icon = shiny::icon("undo"),
          class = "btn-secondary"
        ),
        shiny::modalButton("Close")
      )
    ))
  })

  output$deleted_data_preview_modal <- DT::renderDT({
    all <- userData()
    deleted_ids <- userState$deleted_row_ids %||% integer(0)
    df_del <- all[all$..cyto_id.. %in% deleted_ids, , drop = FALSE]
    df_del$..cyto_id.. <- NULL # Remove the internal ID column for display
    DT::datatable(
      df_del,
      selection = "multiple",
      options = list(scrollX = TRUE, pageLength = 5),
      class = "stripe hover"
    )
  })
  # --- Delete selected from the main table
  shiny::observeEvent(input$delete_selected_rows, {
    sel <- input$filtered_data_preview_rows_selected
    if (length(sel)) {
      to_kill <- filteredData()$..cyto_id..[sel]
      userState$deleted_row_ids <- unique(c(
        userState$deleted_row_ids %||% integer(0),
        to_kill
      ))
    }
  })

  # --- Delete selected from the MODAL filtered table
  shiny::observeEvent(input$delete_selected_rows_modal, {
    sel <- input$filtered_data_preview_modal_rows_selected # DT convention
    if (length(sel)) {
      to_kill <- filteredData()$..cyto_id..[sel]
      userState$deleted_row_ids <- unique(c(
        userState$deleted_row_ids %||% integer(0),
        to_kill
      ))
    }
  })

  # --- Restore selected from the deleted table
  shiny::observeEvent(input$restore_selected_rows, {
    sel <- input$deleted_data_preview_rows_selected
    if (length(sel)) {
      # map row indices in deleted table back to IDs
      all <- userData()
      del <- userState$deleted_row_ids %||% integer(0)
      df_del <- all[all$..cyto_id.. %in% del, , drop = FALSE]
      to_restore <- df_del$..cyto_id..[sel]
      userState$deleted_row_ids <- setdiff(del, to_restore)
    }
  })

  # --- "Restore All" button
  shiny::observeEvent(input$restore_all_rows, {
    userState$deleted_row_ids <- integer(0)
  })

  # --- Restore selected from the MODAL deleted table
  shiny::observeEvent(input$restore_selected_rows_modal, {
    sel <- input$deleted_data_preview_modal_rows_selected # DT convention
    if (length(sel)) {
      all <- userData()
      del <- userState$deleted_row_ids %||% integer(0)
      df_del <- all[all$..cyto_id.. %in% del, , drop = FALSE]
      to_restore <- df_del$..cyto_id..[sel]
      userState$deleted_row_ids <- setdiff(del, to_restore)
    }
  })

  # --- Restore all from the MODAL
  shiny::observeEvent(input$restore_all_rows_modal, {
    userState$deleted_row_ids <- integer(0)
  })

  output$viewSummaryCheckboxes <- shiny::renderUI({
    # Require either built-in data is used OR a file has been successfully uploaded
    shiny::req(input$use_builtin || isTRUE(bioplex$active))
    # If the condition above is met, render the checkboxes
    shiny::tagList(
      shiny::checkboxInput("view_data", "View Data Loaded?", FALSE),
      shiny::checkboxInput("show_summary", "Show summary statistics", FALSE)
    )
  })

  # Logic for the "Fresh Start" button
  output$fresh_start_ui <- shiny::renderUI({
    has_data <- isTRUE(bioplex$active) ||
      (isTRUE(userState$use_builtin) && !is.null(userState$built_in_choice))

    if (!has_data) {
      return(NULL)
    }

    shiny::div(
      class = "mt-2",
      shiny::actionButton(
        "fresh_start",
        "Fresh start",
        icon = shiny::icon("broom")
      )
    )
  })
  output$step1_bottom_block <- shiny::renderUI({
    has_confirmed_data <- isTRUE(bioplex$active) ||
      (isTRUE(input$use_builtin) && !is.null(userState$built_in_choice))

    shiny::tagList(
      # show the view/summary toggles only after confirmed data (built-in OR Save & Use)
      if (has_confirmed_data) shiny::hr(class = "my-2"),
      if (has_confirmed_data) shiny::uiOutput("viewSummaryCheckboxes"),
      if (has_confirmed_data) shiny::hr(class = "my-2"),
      if (has_confirmed_data) shiny::uiOutput("fresh_start_ui")
    )
  })

  # --- Logic for Custom Button Group: Statistical Tests ---
  stat_choices <- c(
    "Univariate Tests (T-test, Wilcoxon)",
    "Multi-level Univariate Tests (Anova, Kruskal-Wallis)",
    "Two-way ANOVA",
    "ANCOVA"
  )
  output$stat_function_ui <- shiny::renderUI({
    lapply(stat_choices, function(choice) {
      shiny::actionButton(
        inputId = paste0("stat_func_", gsub("\\s|\\-", "_", choice)),
        label = choice,
        class = if (choice == selected_stat_func()) {
          "btn-primary"
        } else {
          "btn-secondary"
        }
      )
    })
  })
  lapply(stat_choices, function(choice) {
    shiny::observeEvent(
      input[[paste0("stat_func_", gsub("\\s|\\-", "_", choice))]],
      {
        selected_stat_func(choice)
      }
    )
  })

  # --- Logic for Custom Button Group: Exploratory Vis ---
  exploratory_choices <- c(
    "Boxplots",
    "Violin Plots",
    "Correlation Plots",
    "Error-Bar Plot",
    "Dual-Flashlight Plot",
    "Heatmap",
    "Skewness/Kurtosis",
    "Volcano Plot"
  )
  output$exploratory_function_ui <- shiny::renderUI({
    lapply(exploratory_choices, function(choice) {
      shiny::actionButton(
        inputId = paste0("exp_func_", gsub("\\s|\\-", "_", choice)),
        label = choice,
        class = if (choice == selected_exploratory_func()) {
          "btn-primary"
        } else {
          "btn-secondary"
        }
      )
    })
  })
  lapply(exploratory_choices, function(choice) {
    shiny::observeEvent(
      input[[paste0("exp_func_", gsub("\\s|\\-", "_", choice))]],
      {
        selected_exploratory_func(choice)
        selected_function(choice)
        currentPage("step4")
        currentStep(4)
      }
    )
  })

  # --- Logic for Custom Button Group: Multivariate ---
  multivariate_choices <- c(
    "Principal Component Analysis (PCA)",
    "Partial Least Squares Regression (PLSR)",
    "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)",
    "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
  )
  output$multivariate_function_ui <- shiny::renderUI({
    lapply(multivariate_choices, function(choice) {
      shiny::actionButton(
        inputId = paste0("multi_func_", gsub("\\s|\\-", "_", choice)),
        label = choice,
        class = if (choice == selected_multivariate_func()) {
          "btn-primary"
        } else {
          "btn-secondary"
        }
      )
    })
  })
  lapply(multivariate_choices, function(choice) {
    shiny::observeEvent(
      input[[paste0("multi_func_", gsub("\\s|\\-", "_", choice))]],
      {
        selected_multivariate_func(choice)
      }
    )
  })

  # --- Logic for Custom Button Group: Machine Learning ---
  ml_choices <- c("Random Forest", "Extreme Gradient Boosting (XGBoost)")
  output$ml_function_ui <- shiny::renderUI({
    lapply(ml_choices, function(choice) {
      shiny::actionButton(
        inputId = paste0("ml_func_", gsub("\\s|\\-", "_", choice)),
        label = choice,
        class = if (choice == selected_ml_func()) {
          "btn-primary"
        } else {
          "btn-secondary"
        }
      )
    })
  })
  lapply(ml_choices, function(choice) {
    shiny::observeEvent(
      input[[paste0("ml_func_", gsub("\\s|\\-", "_", choice))]],
      {
        selected_ml_func(choice)
      }
    )
  })
  # Calculating percentage for progress bar
  totalSteps <- 5
  shiny::observeEvent(currentStep(), {
    step_i <- currentStep()
    if (step_i == totalSteps) {
      pct <- 100
      title <- "Finished!"
    } else {
      pct <- round((step_i - 1) / totalSteps * 100)
      title <- paste("Step", step_i, "of", totalSteps)
    }

    shinyWidgets::updateProgressBar(
      session,
      "wizard_pb",
      value = pct,
      title = title
    )
  })
  ## ---------------------------
  ## Navigation: Next/Back Buttons & Save User State
  ## ---------------------------
  shiny::observeEvent(input$next1, {
    # Check if a file has been uploaded OR if the "use built-in" box is checked
    if (!isTRUE(input$use_builtin) && !isTRUE(bioplex$active)) {
      shiny::showModal(shiny::modalDialog(
        title = "Confirm your data first",
        "Please open the Data Editor and click 'Save & Use' (or choose a built-in dataset) before continuing.",
        easyClose = TRUE,
        footer = shiny::actionButton("ok_no_data", "OK")
      ))
    } else {
      currentPage("step2")
      currentStep(2)
    }
  })
  shiny::observeEvent(input$ok_no_data, {
    shiny::removeModal() # close the popup
    currentPage("step1") # navigate back to Upload Data step
  })

  # A) Combine the two checkboxGroupInputs
  selected_columns_combined <- shiny::reactive({
    unique(c(
      input$selected_categorical_cols %||% character(0),
      input$selected_numerical_cols %||% character(0)
    ))
  })

  # B) "Select / Deselect All" observers
  shiny::observeEvent(input$select_all_cat, {
    cat_cols <- step2_typed_col_info()$categorical
    shiny::updateCheckboxGroupInput(
      session,
      "selected_categorical_cols",
      selected = cat_cols
    )
  })
  shiny::observeEvent(input$deselect_all_cat, {
    shiny::updateCheckboxGroupInput(
      session,
      "selected_categorical_cols",
      selected = character(0)
    )
  })
  shiny::observeEvent(input$select_all_num, {
    numeric_cols <- step2_typed_col_info()$numerical
    shiny::updateCheckboxGroupInput(
      session,
      "selected_numerical_cols",
      selected = numeric_cols
    )
  })
  shiny::observeEvent(input$deselect_all_num, {
    shiny::updateCheckboxGroupInput(
      session,
      "selected_numerical_cols",
      selected = character(0)
    )
  })
  # C) Base reactive to apply row deletions and categorical filters
  # 1. Always keep the filtered-but-raw data
  raw_filtered <- shiny::reactive({
    df <- step2_typed_data()
    col_info <- step2_typed_col_info()
    # (1) apply any row deletions
    if (!is.null(userState$deleted_row_ids)) {
      df <- df[!df$..cyto_id.. %in% userState$deleted_row_ids, , drop = FALSE]
    }
    # (2) apply filters
    filter_cols <- intersect(
      input$selected_categorical_cols %||% character(0),
      col_info$categorical
    )
    if (length(filter_cols)) {
      for (col in filter_cols) {
        fid <- paste0("filter_", col)
        if (fid %in% names(input)) {
          df <- df[df[[col]] %in% input[[fid]], , drop = FALSE]
        }
      }
    }
    df
  }) |>
    shiny::debounce(500)

  step2_scale_label <- function(scale_choice) {
    switch(
      scale_choice,
      log2 = "log2",
      log10 = "log10",
      zscore = "Z-score",
      "Transformation"
    )
  }

  data_after_filters <- shiny::reactive({
    df <- raw_filtered()
    shiny::req(df)
    scale_choice <- input$step2_scale %||% "none"
    if (!identical(scale_choice, "none")) {
      num_cols <- intersect(
        input$selected_numerical_cols %||% character(0),
        step2_typed_col_info()$numerical
      )
      if (length(num_cols)) {
        df <- tryCatch(
          apply_scale(
            data = df,
            columns = num_cols,
            scale = scale_choice
          ),
          error = function(e) {
            shiny::validate(shiny::need(FALSE, conditionMessage(e)))
          }
        )
      }
    }
    df
  }) |>
    shiny::debounce(500)

  data_after_imputation <- shiny::reactive({
    dat <- data_after_filters()
    imp <- imputed_data()
    if (!is.null(imp)) {
      return(imp)
    }
    dat
  }) |>
    shiny::debounce(500)

  # D) The main filteredData() used by DT
  filteredData <- shiny::reactive({
    df <- data_after_imputation()
    shiny::req(df)
    cols_to_keep <-
      if (currentStep() >= 3) {
        shiny::req(userState$selected_columns)
        userState$selected_columns
      } else {
        shiny::req(selected_columns_combined())
        selected_columns_combined()
      }

    # Always keep the internal ID for deletes
    final_cols <- union(cols_to_keep, "..cyto_id..")
    df[, intersect(names(df), final_cols), drop = FALSE]
  }) |>
    shiny::debounce(500)

  # Populate the sPLS-DA "Label column" choices whenever the working data changes
  shiny::observeEvent(data_after_filters(), {
    df <- data_after_filters()
    shiny::req(nrow(df) > 0)

    # Start with all non-numeric columns available after filters
    cand <- names(df)[!vapply(df, is.numeric, logical(1))]

    # Always include common label columns if present
    extra <- unique(na.omit(c(
      input$splsda_group_col,
      input$splsda_group_col2,
      input$splsda_multilevel,
      input$splsda_batch_col
    )))
    extra <- intersect(extra, names(df))

    cand <- unique(c(extra, cand))
    cand <- setdiff(cand, "..cyto_id..") # never expose internal id

    shiny::updateSelectInput(
      session,
      "splsda_ind_names_col",
      choices = cand,
      selected = shiny::isolate(userState$splsda_ind_names_col) %||%
        if (length(cand)) cand[1] else NULL
    )
  })
  # E) Render the table in Step 2
  output$filtered_data_preview <- DT::renderDT({
    df <- filteredData()
    shiny::req(nrow(df) > 0)

    df$..cyto_id.. <- NULL # Remove the internal ID column for display
    DT::datatable(
      df,
      selection = "multiple",
      # remove scrollY, let it paginate
      options = list(
        pageLength = 5, # default rows/page
        lengthMenu = list(
          # rows/page dropdown
          c(5, 10, 25, 50, -1),
          c("5", "10", "25", "50", "All")
        ),
        scrollX = TRUE,
        scrollCollapse = TRUE
      ),
      class = "stripe hover"
    )
  })

  shiny::observeEvent(input$expand_filtered, {
    shiny::showModal(shiny::modalDialog(
      title = "Filtered Data Explorer - Full View",
      DT::DTOutput("filtered_data_preview_modal"),
      size = "l",
      easyClose = TRUE,
      footer = shiny::div(
        class = "d-flex justify-content-center gap-2 flex-wrap w-100",
        shiny::actionButton(
          "delete_selected_rows_modal",
          "Delete Selected",
          icon = shiny::icon("trash"),
          class = "btn-danger"
        ),
        shiny::modalButton("Close")
      )
    ))
  })
  output$filtered_data_preview_modal <- DT::renderDT({
    df <- filteredData()
    shiny::req(nrow(df) > 0)

    df$..cyto_id.. <- NULL # Remove the internal ID column for display
    DT::datatable(
      df,
      selection = "multiple",
      # remove scrollY, let it paginate
      options = list(
        pageLength = 5, # default rows/page
        lengthMenu = list(
          # rows/page dropdown
          c(5, 10, 25, 50, -1),
          c("5", "10", "25", "50", "All")
        ),
        scrollX = TRUE,
        scrollCollapse = TRUE
      ),
      class = "stripe hover"
    )
  })

  # E. Update the conditional_filter_ui to use the new categorical input
  output$conditional_filter_ui <- shiny::renderUI({
    # The filter UI now depends only on the selection of categorical columns.
    if (
      !is.null(input$selected_categorical_cols) &&
        length(input$selected_categorical_cols) > 0
    ) {
      bslib::card(
        bslib::card_header(class = "bg-info", "4. Optional: Apply Filters"),
        bslib::card_body(
          bslib::accordion(
            id = "filter_accordion",
            open = shiny::isTruthy(input$filter_accordion),
            bslib::accordion_panel(
              "Filter by Categorical Values",
              shiny::uiOutput("filter_ui"),
              icon = fontawesome::fa("filter")
            )
          )
        )
      )
    } else {
      NULL
    }
  })

  shiny::observeEvent(input$open_impute_modal, {
    shiny::showModal(shiny::modalDialog(
      title = "Treat Missing Values",
      size = "l",
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::actionButton("apply_impute", "Apply", class = "btn-primary"),
        shiny::modalButton("Cancel")
      ),
      # --- simple UI for 5 methods ---
      shiny::fluidRow(
        shiny::column(4, {
          df <- data_after_filters()
          cols <- setdiff(names(df), c(".cyto_id.", "..cyto_id.."))
          shiny::checkboxGroupInput(
            "imp_cols",
            "Columns to include",
            choices = cols,
            selected = {
              if (!is.null(userState$impute_meta$cols)) {
                intersect(cols, userState$impute_meta$cols)
              } else {
                cols
              }
            }
          )
        }),
        shiny::column(
          4,
          shiny::radioButtons(
            ui_imputation_method_input_id(),
            label = shiny::tagList(
              shiny::span("Method", style = "margin-right:10px;"),
              bslib::popover(
                shiny::actionLink(
                  "mv_help",
                  NULL,
                  icon = shiny::icon("question-circle")
                ),
                shiny::div(
                  shiny::tags$h4(
                    "Choosing an imputation method",
                    class = "mb-3"
                  ),
                  shiny::p(
                    shiny::tags$b("Default behavior:"),
                    " Mean imputation is selected by default. For kNN methods, the default number of neighbors is ",
                    shiny::tags$code("k = 5"),
                    ". Numeric standardization for kNN is off by default unless you turn it on."
                  ),
                  shiny::tags$h5("How to choose an imputation method"),
                  shiny::tags$ul(
                    shiny::tags$li(
                      "If missingness is low and you want a simple numeric fill, start with median for skewed cytokine data and mean for roughly symmetric data."
                    ),
                    shiny::tags$li(
                      "If preserving multivariate structure matters, consider kNN instead of a simple fill."
                    ),
                    shiny::tags$li(
                      "Use sample-wise kNN when you expect biologically similar samples or subjects."
                    ),
                    shiny::tags$li(
                      "Use feature-wise kNN when correlated cytokines are expected and only numeric features are being imputed."
                    ),
                    shiny::tags$li(
                      "If missingness is substantial or clearly differs by group, interpret downstream results more cautiously."
                    )
                  ),
                  shiny::tags$h5("Method-by-method guidance"),
                  shiny::p(
                    shiny::tags$b("Mean:"),
                    " numeric columns only. This is a simple baseline when missingness is limited and values are roughly symmetric. It can reduce variability and pull group summaries toward the center."
                  ),
                  shiny::p(
                    shiny::tags$b("Median:"),
                    " numeric columns only. This is often a better choice when cytokine values are skewed or outliers are present. It still compresses spread and can mute real group differences."
                  ),
                  shiny::p(
                    shiny::tags$b("Mode:"),
                    " categorical columns only in the current app. It is useful for labels or discrete annotations, not for continuous cytokine concentrations. It can over-represent the most common category."
                  ),
                  shiny::p(
                    shiny::tags$b("kNN (sample-wise):"),
                    " works across the selected columns using similar samples or rows and can handle mixed data. This is useful when biologically similar subjects are expected. It can blur separation between phenotypes or treatment groups if nearest neighbors come from different groups."
                  ),
                  shiny::p(
                    shiny::tags$b("kNN (feature-wise):"),
                    " numeric columns only. This method uses similar analytes or features across samples, which can be useful when correlated cytokines tend to move together. It can reinforce correlation patterns and overstate coordinated biology."
                  ),
                  shiny::tags$h5("Bias and interpretation notes"),
                  shiny::p(
                    "Simple imputation methods can underestimate variability and affect p-values, clustering, and multivariate models in cytokine profiling studies."
                  ),
                  shiny::p(
                    "kNN usually preserves local structure better than mean or median imputation, but it still inserts modeled values rather than directly observed measurements."
                  )
                ),
                placement = "auto",
                options = list(
                  container = "body",
                  boundary = "viewport",
                  customClass = "impute-method-popover",
                  fallbackPlacements = c("left", "bottom", "top", "right")
                )
              )
            ),
            choices = ui_imputation_method_choices(),
            selected = userState$impute_meta$method %||% "mean"
          ),
          shiny::conditionalPanel(
            ui_imputation_knn_condition_expr(),
            shiny::numericInput(
              "imp_k",
              "k neighbors",
              value = userState$impute_meta$k %||% 5,
              min = 1,
              step = 1
            ),
            shiny::checkboxInput(
              "imp_scale",
              "Standardize numeric vars before k-NN",
              value = isTRUE(userState$impute_meta$scaled)
            )
          )
        ),
        shiny::column(
          4,
          shiny::verbatimTextOutput("imp_na_before"),
          shiny::verbatimTextOutput("imp_na_after")
        )
      )
    ))
  })

  shiny::observe({
    df <- data_after_filters()
    shiny::req(df)
    show_btn <- anyNA(df) # or compute prop_miss >= 0.05
    shinyjs::toggle(id = "open_impute_modal", condition = show_btn)
  })
  output$imp_na_before <- shiny::renderPrint({
    d <- data_after_filters()
    c(
      "Total Missing Values" = sum(is.na(d)),
      "% Missing Values" = round(100 * mean(is.na(d)), 2)
    )
  })
  output$imp_na_after <- shiny::renderPrint({
    shiny::req(imputed_data())
    d <- imputed_data()
    c(
      "Total Missing Values" = sum(is.na(d)),
      "% Missing Values" = round(100 * mean(is.na(d)), 2)
    )
  })
  .stat_mode <- function(x) {
    ux <- unique(x[!is.na(x)])
    if (!length(ux)) {
      return(NA)
    }
    ux[which.max(tabulate(match(x, ux)))]
  }

  impute_data <- function(df, include, method, k = 5, scale_for_knn = TRUE) {
    stopifnot(all(include %in% names(df)))
    dat <- df
    num_cols <- include[sapply(dat[include], is.numeric)]
    cat_cols <- setdiff(include, num_cols)

    if (method %in% c("mean", "median")) {
      if (length(num_cols)) {
        fun <- if (method == "mean") mean else median
        for (nm in num_cols) {
          if (anyNA(dat[[nm]])) {
            dat[[nm]][is.na(dat[[nm]])] <- fun(dat[[nm]], na.rm = TRUE)
          }
        }
      }
    } else if (method == "mode") {
      if (length(cat_cols)) {
        for (nm in cat_cols) {
          m <- .stat_mode(dat[[nm]])
          dat[[nm]][is.na(dat[[nm]])] <- m
          if (is.factor(df[[nm]])) {
            dat[[nm]] <- factor(dat[[nm]], levels = union(levels(df[[nm]]), m))
          }
        }
      }
    } else if (method == "knn_sample") {
      # mixed types via recipes::step_impute_knn (Gower)

      rec <- recipes::recipe(~., data = dat)
      if (scale_for_knn && length(num_cols)) {
        rec <- rec |> recipes::step_normalize(dplyr::all_of(num_cols))
      }
      rec <- rec |>
        recipes::step_impute_knn(dplyr::all_of(include), neighbors = k)
      dat <- recipes::bake(recipes::prep(rec, training = dat), new_data = dat)
    } else if (method == "knn_feature") {
      # numeric-only, neighbors across features using impute::impute.knn
      if (!length(num_cols)) {
        stop("Feature-wise k-NN requires numeric columns.")
      }

      M <- as.matrix(dat[num_cols])
      if (scale_for_knn) {
        center <- colMeans(M, na.rm = TRUE)
        scalev <- apply(M, 2, sd, na.rm = TRUE)
        scalev[!is.finite(scalev) | scalev == 0] <- 1
        Ms <- scale(M, center = center, scale = scalev)
        out <- impute::impute.knn(Ms, k = k)$data
        out <- sweep(out, 2, scalev, "*")
        out <- sweep(out, 2, center, "+")
      } else {
        out <- impute::impute.knn(M, k = k)$data
      }
      dat[num_cols] <- as.data.frame(out)
    }
    dat
  }
  shiny::observeEvent(input$apply_impute, {
    impute_method <- input[[ui_imputation_method_input_id()]]
    df <- data_after_filters() # <-- impute the *filtered* data
    sel <- input$imp_cols
    if (!length(sel)) {
      shiny::showNotification("Select >=1 column.", type = "error")
      return()
    }

    dat_imp <- impute_data(
      df,
      include = sel,
      method = impute_method,
      k = input$imp_k %||% 5,
      scale_for_knn = isTRUE(input$imp_scale)
    )
    imputed_data(dat_imp)
    userState$impute_meta <- list(
      method = impute_method,
      k = input$imp_k %||% 5,
      cols = sel,
      scaled = isTRUE(input$imp_scale)
    )
    shiny::removeModal()
  })

  # ============================================================================
  # Reactive holders for the before/after comparison
  # ============================================================================
  comparison_data <- shiny::reactiveVal(list(
    orig = NULL,
    trans = NULL,
    num_cols = character(),
    scale_key = "none",
    scale_label = "None"
  ))

  show_comparison <- shiny::reactiveVal(FALSE)

  # 1) Define the comparison plot output up front:
  output$norm_compare <- shiny::renderPlot({
    shiny::req(show_comparison())
    cmp <- comparison_data()
    orig <- cmp$orig
    trans <- cmp$trans
    num_cols <- cmp$num_cols
    scale_label <- cmp$scale_label %||% "Transformation"
    shiny::req(length(num_cols) > 0)

    # Cap variables shown in boxplots for readability
    MAX_BOX_VARS <- 40
    box_cols <- if (length(num_cols) > MAX_BOX_VARS) {
      num_cols[seq_len(MAX_BOX_VARS)]
    } else {
      num_cols
    }

    # Long-format data
    make_long <- function(df, cols) {
      out <- tidyr::pivot_longer(
        df[, cols, drop = FALSE],
        cols = tidyr::everything(),
        names_to = "variable",
        values_to = "value"
      )
      out$variable <- factor(out$variable, levels = cols)
      out[!is.na(out$value), , drop = FALSE]
    }

    df_before_hist <- make_long(orig, num_cols)
    df_after_hist <- make_long(trans, num_cols)
    df_before_box <- make_long(orig, box_cols)
    df_after_box <- make_long(trans, box_cols)

    shiny::req(nrow(df_before_hist) > 0, nrow(df_after_hist) > 0)

    # Shared theme
    base_theme <- ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 12, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 8, colour = "gray50"),
        panel.grid.minor = ggplot2::element_blank()
      )
    # Helper: trim x-axis to the 1st-99th percentile for display only
    trim_limits <- function(x, lo = 0.01, hi = 0.99) {
      qs <- stats::quantile(x, probs = c(lo, hi), na.rm = TRUE)
      list(lo = qs[[1]], hi = qs[[2]])
    }
    before_lims <- trim_limits(df_before_hist$value)
    after_lims <- trim_limits(df_after_hist$value)
    # 1. Density plots (top row)
    dens_before <- ggplot2::ggplot(
      df_before_hist,
      ggplot2::aes(x = value)
    ) +
      ggplot2::geom_density(
        colour = "steelblue4",
        fill = "steelblue",
        alpha = 0.25,
        adjust = 1.5, # smoother curve
        na.rm = TRUE
      ) +
      ggplot2::coord_cartesian(
        xlim = c(before_lims$lo, before_lims$hi) # zoom without dropping data
      ) +
      ggplot2::labs(
        title = paste("Pooled Distribution Before", scale_label),
        subtitle = "Showing 1st\u201399th percentile range. All values used for density estimation.",
        x = "Value",
        y = "Density"
      ) +
      base_theme

    dens_after <- ggplot2::ggplot(
      df_after_hist,
      ggplot2::aes(x = value)
    ) +
      ggplot2::geom_density(
        colour = "tomato3",
        fill = "tomato",
        alpha = 0.25,
        adjust = 1.5,
        na.rm = TRUE
      ) +
      ggplot2::coord_cartesian(
        xlim = c(after_lims$lo, after_lims$hi)
      ) +
      ggplot2::labs(
        title = paste("Pooled Distribution After", scale_label),
        subtitle = "Showing 1st\u201399th percentile range. All values used for density estimation.",
        x = "Value",
        y = "Density"
      ) +
      base_theme

    # 2. Horizontal boxplots (bottom row)
    box_before <- ggplot2::ggplot(
      df_before_box,
      ggplot2::aes(x = variable, y = value)
    ) +
      ggplot2::geom_boxplot(
        fill = "steelblue",
        colour = "steelblue4",
        alpha = 0.7,
        outlier.size = 0.8,
        outlier.alpha = 0.5,
        na.rm = TRUE
      ) +
      ggplot2::coord_flip() + # <-- horizontal
      ggplot2::labs(
        title = paste("Before", scale_label),
        x = NULL,
        y = "Value"
      ) +
      base_theme +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 8),
        axis.text.x = ggplot2::element_text(size = 8)
      )

    box_after <- ggplot2::ggplot(
      df_after_box,
      ggplot2::aes(x = variable, y = value)
    ) +
      ggplot2::geom_boxplot(
        fill = "tomato",
        colour = "tomato4",
        alpha = 0.7,
        outlier.size = 0.8,
        outlier.alpha = 0.5,
        na.rm = TRUE
      ) +
      ggplot2::coord_flip() + # <-- horizontal
      ggplot2::labs(
        title = paste("After", scale_label),
        x = NULL,
        y = "Value"
      ) +
      base_theme +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(), # shared y with left panel
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 8)
      )

    # 3. Compose
    comparison_plot <- patchwork::wrap_plots(
      dens_before,
      dens_after,
      box_before,
      box_after,
      ncol = 2,
      nrow = 2,
      heights = c(0.3, 0.7), # give more space to boxplots
      widths = c(0.5, 0.5)
    ) +
      patchwork::plot_annotation(
        title = paste("Before vs After", scale_label),
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold")
        )
      )

    print(comparison_plot)
  })

  # 2) When they click "Next" on Step 2, save the selected columns and log2 option
  shiny::observeEvent(input$next2, {
    # save state
    userState$selected_columns <- selected_columns_combined()
    userState$selected_categorical_cols <- input$selected_categorical_cols
    userState$selected_numerical_cols <- input$selected_numerical_cols
    userState$step2_scale <- input$step2_scale
    userState$step2_factor_cols <- userState$step2_applied_factor_cols %||%
      character(0)
    userState$step2_numeric_override_cols <-
      userState$step2_applied_numeric_override_cols %||%
      character(0)
    userState$step2_factor_order_enable <- isTRUE(
      userState$step2_applied_factor_order_enable
    )
    userState$step2_factor_order_col <- userState$step2_applied_factor_order_col
    userState$step2_factor_levels_csv <-
      userState$step2_applied_factor_levels_csv %||%
      ""
    currentPage("step3")
    currentStep(3)
  })

  shiny::observeEvent(input$preview_transform, {
    scale_choice <- input$step2_scale %||% "none"
    if (identical(scale_choice, "none")) {
      shiny::showNotification(
        "Select a preprocessing method before previewing the transformation.",
        type = "message"
      )
      return()
    }

    orig <- raw_filtered()
    num_cols <- intersect(input$selected_numerical_cols, names(orig))
    shiny::req(length(num_cols) > 0)

    trans <- tryCatch(
      apply_scale(
        data = orig,
        columns = num_cols,
        scale = scale_choice
      ),
      error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
        return(NULL)
      }
    )
    shiny::req(!is.null(trans))
    scale_label <- step2_scale_label(scale_choice)

    comparison_data(list(
      orig = orig,
      trans = trans,
      num_cols = num_cols,
      scale_key = scale_choice,
      scale_label = scale_label
    ))
    show_comparison(TRUE)

    shiny::showModal(
      shiny::modalDialog(
        title = paste("Before vs After", scale_label, "Transformation"),
        shiny::plotOutput("norm_compare", height = "700px"),
        footer = shiny::modalButton("Close"),
        size = "l"
      )
    )
  })

  # Save Step 4 inputs before running the analysis so unchanged defaults persist
  shiny::observeEvent(input$next4, {
    shiny::req(currentStep() == 4, !is.null(selected_function()))
    userState$selected_function <- selected_function()
    if (currentStep() == 4 && !is.null(selected_function())) {
      if (selected_function() == "Univariate Tests (T-test, Wilcoxon)") {
        userState$uv2_method <- input$uv2_method
        userState$uv2_p_adjust_method <- input$uv2_p_adjust_method
      }
      if (
        selected_function() ==
          "Multi-level Univariate Tests (Anova, Kruskal-Wallis)"
      ) {
        userState$uvm_method <- input$uvm_method
        if (!is.null(input$uvm_p_adjust_method)) {
          userState$uvm_p_adjust_method <- input$uvm_p_adjust_method
        }
      }
      if (selected_function() == "Two-way ANOVA") {
        userState$twa_primary_cat_var <- input$twa_primary_cat_var
        userState$twa_secondary_cat_var <- input$twa_secondary_cat_var
        userState$twa_include_primary_secondary_interaction <-
          input$twa_include_primary_secondary_interaction
      }
      if (selected_function() == "ANCOVA") {
        userState$anc_primary_cat_var <- input$anc_primary_cat_var
        userState$anc_secondary_cat_var <- input$anc_secondary_cat_var
        userState$anc_covariate_col <- input$anc_covariate_col
        userState$anc_include_primary_secondary_interaction <-
          input$anc_include_primary_secondary_interaction
        userState$anc_include_primary_covariate_interaction <-
          input$anc_include_primary_covariate_interaction
        userState$anc_include_secondary_covariate_interaction <-
          input$anc_include_secondary_covariate_interaction
      }
      if (selected_function() == "Boxplots") {
        userState$bp_group_by <- input$bp_group_by
        userState$bp_y_lim <- input$bp_y_lim
        userState$bp_bin_size <- input$bp_bin_size
      }
      if (selected_function() == "Violin Plots") {
        userState$vio_group_by <- input$vio_group_by
        userState$vio_bin_size <- input$vio_bin_size
        userState$vio_y_lim <- input$vio_y_lim
        userState$vio_boxplot_overlay <- input$vio_boxplot_overlay
      }
      if (selected_function() == "Error-Bar Plot") {
        userState$eb_group_col <- input$eb_group_col
        userState$eb_p_lab <- input$eb_p_lab
        userState$eb_es_lab <- input$eb_es_lab
        userState$eb_class_symbol <- input$eb_class_symbol
        userState$eb_x_lab <- input$eb_x_lab
        userState$eb_y_lab <- input$eb_y_lab
        userState$eb_title <- input$eb_title
        userState$eb_stat <- input$eb_stat
        userState$eb_error <- input$eb_error
        userState$eb_method <- input$eb_method
        userState$eb_p_adjust_method <- input$eb_p_adjust_method
        userState$eb_n_col <- input$eb_n_col
        userState$eb_fill_palette <- if (
          identical(input$eb_fill_palette, "grey")
        ) {
          "gray"
        } else {
          input$eb_fill_palette
        }
      }
      if (selected_function() == "Dual-Flashlight Plot") {
        userState$df_group_var <- input$df_group_var
        userState$df_ssmd_thresh <- input$df_ssmd_thresh
        userState$df_log2fc_thresh <- input$df_log2fc_thresh
        userState$df_top_labels <- input$df_top_labels
        userState$df_cond1 <- input$df_cond1
        userState$df_cond2 <- input$df_cond2
      }
      if (selected_function() == "Heatmap") {
        userState$hm_annotation <- input$hm_annotation
        userState$hm_ann_side <- input$hm_ann_side
      }
      if (selected_function() == "Correlation Plots") {
        userState$corr_group_col <- input$corr_group_col
        userState$corr_target <- input$corr_target
        userState$corr_by_group <- input$corr_by_group
      }
      if (selected_function() == "Principal Component Analysis (PCA)") {
        userState$pca_group_col <- input$pca_group_col
        userState$pca_group_col2 <- input$pca_group_col2
        userState$pca_comp_num <- input$pca_comp_num
        userState$pca_ellipse <- input$pca_ellipse
        userState$pca_style <- input$pca_style
        userState$pca_pch <- input$pca_pch
        userState$pca_colors <- input$pca_colors
      }
      if (selected_function() == "Partial Least Squares Regression (PLSR)") {
        userState$plsr_group_col <- input$plsr_group_col
        userState$plsr_response_col <- input$plsr_response_col
        userState$plsr_predictor_cols <- input$plsr_predictor_cols
        userState$plsr_comp_num <- input$plsr_comp_num
        userState$plsr_sparse <- input$plsr_sparse
        userState$plsr_keepX <- input$plsr_keepX
        userState$plsr_cv_opt <- input$plsr_cv_opt
        userState$plsr_fold_num <- input$plsr_fold_num
        userState$plsr_ellipse <- input$plsr_ellipse
        userState$plsr_colors <- input$plsr_colors
      }
      if (selected_function() == "Random Forest") {
        userState$rf_group_col <- input$rf_group_col
        userState$rf_ntree <- input$rf_ntree
        userState$rf_mtry <- input$rf_mtry
        userState$rf_train_fraction <- input$rf_train_fraction
        userState$rf_plot_roc <- input$rf_plot_roc
        userState$rf_run_rfcv <- input$rf_run_rfcv
        userState$rf_k_folds <- input$rf_k_folds
        userState$rf_step <- input$rf_step
      }
      if (selected_function() == "Skewness/Kurtosis") {
        userState$skku_group_cols <- input$skku_group_cols
        userState$skku_print_raw <- input$skku_print_raw
        userState$skku_print_log <- input$skku_print_log
      }
      if (
        selected_function() ==
          "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
      ) {
        userState$splsda_group_col <- input$splsda_group_col
        userState$splsda_group_col2 <- input$splsda_group_col2
        userState$splsda_use_batch_corr <- input$splsda_use_batch_corr
        userState$splsda_batch_col <- input$splsda_batch_col
        userState$splsda_use_multilevel <- input$splsda_use_multilevel
        userState$splsda_multilevel <- input$splsda_multilevel
        userState$splsda_var_num <- input$splsda_var_num
        userState$splsda_cv_opt <- input$splsda_cv_opt
        userState$splsda_fold_num <- input$splsda_fold_num
        userState$splsda_comp_num <- input$splsda_comp_num
        userState$splsda_pch <- input$splsda_pch
        userState$splsda_ind_names_mode <- input$splsda_ind_names_mode
        userState$splsda_ind_names_col <- input$splsda_ind_names_col
        userState$splsda_style <- input$splsda_style
        userState$splsda_roc <- input$splsda_roc
        userState$splsda_ellipse <- input$splsda_ellipse
        userState$splsda_bg <- input$splsda_bg
        userState$splsda_conf_mat <- input$splsda_conf_mat
        userState$splsda_colors <- input$splsda_colors
      }
      if (
        selected_function() ==
          "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
      ) {
        userState$mint_splsda_group_col <- input$mint_splsda_group_col
        userState$mint_splsda_group_col2 <- input$mint_splsda_group_col2
        userState$mint_splsda_batch_col <- input$mint_splsda_batch_col
        userState$mint_splsda_var_num <- input$mint_splsda_var_num
        userState$mint_splsda_comp_num <- input$mint_splsda_comp_num
        userState$mint_splsda_cim <- input$mint_splsda_cim
        userState$mint_splsda_ellipse <- input$mint_splsda_ellipse
        userState$mint_splsda_bg <- input$mint_splsda_bg
        userState$mint_splsda_roc <- input$mint_splsda_roc
        userState$mint_splsda_colors <- input$mint_splsda_colors
      }
      if (selected_function() == "Volcano Plot") {
        userState$volc_group_col <- input$volc_group_col
        userState$volc_cond1 <- input$volc_cond1
        userState$volc_cond2 <- input$volc_cond2
        userState$volc_fold_change_thresh <- input$volc_fold_change_thresh
        userState$volc_p_value_thresh <- input$volc_p_value_thresh
        userState$volc_top_labels <- input$volc_top_labels
      }
      if (selected_function() == "Extreme Gradient Boosting (XGBoost)") {
        userState$xgb_group_col <- input$xgb_group_col
        userState$xgb_train_fraction <- input$xgb_train_fraction
        userState$xgb_nrounds <- input$xgb_nrounds
        userState$xgb_max_depth <- input$xgb_max_depth
        userState$xgb_eta <- input$xgb_eta
        userState$xgb_nfold <- input$xgb_nfold
        userState$xgb_cv <- input$xgb_cv
        userState$xgb_eval_metric <- input$xgb_eval_metric
        userState$xgb_top_n_features <- input$xgb_top_n_features
        userState$xgb_plot_roc <- input$xgb_plot_roc
      }

      font_spec <- get_analysis_font_spec(selected_function())
      if (!is.null(font_spec)) {
        userState[[font_spec$state_key]] <- font_settings_state_from_inputs(
          input = input,
          prefix = font_spec$prefix,
          supported_fields = font_spec$supported_fields,
          default_font_settings = font_spec$default_font_settings
        )
      }
    }
  })
  # For error message screen.
  shiny::observeEvent(input$back6, {
    currentPage("step4")
    currentStep(4)
  })

  invisible(app_stage_commit(app_ctx, stage_env))
}

init_update_inputs_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  userState <- app_ctx$userState
  currentStep <- app_ctx$currentStep
  step2_typed_col_info <- app_ctx$step2_typed_col_info
  data_after_filters <- app_ctx$data_after_filters
  get_analysis_font_spec <- app_ctx$get_analysis_font_spec
  restore_font_settings_inputs <- app_ctx$restore_font_settings_inputs
  selected_function <- app_ctx$selected_function
  filteredData <- app_ctx$filteredData
  ## ---------------------------
  ## Updating inputs by userState
  ## ---------------------------

  sync_step2_bucket_inputs <- function() {
    col_info <- step2_typed_col_info()
    categorical_cols <- col_info$categorical
    numerical_cols <- col_info$numerical

    cat_selected <- step2_restore_bucket_selection(
      userState$selected_columns,
      categorical_cols
    )
    num_selected <- step2_restore_bucket_selection(
      userState$selected_columns,
      numerical_cols
    )

    shiny::updateCheckboxGroupInput(
      session,
      "selected_categorical_cols",
      choices = categorical_cols,
      selected = cat_selected
    )
    shiny::updateCheckboxGroupInput(
      session,
      "selected_numerical_cols",
      choices = numerical_cols,
      selected = num_selected
    )
  }

  shiny::observe({
    if (!identical(currentStep(), 2)) {
      return()
    }

    sync_step2_bucket_inputs()
  })

  shiny::observeEvent(currentStep(), {
    shiny::req(currentStep())
    if (currentStep() == 1) {
      # restore the Use built-in toggle
      shiny::updateCheckboxInput(
        session,
        "use_builtin",
        value = userState$use_builtin
      )
      if (
        isTRUE(userState$use_builtin) && !is.null(userState$built_in_choice)
      ) {
        shiny::updateSelectInput(
          session,
          "built_in_choice",
          selected = userState$built_in_choice
        )
      }
    }
    if (currentStep() == 2) {
      # recompute the same choices used in step2UI()
      col_info <- step2_typed_col_info()
      all_cols <- col_info$all
      sync_step2_bucket_inputs()
      # restore the preprocessing selector
      shiny::updateSelectInput(
        session,
        "step2_scale",
        selected = userState$step2_scale %||% "none"
      )
      shiny::updateSelectizeInput(
        session,
        "factor_cols",
        choices = all_cols,
        selected = intersect(
          userState$step2_factor_cols %||% character(0),
          all_cols
        )
      )
      shiny::updateSelectizeInput(
        session,
        "numeric_override_cols",
        choices = all_cols,
        selected = intersect(
          userState$step2_numeric_override_cols %||% character(0),
          all_cols
        )
      )
      shiny::updateCheckboxInput(
        session,
        "factor_order_enable",
        value = isTRUE(userState$step2_factor_order_enable)
      )
      factor_order_choices <- intersect(
        userState$step2_factor_cols %||% character(0),
        all_cols
      )
      factor_order_selected <- intersect(
        userState$step2_factor_order_col %||% character(0),
        factor_order_choices
      )
      if (!length(factor_order_selected) && length(factor_order_choices)) {
        factor_order_selected <- factor_order_choices[1]
      }
      shiny::updateSelectInput(
        session,
        "factor_order_col",
        choices = factor_order_choices,
        selected = factor_order_selected
      )
      shiny::updateTextInput(
        session,
        "factor_levels_csv",
        value = userState$step2_factor_levels_csv %||% ""
      )
    }
    if (currentStep() == 3) {
      if (!is.null(userState$analysis_categories)) {
        shiny::updateRadioButtons(
          session,
          "analysis_categories",
          selected = userState$analysis_categories
        )
      }
      # re-select the dropdowns
      if (!is.null(userState$stat_function)) {
        shiny::updateSelectInput(
          session,
          "stat_function",
          selected = userState$stat_function
        )
      }
      if (!is.null(userState$exploratory_function)) {
        shiny::updateSelectInput(
          session,
          "exploratory_function",
          selected = userState$exploratory_function
        )
      }
      if (!is.null(userState$multivariate_function)) {
        shiny::updateSelectInput(
          session,
          "multivariate_function",
          selected = userState$multivariate_function
        )
      }
      if (!is.null(userState$ml_function)) {
        shiny::updateSelectInput(
          session,
          "ml_function",
          selected = userState$ml_function
        )
      }
    }
    if (currentStep() == 4 && !is.null(userState$selected_function)) {
      # Boxplots
      if (userState$selected_function == "Boxplots") {
        shiny::updateTextInput(session, "bp_y_lim", value = userState$bp_y_lim)
        shiny::updateNumericInput(
          session,
          "bp_bin_size",
          value = userState$bp_bin_size
        )
        shiny::updateSelectizeInput(
          session,
          "bp_group_by",
          selected = userState$bp_group_by
        )
      }

      if (userState$selected_function == "Violin Plots") {
        shiny::updateSelectizeInput(
          session,
          "vio_group_by",
          selected = userState$vio_group_by
        )
        shiny::updateNumericInput(
          session,
          "vio_bin_size",
          value = userState$vio_bin_size
        )
        shiny::updateTextInput(
          session,
          "vio_y_lim",
          value = userState$vio_y_lim
        )
        shiny::updateCheckboxInput(
          session,
          "vio_boxplot_overlay",
          value = userState$vio_boxplot_overlay
        )
      }
      if (
        userState$selected_function == "Univariate Tests (T-test, Wilcoxon)"
      ) {
        shiny::updateSelectInput(
          session,
          "uv2_method",
          selected = userState$uv2_method
        )
        shiny::updateSelectInput(
          session,
          "uv2_p_adjust_method",
          selected = userState$uv2_p_adjust_method %||% "BH"
        )
      }
      if (
        userState$selected_function ==
          "Multi-level Univariate Tests (Anova, Kruskal-Wallis)"
      ) {
        shiny::updateSelectInput(
          session,
          "uvm_method",
          selected = userState$uvm_method
        )
        shiny::updateSelectInput(
          session,
          "uvm_p_adjust_method",
          selected = userState$uvm_p_adjust_method %||% "BH"
        )
      }
      if (userState$selected_function == "Two-way ANOVA") {
        shiny::updateSelectInput(
          session,
          "twa_primary_cat_var",
          selected = userState$twa_primary_cat_var
        )
        shiny::updateSelectInput(
          session,
          "twa_secondary_cat_var",
          selected = userState$twa_secondary_cat_var
        )
        shiny::updateCheckboxInput(
          session,
          "twa_include_primary_secondary_interaction",
          value = userState$twa_include_primary_secondary_interaction
        )
      }
      if (userState$selected_function == "ANCOVA") {
        shiny::updateSelectInput(
          session,
          "anc_primary_cat_var",
          selected = userState$anc_primary_cat_var
        )
        shiny::updateSelectInput(
          session,
          "anc_secondary_cat_var",
          selected = userState$anc_secondary_cat_var
        )
        shiny::updateSelectInput(
          session,
          "anc_covariate_col",
          selected = userState$anc_covariate_col
        )
        shiny::updateCheckboxInput(
          session,
          "anc_include_primary_secondary_interaction",
          value = userState$anc_include_primary_secondary_interaction
        )
        shiny::updateCheckboxInput(
          session,
          "anc_include_primary_covariate_interaction",
          value = userState$anc_include_primary_covariate_interaction
        )
        shiny::updateCheckboxInput(
          session,
          "anc_include_secondary_covariate_interaction",
          value = userState$anc_include_secondary_covariate_interaction
        )
      }
      if (userState$selected_function == "Error-Bar Plot") {
        shiny::updateSelectInput(
          session,
          "eb_group_col",
          selected = userState$eb_group_col
        )
        shiny::updateCheckboxInput(
          session,
          "eb_p_lab",
          value = userState$eb_p_lab
        )
        shiny::updateCheckboxInput(
          session,
          "eb_es_lab",
          value = userState$eb_es_lab
        )
        shiny::updateCheckboxInput(
          session,
          "eb_class_symbol",
          value = userState$eb_class_symbol
        )
        shiny::updateTextInput(session, "eb_x_lab", value = userState$eb_x_lab)
        shiny::updateTextInput(session, "eb_y_lab", value = userState$eb_y_lab)
        shiny::updateTextInput(session, "eb_title", value = userState$eb_title)
        shiny::updateSelectInput(
          session,
          "eb_stat",
          selected = userState$eb_stat
        )
        shiny::updateSelectInput(
          session,
          "eb_error",
          selected = userState$eb_error
        )
        shiny::updateSelectInput(
          session,
          "eb_method",
          selected = userState$eb_method
        )
        shiny::updateSelectInput(
          session,
          "eb_p_adjust_method",
          selected = userState$eb_p_adjust_method %||% "BH"
        )
        shiny::updateNumericInput(
          session,
          "eb_n_col",
          value = userState$eb_n_col %||% 3
        )
        eb_fill_palette_selected <- userState$eb_fill_palette %||% "gray"
        if (identical(eb_fill_palette_selected, "grey")) {
          eb_fill_palette_selected <- "gray"
        }
        shiny::updateSelectInput(
          session,
          "eb_fill_palette",
          selected = eb_fill_palette_selected
        )
      }
      # Dual-Flashlight Plot
      if (userState$selected_function == "Dual-Flashlight Plot") {
        shiny::updateSelectInput(
          session,
          "df_group_var",
          selected = userState$df_group_var
        )
        shiny::updateSelectInput(
          session,
          "df_cond1",
          selected = userState$df_cond1
        )
        shiny::updateSelectInput(
          session,
          "df_cond2",
          selected = userState$df_cond2
        )
        shiny::updateNumericInput(
          session,
          "df_ssmd_thresh",
          value = userState$df_ssmd_thresh
        )
        shiny::updateNumericInput(
          session,
          "df_log2fc_thresh",
          value = userState$df_log2fc_thresh
        )
        shiny::updateNumericInput(
          session,
          "df_top_labels",
          value = userState$df_top_labels
        )
      }

      # Heatmap
      if (userState$selected_function == "Heatmap") {
        shiny::updateSelectInput(
          session,
          "hm_annotation",
          selected = userState$hm_annotation
        )
        shiny::updateSelectInput(
          session,
          "hm_ann_side",
          selected = userState$hm_ann_side
        )
      }

      # Principal Component Analysis (PCA)
      if (userState$selected_function == "Principal Component Analysis (PCA)") {
        shiny::updateSelectInput(
          session,
          "pca_group_col",
          selected = userState$pca_group_col
        )
        shiny::updateSelectInput(
          session,
          "pca_group_col2",
          selected = userState$pca_group_col2
        )
        shiny::updateNumericInput(
          session,
          "pca_comp_num",
          value = userState$pca_comp_num
        )
        shiny::updateCheckboxInput(
          session,
          "pca_ellipse",
          value = userState$pca_ellipse
        )
        shiny::updateSelectInput(
          session,
          "pca_style",
          selected = userState$pca_style
        )
        shiny::updateSelectizeInput(
          session,
          "pca_pch",
          selected = userState$pca_pch
        )
        shiny::updateSelectizeInput(
          session,
          "pca_colors",
          selected = userState$pca_colors
        )
      }
      # Partial Least Squares Regression (PLSR)
      if (
        userState$selected_function == "Partial Least Squares Regression (PLSR)"
      ) {
        shiny::updateSelectInput(
          session,
          "plsr_group_col",
          selected = userState$plsr_group_col
        )
        shiny::updateSelectInput(
          session,
          "plsr_response_col",
          selected = userState$plsr_response_col
        )
        shiny::updateSelectizeInput(
          session,
          "plsr_predictor_cols",
          selected = userState$plsr_predictor_cols
        )
        shiny::updateNumericInput(
          session,
          "plsr_comp_num",
          value = userState$plsr_comp_num
        )
        shiny::updateCheckboxInput(
          session,
          "plsr_sparse",
          value = userState$plsr_sparse
        )
        shiny::updateNumericInput(
          session,
          "plsr_keepX",
          value = userState$plsr_keepX
        )
        shiny::updateRadioButtons(
          session,
          "plsr_cv_opt",
          selected = userState$plsr_cv_opt
        )
        shiny::updateNumericInput(
          session,
          "plsr_fold_num",
          value = userState$plsr_fold_num
        )
        shiny::updateCheckboxInput(
          session,
          "plsr_ellipse",
          value = userState$plsr_ellipse
        )
        shiny::updateSelectizeInput(
          session,
          "plsr_colors",
          selected = userState$plsr_colors
        )
      }

      # Correlation Plots
      if (userState$selected_function == "Correlation Plots") {
        shiny::updateSelectInput(
          session,
          "corr_group_col",
          selected = userState$corr_group_col
        )
        shiny::updateSelectInput(
          session,
          "corr_target",
          selected = userState$corr_target
        )
        shiny::updateCheckboxInput(
          session,
          "corr_by_group",
          value = userState$corr_by_group
        )
      }

      # Random Forest
      if (userState$selected_function == "Random Forest") {
        shiny::updateSelectInput(
          session,
          "rf_group_col",
          selected = userState$rf_group_col
        )
        shiny::updateNumericInput(
          session,
          "rf_ntree",
          value = userState$rf_ntree
        )
        shiny::updateNumericInput(session, "rf_mtry", value = userState$rf_mtry)
        shiny::updateNumericInput(
          session,
          "rf_train_fraction",
          value = userState$rf_train_fraction
        )
        shiny::updateCheckboxInput(
          session,
          "rf_plot_roc",
          value = userState$rf_plot_roc
        )
        shiny::updateCheckboxInput(
          session,
          "rf_run_rfcv",
          value = userState$rf_run_rfcv
        )
        shiny::updateNumericInput(
          session,
          "rf_k_folds",
          value = userState$rf_k_folds
        )
        shiny::updateNumericInput(session, "rf_step", value = userState$rf_step)
      }

      # Skewness/Kurtosis
      if (userState$selected_function == "Skewness/Kurtosis") {
        shiny::updateSelectInput(
          session,
          "skku_group_cols",
          selected = userState$skku_group_cols
        )
        shiny::updateCheckboxInput(
          session,
          "skku_print_raw",
          value = userState$skku_print_raw
        )
        shiny::updateCheckboxInput(
          session,
          "skku_print_log",
          value = userState$skku_print_log
        )
      }

      # Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)
      if (
        userState$selected_function ==
          "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
      ) {
        df_now <- shiny::isolate(data_after_filters())
        if (shiny::isTruthy(df_now)) {
          cand <- names(df_now)[!vapply(df_now, is.numeric, logical(1))]
          cand <- setdiff(cand, "..cyto_id..")
        } else {
          cand <- character(0)
        }
        shiny::updateSelectInput(
          session,
          "splsda_group_col",
          selected = userState$splsda_group_col
        )
        shiny::updateSelectInput(
          session,
          "splsda_group_col2",
          selected = userState$splsda_group_col2
        )
        shiny::updateCheckboxInput(
          session,
          "splsda_use_batch_corr",
          value = userState$splsda_use_batch_corr
        )
        shiny::updateSelectInput(
          session,
          "splsda_batch_col",
          selected = userState$splsda_batch_col
        )
        shiny::updateCheckboxInput(
          session,
          "splsda_use_multilevel",
          value = userState$splsda_use_multilevel
        )
        shiny::updateSelectInput(
          session,
          "splsda_multilevel",
          selected = userState$splsda_multilevel
        )
        shiny::updateNumericInput(
          session,
          "splsda_var_num",
          value = userState$splsda_var_num
        )
        shiny::updateRadioButtons(
          session,
          "splsda_cv_opt",
          selected = userState$splsda_cv_opt
        )
        shiny::updateNumericInput(
          session,
          "splsda_fold_num",
          value = userState$splsda_fold_num
        )
        shiny::updateNumericInput(
          session,
          "splsda_comp_num",
          value = userState$splsda_comp_num
        )
        shiny::updateRadioButtons(
          session,
          "splsda_ind_names_mode",
          selected = userState$splsda_ind_names_mode %||% "off"
        )
        shiny::updateSelectInput(
          session,
          "splsda_ind_names_col",
          choices = cand,
          selected = userState$splsda_ind_names_col %||%
            if (length(cand)) cand[1] else NULL
        )
        shiny::updateSelectizeInput(
          session,
          "splsda_pch",
          selected = userState$splsda_pch
        )
        shiny::updateSelectInput(
          session,
          "splsda_style",
          selected = userState$splsda_style
        )
        shiny::updateCheckboxInput(
          session,
          "splsda_roc",
          value = userState$splsda_roc
        )
        shiny::updateCheckboxInput(
          session,
          "splsda_ellipse",
          value = userState$splsda_ellipse
        )
        shiny::updateCheckboxInput(
          session,
          "splsda_bg",
          value = userState$splsda_bg
        )
        shiny::updateCheckboxInput(
          session,
          "splsda_conf_mat",
          value = userState$splsda_conf_mat
        )
        shiny::updateSelectizeInput(
          session,
          "splsda_colors",
          selected = userState$splsda_colors
        )
      }
      # Volcano Plot
      if (userState$selected_function == "Volcano Plot") {
        shiny::updateSelectInput(
          session,
          "volc_group_col",
          selected = userState$volc_group_col
        )
        shiny::updateSelectInput(
          session,
          "volc_cond1",
          selected = userState$volc_cond1
        )
        shiny::updateSelectInput(
          session,
          "volc_cond2",
          selected = userState$volc_cond2
        )
        shiny::updateNumericInput(
          session,
          "volc_fold_change_thresh",
          value = userState$volc_fold_change_thresh
        )
        shiny::updateNumericInput(
          session,
          "volc_p_value_thresh",
          value = userState$volc_p_value_thresh
        )
        shiny::updateNumericInput(
          session,
          "volc_top_labels",
          value = userState$volc_top_labels
        )
      }

      # Extreme Gradient Boosting (XGBoost)
      if (
        userState$selected_function == "Extreme Gradient Boosting (XGBoost)"
      ) {
        shiny::updateSelectInput(
          session,
          "xgb_group_col",
          selected = userState$xgb_group_col
        )
        shiny::updateNumericInput(
          session,
          "xgb_train_fraction",
          value = userState$xgb_train_fraction
        )
        shiny::updateNumericInput(
          session,
          "xgb_nrounds",
          value = userState$xgb_nrounds
        )
        shiny::updateNumericInput(
          session,
          "xgb_max_depth",
          value = userState$xgb_max_depth
        )
        shiny::updateNumericInput(session, "xgb_eta", value = userState$xgb_eta)
        shiny::updateSelectInput(
          session,
          "xgb_eval_metric",
          selected = userState$xgb_eval_metric
        )
        shiny::updateNumericInput(
          session,
          "xgb_top_n_features",
          value = userState$xgb_top_n_features
        )
        shiny::updateCheckboxInput(
          session,
          "xgb_plot_roc",
          value = userState$xgb_plot_roc
        )
        shiny::updateCheckboxInput(session, "xgb_cv", value = userState$xgb_cv)
        shiny::updateNumericInput(
          session,
          "xgb_nfold",
          value = userState$xgb_nfold
        )
      }

      font_spec <- get_analysis_font_spec(userState$selected_function)
      if (!is.null(font_spec)) {
        restore_font_settings_inputs(
          session = session,
          prefix = font_spec$prefix,
          supported_fields = font_spec$supported_fields,
          state = userState[[font_spec$state_key]],
          default_font_settings = font_spec$default_font_settings
        )
      }
    }
  })

  analysis_inputs <- shiny::reactive({
    list(
      # The function name
      func_name = selected_function(),
      # Data
      df = filteredData(),
      args = list(
        # Boxplots
        bp_bin_size = input$bp_bin_size,
        bp_group_by = input$bp_group_by,
        bp_y_lim = input$bp_y_lim,

        # Violin Plots
        vio_group_by = input$vio_group_by,
        vio_bin_size = input$vio_bin_size,
        vio_y_lim = input$vio_y_lim,
        vio_boxplot_overlay = input$vio_boxplot_overlay,

        # Univariate Tests
        uv2_method = input$uv2_method,
        uv2_p_adjust_method = input$uv2_p_adjust_method,
        uvm_method = input$uvm_method,
        uvm_p_adjust_method = input$uvm_p_adjust_method,
        twa_primary_cat_var = input$twa_primary_cat_var,
        twa_secondary_cat_var = input$twa_secondary_cat_var,
        twa_include_primary_secondary_interaction = input$twa_include_primary_secondary_interaction,
        anc_primary_cat_var = input$anc_primary_cat_var,
        anc_secondary_cat_var = input$anc_secondary_cat_var,
        anc_covariate_col = input$anc_covariate_col,
        anc_include_primary_secondary_interaction = input$anc_include_primary_secondary_interaction,
        anc_include_primary_covariate_interaction = input$anc_include_primary_covariate_interaction,
        anc_include_secondary_covariate_interaction = input$anc_include_secondary_covariate_interaction,

        # Error-Bar Plot
        eb_group_col = input$eb_group_col,
        eb_p_lab = input$eb_p_lab,
        eb_es_lab = input$eb_es_lab,
        eb_class_symbol = input$eb_class_symbol,
        eb_x_lab = input$eb_x_lab,
        eb_y_lab = input$eb_y_lab,
        eb_title = input$eb_title,
        eb_stat = input$eb_stat,
        eb_error = input$eb_error,
        eb_method = input$eb_method,
        eb_p_adjust_method = input$eb_p_adjust_method,
        eb_n_col = input$eb_n_col,
        eb_fill_palette = input$eb_fill_palette,
        # Dual-Flashlight Plot
        df_group_var = input$df_group_var,
        df_cond1 = input$df_cond1,
        df_cond2 = input$df_cond2,
        df_ssmd_thresh = input$df_ssmd_thresh,
        df_log2fc_thresh = input$df_log2fc_thresh,
        df_top_labels = input$df_top_labels,

        # Heatmap
        hm_annotation = input$hm_annotation,
        hm_ann_side = input$hm_ann_side,

        # Correlation Plots
        corr_group_col = input$corr_group_col,
        corr_target = input$corr_target,
        corr_by_group = input$corr_by_group,

        # PCA
        pca_group_col = input$pca_group_col,
        pca_group_col2 = input$pca_group_col2,
        pca_comp_num = input$pca_comp_num,
        pca_ellipse = input$pca_ellipse,
        pca_style = input$pca_style,
        pca_pch = input$pca_pch,
        pca_colors = input$pca_colors,

        # PLSR
        plsr_group_col = input$plsr_group_col,
        plsr_response_col = input$plsr_response_col,
        plsr_predictor_cols = input$plsr_predictor_cols,
        plsr_comp_num = input$plsr_comp_num,
        plsr_sparse = input$plsr_sparse,
        plsr_keepX = input$plsr_keepX,
        plsr_cv_opt = input$plsr_cv_opt,
        plsr_fold_num = input$plsr_fold_num,
        plsr_ellipse = input$plsr_ellipse,
        plsr_colors = input$plsr_colors,

        # Random Forest
        rf_group_col = input$rf_group_col,
        rf_ntree = input$rf_ntree,
        rf_mtry = input$rf_mtry,
        rf_train_fraction = input$rf_train_fraction,
        rf_plot_roc = input$rf_plot_roc,
        rf_run_rfcv = input$rf_run_rfcv,
        rf_k_folds = input$rf_k_folds,
        rf_step = input$rf_step,

        # Skewness/Kurtosis
        skku_group_cols = input$skku_group_cols,
        skku_print_raw = input$skku_print_raw,
        skku_print_log = input$skku_print_log,

        # sPLS-DA
        splsda_group_col = input$splsda_group_col,
        splsda_group_col2 = input$splsda_group_col2,
        splsda_use_batch_corr = input$splsda_use_batch_corr,
        splsda_batch_col = input$splsda_batch_col,
        splsda_use_multilevel = input$splsda_use_multilevel,
        splsda_multilevel = input$splsda_multilevel,
        splsda_var_num = input$splsda_var_num,
        splsda_cv_opt = input$splsda_cv_opt,
        splsda_fold_num = input$splsda_fold_num,
        splsda_comp_num = input$splsda_comp_num,
        splsda_pch = input$splsda_pch,
        splsda_ind_names_mode = input$splsda_ind_names_mode,
        splsda_ind_names_col = input$splsda_ind_names_col,
        splsda_style = input$splsda_style,
        splsda_roc = input$splsda_roc,
        splsda_ellipse = input$splsda_ellipse,
        splsda_bg = input$splsda_bg,
        splsda_conf_mat = input$splsda_conf_mat,
        splsda_colors = input$splsda_colors,

        # MINT sPLS-DA
        mint_splsda_group_col = input$mint_splsda_group_col,
        mint_splsda_group_col2 = input$mint_splsda_group_col2,
        mint_splsda_batch_col = input$mint_splsda_batch_col,
        mint_splsda_var_num = input$mint_splsda_var_num,
        mint_splsda_comp_num = input$mint_splsda_comp_num,
        mint_splsda_cim = input$mint_splsda_cim,
        mint_splsda_ellipse = input$mint_splsda_ellipse,
        mint_splsda_bg = input$mint_splsda_bg,
        mint_splsda_roc = input$mint_splsda_roc,
        mint_splsda_colors = input$mint_splsda_colors,

        # Volcano Plot
        volc_group_col = input$volc_group_col,
        volc_cond1 = input$volc_cond1,
        volc_cond2 = input$volc_cond2,
        volc_fold_change_thresh = input$volc_fold_change_thresh,
        volc_p_value_thresh = input$volc_p_value_thresh,
        volc_top_labels = input$volc_top_labels,

        # XGBoost
        xgb_group_col = input$xgb_group_col,
        xgb_train_fraction = input$xgb_train_fraction,
        xgb_nrounds = input$xgb_nrounds,
        xgb_max_depth = input$xgb_max_depth,
        xgb_eta = input$xgb_eta,
        xgb_nfold = input$xgb_nfold,
        xgb_cv = input$xgb_cv,
        xgb_eval_metric = input$xgb_eval_metric,
        xgb_top_n_features = input$xgb_top_n_features,
        xgb_plot_roc = input$xgb_plot_roc
      )
    )
  })

  invisible(app_stage_commit(app_ctx, stage_env))
}

init_analysis_results_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  userState <- app_ctx$userState
  filteredData <- app_ctx$filteredData
  selected_function <- app_ctx$selected_function
  userData <- app_ctx$userData
  get_analysis_font_spec <- app_ctx$get_analysis_font_spec
  font_settings_state_from_inputs <- app_ctx$font_settings_state_from_inputs
  font_settings_state_to_backend <- app_ctx$font_settings_state_to_backend
  ## ---------------------------
  ## Analysis and Results
  ## ---------------------------
  errorMessage <- shiny::reactiveVal(NULL)
  warningMessage <- shiny::reactiveVal(character())

  parse_numeric_input <- function(x) {
    if (is.null(x) || !nzchar(trimws(x))) {
      return(NULL)
    }

    as.numeric(trimws(strsplit(x, ",")[[1]]))
  }

  null_if_blank <- function(x) {
    if (is.null(x) || !nzchar(trimws(x))) {
      return(NULL)
    }

    x
  }

  analysis_p_adjust_input <- function(x, none_value, default = "BH") {
    if (is.null(x)) {
      return(default)
    }
    if (!nzchar(trimws(x))) {
      return(none_value)
    }

    x
  }

  analysis_font_settings <- function(input, func_name, user_state = NULL) {
    spec <- get_analysis_font_spec(func_name)
    if (is.null(spec)) {
      return(NULL)
    }
    if (is.null(user_state)) {
      user_state <- userState
    }

    state <- font_settings_state_from_inputs(
      input = input,
      prefix = spec$prefix,
      supported_fields = spec$supported_fields,
      default_font_settings = spec$default_font_settings
    )
    if (is.null(state)) {
      state <- user_state[[spec$state_key]]
    }

    font_settings_state_to_backend(
      state,
      default_font_settings = spec$default_font_settings
    )
  }

  copyable_text_dependencies <- function() {
    shiny::tagList(
      shiny::singleton(
        shiny::tags$style(
          shiny::HTML(
            "
          .copyable-text {
            position: relative;
            margin: 0;
          }
          .copyable-text__button {
            position: absolute;
            top: 0.5rem;
            right: 0.5rem;
            opacity: 0;
            transition: opacity 0.15s ease-in-out;
            z-index: 1;
          }
          .copyable-text:hover .copyable-text__button,
          .copyable-text:focus-within .copyable-text__button {
            opacity: 1;
          }
          .copyable-text__content {
            margin: 0;
            padding: 0.75rem 1rem;
            white-space: pre-wrap;
            word-break: break-word;
          }
          "
          )
        )
      ),
      shiny::singleton(
        shiny::tags$script(
          shiny::HTML(
            "
          window.cytCopyText = function(button) {
            var container = button.closest('.copyable-text');
            var content = container ? container.querySelector('.copyable-text__content') : null;
            if (!content) {
              return;
            }

            var text = content.innerText;
            var fallbackCopy = function(value) {
              var textarea = document.createElement('textarea');
              textarea.value = value;
              textarea.setAttribute('readonly', '');
              textarea.style.position = 'absolute';
              textarea.style.left = '-9999px';
              document.body.appendChild(textarea);
              textarea.select();
              document.execCommand('copy');
              document.body.removeChild(textarea);
            };
            var showCopied = function() {
              var defaultLabel = button.getAttribute('data-default-label') || 'Copy';
              button.textContent = 'Copied';
              window.setTimeout(function() {
                button.textContent = defaultLabel;
              }, 1500);
            };

            if (navigator.clipboard && window.isSecureContext) {
              navigator.clipboard.writeText(text).then(showCopied).catch(function() {
                fallbackCopy(text);
                showCopied();
              });
            } else {
              fallbackCopy(text);
              showCopied();
            }
          };
          "
          )
        )
      )
    )
  }

  copyable_text_block <- function(text, button_label = "Copy") {
    if (is.null(text) || !length(text)) {
      return(NULL)
    }

    text <- paste(as.character(text), collapse = "\n")

    shiny::tags$div(
      class = "copyable-text",
      shiny::tags$button(
        type = "button",
        class = "btn btn-sm btn-outline-secondary copyable-text__button",
        onclick = "window.cytCopyText(this)",
        `data-default-label` = button_label,
        button_label
      ),
      shiny::tags$pre(class = "copyable-text__content", text)
    )
  }

  collect_exportable_plots <- function(x, prefix = "plot") {
    if (is.null(x)) {
      return(list())
    }

    if (inherits(x, "pheatmap")) {
      draw_fn <- function() {
        grid::grid.newpage()
        grid::grid.draw(x$gtable)
      }
      return(stats::setNames(list(draw_fn), prefix))
    }

    if (
      inherits(x, c("gg", "ggplot", "grob", "gtable", "recordedplot")) ||
        is.function(x)
    ) {
      return(stats::setNames(list(x), prefix))
    }

    if (!is.list(x) || is.data.frame(x) || is.matrix(x)) {
      return(list())
    }

    out <- list()
    child_names <- names(x)
    if (is.null(child_names)) {
      child_names <- as.character(seq_along(x))
    }

    for (i in seq_along(x)) {
      child_name <- child_names[[i]]
      if (is.null(child_name) || is.na(child_name) || !nzchar(child_name)) {
        child_name <- as.character(i)
      }

      out <- c(
        out,
        collect_exportable_plots(
          x[[i]],
          prefix = paste(prefix, child_name, sep = "_")
        )
      )
    }

    out
  }

  analysisResult <- shiny::eventReactive(input$next4, {
    errorMessage(NULL)
    warningMessage(character())

    shiny::req(filteredData())
    prog <- shiny::Progress$new()
    on.exit(prog$close())

    tryCatch(
      {
        withCallingHandlers(
          {
            df <- filteredData()
            df$..cyto_id.. <- NULL

            # Simplified: always generate a file path for functions that can output a PDF

            func_to_run <- selected_function()
            shiny::req(func_to_run)

            results <- switch(
              func_to_run,

              # -- Statistical Tests --
              "Univariate Tests (T-test, Wilcoxon)" = cyt_univariate(
                data = df,
                method = input$uv2_method %||% "auto",
                p_adjust_method = analysis_p_adjust_input(
                  input$uv2_p_adjust_method,
                  none_value = "none"
                ),
                progress = prog,
                scale = NULL,
                format_output = TRUE
              ),

              "Multi-level Univariate Tests (Anova, Kruskal-Wallis)" = cyt_univariate_multi(
                data = df,
                method = input$uvm_method %||% "anova",
                p_adjust_method = analysis_p_adjust_input(
                  input$uvm_p_adjust_method,
                  none_value = "none"
                ),
                progress = prog,
                format_output = TRUE
              ),
              "Two-way ANOVA" = cyt_univariate_multi(
                data = df,
                method = "anova",
                design = "two_way",
                primary_cat_var = input$twa_primary_cat_var,
                secondary_cat_var = input$twa_secondary_cat_var,
                include_primary_secondary_interaction = isTRUE(
                  input$twa_include_primary_secondary_interaction
                ),
                progress = prog,
                format_output = TRUE
              ),
              "ANCOVA" = cyt_univariate_multi(
                data = df,
                method = "anova",
                design = "ancova",
                primary_cat_var = input$anc_primary_cat_var,
                secondary_cat_var = null_if_blank(input$anc_secondary_cat_var),
                covariate_col = input$anc_covariate_col,
                include_primary_secondary_interaction = isTRUE(
                  input$anc_include_primary_secondary_interaction
                ),
                include_primary_covariate_interaction = isTRUE(
                  input$anc_include_primary_covariate_interaction
                ),
                include_secondary_covariate_interaction = isTRUE(
                  input$anc_include_secondary_covariate_interaction
                ),
                progress = prog,
                format_output = TRUE
              ),

              # -- Exploratory Visualization --
              "Boxplots" = cyt_bp(
                data = df,
                progress = prog,
                group_by = if (length(input$bp_group_by)) {
                  input$bp_group_by
                } else {
                  NULL
                },
                bin_size = input$bp_bin_size,
                y_lim = parse_numeric_input(input$bp_y_lim),
                scale = "none",
                font_settings = analysis_font_settings(input, "Boxplots")
              ),

              "Violin Plots" = cyt_violin(
                data = df,
                progress = prog,
                group_by = if (length(input$vio_group_by)) {
                  input$vio_group_by
                } else {
                  NULL
                },
                bin_size = input$vio_bin_size,
                y_lim = parse_numeric_input(input$vio_y_lim),
                scale = "none",
                boxplot_overlay = isTRUE(input$vio_boxplot_overlay),
                font_settings = analysis_font_settings(input, "Violin Plots")
              ),

              "Error-Bar Plot" = cyt_errbp(
                data = df,
                progress = prog,
                group_col = input$eb_group_col,
                p_lab = input$eb_p_lab,
                es_lab = input$eb_es_lab,
                class_symbol = input$eb_class_symbol,
                x_lab = input$eb_x_lab,
                y_lab = input$eb_y_lab,
                title = input$eb_title,
                stat = input$eb_stat %||% "mean",
                error = input$eb_error %||% "se",
                scale = "none",
                method = input$eb_method %||% "auto",
                p_adjust_method = analysis_p_adjust_input(
                  input$eb_p_adjust_method,
                  none_value = NULL
                ),
                n_col = input$eb_n_col %||% 3,
                fill_palette = {
                  eb_fill_palette_choice <- input$eb_fill_palette %||% "gray"
                  if (identical(eb_fill_palette_choice, "grey")) {
                    eb_fill_palette_choice <- "gray"
                  }
                  switch(
                    eb_fill_palette_choice,
                    "gray" = NULL,
                    "tableau" = c(
                      "#4E79A7",
                      "#F28E2B",
                      "#E15759",
                      "#76B7B2",
                      "#59A14F",
                      "#EDC948"
                    ),
                    "colorblind" = c(
                      "#0072B2",
                      "#E69F00",
                      "#009E73",
                      "#CC79A7",
                      "#56B4E9",
                      "#D55E00"
                    ),
                    "pastel" = c(
                      "#AEC6CF",
                      "#FFD1DC",
                      "#B5EAD7",
                      "#FFDAC1",
                      "#C7CEEA",
                      "#E2F0CB"
                    )
                  )
                },
                font_settings = analysis_font_settings(input, "Error-Bar Plot")
              ),

              "Dual-Flashlight Plot" = cyt_dualflashplot(
                data = df,
                group_var = input$df_group_var,
                group1 = input$df_cond1,
                group2 = input$df_cond2,

                progress = prog,
                ssmd_thresh = input$df_ssmd_thresh,
                log2fc_thresh = input$df_log2fc_thresh,
                top_labels = input$df_top_labels,
                font_settings = analysis_font_settings(
                  input,
                  "Dual-Flashlight Plot"
                )
              ),
              "Heatmap" = {
                num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
                invalid_heatmap <- summarize_invalid_numeric_columns(
                  df,
                  columns = num_cols
                )
                if (nrow(invalid_heatmap) > 0) {
                  stop(
                    paste(
                      "Heatmap uses the Step 2-transformed matrix directly and calls cyt_heatmap(..., scale = NULL), so this is not a second log transformation.",
                      "Invalid values were found in the working numeric matrix before clustering:",
                      format_invalid_numeric_summary(invalid_heatmap),
                      "Use Treat missing values or choose a transformation compatible with your data."
                    ),
                    call. = FALSE
                  )
                }

                ann_arg <- if (
                  !is.null(input$hm_annotation) && nzchar(input$hm_annotation)
                ) {
                  input$hm_annotation
                } else {
                  NULL
                }
                side_arg <- input$hm_ann_side %||% "auto"

                # build (silent) pheatmap object; nothing is drawn yet
                ph <- cyt_heatmap(
                  data = df,
                  scale = NULL,
                  annotation_col = ann_arg,
                  annotation_side = side_arg,
                  font_settings = analysis_font_settings(input, "Heatmap")
                )
                ph # <- return the pheatmap object
              },

              "Skewness/Kurtosis" = cyt_skku(
                data = df,

                progress = prog,
                group_cols = input$skku_group_cols,
                print_res_raw = input$skku_print_raw,
                print_res_log = input$skku_print_log,
                font_settings = analysis_font_settings(
                  input,
                  "Skewness/Kurtosis"
                )
              ),

              "Volcano Plot" = cyt_volc(
                data = df,

                progress = prog,
                group_col = input$volc_group_col,
                cond1 = input$volc_cond1,
                cond2 = input$volc_cond2,
                fold_change_thresh = input$volc_fold_change_thresh,
                p_value_thresh = input$volc_p_value_thresh,
                top_labels = input$volc_top_labels,
                font_settings = analysis_font_settings(input, "Volcano Plot")
              ),

              # -- Multivariate Analysis --
              "Principal Component Analysis (PCA)" = {
                pch_vals <- as.numeric(input$pca_pch)
                grp <- df[[input$pca_group_col]]
                uniq <- unique(na.omit(grp))
                n_levels <- length(uniq)

                if (!length(pch_vals)) {
                  pch_vals <- c(16, 17) # fallback if nothing selected
                }

                if (length(pch_vals) < n_levels) {
                  needed <- n_levels - length(pch_vals)
                  pool <- setdiff(0:25, pch_vals) # standard numeric pch set

                  extra <- if (length(pool) >= needed) {
                    sample(pool, needed, replace = FALSE)
                  } else {
                    c(pool, sample(0:25, needed - length(pool), replace = TRUE))
                  }

                  pch_vals <- c(pch_vals, extra)
                }

                cols <- if (length(input$pca_colors)) {
                  input$pca_colors
                } else {
                  rainbow(length(uniq))
                }

                cyt_pca(
                  data = df,
                  progress = prog,
                  group_col = input$pca_group_col,
                  group_col2 = if (nzchar(input$pca_group_col2)) {
                    input$pca_group_col2
                  } else {
                    NULL
                  },
                  comp_num = input$pca_comp_num,
                  scale = NULL,
                  ellipse = input$pca_ellipse,
                  style = if (input$pca_style == "3D") "3d" else NULL,
                  pch_values = pch_vals,
                  pca_colors = cols,
                  font_settings = analysis_font_settings(
                    input,
                    "Principal Component Analysis (PCA)"
                  )
                )
              },

              "Partial Least Squares Regression (PLSR)" = {
                df <- filteredData()
                grp <- if (
                  !is.null(input$plsr_group_col) && nzchar(input$plsr_group_col)
                ) {
                  input$plsr_group_col
                } else {
                  NULL
                }

                # Resolve predictor cols - NULL means "use all"
                pred_cols <- if (length(input$plsr_predictor_cols)) {
                  input$plsr_predictor_cols
                } else {
                  NULL
                }

                res <- cyt_plsr(
                  data = df,
                  group_col = grp,
                  response_col = input$plsr_response_col,
                  predictor_cols = pred_cols, # <-- ADD
                  comp_num = input$plsr_comp_num,
                  sparse = input$plsr_sparse,
                  var_num = input$plsr_keepX,
                  scale = NULL,
                  ellipse = isTRUE(input$plsr_ellipse),
                  cv_opt = if (input$plsr_cv_opt == "None") {
                    NULL
                  } else {
                    input$plsr_cv_opt
                  },
                  fold_num = input$plsr_fold_num,
                  pls_colors = input$plsr_colors,
                  font_settings = analysis_font_settings(
                    input,
                    "Partial Least Squares Regression (PLSR)"
                  ),
                  progress = prog
                )
              },

              "Correlation Plots" = {
                tgt <- input$corr_target
                grp <- input$corr_group_col
                bygrp <- isTRUE(input$corr_by_group) && nzchar(grp)

                # overall: both methods at once
                both <- cyt_corr(
                  data = df,
                  target = tgt,
                  methods = c("spearman", "pearson"),
                  group_var = if (bygrp) input$corr_group_col else NULL,
                  compare_groups = FALSE,
                  plot = TRUE,
                  font_settings = analysis_font_settings(
                    input,
                    "Correlation Plots"
                  ),
                  progress = prog
                )

                list(
                  spearman = list(
                    table = both$spearman$table,
                    heat_mat = both$spearman$heat_mat,
                    plot = both$spearman$plot,
                    group_plots = both$spearman$group_plots
                  ),
                  pearson = list(
                    table = both$pearson$table,
                    heat_mat = both$pearson$heat_mat,
                    plot = both$pearson$plot,
                    group_plots = both$pearson$group_plots
                  ),
                  corr_target = tgt
                )
              },

              "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = {
                pch_vals <- as.numeric(input$splsda_pch)
                pch_vals <- pch_vals[!is.na(pch_vals)]
                grp <- df[[input$splsda_group_col]]
                uniq <- unique(na.omit(grp))
                n_levels <- length(uniq)

                if (!length(pch_vals)) {
                  pch_vals <- c(16, 17) # sensible fallback if nothing selected
                }

                if (length(pch_vals) < n_levels) {
                  needed <- n_levels - length(pch_vals)
                  pool <- setdiff(0:25, pch_vals) # standard numeric pch pool, avoid duplicates

                  extra <- if (length(pool) >= needed) {
                    sample(pool, needed, replace = FALSE)
                  } else {
                    c(pool, sample(0:25, needed - length(pool), replace = TRUE))
                  }
                  pch_vals <- c(pch_vals, extra)
                }

                cols <- if (length(input$splsda_colors)) {
                  input$splsda_colors
                } else {
                  rainbow(length(uniq))
                }
                multilevel <- if (isTRUE(input$splsda_use_multilevel)) {
                  input$splsda_multilevel
                } else {
                  NULL
                }
                batch_col <- if (isTRUE(input$splsda_use_batch_corr)) {
                  input$splsda_batch_col
                } else {
                  NULL
                }
                ind_names_val <- switch(
                  input$splsda_ind_names_mode %||% "off",
                  "off" = FALSE,
                  "rownames" = TRUE,
                  "column" = {
                    shiny::req(input$splsda_ind_names_col)
                    as.character(df[[input$splsda_ind_names_col]])
                  },
                  FALSE
                )
                cyt_splsda(
                  data = df,
                  progress = prog,
                  group_col = input$splsda_group_col,
                  group_col2 = input$splsda_group_col2,
                  batch_col = batch_col,
                  multilevel_col = multilevel,
                  var_num = input$splsda_var_num,
                  cv_opt = if (input$splsda_cv_opt == "None") {
                    NULL
                  } else {
                    tolower(input$splsda_cv_opt)
                  },
                  fold_num = input$splsda_fold_num,
                  scale = NULL,
                  comp_num = input$splsda_comp_num,
                  style = if (input$splsda_style == "3D") "3d" else NULL,
                  pch_values = pch_vals,
                  splsda_colors = cols,
                  roc = input$splsda_roc,
                  ellipse = input$splsda_ellipse,
                  ind_names = ind_names_val,
                  bg = input$splsda_bg,
                  conf_mat = input$splsda_conf_mat,
                  font_settings = analysis_font_settings(
                    input,
                    "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
                  )
                )
              },

              "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)" = {
                grp <- df[[input$mint_splsda_group_col]]
                uniq <- unique(grp)
                cols <- if (length(input$mint_splsda_colors)) {
                  input$mint_splsda_colors
                } else {
                  rainbow(length(uniq))
                }

                cyt_mint_splsda(
                  data = df,

                  progress = prog,
                  group_col = input$mint_splsda_group_col,
                  group_col2 = input$mint_splsda_group_col2,
                  batch_col = input$mint_splsda_batch_col,
                  var_num = input$mint_splsda_var_num,
                  comp_num = input$mint_splsda_comp_num,
                  cim = input$mint_splsda_cim,
                  scale = NULL,
                  colors = cols,
                  roc = input$mint_splsda_roc,
                  ellipse = input$mint_splsda_ellipse,
                  bg = input$mint_splsda_bg,
                  font_settings = analysis_font_settings(
                    input,
                    "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
                  )
                )
              },

              # -- Machine Learning --
              "Random Forest" = cyt_rf(
                data = df,
                progress = prog,
                group_col = input$rf_group_col,
                ntree = input$rf_ntree,
                mtry = input$rf_mtry,
                train_fraction = input$rf_train_fraction,
                plot_roc = isTRUE(input$rf_plot_roc),
                run_rfcv = isTRUE(input$rf_run_rfcv),
                k_folds = input$rf_k_folds,
                step = input$rf_step,
                scale = "none",
                seed = 123,
                font_settings = analysis_font_settings(input, "Random Forest")
              ),

              "Extreme Gradient Boosting (XGBoost)" = cyt_xgb(
                data = df,
                progress = prog,
                group_col = input$xgb_group_col,
                train_fraction = input$xgb_train_fraction,
                nrounds = input$xgb_nrounds,
                max_depth = input$xgb_max_depth,
                learning_rate = input$xgb_eta,
                nfold = input$xgb_nfold,
                cv = isTRUE(input$xgb_cv),
                eval_metric = input$xgb_eval_metric,
                top_n_features = input$xgb_top_n_features,
                plot_roc = isTRUE(input$xgb_plot_roc),
                scale = "none",
                seed = 123,
                font_settings = analysis_font_settings(
                  input,
                  "Extreme Gradient Boosting (XGBoost)"
                )
              )
            ) # end switch

            results
          },
          warning = function(w) {
            warningMessage(unique(c(
              warningMessage(),
              conditionMessage(w)
            )))
            invokeRestart("muffleWarning")
          }
        )
      },
      error = function(e) {
        errorMessage(conditionMessage(e))
        NULL
      }
    ) # end tryCatch
  })

  reactivePlots <- shiny::reactive({
    shiny::req(analysisResult())
    analysisResult()
  })

  exportablePlots <- shiny::reactive({
    shiny::req(analysisResult(), selected_function())

    func_name <- selected_function()
    res <- analysisResult()

    switch(
      func_name,
      "Univariate Tests (T-test, Wilcoxon)" = list(),
      "Multi-level Univariate Tests (Anova, Kruskal-Wallis)" = list(),
      "Two-way ANOVA" = list(),
      "ANCOVA" = list(),
      "Heatmap" = collect_exportable_plots(res, "heatmap"),
      "Random Forest" = collect_exportable_plots(
        list(
          vip_plot = res$vip_plot,
          roc_plot = res$roc_plot,
          rfcv_plot = res$rfcv_plot
        ),
        "rf"
      ),
      "Extreme Gradient Boosting (XGBoost)" = collect_exportable_plots(
        list(
          importance_plot = res$importance_plot,
          roc_plot = res$roc_plot
        ),
        "xgb"
      ),
      "Skewness/Kurtosis" = collect_exportable_plots(
        list(
          skew_plot = res$p_skew,
          kurt_plot = res$p_kurt
        ),
        "skku"
      ),
      collect_exportable_plots(
        res,
        gsub("[^A-Za-z0-9]+", "_", tolower(func_name))
      )
    )
  })

  exportableTables <- shiny::reactive({
    shiny::req(analysisResult(), selected_function())

    func_name <- selected_function()
    res <- analysisResult()

    switch(
      func_name,
      "Univariate Tests (T-test, Wilcoxon)" = {
        if (is.data.frame(res)) {
          list(results = res)
        } else {
          list()
        }
      },
      "Multi-level Univariate Tests (Anova, Kruskal-Wallis)" = {
        if (!is.list(res) || is.null(res$results)) {
          return(list())
        }

        tables <- list(
          global = res$results,
          pairwise = res$pairwise %||% data.frame()
        )
        if (!is.null(res$assumptions)) {
          tables$assumptions <- res$assumptions
        }
        tables
      },
      "Two-way ANOVA" = {
        if (!is.list(res) || is.null(res$results)) {
          return(list())
        }

        tables <- list(
          global = res$results,
          pairwise = res$pairwise %||% data.frame()
        )
        if (!is.null(res$assumptions)) {
          tables$assumptions <- res$assumptions
        }
        tables
      },
      "ANCOVA" = {
        if (!is.list(res) || is.null(res$results)) {
          return(list())
        }

        tables <- list(
          global = res$results,
          pairwise = res$pairwise %||% data.frame()
        )
        if (!is.null(res$assumptions)) {
          tables$assumptions <- res$assumptions
        }
        tables
      },
      list()
    )
  })

  output$errorText <- shiny::renderUI({
    shiny::req(errorMessage())
    shiny::div(
      style = "color:red; padding:5px; border:1px solid red;",
      shiny::strong("Error:"),
      shiny::tags$pre(errorMessage())
    )
  })

  output$warningText <- shiny::renderUI({
    warnings <- warningMessage()
    shiny::req(length(warnings) > 0L)
    shiny::div(
      style = "color:orange; padding:5px; border:1px solid orange;",
      shiny::strong("Warning:"),
      shiny::tags$pre(paste(unique(warnings), collapse = "\n"))
    )
  })

  output$result_display <- shiny::renderUI({
    err <- errorMessage()
    if (shiny::isTruthy(err)) {
      return(shiny::tagList(
        shiny::uiOutput("errorText"),
        shiny::actionButton(
          "back6",
          "Change Inputs",
          icon = shiny::icon("arrow-left"),
          class = "btn-secondary"
        )
      ))
    }
    shiny::req(analysisResult())
    res <- analysisResult()
    func_name <- selected_function()
    shiny::req(func_name)

    # Main UI container
    shiny::tagList(
      copyable_text_dependencies(),
      shiny::uiOutput("warningText"), # Display warnings if any
      shiny::hr(),

      # Interactive Mode: Use a switch to render the correct UI
      switch(
        func_name,

        # --- sPLS-DA UI ---
        "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = {
          if ("overall_indiv_plot" %in% names(res)) {
            # Single analysis UI
            shiny::tagList(
              shiny::h4("sPLS-DA Results"),
              shiny::tabsetPanel(
                id = "splsda_tabs",
                type = "tabs",
                shiny::tabPanel(
                  "sPLS-DA Plot",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput(
                      "splsda_overallIndivPlot",
                      height = "500px"
                    ),
                    type = 8
                  )
                ),
                shiny::tabPanel(
                  "Loadings",
                  shinycssloaders::withSpinner(
                    shiny::uiOutput("splsda_loadingsUI"),
                    type = 8
                  )
                ),
                shiny::tabPanel(
                  "VIP Scores",
                  shinycssloaders::withSpinner(
                    shiny::uiOutput("splsda_vipScoresUI"),
                    type = 8
                  )
                ),
                if (!is.null(res$vip_indiv_plot)) {
                  shiny::tabPanel(
                    "VIP Model Plot",
                    shinycssloaders::withSpinner(
                      shiny::plotOutput(
                        "splsda_vipIndivPlot",
                        height = "500px"
                      ),
                      type = 8
                    )
                  )
                },
                if (!is.null(res$vip_loadings)) {
                  shiny::tabPanel(
                    "VIP Loadings",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput("splsda_vipLoadingsUI"),
                      type = 8
                    )
                  )
                },
                if (!is.null(res$overall_3D)) {
                  shiny::tabPanel(
                    "3D Plot",
                    shinycssloaders::withSpinner(
                      shiny::plotOutput(
                        "splsda_overall3DPlot",
                        height = "500px"
                      ),
                      type = 8
                    ),
                    # break line
                    shiny::br(),
                    shiny::actionButton(
                      "splsda_show3d_interactive",
                      "Interactive 3D",
                      icon = shiny::icon("fas fa-cube")
                    ),
                    # break line
                    shiny::br(),
                  )
                },
                if (!is.null(res$vip_3D)) {
                  shiny::tabPanel(
                    "3D Plot (VIP>1)",
                    shinycssloaders::withSpinner(
                      shiny::plotOutput("splsda_vip3DPlot", height = "500px"),
                      type = 8
                    ),
                    # break line
                    shiny::br(),
                    shiny::actionButton(
                      "splsda_show3d_interactive_vip",
                      "Interactive 3D (VIP)",
                      icon = shiny::icon("fas fa-cube")
                    ),
                    # break line
                    shiny::br(),
                  )
                },

                if (!is.null(res$overall_ROC)) {
                  shiny::tabPanel(
                    "ROC",
                    shinycssloaders::withSpinner(
                      shiny::plotOutput(
                        "splsda_overallRocPlot",
                        height = "400px"
                      ),
                      type = 8
                    )
                  )
                },

                if (!is.null(res$overall_CV)) {
                  shiny::tabPanel(
                    "Cross-Validation",
                    shinycssloaders::withSpinner(
                      shiny::plotOutput(
                        "splsda_overallCvPlot",
                        height = "400px"
                      ),
                      type = 8
                    )
                  )
                },
                if (!is.null(res$conf_matrix)) {
                  shiny::tabPanel(
                    "Confusion Matrix",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput("splsda_confMatrix"),
                      type = 8
                    )
                  )
                }
              )
            )
          } else {
            # Multi-group analysis UI (tabs for each group)
            do.call(
              shiny::tabsetPanel,
              c(
                list(id = "splsda_multigroup_tabs"),
                lapply(names(res), function(trt) {
                  shiny::tabPanel(
                    title = trt,
                    shiny::tabsetPanel(
                      type = "tabs",
                      shiny::tabPanel(
                        "sPLS-DA Plot",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0("splsda_overallIndivPlot_", trt),
                            height = "500px"
                          ),
                          type = 8
                        )
                      ),
                      shiny::tabPanel(
                        "Loadings",
                        shinycssloaders::withSpinner(
                          shiny::uiOutput(paste0("splsda_loadingsUI_", trt)),
                          type = 8
                        )
                      ),
                      shiny::tabPanel(
                        "VIP Scores",
                        shinycssloaders::withSpinner(
                          shiny::uiOutput(paste0("splsda_vipScoresUI_", trt)),
                          type = 8
                        )
                      ),
                      if (!is.null(res[[trt]]$vip_indiv_plot)) {
                        shiny::tabPanel(
                          "VIP Model Plot",
                          shinycssloaders::withSpinner(
                            shiny::plotOutput(
                              paste0("splsda_vipIndivPlot_", trt),
                              height = "500px"
                            ),
                            type = 8
                          )
                        )
                      },
                      if (!is.null(res[[trt]]$vip_loadings)) {
                        shiny::tabPanel(
                          "VIP Loadings",
                          shinycssloaders::withSpinner(
                            shiny::uiOutput(paste0(
                              "splsda_vipLoadingsUI_",
                              trt
                            )),
                            type = 8
                          )
                        )
                      },
                      if (!is.null(res[[trt]]$overall_3D)) {
                        shiny::tabPanel(
                          "3D Plot",
                          shinycssloaders::withSpinner(
                            shiny::plotOutput(
                              paste0("splsda_overall3DPlot_", trt),
                              height = "500px"
                            ),
                            type = 8
                          ),
                          # break line
                          shiny::br(),
                          shiny::actionButton(
                            paste0("splsda_show3d_interactive_", trt),
                            "Interactive 3D",
                            icon = shiny::icon("fas fa-cube")
                          ),
                          # break line
                          shiny::br(),
                        )
                      },

                      # VIP 3D tab per-trt (no div wrapper)
                      if (!is.null(res[[trt]]$vip_3D)) {
                        shiny::tabPanel(
                          "3D Plot (VIP>1)",
                          shinycssloaders::withSpinner(
                            shiny::plotOutput(
                              paste0("splsda_vip3DPlot_", trt),
                              height = "500px"
                            ),
                            type = 8
                          ),
                          # break line
                          shiny::br(),
                          shiny::actionButton(
                            paste0("splsda_show3d_interactive_vip_", trt),
                            "Interactive 3D (VIP)",
                            icon = shiny::icon("fas fa-cube")
                          ),
                          # break line
                          shiny::br(),
                        )
                      },
                      if (!is.null(res[[trt]]$overall_ROC)) {
                        shiny::tabPanel(
                          "ROC",
                          shinycssloaders::withSpinner(
                            shiny::plotOutput(
                              paste0(
                                "splsda_overallRocPlot_",
                                trt
                              ),
                              height = "400px",
                            ),
                            type = 8
                          )
                        )
                      },

                      if (!is.null(res[[trt]]$overall_CV)) {
                        shiny::tabPanel(
                          "Cross-Validation",
                          shinycssloaders::withSpinner(
                            shiny::plotOutput(
                              paste0(
                                "splsda_overallCvPlot_",
                                trt
                              ),
                              height = "400px"
                            ),
                            type = 8
                          )
                        )
                      },
                      if (!is.null(res[[trt]]$conf_matrix)) {
                        shiny::tabPanel(
                          "Confusion Matrix",
                          shinycssloaders::withSpinner(
                            shiny::uiOutput(
                              paste0("splsda_confMatrix_", trt)
                            ),
                            type = 8
                          )
                        )
                      }
                    )
                  )
                })
              )
            )
          }
        },
        # --- MINT sPLS-DA UI ---
        "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)" = {
          # Check if the result is for a single analysis or multiple (nested list)
          is_nested <- is.list(res) &&
            !is.null(names(res)) &&
            is.list(res[[1]]) &&
            "global_indiv_plot" %in% names(res[[1]])

          if (!is_nested) {
            # UI for a single analysis (original behavior)
            shiny::tagList(
              shiny::h4("MINT sPLS-DA Results"),
              shiny::tabsetPanel(
                id = "mint_splsda_tabs",
                shiny::tabPanel(
                  "Global Sample Plot",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("mint_splsda_global_plot"),
                    type = 8
                  )
                ),
                shiny::tabPanel(
                  "Partial Sample Plots",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("mint_splsda_partial_plot"),
                    type = 8
                  )
                ),
                shiny::tabPanel(
                  "Variable Loadings",
                  shinycssloaders::withSpinner(
                    shiny::uiOutput("mint_splsda_loadings_ui"),
                    type = 8
                  )
                ),
                shiny::tabPanel(
                  "Correlation Circle",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("mint_splsda_corr_circle_plot"),
                    type = 8
                  )
                ),
                if (!is.null(res$cim_obj)) {
                  shiny::tabPanel(
                    "Heatmap (CIM)",
                    shinycssloaders::withSpinner(
                      shiny::plotOutput(
                        "mint_splsda_cim_plot",
                        height = "600px"
                      ),
                      type = 8
                    )
                  )
                },
                if (!is.null(res$roc_plot)) {
                  shiny::tabPanel(
                    "ROC Curve",
                    shinycssloaders::withSpinner(
                      shiny::plotOutput("mint_splsda_roc_plot"),
                      type = 8
                    )
                  )
                }
              )
            )
          } else {
            # UI for multi-group analysis (nested tabs)
            do.call(
              shiny::tabsetPanel,
              c(
                list(id = "mint_splsda_multigroup_tabs"),
                lapply(names(res), function(trt) {
                  shiny::tabPanel(
                    title = trt,
                    shiny::tabsetPanel(
                      type = "tabs",
                      shiny::tabPanel(
                        "Global Plot",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(paste0(
                            "mint_splsda_global_",
                            trt
                          )),
                          type = 8
                        )
                      ),
                      shiny::tabPanel(
                        "Partial Plots",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(paste0(
                            "mint_splsda_partial_",
                            trt
                          )),
                          type = 8
                        )
                      ),
                      shiny::tabPanel(
                        "Variable Loadings",
                        shinycssloaders::withSpinner(
                          shiny::uiOutput(paste0(
                            "mint_splsda_loadings_",
                            trt
                          )),
                          type = 8
                        )
                      ),
                      shiny::tabPanel(
                        "Correlation",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(paste0(
                            "mint_splsda_corr_",
                            trt
                          )),
                          type = 8
                        )
                      ),
                      if (!is.null(res[[trt]]$cim_obj)) {
                        shiny::tabPanel(
                          "CIM",
                          shinycssloaders::withSpinner(
                            shiny::plotOutput(
                              paste0("mint_splsda_cim_", trt),
                              height = "600px"
                            ),
                            type = 8
                          )
                        )
                      },
                      if (!is.null(res[[trt]]$roc_plot)) {
                        shiny::tabPanel(
                          "ROC",
                          shinycssloaders::withSpinner(
                            shiny::plotOutput(paste0(
                              "mint_splsda_roc_",
                              trt
                            )),
                            type = 8
                          )
                        )
                      }
                    )
                  )
                })
              )
            )
          }
        },

        # --- PCA UI ---
        "Principal Component Analysis (PCA)" = {
          if ("overall_indiv_plot" %in% names(res)) {
            shiny::tagList(
              shiny::h4("PCA Results"),
              shiny::tabsetPanel(
                type = "tabs",
                shiny::tabPanel(
                  "PCA Plot",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("pca_indivPlot", height = "400px"),
                    type = 8
                  )
                ),
                shiny::tabPanel(
                  "Scree Plot",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("pca_screePlot", height = "400px"),
                    type = 8
                  )
                ),
                shiny::tabPanel(
                  "Loadings Plots",
                  shinycssloaders::withSpinner(
                    shiny::uiOutput("pca_loadingsUI"),
                    type = 8
                  )
                ),
                shiny::tabPanel(
                  "Biplot",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("pca_biplot", height = "400px"),
                    type = 8
                  )
                ),
                shiny::tabPanel(
                  "Correlation Circle",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("pca_corrCircle", height = "400px"),
                    type = 8
                  )
                ),
                if (!is.null(res$overall_3D)) {
                  shiny::tabPanel(
                    "3D Plot",
                    shinycssloaders::withSpinner(
                      shiny::plotOutput("pca_3DPlot", height = "400px"),
                      type = 8
                    )
                  )
                }
              )
            )
          } else {
            do.call(
              shiny::tabsetPanel,
              c(
                list(id = "pca_multigroup_tabs"),
                lapply(names(res), function(trt) {
                  shiny::tabPanel(
                    title = trt,
                    shiny::tabsetPanel(
                      type = "tabs",
                      shiny::tabPanel(
                        "PCA Plot",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0("pca_indivPlot_", trt),
                            height = "400px"
                          ),
                          type = 8
                        )
                      ),
                      shiny::tabPanel(
                        "Scree Plot",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0("pca_screePlot_", trt),
                            height = "400px"
                          ),
                          type = 8
                        )
                      ),
                      shiny::tabPanel(
                        "Loadings Plots",
                        shinycssloaders::withSpinner(
                          shiny::uiOutput(paste0("pca_loadingsUI_", trt)),
                          type = 8
                        )
                      ),
                      shiny::tabPanel(
                        "Biplot",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0("pca_biplot_", trt),
                            height = "400px"
                          ),
                          type = 8
                        )
                      ),
                      shiny::tabPanel(
                        "Correlation Circle",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0("pca_corrCircle_", trt),
                            height = "400px"
                          ),
                          type = 8
                        )
                      ),
                      if (!is.null(res[[trt]]$overall_3D)) {
                        shiny::tabPanel(
                          "3D Plot",
                          shinycssloaders::withSpinner(
                            shiny::plotOutput(
                              paste0("pca_3DPlot_", trt),
                              height = "400px"
                            ),
                            type = 8
                          )
                        )
                      }
                    )
                  )
                })
              )
            )
          }
        },
        # --- PLSR UI ---
        "Partial Least Squares Regression (PLSR)" = {
          shiny::tagList(
            shiny::h4("PLSR Results"),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Scores Plot",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("plsr_indivPlot", height = "400px"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Predicted vs Observed",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("plsr_predPlot", height = "400px"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Residuals vs Fitted",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("plsr_residPlot", height = "400px"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Loadings Plots",
                shinycssloaders::withSpinner(
                  shiny::uiOutput("plsr_loadingsUI"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "VIP Scores",
                shinycssloaders::withSpinner(
                  shiny::uiOutput("plsr_vipUI"),
                  type = 8
                )
              ),
              if (!is.null(res$cv_plot)) {
                shiny::tabPanel(
                  "Cross-Validation",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("plsr_cvPlot", height = "400px"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$vip_scores_indiv)) {
                shiny::tabPanel(
                  "VIP > 1: Scores",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("plsr_vipIndivPlot", height = "400px"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$vip_cv_plot)) {
                shiny::tabPanel(
                  "VIP > 1: Cross-Validation",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("plsr_vipCVPlot", height = "400px"),
                    type = 8
                  )
                )
              }
            )
          )
        },

        # --- Correlation UI ---
        "Correlation Plots" = {
          shiny::tagList(
            shiny::h4("Correlation Results"),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Spearman",
                shiny::tabsetPanel(
                  type = "tabs",
                  selected = "Spearman Heatmap",
                  shiny::tabPanel(
                    "Spearman Heatmap",
                    shinycssloaders::withSpinner(
                      shiny::plotOutput(
                        "corr_heatmap_spearman",
                        height = "600px"
                      ),
                      type = 8
                    )
                  ),
                  shiny::tabPanel(
                    "Spearman Table",
                    shinycssloaders::withSpinner(
                      DT::dataTableOutput("corr_tbl_spearman"),
                      type = 8
                    )
                  ),
                  if (
                    !is.null(res$spearman$group_plots) &&
                      length(res$spearman$group_plots)
                  ) {
                    shiny::tabPanel(
                      "Spearman Per-Group Heatmaps",
                      shinycssloaders::withSpinner(
                        shiny::uiOutput("corr_group_heatmap_ui_spearman"),
                        type = 8
                      )
                    )
                  }
                )
              ),
              shiny::tabPanel(
                "Pearson",
                shiny::tabsetPanel(
                  type = "tabs",
                  selected = "Pearson Heatmap", # <- default
                  shiny::tabPanel(
                    "Pearson Heatmap",
                    shinycssloaders::withSpinner(
                      shiny::plotOutput(
                        "corr_heatmap_pearson",
                        height = "600px"
                      ),
                      type = 8
                    )
                  ),
                  shiny::tabPanel(
                    "Pearson Table",
                    shinycssloaders::withSpinner(
                      DT::dataTableOutput("corr_tbl_pearson"),
                      type = 8
                    )
                  ),
                  if (
                    !is.null(res$pearson$group_plots) &&
                      length(res$pearson$group_plots)
                  ) {
                    shiny::tabPanel(
                      "Pearson Per-Group Heatmaps",
                      shinycssloaders::withSpinner(
                        shiny::uiOutput("corr_group_heatmap_ui_pearson"),
                        type = 8
                      )
                    )
                  }
                )
              )
            )
          )
        },
        # --- Random Forest UI ---
        "Random Forest" = {
          shiny::tagList(
            shiny::h4("Random Forest Results"),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel("Summary", shiny::uiOutput("rf_summary")),
              shiny::tabPanel(
                "Variable Importance",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("rf_vipPlot", height = "400px"),
                  type = 8
                )
              ),
              if (!is.null(res$roc_plot)) {
                shiny::tabPanel(
                  "ROC Curve",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("rf_rocPlot", height = "400px"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$rfcv_plot)) {
                shiny::tabPanel(
                  "Cross-Validation",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("rf_rfcvPlot", height = "400px"),
                    type = 8
                  )
                )
              }
            )
          )
        },

        # --- XGBoost UI ---
        "Extreme Gradient Boosting (XGBoost)" = {
          shiny::tagList(
            shiny::h4("XGBoost Results"),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Summary",
                shiny::uiOutput("xgb_summary")
              ),
              shiny::tabPanel(
                "Variable Importance",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("xgb_vipPlot", height = "400px"),
                  type = 8
                )
              ),
              if (!is.null(res$roc_plot)) {
                shiny::tabPanel(
                  "ROC Curve",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("xgb_rocPlot", height = "400px"),
                    type = 8
                  )
                )
              }
            )
          )
        },

        # --- Volcano Plot UI ---
        "Volcano Plot" = {
          shiny::tagList(
            shiny::h4("Volcano Plot Results"),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Plot",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("volcPlotOutput", height = "400px"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Statistics Table",
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("volcStats"),
                  type = 8
                )
              )
            ),
            shiny::div(
              style = "margin-top: .75rem;",
              shiny::p(
                shiny::tags$a(
                  "Learn how to interpret volcano plot results",
                  href = "https://shinyinfo.cytokineprofile.org/articles/Understanding-Volcano-Plot.html",
                  target = "_blank",
                  style = "text-decoration: underline;"
                )
              )
            )
          )
        },

        # --- Heatmap UI ---
        "Heatmap" = {
          shiny::tagList(
            shiny::h4("Heatmap Results"),
            shinycssloaders::withSpinner(
              shiny::imageOutput(
                "heatmapImage",
                height = "auto",
                width = "100%"
              ),
              type = 8
            )
          )
        },

        # --- Dual-Flashlight Plot UI ---
        "Dual-Flashlight Plot" = {
          shiny::tagList(
            shiny::h4("Dual-Flashlight Plot Results"),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Plot",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("dualflashPlotOutput", height = "400px"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Statistics Table",
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("dualflashStats"),
                  type = 8
                )
              )
            ),
            shiny::div(
              style = "margin-top: .75rem;",
              shiny::p(
                shiny::tags$a(
                  "Learn how to interpret dualflashlight plot results",
                  href = "https://shinyinfo.cytokineprofile.org/articles/Understanding-Dual-Flashlight-Plot.html",
                  target = "_blank",
                  style = "text-decoration: underline;"
                )
              )
            )
          )
        },
        "Skewness/Kurtosis" = {
          shiny::tagList(
            shiny::h4("Skewness and Kurtosis Results"),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "Skewness Plot",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("skku_skewPlot", height = "400px"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Kurtosis Plot",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("skku_kurtPlot", height = "400px"),
                  type = 8
                )
              ),
              if (input$skku_print_raw) {
                shiny::tabPanel(
                  "Raw Data",
                  shinycssloaders::withSpinner(
                    DT::dataTableOutput("skku_raw_results"),
                    type = 8
                  )
                )
              },
              if (input$skku_print_log) {
                shiny::tabPanel(
                  "Log-Transformed Data",
                  shinycssloaders::withSpinner(
                    DT::dataTableOutput("skku_log_results"),
                    type = 8
                  )
                )
              }
            )
          )
        },
        "Error-Bar Plot" = {
          shiny::tagList(
            shiny::h4("Error-Bar Plot Results"),
            shinycssloaders::withSpinner(
              shiny::plotOutput("errorBarPlotOutput", height = "auto"),
              type = 8
            )
          )
        },
        "Univariate Tests (T-test, Wilcoxon)" = {
          shiny::tagList(
            shiny::h4("Univariate Test Results"),
            shinycssloaders::withSpinner(
              DT::dataTableOutput("univariateResults"),
              type = 8
            )
          )
        },
        "Multi-level Univariate Tests (Anova, Kruskal-Wallis)" = {
          method_used <- input$uvm_method %||% "anova"
          shiny::tagList(
            shiny::h4("Multi-level Univariate Test Results"),
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Global Tests",
                shiny::br(),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("univariateResults"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Pairwise Comparisons",
                shiny::br(),
                if (method_used == "kruskal") {
                  shiny::helpText(
                    shiny::icon("circle-info"),
                    "Pairwise Wilcoxon tests are shown only when the global Kruskal-Wallis test is significant."
                  )
                },
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("univariatePairwiseResults"),
                  type = 8
                )
              ),
              if (method_used == "anova") {
                shiny::tabPanel(
                  "Assumption Checks",
                  shiny::br(),
                  shiny::helpText(
                    shiny::icon("circle-info"),
                    "Residual normality is assessed via Shapiro-Wilk on model residuals.",
                    "Homogeneity of variance is assessed via Bartlett's test.",
                    "A p-value > 0.05 indicates the assumption is met.",
                    "If either assumption is violated, consider switching the method to Kruskal-Wallis,",
                    "which is non-parametric and does not require normality or equal variances."
                  ),
                  shiny::br(),
                  shinycssloaders::withSpinner(
                    DT::dataTableOutput("anovaAssumptionTable"),
                    type = 8
                  )
                )
              }
            )
          )
        },
        "Two-way ANOVA" = {
          shiny::tagList(
            shiny::h4("Two-way ANOVA Results"),
            shiny::helpText(
              shiny::icon("circle-info"),
              "Type II and Type III sums of squares are generally preferred for unbalanced designs.",
              "Type I is included for completeness and comparison.",
              "If a modeled interaction is significant, main-effect pairwise comparisons are marginal means and should be interpreted cautiously."
            ),
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Model Terms",
                shiny::br(),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("univariateResults"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Pairwise Comparisons",
                shiny::br(),
                shiny::helpText(
                  shiny::icon("circle-info"),
                  "Pairwise comparisons are reported for factor main effects only.",
                  "Interaction-specific simple effects are not shown in this version."
                ),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("univariatePairwiseResults"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Assumption Checks",
                shiny::br(),
                shiny::helpText(
                  shiny::icon("circle-info"),
                  "Residual normality is assessed via Shapiro-Wilk on model residuals.",
                  "Homogeneity of variance is assessed via Levene's test across the effective factor cells.",
                  "Warnings also report low cell counts and significant interaction cautions."
                ),
                shiny::br(),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("anovaAssumptionTable"),
                  type = 8
                )
              )
            )
          )
        },
        "ANCOVA" = {
          shiny::tagList(
            shiny::h4("ANCOVA Results"),
            shiny::helpText(
              shiny::icon("circle-info"),
              "Type II and Type III sums of squares are generally preferred for unbalanced designs.",
              "Type I is included for completeness and comparison.",
              "If a modeled interaction is significant, main-effect pairwise comparisons are marginal means and should be interpreted cautiously."
            ),
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Model Terms",
                shiny::br(),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("univariateResults"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Pairwise Comparisons",
                shiny::br(),
                shiny::helpText(
                  shiny::icon("circle-info"),
                  "Pairwise comparisons are based on adjusted marginal means at the covariate mean.",
                  "When a factor:covariate interaction is modeled, this table also reports slope comparisons and factor contrasts at the covariate mean and mean +/- 1 SD.",
                  "Factor-by-factor simple effects are not shown in this version."
                ),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("univariatePairwiseResults"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Assumption Checks",
                shiny::br(),
                shiny::helpText(
                  shiny::icon("circle-info"),
                  "Residual normality is assessed via Shapiro-Wilk on model residuals.",
                  "Homogeneity of variance is assessed via Levene's test across the effective factor cells.",
                  "If primary:covariate or secondary:covariate is not modeled directly, the table also reports the corresponding slope-homogeneity check.",
                  "Slope fields show Modeled when the interaction term is included in the ANCOVA directly."
                ),
                shiny::br(),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("anovaAssumptionTable"),
                  type = 8
                )
              )
            )
          )
        },
        # --- Default UI for other functions (plots or tables) ---
        # This will catch simpler functions that return one plot or one table
        {
          if (
            is.list(res) &&
              length(res) > 0 &&
              all(sapply(res, function(x) inherits(x, "ggplot")))
          ) {
            # Handle functions that return a list of ggplot objects (like Boxplots)
            do.call(
              shiny::tagList,
              lapply(seq_along(res), function(i) {
                shinycssloaders::withSpinner(
                  shiny::plotOutput(
                    paste0("dynamicPlot_", i),
                    height = "400px"
                  ),
                  type = 8
                )
              })
            )
          }
        }
      )
    )
  }) |>
    shiny::bindCache(
      shiny::reactiveValuesToList(userState, all.names = TRUE),
      userData(),
      cache = "session"
    )

  output$textResults <- shiny::renderPrint({
    res <- analysisResult()
    # Add a check here:
    if (
      !(is.list(res) &&
        length(res) > 0 &&
        all(sapply(res, function(x) inherits(x, "ggplot"))))
    ) {
      # Only print if the result is NOT a list of ggplot objects
      print(res)
    } else {
      # Optionally print a message or nothing if it's a plot list
      cat("Plot results displayed below.")
    }
  }) |>
    shiny::bindCache(
      shiny::reactiveValuesToList(userState, all.names = TRUE),
      userData(),
      cache = "session"
    )

  shiny::observe({
    res <- analysisResult()
    if (
      is.list(res) &&
        length(res) > 0 &&
        all(sapply(res, function(x) inherits(x, "ggplot")))
    ) {
      for (i in seq_along(res)) {
        local({
          my_i <- i
          output[[paste0("dynamicPlot_", my_i)]] <- shiny::renderPlot({
            res[[my_i]]
          })
        })
      }
    }
  })

  shiny::observeEvent(analysisResult(), {
    # Get results and function name
    res <- analysisResult()
    func_name <- selected_function()
    shiny::req(res, func_name)

    # --- sPLS-DA Rendering Logic ---
    if (
      func_name ==
        "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
    ) {
      if ("overall_indiv_plot" %in% names(res)) {
        # --- Single Analysis Rendering ---
        output$splsda_overallIndivPlot <- shiny::renderPlot({
          grDevices::replayPlot(res$overall_indiv_plot)
        })
        output$splsda_overall3DPlot <- shiny::renderPlot({
          if (!is.null(res$overall_3D)) grDevices::replayPlot(res$overall_3D)
        })
        output$splsda_vip3DPlot <- shiny::renderPlot({
          if (!is.null(res$vip_3D)) grDevices::replayPlot(res$vip_3D)
        })
        output$splsda_interactive_plot <- plotly::renderPlotly({
          res$overall_3D_interactive
        })
        output$splsda_interactive_plot_vip <- plotly::renderPlotly({
          res$vip_3D_interactive
        })
        output$splsda_overallRocPlot <- shiny::renderPlot({
          if (!is.null(res$overall_ROC)) grDevices::replayPlot(res$overall_ROC)
        })
        output$splsda_overallCvPlot <- shiny::renderPlot({
          if (!is.null(res$overall_CV)) print(res$overall_CV)
        })
        output$splsda_vipIndivPlot <- shiny::renderPlot({
          if (!is.null(res$vip_indiv_plot)) {
            grDevices::replayPlot(res$vip_indiv_plot)
          }
        })
        output$splsda_confMatrix <- shiny::renderUI({
          if (!is.null(res$conf_matrix)) {
            copyable_text_block(res$conf_matrix)
          }
        })

        output$splsda_loadingsUI <- shiny::renderUI({
          shiny::req(res$loadings)
          lapply(seq_along(res$loadings), function(i) {
            shiny::plotOutput(
              paste0("splsda_loading_plot_", i),
              height = "400px"
            )
          })
        })
        lapply(seq_along(res$loadings), function(i) {
          output[[paste0("splsda_loading_plot_", i)]] <- shiny::renderPlot({
            grDevices::replayPlot(res$loadings[[i]])
          })
        })

        output$splsda_vipScoresUI <- shiny::renderUI({
          shiny::req(res$vip_scores)
          lapply(seq_along(res$vip_scores), function(i) {
            shiny::plotOutput(paste0("splsda_vip_plot_", i), height = "400px")
          })
        })
        lapply(seq_along(res$vip_scores), function(i) {
          output[[paste0("splsda_vip_plot_", i)]] <- shiny::renderPlot({
            print(res$vip_scores[[i]])
          })
        })

        output$splsda_vipLoadingsUI <- shiny::renderUI({
          shiny::req(res$vip_loadings)
          lapply(seq_along(res$vip_loadings), function(i) {
            shiny::plotOutput(
              paste0("splsda_vip_loading_plot_", i),
              height = "400px"
            )
          })
        })
        lapply(seq_along(res$vip_loadings), function(i) {
          output[[paste0("splsda_vip_loading_plot_", i)]] <- shiny::renderPlot({
            grDevices::replayPlot(res$vip_loadings[[i]])
          })
        })
      } else {
        # --- Multi-Group Analysis Rendering (FULLY IMPLEMENTED) ---
        for (trt in names(res)) {
          local({
            current_trt <- trt
            sub_res <- res[[current_trt]]

            output[[paste0(
              "splsda_overallIndivPlot_",
              current_trt
            )]] <- shiny::renderPlot({
              grDevices::replayPlot(sub_res$overall_indiv_plot)
            })
            output[[paste0(
              "splsda_vipIndivPlot_",
              current_trt
            )]] <- shiny::renderPlot(
              {
                if (!is.null(sub_res$vip_indiv_plot)) {
                  grDevices::replayPlot(sub_res$vip_indiv_plot)
                }
              }
            )

            output[[paste0(
              "splsda_loadingsUI_",
              current_trt
            )]] <- shiny::renderUI({
              shiny::req(sub_res$loadings)
              lapply(seq_along(sub_res$loadings), function(i) {
                shiny::plotOutput(
                  paste0("splsda_loading_plot_", current_trt, "_", i),
                  height = "400px"
                )
              })
            })
            lapply(seq_along(sub_res$loadings), function(i) {
              output[[paste0(
                "splsda_loading_plot_",
                current_trt,
                "_",
                i
              )]] <- shiny::renderPlot({
                grDevices::replayPlot(sub_res$loadings[[i]])
              })
            })

            output[[paste0(
              "splsda_vipScoresUI_",
              current_trt
            )]] <- shiny::renderUI({
              shiny::req(sub_res$vip_scores)
              lapply(seq_along(sub_res$vip_scores), function(i) {
                shiny::plotOutput(
                  paste0("splsda_vip_plot_", current_trt, "_", i),
                  height = "400px"
                )
              })
            })
            lapply(seq_along(sub_res$vip_scores), function(i) {
              output[[paste0(
                "splsda_vip_plot_",
                current_trt,
                "_",
                i
              )]] <- shiny::renderPlot({
                print(sub_res$vip_scores[[i]])
              })
            })

            output[[paste0(
              "splsda_vipLoadingsUI_",
              current_trt
            )]] <- shiny::renderUI({
              shiny::req(sub_res$vip_loadings)
              lapply(seq_along(sub_res$vip_loadings), function(i) {
                shiny::plotOutput(
                  paste0("splsda_vip_loading_plot_", current_trt, "_", i),
                  height = "400px"
                )
              })
            })
            lapply(seq_along(sub_res$vip_loadings), function(i) {
              output[[paste0(
                "splsda_vip_loading_plot_",
                current_trt,
                "_",
                i
              )]] <- shiny::renderPlot({
                grDevices::replayPlot(sub_res$vip_loadings[[i]])
              })
            })
            output[[paste0(
              "splsda_overall3DPlot_",
              current_trt
            )]] <- shiny::renderPlot({
              if (!is.null(sub_res$overall_3D)) {
                grDevices::replayPlot(sub_res$overall_3D)
              }
            })
            output[[paste0(
              "splsda_vip3DPlot_",
              current_trt
            )]] <- shiny::renderPlot({
              if (!is.null(sub_res$vip_3D)) {
                grDevices::replayPlot(sub_res$vip_3D)
              }
            })
            # ----- Interactive OVERALL 3D per-trt: open modal -----
            shiny::observeEvent(
              input[[paste0("splsda_show3d_interactive_", current_trt)]],
              {
                shiny::showModal(shiny::modalDialog(
                  plotly::plotlyOutput(
                    paste0("splsda_interactive_plot_", current_trt),
                    width = "100%",
                    height = "600px"
                  ),
                  easyClose = TRUE,
                  size = "l"
                ))
              }
            )

            # ----- Interactive OVERALL 3D per-trt: render Plotly -----
            output[[paste0(
              "splsda_interactive_plot_",
              current_trt
            )]] <- plotly::renderPlotly({
              shiny::req(sub_res$overall_3D_interactive)
              sub_res$overall_3D_interactive
            })

            # ----- Interactive VIP 3D per-trt: open modal -----
            shiny::observeEvent(
              input[[paste0("splsda_show3d_interactive_vip_", current_trt)]],
              {
                shiny::showModal(shiny::modalDialog(
                  plotly::plotlyOutput(
                    paste0("splsda_interactive_plot_vip_", current_trt),
                    width = "100%",
                    height = "75vh"
                  ),
                  easyClose = TRUE,
                  size = "l"
                ))
              }
            )

            # ----- Interactive VIP 3D per-trt: render Plotly -----
            output[[paste0(
              "splsda_interactive_plot_vip_",
              current_trt
            )]] <- plotly::renderPlotly({
              shiny::req(sub_res$vip_3D_interactive)
              sub_res$vip_3D_interactive
            })
            output[[paste0(
              "splsda_overallRocPlot_",
              current_trt
            )]] <- shiny::renderPlot({
              if (!is.null(sub_res$overall_ROC)) {
                grDevices::replayPlot(sub_res$overall_ROC)
              }
            })
            output[[paste0(
              "splsda_overallCvPlot_",
              current_trt
            )]] <- shiny::renderPlot({
              if (!is.null(sub_res$overall_CV)) {
                print(sub_res$overall_CV)
              }
            })
            output[[paste0(
              "splsda_confMatrix_",
              current_trt
            )]] <- shiny::renderUI({
              if (!is.null(sub_res$conf_matrix)) {
                copyable_text_block(sub_res$conf_matrix)
              }
            })
          })
        }
      }
    }
    # --- MINT sPLS-DA Rendering Logic ---
    if (
      func_name ==
        "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
    ) {
      shiny::req(res)
      is_nested <- is.list(res) &&
        !is.null(names(res)) &&
        is.list(res[[1]]) &&
        "global_indiv_plot" %in% names(res[[1]])

      if (!is_nested) {
        # --- Single Analysis Rendering ---
        output$mint_splsda_global_plot <- shiny::renderPlot({
          grDevices::replayPlot(res$global_indiv_plot)
        })
        output$mint_splsda_partial_plot <- shiny::renderPlot({
          grDevices::replayPlot(res$partial_indiv_plot)
        })
        if (!is.null(res$partial_loadings_plots)) {
          output$mint_splsda_loadings_ui <- shiny::renderUI({
            shiny::req(res$partial_loadings_plots)
            shiny::tagList(lapply(
              seq_along(res$partial_loadings_plots),
              function(i) {
                shiny::plotOutput(
                  paste0("mint_splsda_loading_plot_", i),
                  height = "400px"
                )
              }
            ))
          })

          # Use a for loop to correctly scope the index 'i'
          for (i in seq_along(res$partial_loadings_plots)) {
            local({
              # Use local() to ensure 'i' is captured correctly
              local_i <- i
              output[[paste0(
                "mint_splsda_loading_plot_",
                local_i
              )]] <- shiny::renderPlot({
                grDevices::replayPlot(res$partial_loadings_plots[[local_i]])
              })
            })
          }
        }
        output$mint_splsda_corr_circle_plot <- shiny::renderPlot({
          grDevices::replayPlot(res$correlation_circle_plot)
        })
        output$mint_splsda_cim_plot <- shiny::renderPlot(
          {
            if (
              !is.null(res$cim_obj) && inherits(res$cim_obj, "recordedplot")
            ) {
              grDevices::replayPlot(res$cim_obj)
            }
          },
          height = 600
        )
        output$mint_splsda_roc_plot <- shiny::renderPlot({
          if (!is.null(res$roc_plot)) grDevices::replayPlot(res$roc_plot)
        })
      } else {
        # --- Multi-Group Analysis Rendering ---
        for (trt in names(res)) {
          local({
            current_trt <- trt
            sub_res <- res[[current_trt]]
            if (!is.null(sub_res)) {
              output[[paste0(
                "mint_splsda_global_",
                current_trt
              )]] <- shiny::renderPlot({
                grDevices::replayPlot(sub_res$global_indiv_plot)
              })
              output[[paste0(
                "mint_splsda_partial_",
                current_trt
              )]] <- shiny::renderPlot({
                grDevices::replayPlot(sub_res$partial_indiv_plot)
              })
              output[[paste0(
                "mint_splsda_loadings_",
                current_trt
              )]] <- shiny::renderUI({
                shiny::req(sub_res$partial_loadings_plots)
                lapply(seq_along(sub_res$partial_loadings_plots), function(i) {
                  shiny::plotOutput(
                    paste0("mint_splsda_loading_", current_trt, "_", i),
                    height = "400px"
                  )
                })
              })

              lapply(seq_along(sub_res$partial_loadings_plots), function(i) {
                output[[paste0(
                  "mint_splsda_loading_",
                  current_trt,
                  "_",
                  i
                )]] <- shiny::renderPlot({
                  grDevices::replayPlot(sub_res$partial_loadings_plots[[i]])
                })
              })
              output[[paste0(
                "mint_splsda_corr_",
                current_trt
              )]] <- shiny::renderPlot({
                grDevices::replayPlot(sub_res$correlation_circle_plot)
              })
              output[[paste0(
                "mint_splsda_cim_",
                current_trt
              )]] <- shiny::renderPlot(
                {
                  if (
                    !is.null(sub_res$cim_obj) &&
                      inherits(sub_res$cim_obj, "recordedplot")
                  ) {
                    grDevices::replayPlot(sub_res$cim_obj)
                  }
                },
                height = 600
              )
              output[[paste0(
                "mint_splsda_roc_",
                current_trt
              )]] <- shiny::renderPlot({
                if (!is.null(sub_res$roc_plot)) {
                  grDevices::replayPlot(sub_res$roc_plot)
                }
              })
            }
          })
        }
      }
    }
    if (func_name == "Principal Component Analysis (PCA)" && is.list(res)) {
      if ("overall_indiv_plot" %in% names(res)) {
        output$pca_indivPlot <- shiny::renderPlot({
          grDevices::replayPlot(res$overall_indiv_plot)
        })
        output$pca_3DPlot <- shiny::renderPlot({
          if (!is.null(res$overall_3D)) grDevices::replayPlot(res$overall_3D)
        })
        output$pca_screePlot <- shiny::renderPlot({
          if (!is.null(res$overall_scree_plot)) {
            grDevices::replayPlot(res$overall_scree_plot)
          }
        })
        output$pca_biplot <- shiny::renderPlot({
          if (!is.null(res$biplot)) grDevices::replayPlot(res$biplot)
        })
        output$pca_corrCircle <- shiny::renderPlot({
          if (!is.null(res$correlation_circle)) {
            grDevices::replayPlot(res$correlation_circle)
          }
        })

        output$pca_loadingsUI <- shiny::renderUI({
          if (!is.null(res$loadings)) {
            shiny::tagList(lapply(seq_along(res$loadings), function(i) {
              shiny::plotOutput(paste0("pca_loadings_", i), height = "300px")
            }))
          }
        })

        if (!is.null(res$loadings)) {
          for (i in seq_along(res$loadings)) {
            local({
              local_i <- i
              output[[paste0("pca_loadings_", local_i)]] <- shiny::renderPlot({
                grDevices::replayPlot(res$loadings[[local_i]])
              })
            })
          }
        }
      } else {
        for (lvl in names(res)) {
          local({
            currentGroup <- lvl
            subres <- res[[currentGroup]]
            output[[paste0(
              "pca_indivPlot_",
              currentGroup
            )]] <- shiny::renderPlot(
              {
                if (!is.null(subres$overall_indiv_plot)) {
                  grDevices::replayPlot(subres$overall_indiv_plot)
                }
              }
            )
            output[[paste0("pca_3DPlot_", currentGroup)]] <- shiny::renderPlot({
              if (!is.null(subres$overall_3D)) {
                grDevices::replayPlot(subres$overall_3D)
              }
            })
            output[[paste0(
              "pca_screePlot_",
              currentGroup
            )]] <- shiny::renderPlot(
              {
                if (!is.null(subres$overall_scree_plot)) {
                  grDevices::replayPlot(subres$overall_scree_plot)
                }
              }
            )
            output[[paste0("pca_biplot_", currentGroup)]] <- shiny::renderPlot({
              if (!is.null(subres$biplot)) grDevices::replayPlot(subres$biplot)
            })
            output[[paste0(
              "pca_corrCircle_",
              currentGroup
            )]] <- shiny::renderPlot({
              if (!is.null(subres$correlation_circle)) {
                grDevices::replayPlot(subres$correlation_circle)
              }
            })

            output[[paste0(
              "pca_loadingsUI_",
              currentGroup
            )]] <- shiny::renderUI({
              if (!is.null(subres$loadings)) {
                shiny::tagList(lapply(seq_along(subres$loadings), function(i) {
                  safeGroup <- gsub("[^A-Za-z0-9_]+", "_", currentGroup)
                  shiny::plotOutput(
                    paste0("pca_loadings_", safeGroup, "_", i),
                    height = "300px"
                  )
                }))
              }
            })
            if (!is.null(subres$loadings)) {
              for (i in seq_along(subres$loadings)) {
                local({
                  local_i <- i
                  safeGroup <- gsub("[^A-Za-z0-9_]+", "_", currentGroup)
                  output[[paste0(
                    "pca_loadings_",
                    safeGroup,
                    "_",
                    local_i
                  )]] <- shiny::renderPlot({
                    grDevices::replayPlot(subres$loadings[[local_i]])
                  })
                })
              }
            }
          })
        }
      }
    }

    # --- PLSR Rendering Logic ---
    if (func_name == "Partial Least Squares Regression (PLSR)") {
      # Scores (recordedplot)
      output$plsr_indivPlot <- shiny::renderPlot({
        shiny::req(res$scores_plot)
        grDevices::replayPlot(res$scores_plot)
      })

      # Predicted vs Observed (recordedplot)
      output$plsr_predPlot <- shiny::renderPlot({
        shiny::req(res$pred_vs_obs)
        grDevices::replayPlot(res$pred_vs_obs)
      })

      # Residuals vs Fitted (recordedplot)
      output$plsr_residPlot <- shiny::renderPlot({
        shiny::req(res$residuals_plot)
        grDevices::replayPlot(res$residuals_plot)
      })

      # CV plot (recordedplot) + metrics text
      output$plsr_cvPlot <- shiny::renderPlot({
        shiny::req(res$cv_plot)
        grDevices::replayPlot(res$cv_plot)
      })

      # Per-component loadings (each element is a recordedplot)
      output$plsr_loadingsUI <- shiny::renderUI({
        shiny::req(res$loadings)
        shiny::tagList(lapply(seq_along(res$loadings), function(i) {
          shiny::plotOutput(paste0("plsr_loading_plot_", i), height = "350px")
        }))
      })
      if (!is.null(res$loadings)) {
        for (i in seq_along(res$loadings)) {
          local({
            ii <- i
            output[[paste0("plsr_loading_plot_", ii)]] <- shiny::renderPlot({
              grDevices::replayPlot(res$loadings[[ii]])
            })
          })
        }
      }

      # VIP barplots (ggplot objects - do NOT use replayPlot)
      output$plsr_vipUI <- shiny::renderUI({
        shiny::req(res$vip_scores)
        shiny::tagList(lapply(seq_along(res$vip_scores), function(i) {
          shiny::plotOutput(paste0("plsr_vip_plot_", i), height = "350px")
        }))
      })
      if (!is.null(res$vip_scores)) {
        for (i in seq_along(res$vip_scores)) {
          local({
            ii <- i
            output[[paste0("plsr_vip_plot_", ii)]] <- shiny::renderPlot({
              # ggplot object; just return it
              res$vip_scores[[ii]]
            })
          })
        }
      }

      # VIP>1 preview plots (recordedplot, conditional)
      output$plsr_vipIndivPlot <- shiny::renderPlot({
        shiny::req(res$vip_scores_indiv)
        grDevices::replayPlot(res$vip_scores_indiv)
      })
      output$plsr_vipCVPlot <- shiny::renderPlot({
        shiny::req(res$vip_cv_plot)
        grDevices::replayPlot(res$vip_cv_plot)
      })
    }

    # --- Correlation Rendering Logic ---
    # Tables
    output$corr_tbl_spearman <- DT::renderDataTable({
      shiny::req(res$spearman$table)
      DT::datatable(
        res$spearman$table,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE
      )
    })
    output$corr_tbl_pearson <- DT::renderDataTable({
      shiny::req(res$pearson$table)
      DT::datatable(
        res$pearson$table,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # Heatmaps (overall)
    output$corr_heatmap_spearman <- shiny::renderPlot({
      shiny::req(res$spearman$plot)
      print(res$spearman$plot)
    })

    output$corr_heatmap_pearson <- shiny::renderPlot({
      shiny::req(res$pearson$plot)
      print(res$pearson$plot)
    })

    # Per-group heatmaps (if any) - Spearman
    output$corr_group_heatmap_ui_spearman <- shiny::renderUI({
      shiny::req(res$spearman$group_plots)
      tabs <- lapply(names(res$spearman$group_plots), function(lv) {
        shiny::tabPanel(
          title = lv,
          shiny::plotOutput(
            paste0("corr_heatmap_grp_spear_", gsub("\\W+", "_", lv)),
            height = "600px"
          )
        )
      })
      do.call(shiny::tabsetPanel, c(list(type = "tabs"), tabs))
    })

    if (!is.null(res$spearman$group_plots)) {
      for (lv in names(res$spearman$group_plots)) {
        local({
          lvl <- lv
          output[[paste0(
            "corr_heatmap_grp_spear_",
            gsub("\\W+", "_", lvl)
          )]] <- shiny::renderPlot({
            print(res$spearman$group_plots[[lvl]])
          })
        })
      }
    }

    # Per-group heatmaps (if any) - Pearson
    output$corr_group_heatmap_ui_pearson <- shiny::renderUI({
      shiny::req(res$pearson$group_plots)
      tabs <- lapply(names(res$pearson$group_plots), function(lv) {
        shiny::tabPanel(
          title = lv,
          shiny::plotOutput(
            paste0("corr_heatmap_grp_pear_", gsub("\\W+", "_", lv)),
            height = "600px"
          )
        )
      })
      do.call(shiny::tabsetPanel, c(list(type = "tabs"), tabs))
    })

    if (!is.null(res$pearson$group_plots)) {
      for (lv in names(res$pearson$group_plots)) {
        local({
          lvl <- lv
          output[[paste0(
            "corr_heatmap_grp_pear_",
            gsub("\\W+", "_", lvl)
          )]] <- shiny::renderPlot({
            print(res$pearson$group_plots[[lvl]])
          })
        })
      }
    }

    # --- Random Forest Rendering Logic ---
    if (func_name == "Random Forest") {
      output$rf_summary <- shiny::renderUI({
        copyable_text_block(res$summary_text)
      })
      output$rf_vipPlot <- shiny::renderPlot({
        print(res$vip_plot)
      })
      output$rf_rocPlot <- shiny::renderPlot({
        if (!is.null(res$roc_plot)) print(res$roc_plot)
      })
      output$rf_rfcvPlot <- shiny::renderPlot({
        if (!is.null(res$rfcv_plot)) print(res$rfcv_plot)
      })
    }

    # --- XGBoost Rendering Logic ---
    if (func_name == "Extreme Gradient Boosting (XGBoost)") {
      output$xgb_summary <- shiny::renderUI({
        copyable_text_block(res$summary_text)
      })
      output$xgb_vipPlot <- shiny::renderPlot({
        print(res$importance_plot)
      })
      output$xgb_rocPlot <- shiny::renderPlot({
        if (!is.null(res$roc_plot)) print(res$roc_plot)
      })
    }

    # --- Volcano Plot Rendering Logic ---
    if (func_name == "Volcano Plot") {
      output$volcPlotOutput <- shiny::renderPlot({
        print(res$plot)
      })
      output$volcStats <- DT::renderDataTable(
        {
          res$stats
        },
        options = list(pageLength = 10, scrollX = TRUE)
      )
    }

    # --- Heatmap Rendering Logic ---
    if (func_name == "Heatmap") {
      output$heatmapImage <- shiny::renderImage(
        {
          shiny::req(res)

          # res is the pheatmap object
          ph <- res

          # size for preview; UI will scale it down responsively
          panel_px <- session$clientData$output_heatmapImage_width %||% 1000
          out_png <- file.path(
            tempdir(),
            paste0("cyt_heatmap_", Sys.Date(), ".png")
          )

          grDevices::png(
            out_png,
            width = panel_px,
            height = round(panel_px * 0.62),
            res = 96
          )
          grid::grid.newpage()
          grid::grid.draw(ph$gtable) # draw the heatmap object
          grDevices::dev.off()

          list(src = out_png, contentType = "image/png", alt = "Heatmap")
        },
        deleteFile = FALSE
      )
    }

    # --- Dual-Flashlight Plot Rendering ---
    if (func_name == "Dual-Flashlight Plot") {
      output$dualflashPlotOutput <- shiny::renderPlot({
        print(res$plot)
      })
      output$dualflashStats <- DT::renderDataTable(
        {
          res$stats
        },
        options = list(pageLength = 10, scrollX = TRUE)
      )
    }
    # --- Skewness/Kurtosis Rendering ---
    if (func_name == "Skewness/Kurtosis") {
      output$skku_skewPlot <- shiny::renderPlot({
        print(res$p_skew)
      })
      output$skku_kurtPlot <- shiny::renderPlot({
        print(res$p_kurt)
      })
      output$skku_raw_results <- DT::renderDataTable(
        {
          res$raw_results
        },
        options = list(pageLength = 10, scrollX = TRUE)
      )
      output$skku_log_results <- DT::renderDataTable(
        {
          res$log_results
        },
        options = list(pageLength = 10, scrollX = TRUE)
      )
    }

    # --- Generic Plot List Rendering Logic (for Boxplots, etc.) ---
    if (
      is.list(res) &&
        length(res) > 0 &&
        all(sapply(res, function(x) inherits(x, "ggplot")))
    ) {
      lapply(seq_along(res), function(i) {
        local({
          my_i <- i
          output[[paste0("dynamicPlot_", my_i)]] <- shiny::renderPlot({
            res[[my_i]]
          })
        })
      })
    }
  })
  # --- Univariate results table ---
  output$univariateResults <- DT::renderDataTable(
    {
      res <- analysisResult()
      shiny::req(
        selected_function() %in%
          c(
            "Univariate Tests (T-test, Wilcoxon)",
            "Multi-level Univariate Tests (Anova, Kruskal-Wallis)",
            "Two-way ANOVA",
            "ANCOVA"
          )
      )
      # Multi-level tests return a named list; t-test/Wilcoxon returns a plain data frame
      if (is.list(res) && !is.data.frame(res) && !is.null(res$results)) {
        res$results
      } else {
        res
      }
    },
    options = list(pageLength = 10, scrollX = TRUE),
    rownames = FALSE
  )
  output$univariatePairwiseResults <- DT::renderDataTable(
    {
      res <- analysisResult()
      shiny::req(
        selected_function() %in%
          c(
            "Multi-level Univariate Tests (Anova, Kruskal-Wallis)",
            "Two-way ANOVA",
            "ANCOVA"
          ),
        is.list(res),
        !is.null(res$pairwise)
      )
      res$pairwise
    },
    options = list(pageLength = 10, scrollX = TRUE),
    rownames = FALSE
  )
  output$anovaAssumptionTable <- DT::renderDataTable(
    {
      res <- analysisResult()
      shiny::req(
        selected_function() %in%
          c(
            "Multi-level Univariate Tests (Anova, Kruskal-Wallis)",
            "Two-way ANOVA",
            "ANCOVA"
          ),
        is.list(res),
        !is.null(res$assumptions)
      )
      res$assumptions
    },
    options = list(pageLength = 10, scrollX = TRUE),
    rownames = FALSE
  )
  output$errorBarPlotOutput <- shiny::renderPlot(
    {
      shiny::req(analysisResult())
      res <- analysisResult()
      print(res)
    },
    height = function() {
      res <- analysisResult()
      if (is.null(res) || !inherits(res, "ggplot")) {
        return(400L)
      }
      # Extract number of facets from the built plot data if possible
      n_facets <- tryCatch(
        {
          pb <- ggplot2::ggplot_build(res)
          length(unique(pb$data[[1]]$PANEL))
        },
        error = function(e) 9L
      )
      n_cols <- input$eb_n_col %||% 3L
      n_rows <- ceiling(n_facets / n_cols)
      max(400L, n_rows * 180L)
    }
  )

  # Render the download UI conditionally
  output$download_ui <- shiny::renderUI({
    plots <- exportablePlots()
    tables <- exportableTables()

    if (length(plots) > 0) {
      shiny::tagList(
        shiny::textInput(
          "download_filename",
          "Enter filename for download:",
          value = "Results"
        ),
        shiny::selectInput(
          "download_format",
          "Export format",
          choices = c(
            "PDF" = "pdf",
            "PNG" = "png",
            "JPEG" = "jpeg",
            "TIFF" = "tiff",
            "SVG" = "svg"
          ),
          selected = "pdf"
        ),
        shiny::downloadButton("download_output", "Download Results")
      )
    } else if (length(tables) > 0) {
      is_multi_univariate <-
        selected_function() %in%
        c(
          "Multi-level Univariate Tests (Anova, Kruskal-Wallis)",
          "Two-way ANOVA",
          "ANCOVA"
        )
      shiny::tagList(
        shiny::textInput(
          "download_filename",
          "Enter filename for download:",
          value = if (is_multi_univariate) {
            "Results"
          } else {
            "Results"
          }
        ),
        shiny::downloadButton(
          "download_output",
          if (is_multi_univariate) {
            "Download Results (ZIP)"
          } else {
            "Download Results (CSV)"
          }
        )
      )
    }
  })

  # Download handler
  output$download_output <- shiny::downloadHandler(
    filename = function() {
      tables <- exportableTables()
      if (length(tables) > 0 && length(exportablePlots()) == 0) {
        ext <- if (length(tables) > 1L) "zip" else "csv"
        return(paste0(
          input$download_filename %||% "Results",
          ".",
          ext
        ))
      }

      fmt <- tolower(input$download_format %||% "pdf")
      plot_count <- length(exportablePlots())
      ext <- if (fmt == "pdf" || plot_count <= 1L) fmt else "zip"
      paste0(input$download_filename %||% "Results", ".", ext)
    },
    content = function(file) {
      tables <- exportableTables()
      if (length(tables) > 0 && length(exportablePlots()) == 0) {
        if (length(tables) == 1L) {
          utils::write.csv(tables[[1]], file, row.names = FALSE)
          return(invisible(NULL))
        }

        base_file <- tools::file_path_sans_ext(file)
        generated_files <- vapply(
          names(tables),
          function(table_name) {
            generated_file <- paste0(base_file, "_", table_name, ".csv")
            utils::write.csv(
              tables[[table_name]],
              generated_file,
              row.names = FALSE
            )
            basename(generated_file)
          },
          character(1)
        )

        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(dirname(file))
        utils::zip(zipfile = file, files = generated_files)
        return(invisible(NULL))
      }

      plots <- exportablePlots()
      shiny::req(length(plots) > 0)

      fmt <- tolower(input$download_format %||% "pdf")
      base_file <- tools::file_path_sans_ext(file)

      if (fmt == "pdf") {
        cyt_export(
          plots = plots,
          filename = base_file,
          format = fmt
        )
        return(invisible(NULL))
      }

      cyt_export(
        plots = plots,
        filename = base_file,
        format = fmt
      )

      if (length(plots) == 1L) {
        generated_file <- paste0(base_file, "_001.", fmt)
        if (!file.rename(generated_file, file)) {
          file.copy(generated_file, file, overwrite = TRUE)
        }
        return(invisible(NULL))
      }

      generated_files <- list.files(
        path = dirname(file),
        pattern = paste0("^", basename(base_file), "_[0-9]{3}\\.", fmt, "$"),
        full.names = TRUE
      )
      if (!length(generated_files)) {
        stop("No export files were generated.")
      }

      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(dirname(file))
      utils::zip(zipfile = file, files = basename(generated_files))
    }
  )

  invisible(app_stage_commit(app_ctx, stage_env))
}

init_save_key_inputs_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  userState <- app_ctx$userState
  analysis_font_specs <- app_ctx$analysis_font_specs
  filteredData <- app_ctx$filteredData
  selected_function <- app_ctx$selected_function
  selected_stat_func <- app_ctx$selected_stat_func
  currentPage <- app_ctx$currentPage
  currentStep <- app_ctx$currentStep
  font_settings_state_from_inputs <- app_ctx$font_settings_state_from_inputs
  ## ---------------------------
  ## Save key inputs using shiny::reactiveValues
  ## ---------------------------
  shiny::observeEvent(
    input$selected_columns,
    {
      if (
        !is.null(input$selected_columns) && length(input$selected_columns) > 0
      ) {
        userState$selected_columns <- input$selected_columns
      }
    },
    ignoreNULL = TRUE
  )

  # Keep `userState$selected_columns` in sync with the Step 2 checkbox groups so
  # changes are immediately visible to downstream logic (e.g. variable counts).
  shiny::observeEvent(
    list(input$selected_categorical_cols, input$selected_numerical_cols),
    {
      if (
        is.null(input$selected_categorical_cols) &&
          is.null(input$selected_numerical_cols)
      ) {
        return()
      }

      userState$selected_categorical_cols <- input$selected_categorical_cols
      userState$selected_numerical_cols <- input$selected_numerical_cols

      # Combine category + numeric selections into a single vector (may be empty)
      sc <- unique(c(
        input$selected_categorical_cols %||% character(0),
        input$selected_numerical_cols %||% character(0)
      ))
      userState$selected_columns <- sc

      # Reset manual override flags so defaults will recompute when the
      # user changes which columns are selected. Also trigger UI updates
      # to reflect the new computed defaults.
      userState$plsr_keepX_manual <- FALSE
      userState$splsda_var_num_manual <- FALSE
      userState$mint_splsda_var_num_manual <- FALSE

      # Compute defaults based on the current filtered data and update
      # inputs where present. Use try(...) to avoid errors when UI not yet
      # created.
      df_now <- tryCatch(filteredData(), error = function(e) NULL)
      if (!is.null(df_now)) {
        default_vars <- sum(sapply(df_now, is.numeric))
        userState$plsr_keepX <- default_vars
        userState$splsda_var_num <- default_vars
        userState$mint_splsda_var_num <- default_vars
        try(
          shiny::updateNumericInput(
            session,
            "plsr_keepX",
            value = default_vars
          ),
          silent = TRUE
        )
        try(
          shiny::updateNumericInput(
            session,
            "splsda_var_num",
            value = default_vars
          ),
          silent = TRUE
        )
        try(
          shiny::updateNumericInput(
            session,
            "mint_splsda_var_num",
            value = default_vars
          ),
          silent = TRUE
        )
      }
    },
    ignoreNULL = FALSE
  )
  shiny::observeEvent(input$step2_scale, {
    userState$step2_scale <- input$step2_scale
  })
  shiny::observeEvent(selected_function(), {
    userState$selected_function <- selected_function()
  })
  lapply(names(analysis_font_specs), function(func_name) {
    spec <- analysis_font_specs[[func_name]]
    shiny::observe({
      state <- font_settings_state_from_inputs(
        input = input,
        prefix = spec$prefix,
        supported_fields = spec$supported_fields,
        default_font_settings = spec$default_font_settings
      )
      if (!is.null(state)) {
        userState[[spec$state_key]] <- state
      }
    })
  })
  # For sheet name
  shiny::observeEvent(input$sheet_name, {
    userState$sheet_name <- input$sheet_name
  })
  # For Boxplots
  shiny::observeEvent(input$bp_bin_size, {
    userState$bp_bin_size <- input$bp_bin_size
  })
  shiny::observeEvent(input$bp_group_by, {
    userState$bp_group_by <- input$bp_group_by
  })
  shiny::observeEvent(input$bp_y_lim, {
    userState$bp_y_lim <- input$bp_y_lim
  })
  # For Violin Plots
  shiny::observeEvent(input$vio_group_by, {
    userState$vio_group_by <- input$vio_group_by
  })
  shiny::observeEvent(input$vio_bin_size, {
    userState$vio_bin_size <- input$vio_bin_size
  })
  shiny::observeEvent(input$vio_y_lim, {
    userState$vio_y_lim <- input$vio_y_lim
  })
  shiny::observeEvent(input$vio_boxplot_overlay, {
    userState$vio_boxplot_overlay <- input$vio_boxplot_overlay
  })
  # For Error-Bar Plot
  shiny::observeEvent(input$eb_group_col, {
    userState$eb_group_col <- input$eb_group_col
  })
  shiny::observeEvent(input$eb_p_lab, {
    userState$eb_p_lab <- input$eb_p_lab
  })
  shiny::observeEvent(input$eb_es_lab, {
    userState$eb_es_lab <- input$eb_es_lab
  })
  shiny::observeEvent(input$eb_class_symbol, {
    userState$eb_class_symbol <- input$eb_class_symbol
  })
  shiny::observeEvent(input$eb_x_lab, {
    userState$eb_x_lab <- input$eb_x_lab
  })
  shiny::observeEvent(input$eb_y_lab, {
    userState$eb_y_lab <- input$eb_y_lab
  })
  shiny::observeEvent(input$eb_title, {
    userState$eb_title <- input$eb_title
  })
  shiny::observeEvent(input$eb_stat, {
    userState$eb_stat <- input$eb_stat
  })
  shiny::observeEvent(input$eb_error, {
    userState$eb_error <- input$eb_error
  })
  shiny::observeEvent(input$eb_method, {
    userState$eb_method <- input$eb_method
  })
  shiny::observeEvent(input$eb_p_adjust_method, {
    userState$eb_p_adjust_method <- input$eb_p_adjust_method
  })
  shiny::observeEvent(input$eb_n_col, {
    userState$eb_n_col <- input$eb_n_col
  })
  shiny::observeEvent(input$eb_fill_palette, {
    userState$eb_fill_palette <- if (identical(input$eb_fill_palette, "grey")) {
      "gray"
    } else {
      input$eb_fill_palette
    }
  })
  # For Univariate Tests
  shiny::observeEvent(input$uv2_method, {
    userState$uv2_method <- input$uv2_method
  })
  shiny::observeEvent(input$uv2_p_adjust_method, {
    userState$uv2_p_adjust_method <- input$uv2_p_adjust_method
  })
  shiny::observeEvent(input$uvm_method, {
    userState$uvm_method <- input$uvm_method
  })
  shiny::observeEvent(input$uvm_p_adjust_method, {
    userState$uvm_p_adjust_method <- input$uvm_p_adjust_method
  })
  shiny::observeEvent(input$twa_primary_cat_var, {
    userState$twa_primary_cat_var <- input$twa_primary_cat_var
  })
  shiny::observeEvent(input$twa_secondary_cat_var, {
    userState$twa_secondary_cat_var <- input$twa_secondary_cat_var
  })
  shiny::observeEvent(input$twa_include_primary_secondary_interaction, {
    userState$twa_include_primary_secondary_interaction <-
      input$twa_include_primary_secondary_interaction
  })
  shiny::observeEvent(input$anc_primary_cat_var, {
    userState$anc_primary_cat_var <- input$anc_primary_cat_var
  })
  shiny::observeEvent(input$anc_secondary_cat_var, {
    userState$anc_secondary_cat_var <- input$anc_secondary_cat_var
  })
  shiny::observeEvent(input$anc_covariate_col, {
    userState$anc_covariate_col <- input$anc_covariate_col
  })
  shiny::observeEvent(input$anc_include_primary_secondary_interaction, {
    userState$anc_include_primary_secondary_interaction <-
      input$anc_include_primary_secondary_interaction
  })
  shiny::observeEvent(input$anc_include_primary_covariate_interaction, {
    userState$anc_include_primary_covariate_interaction <-
      input$anc_include_primary_covariate_interaction
  })
  shiny::observeEvent(input$anc_include_secondary_covariate_interaction, {
    userState$anc_include_secondary_covariate_interaction <-
      input$anc_include_secondary_covariate_interaction
  })
  # For Dual-Flashlight Plot
  shiny::observeEvent(input$df_group_var, {
    userState$df_group_var <- input$df_group_var
  })
  shiny::observeEvent(input$df_cond1, {
    userState$df_cond1 <- input$df_cond1
  })
  shiny::observeEvent(input$df_cond2, {
    userState$df_cond2 <- input$df_cond2
  })
  shiny::observeEvent(input$df_ssmd_thresh, {
    userState$df_ssmd_thresh <- input$df_ssmd_thresh
  })
  shiny::observeEvent(input$df_log2fc_thresh, {
    userState$df_log2fc_thresh <- input$df_log2fc_thresh
  })
  shiny::observeEvent(input$df_top_labels, {
    userState$df_top_labels <- input$df_top_labels
  })
  # For Heatmap
  shiny::observeEvent(input$hm_annotation, {
    userState$hm_annotation <- input$hm_annotation
  })
  shiny::observeEvent(input$hm_ann_side, {
    userState$hm_ann_side <- input$hm_ann_side
  })
  # For PCA
  shiny::observeEvent(input$pca_group_col, {
    userState$pca_group_col <- input$pca_group_col
  })
  shiny::observeEvent(input$pca_group_col2, {
    userState$pca_group_col2 <- input$pca_group_col2
  })
  shiny::observeEvent(input$pca_comp_num, {
    userState$pca_comp_num <- input$pca_comp_num
  })
  shiny::observeEvent(input$pca_ellipse, {
    userState$pca_ellipse <- input$pca_ellipse
  })
  shiny::observeEvent(input$pca_style, {
    userState$pca_style <- input$pca_style
  })
  shiny::observeEvent(input$pca_pch, {
    userState$pca_pch <- input$pca_pch
  })
  shiny::observeEvent(input$pca_colors, {
    userState$pca_colors <- input$pca_colors
  })
  # For Random Forest
  shiny::observeEvent(input$rf_group_col, {
    userState$rf_group_col <- input$rf_group_col
  })
  shiny::observeEvent(input$rf_ntree, {
    userState$rf_ntree <- input$rf_ntree
  })
  shiny::observeEvent(input$rf_mtry, {
    userState$rf_mtry <- input$rf_mtry
  })
  shiny::observeEvent(input$rf_train_fraction, {
    userState$rf_train_fraction <- input$rf_train_fraction
  })
  shiny::observeEvent(input$rf_plot_roc, {
    userState$rf_plot_roc <- input$rf_plot_roc
  })
  shiny::observeEvent(input$rf_run_rfcv, {
    userState$rf_run_rfcv <- input$rf_run_rfcv
  })
  shiny::observeEvent(input$rf_k_folds, {
    userState$rf_k_folds <- input$rf_k_folds
  })
  shiny::observeEvent(input$rf_step, {
    userState$rf_step <- input$rf_step
  })
  # For Skewness/Kurtosis
  shiny::observeEvent(input$skku_group_cols, {
    userState$skku_group_cols <- input$skku_group_cols
  })
  shiny::observeEvent(input$skku_print_raw, {
    userState$skku_print_raw <- input$skku_print_raw
  })
  shiny::observeEvent(input$skku_print_log, {
    userState$skku_print_log <- input$skku_print_log
  })

  # For PLSR
  shiny::observeEvent(input$plsr_group_col, {
    userState$plsr_group_col <- input$plsr_group_col
  })
  shiny::observeEvent(input$plsr_response_col, {
    userState$plsr_response_col <- input$plsr_response_col
  })
  shiny::observeEvent(input$plsr_predictor_cols, {
    userState$plsr_predictor_cols <- input$plsr_predictor_cols
  })
  shiny::observeEvent(input$plsr_comp_num, {
    userState$plsr_comp_num <- input$plsr_comp_num
  })
  shiny::observeEvent(input$plsr_sparse, {
    userState$plsr_sparse <- input$plsr_sparse
  })
  shiny::observeEvent(input$plsr_keepX, {
    df <- filteredData()
    shiny::req(df)
    computed_default <- sum(sapply(df, is.numeric))
    # Mark as manually changed if the input does not equal the computed default.
    userState$plsr_keepX_manual <- (input$plsr_keepX != computed_default)
    # Also update the stored value so it is available if the user has modified it.
    userState$plsr_keepX <- input$plsr_keepX
  })

  # Keep PLSR `keepX` in sync with data unless user manually set it.
  shiny::observeEvent(
    filteredData(),
    {
      df <- filteredData()
      shiny::req(df)
      computed_default <- sum(sapply(df, is.numeric))
      if (!isTRUE(userState$plsr_keepX_manual)) {
        userState$plsr_keepX <- computed_default
        try(
          shiny::updateNumericInput(
            session,
            "plsr_keepX",
            value = computed_default
          ),
          silent = TRUE
        )
      }
    },
    ignoreNULL = TRUE
  )
  shiny::observeEvent(input$plsr_comp_num, {
    userState$plsr_comp_num <- input$plsr_comp_num
  })
  shiny::observeEvent(input$plsr_cv_opt, {
    userState$plsr_cv_opt <- input$plsr_cv_opt
  })
  shiny::observeEvent(input$plsr_fold_num, {
    userState$plsr_fold_num <- input$plsr_fold_num
  })
  shiny::observeEvent(input$plsr_ellipse, {
    userState$plsr_ellipse <- input$plsr_ellipse
  })
  shiny::observeEvent(input$plsr_colors, {
    userState$plsr_colors <- input$plsr_colors
  })
  # For sPLS-DA
  shiny::observeEvent(input$splsda_group_col, {
    userState$splsda_group_col <- input$splsda_group_col
  })
  shiny::observeEvent(input$splsda_group_col2, {
    userState$splsda_group_col2 <- input$splsda_group_col2
  })
  shiny::observeEvent(input$splsda_use_batch_corr, {
    userState$splsda_use_batch_corr <- input$splsda_use_batch_corr
  })
  shiny::observeEvent(input$splsda_batch_col, {
    userState$splsda_batch_col <- input$splsda_batch_col
  })
  shiny::observeEvent(input$splsda_use_multilevel, {
    userState$splsda_use_multilevel <- input$splsda_use_multilevel
  })
  shiny::observeEvent(input$splsda_multilevel, {
    userState$splsda_multilevel <- input$splsda_multilevel
  })
  shiny::observeEvent(input$splsda_var_num, {
    df <- filteredData()
    shiny::req(df)
    computed_default <- sum(sapply(df, is.numeric))
    # Mark as manually changed if the input does not equal the computed default.
    userState$splsda_var_num_manual <- (input$splsda_var_num !=
      computed_default)
    # Also update the stored value so it is available if the user has modified it.
    userState$splsda_var_num <- input$splsda_var_num
  })

  # Keep sPLS-DA variable count in sync with data unless user manually set it.
  shiny::observeEvent(
    filteredData(),
    {
      df <- filteredData()
      shiny::req(df)
      computed_default <- sum(sapply(df, is.numeric))
      # Only update the input when the user hasn't manually overridden it.
      if (!isTRUE(userState$splsda_var_num_manual)) {
        userState$splsda_var_num <- computed_default
        # If the UI exists, update it as well so the number shown matches.
        try(
          shiny::updateNumericInput(
            session,
            "splsda_var_num",
            value = computed_default
          ),
          silent = TRUE
        )
      }
    },
    ignoreNULL = TRUE
  )

  shiny::observeEvent(input$splsda_cv_opt, {
    userState$splsda_cv_opt <- input$splsda_cv_opt
  })
  shiny::observeEvent(input$splsda_fold_num, {
    userState$splsda_fold_num <- input$splsda_fold_num
  })
  shiny::observeEvent(input$splsda_comp_num, {
    userState$splsda_comp_num <- input$splsda_comp_num
  })
  shiny::observeEvent(input$splsda_ind_names_mode, {
    userState$splsda_ind_names_mode <- input$splsda_ind_names_mode
  })
  shiny::observeEvent(input$splsda_ind_names_col, {
    userState$splsda_ind_names_col <- input$splsda_ind_names_col
  })
  shiny::observeEvent(input$splsda_pch, {
    userState$splsda_pch <- input$splsda_pch
  })
  shiny::observeEvent(input$splsda_style, {
    userState$splsda_style <- input$splsda_style
  })
  shiny::observeEvent(input$splsda_show3d_interactive, {
    shiny::showModal(shiny::modalDialog(
      plotly::plotlyOutput(
        "splsda_interactive_plot",
        width = "100%",
        height = "600px"
      ),
      easyClose = TRUE,
      size = "l"
    ))
  })
  shiny::observeEvent(input$splsda_show3d_interactive_vip, {
    shiny::showModal(shiny::modalDialog(
      plotly::plotlyOutput(
        "splsda_interactive_plot_vip",
        width = "100%",
        height = "600px"
      ),
      easyClose = TRUE,
      size = "l"
    ))
  })
  shiny::observeEvent(input$splsda_roc, {
    userState$splsda_roc <- input$splsda_roc
  })
  shiny::observeEvent(input$splsda_ellipse, {
    userState$splsda_ellipse <- input$splsda_ellipse
  })
  shiny::observeEvent(input$splsda_bg, {
    userState$splsda_bg <- input$splsda_bg
  })
  shiny::observeEvent(input$splsda_conf_mat, {
    userState$splsda_conf_mat <- input$splsda_conf_mat
  })
  shiny::observeEvent(input$splsda_colors, {
    userState$splsda_colors <- input$splsda_colors
  })
  # For MINT sPLS-DA
  shiny::observeEvent(input$mint_splsda_group_col, {
    userState$mint_splsda_group_col <- input$mint_splsda_group_col
  })
  shiny::observeEvent(input$mint_splsda_group_col2, {
    userState$mint_splsda_group_col2 <- input$mint_splsda_group_col2
  })
  shiny::observeEvent(input$mint_splsda_batch_col, {
    userState$mint_splsda_batch_col <- input$mint_splsda_batch_col
  })
  shiny::observeEvent(input$mint_splsda_var_num, {
    df <- filteredData()
    shiny::req(df)
    computed_default <- sum(sapply(df, is.numeric))
    # Mark as manually changed if the input does not equal the computed default.
    userState$mint_splsda_var_num_manual <- (input$mint_splsda_var_num !=
      computed_default)
    # Also update the stored value so it is available if the user has modified it.
    userState$mint_splsda_var_num <- input$mint_splsda_var_num
  })

  # Keep MINT sPLS-DA variable count in sync with data unless user manually set it.
  shiny::observeEvent(
    filteredData(),
    {
      df <- filteredData()
      shiny::req(df)
      computed_default <- sum(sapply(df, is.numeric))
      if (!isTRUE(userState$mint_splsda_var_num_manual)) {
        userState$mint_splsda_var_num <- computed_default
        try(
          shiny::updateNumericInput(
            session,
            "mint_splsda_var_num",
            value = computed_default
          ),
          silent = TRUE
        )
      }
    },
    ignoreNULL = TRUE
  )
  shiny::observeEvent(input$mint_splsda_comp_num, {
    userState$mint_splsda_comp_num <- input$mint_splsda_comp_num
  })
  shiny::observeEvent(input$mint_splsda_cim, {
    userState$mint_splsda_cim <- input$mint_splsda_cim
  })
  shiny::observeEvent(input$mint_splsda_ellipse, {
    userState$mint_splsda_ellipse <- input$mint_splsda_ellipse
  })
  shiny::observeEvent(input$mint_splsda_bg, {
    userState$mint_splsda_bg <- input$mint_splsda_bg
  })
  shiny::observeEvent(input$mint_splsda_roc, {
    userState$mint_splsda_roc <- input$mint_splsda_roc
  })
  shiny::observeEvent(input$mint_splsda_colors, {
    userState$mint_splsda_colors <- input$mint_splsda_colors
  })
  # For Volcano Plot
  shiny::observeEvent(input$volc_group_col, {
    userState$volc_group_col <- input$volc_group_col
  })
  shiny::observeEvent(input$volc_cond1, {
    userState$volc_cond1 <- input$volc_cond1
  })
  shiny::observeEvent(input$volc_cond2, {
    userState$volc_cond2 <- input$volc_cond2
  })
  shiny::observeEvent(input$volc_fold_change_thresh, {
    userState$volc_fold_change_thresh <- input$volc_fold_change_thresh
  })
  shiny::observeEvent(input$volc_p_value_thresh, {
    userState$volc_p_value_thresh <- input$volc_p_value_thresh
  })
  shiny::observeEvent(input$volc_top_labels, {
    userState$volc_top_labels <- input$volc_top_labels
  })
  # For XGBoost
  shiny::observeEvent(input$xgb_group_col, {
    userState$xgb_group_col <- input$xgb_group_col
  })
  shiny::observeEvent(input$xgb_train_fraction, {
    userState$xgb_train_fraction <- input$xgb_train_fraction
  })
  shiny::observeEvent(input$xgb_nrounds, {
    userState$xgb_nrounds <- input$xgb_nrounds
  })
  shiny::observeEvent(input$xgb_max_depth, {
    userState$xgb_max_depth <- input$xgb_max_depth
  })
  shiny::observeEvent(input$xgb_eta, {
    userState$xgb_eta <- input$xgb_eta
  })
  shiny::observeEvent(input$xgb_nfold, {
    userState$xgb_nfold <- input$xgb_nfold
  })
  shiny::observeEvent(input$xgb_cv, {
    userState$xgb_cv <- input$xgb_cv
  })
  shiny::observeEvent(input$xgb_eval_metric, {
    userState$xgb_eval_metric <- input$xgb_eval_metric
  })
  shiny::observeEvent(input$xgb_top_n_features, {
    userState$xgb_top_n_features <- input$xgb_top_n_features
  })
  shiny::observeEvent(input$xgb_plot_roc, {
    userState$xgb_plot_roc <- input$xgb_plot_roc
  })

  # For Correlation Analysis
  shiny::observeEvent(input$corr_target, {
    userState$corr_target <- input$corr_target
  })
  shiny::observeEvent(input$corr_group_col, {
    userState$corr_group_col <- input$corr_group_col
  })
  shiny::observeEvent(input$corr_by_group, {
    userState$corr_by_group <- input$corr_by_group
  })

  # remember which categories the user checked
  shiny::observeEvent(
    input$analysis_categories,
    {
      userState$analysis_categories <- input$analysis_categories
    },
    ignoreNULL = FALSE
  )
  # remember which function picker they chose
  shiny::observeEvent(input$stat_function, {
    userState$stat_function <- input$stat_function
  })
  shiny::observeEvent(input$exploratory_function, {
    userState$exploratory_function <- input$exploratory_function
  })
  shiny::observeEvent(input$multivariate_function, {
    userState$multivariate_function <- input$multivariate_function
  })
  shiny::observeEvent(input$ml_function, {
    userState$ml_function <- input$ml_function
  })
  shiny::observeEvent(
    input$built_in_choice,
    {
      userState$built_in_choice <- input$built_in_choice
    },
    ignoreNULL = FALSE
  )

  shiny::observeEvent(input$menu_univariate_2lvl, {
    selected_stat_func("Univariate Tests (T-test, Wilcoxon)")
    selected_function("Univariate Tests (T-test, Wilcoxon)")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_univariate_multi, {
    selected_stat_func("Multi-level Univariate Tests (Anova, Kruskal-Wallis)")
    selected_function("Multi-level Univariate Tests (Anova, Kruskal-Wallis)")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_two_way_anova, {
    selected_stat_func("Two-way ANOVA")
    selected_function("Two-way ANOVA")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_ancova, {
    selected_stat_func("ANCOVA")
    selected_function("ANCOVA")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_boxplots, {
    selected_function("Boxplots")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_violin, {
    selected_function("Violin Plots")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_errorbp, {
    selected_function("Error-Bar Plot")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_dualflash, {
    selected_function("Dual-Flashlight Plot")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_volcano, {
    selected_function("Volcano Plot")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_heatmap, {
    selected_function("Heatmap")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_correlation, {
    selected_function("Correlation Plots")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_PCA, {
    selected_function("Principal Component Analysis (PCA)")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_PLSR, {
    selected_function("Partial Least Squares Regression (PLSR)")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_splsda, {
    selected_function(
      "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
    )
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_mint_splsda, {
    selected_function(
      "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
    )
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_skewkurt, {
    selected_function("Skewness/Kurtosis")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_rf, {
    selected_function("Random Forest")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_xgb, {
    selected_function("Extreme Gradient Boosting (XGBoost)")
    currentPage("step4")
    currentStep(4)
  })

  invisible(app_stage_commit(app_ctx, stage_env))
}
