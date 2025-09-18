library(shiny)
library(mixOmics)
library(dplyr)
library(tidyr)
library(rlang)
library(broom)
library(ggplot2)
library(ggcorrplot)
library(readxl)
library(bslib)
library(shinyhelper)
library(DT)
library(shinyFeedback)
library(skimr)
library(shinycssloaders)
library(patchwork)
library(recipes)
library(impute)

# Define server logic
server <- function(input, output, session) {
  # Creating a temp dir for data uploads
  upload_dir <- file.path(tempdir(), "uploads")
  dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)
  builtins_dir <- file.path(tempdir(), "builtins")
  dir.create(builtins_dir, recursive = TRUE, showWarnings = FALSE)

  # Helpers
  shinyhelper::observe_helpers()
  shinyFeedback::useShinyFeedback()
  ## ---------------------------
  ## Theme Toggle
  ## ---------------------------
  # Set an initial theme
  session$setCurrentTheme(
    bslib::bs_theme(
      bootswatch = "darkly",
      base_font = font_google("Inter"),
      code_font = font_google("Roboto Mono")
    )
  )

  # Rebuild the theme when the user picks a new theme
  shiny::observeEvent(input$theme_choice, {
    session$setCurrentTheme(
      bslib::bs_theme(
        bootswatch = input$theme_choice,
        base_font = font_google("Inter"),
        code_font = font_google("Roboto Mono")
      )
    )
  })
  ## ---------------------------
  ## Wizard Step Control
  ## ---------------------------
  currentStep <- shiny::reactiveVal(1)
  output$currentStep <- shiny::reactive({
    currentStep()
  })
  shiny::outputOptions(output, "currentStep", suspendWhenHidden = FALSE)

  ## ---------------------------
  ## Persistent State: Create a shiny::reactiveValues object
  ## ---------------------------
  selected_function <- shiny::reactiveVal(NULL)
  deleted_row_ids <- shiny::reactiveVal(character())
  imputed_data <- shiny::reactiveVal(NULL)

  userState <- shiny::reactiveValues(
    # General state
    selected_columns = NULL,
    # Built=in Data built‑in tracking:
    use_builtin = FALSE,
    built_in_choice = NULL,

    # Imputation
    impute_meta = NULL,

    # Step 2 log2 transformation checkbox
    step2_log2 = FALSE,

    # Boxplots options
    bp_bin_size = NULL,
    bp_mf_row = NULL,
    bp_y_lim = NULL,

    # Enhanced Boxplots options
    bp2_mf_row = NULL,
    bp2_y_lim = NULL,

    # Error-Bar Plot
    eb_group_col = NULL,
    eb_p_lab = NULL,
    eb_es_lab = NULL,
    eb_class_symbol = NULL,
    eb_x_lab = NULL,
    eb_y_lab = NULL,
    eb_title = NULL,

    # Dual-Flashlight Plot options
    df_group_var = NULL,
    df_cond1 = NULL,
    df_cond2 = NULL,
    df_ssmd_thresh = NULL,
    df_log2fc_thresh = NULL,
    df_top_labels = NULL,

    # Heatmap options
    hm_annotation = NULL,

    # PCA options
    pca_group_col = NULL,
    pca_group_col2 = NULL,
    pca_comp_num = NULL,
    pca_ellipse = NULL,
    pca_style = NULL,
    pca_pch = NULL,
    pca_colors = NULL,

    # Random Forest options
    rf_group_col = NULL,
    rf_ntree = NULL,
    rf_mtry = NULL,
    rf_train_fraction = NULL,
    rf_plot_roc = NULL,
    rf_run_rfcv = NULL,
    rf_k_folds = NULL,
    rf_step = NULL,

    # PLSR options
    plsr_group_col = NULL,
    plsr_response_col = NULL,
    plsr_comp_num = NULL,
    plsr_keepX = NULL,
    plsr_keepX_manual = FALSE,
    plsr_sparse = FALSE,
    plsr_cv_opt = NULL,
    plsr_fold_num = NULL,
    plsr_ellipse = NULL,
    plsr_colors = NULL,

    # Skewness/Kurtosis options
    skku_group_cols = NULL,
    skku_print_raw = NULL,
    skku_print_log = NULL,

    # sPLS-DA options
    splsda_group_col = NULL,
    splsda_group_col2 = NULL,
    spsda_batch_col = NULL,
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

    # Volcano Plot options
    volc_group_col = NULL,
    volc_cond1 = NULL,
    volc_cond2 = NULL,
    volc_fold_change_thresh = NULL,
    volc_p_value_thresh = NULL,
    volc_top_labels = NULL,

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

    # Correlation options
    corr_target = NULL,
    corr_group_col = NULL,
    corr_by_group = FALSE
  )
  # Reactive values to store the selection from our new custom buttons
  selected_stat_func <- shiny::reactiveVal("ANOVA")
  selected_exploratory_func <- shiny::reactiveVal("Boxplots")
  selected_multivariate_func <- shiny::reactiveVal(
    "Principal Component Analysis (PCA)"
  )
  selected_ml_func <- shiny::reactiveVal("Random Forest")
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
  userData <- shiny::reactive({
    # built‑in branch stays the same
    if (isTRUE(input$use_builtin)) {
      req(input$built_in_choice)
      # pull the data frame out of memory
      df <- get(input$built_in_choice)
      # snapshot it to disk so it "sticks around" just like an upload
      dest <- file.path(builtins_dir, paste0(input$built_in_choice, ".rds"))
      if (!file.exists(dest)) {
        saveRDS(df, dest)
      }
    } else {
      # user upload branch
      req(input$datafile)
      # copy into our session‐local tempdir
      dest <- file.path(upload_dir, input$datafile$name)
      if (!file.exists(dest)) {
        file.copy(input$datafile$datapath, dest, overwrite = TRUE)
      }
      # read from dest (not from the app root!)
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
        sheet_num <- ifelse(is.null(input$sheet), 1, input$sheet)
        df <- readxl::read_excel(dest, sheet = sheet_num) %>% as.data.frame()
      } else {
        stop("Unsupported file type.")
      }
    }

    # Add a unique internal ID to each row for tracking deletions
    df$..cyto_id.. <- 1:nrow(df)
    df
  })

  ## ---------------------------
  ## UI for Sheet Selector and Built-in Data Choice
  ## ---------------------------
  output$sheet_selector <- shiny::renderUI({
    req(input$datafile)
    ext <- tolower(tools::file_ext(input$datafile$name))
    if (ext %in% c("xls", "xlsx")) {
      numericInput("sheet", "Sheet Number", value = 1, min = 1)
    } else {
      return(NULL)
    }
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

  # REACTIVE & OUTPUT to track if data is loaded
  output$data_is_loaded <- shiny::reactive({
    isTruthy(userData())
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
    req(isTruthy(input$datafile) || input$use_builtin)
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

  ## ---------------------------
  ## Data Filtering and Column Selection
  ## ---------------------------

  # Decoupled UI generation to prevent reactive loops
  output$filter_ui <- shiny::renderUI({
    df <- userData()
    req(df, input$selected_categorical_cols)

    # Only create filters for the categorical columns the user has selected
    factor_cols <- intersect(
      names(df)[sapply(df, function(x) is.factor(x) || is.character(x))],
      input$selected_categorical_cols
    )

    if (length(factor_cols) == 0) {
      return(NULL)
    }

    lapply(factor_cols, function(col) {
      all_levels <- sort(unique(df[[col]]))

      # Use isolate() to read the input value without creating a dependency
      # On first load, default to all levels being selected
      selected_now <- isolate(input[[paste0("filter_", col)]]) %||% all_levels

      selectizeInput(
        inputId = paste0("filter_", col),
        label = paste("Filter", col, "(select levels)"),
        choices = all_levels,
        selected = selected_now,
        multiple = TRUE,
        options = list(plugins = c("remove_button", "restore_on_backspace"))
      )
    })
  })

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
    "black"
  )

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
    req(func)

    userState$selected_function <- func
    func_name <- func
    ui_list <- list()
    # For ANOVA / Two-Sample T-Test we show no options, just a short hint.
    if (func %in% c("ANOVA", "Two-Sample T-Test")) {
      return(div(
        class = "well",
        h4(paste(func, "options")),
        p("No additional options for this analysis."),
        p("Click", strong(paste("Run Analysis")), "to continue.")
      ))
    }
    switch(
      func_name,
      # ------------------------
      # Boxplots
      # ------------------------
      "Boxplots" = {
        ui_list <- tagList(
          # first row: Bin size + Graphs per row/col
          fluidRow(
            column(
              width = 6,
              numericInput(
                "bp_bin_size",
                label = helper(
                  type = "inline",
                  title = "Bin size for boxplots",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Bin Size</span>"
                  ),
                  content = "Determines the number of columns (variables) to group together in each set of box plots. For example, a bin size of 25 will display box plots for up to 25 columns (variables) at a time. If there are more columns, multiple sets of box plots will be generated.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$bp_bin_size) %||% 25,
                min = 1
              )
            ),
            column(
              width = 6,
              textInput(
                "bp_mf_row",
                label = helper(
                  type = "inline",
                  title = "Graphs per Row and Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Graphs per Row and Columns</span><br>(rows, cols; comma-separated)"
                  ),
                  content = "Specify the layout of multiple box plots on the plotting area, defined as (rows, cols). 
                         For example, '2,2' will arrange up to 4 box plots in a 2x2 grid.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$bp_mf_row) %||% "1,1"
              )
            )
          ),

          # second row: Y-axis limits + log2 checkbox
          fluidRow(
            column(
              width = 6,
              textInput(
                "bp_y_lim",
                label = helper(
                  type = "inline",
                  title = "Y-axis Limits",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Y-axis Limits</span><br>(min, max; comma-separated; leave blank for auto)"
                  ),
                  content = "Set the minimum and maximum values for the Y-axis, entered as (min, max) e.g., '0,100'. Leave blank for automatic scaling based on data range. 
                         This controls the vertical range displayed on the plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$bp_y_lim) %||% ""
              )
            )
          )
        )
      },
      # ------------------------
      # Enhanced Boxplots
      # ------------------------
      "Enhanced Boxplots" = {
        ui_list <- tagList(
          fluidRow(
            column(
              6,
              textInput(
                "bp2_mf_row",
                label = helper(
                  type = "inline",
                  title = "Graphs per Row and Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Graphs per Row & Columns</span><br>(rows, cols; comma-separated)"
                  ),
                  content = "Specify the layout of multiple enhanced box plots on the plotting area, defined as (rows, cols).",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$bp2_mf_row) %||% "1,1"
              )
            ),
            column(
              6,
              textInput(
                "bp2_y_lim",
                label = helper(
                  type = "inline",
                  title = "Y-axis Limits",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Y-axis Limits</span><br>(min, max; leave blank for auto)"
                  ),
                  content = "Set the vertical bounds for the plot as (min, max).",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$bp2_y_lim) %||% ""
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

        ui_list <- tagList(
          fluidRow(
            column(
              6,
              selectInput(
                "eb_group_col",
                label = helper(
                  type = "inline",
                  title = "Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Comparison Column</span>"
                  ),
                  content = "The column to use for comparing groups in the error-bar plot. This should be a categorical variable.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cat_vars,
                selected = isolate(userState$eb_group_col) %||% cat_vars[1]
              )
            )
          ),
          fluidRow(
            column(
              6,
              checkboxInput(
                "eb_p_lab",
                label = helper(
                  type = "inline",
                  title = "P-Value Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>P-Value Label</span>"
                  ),
                  content = "Label for the p-value annotation on the plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$eb_p_lab) %||% FALSE
              )
            ),
            column(
              6,
              checkboxInput(
                "eb_es_lab",
                label = helper(
                  type = "inline",
                  title = "Effect-Size Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Effect-Size Label</span>"
                  ),
                  content = HTML(paste0(
                    "Display effect size labels above the bar plots to indicate the effect observed by strictly
                standardized mean differences (SSMD).
                Learn more about SSMD at <a href='https://doi.org/10.1177/1087057111405851' target='_blank' rel='noopener noreferrer'>link 1</a>, and 
                <a href='https://doi.org/10.1177/1087057107300645' target='_blank' rel='noopener noreferrer'>link 2</a>"
                  )),
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$eb_es_lab) %||% FALSE
              )
            )
          ),
          fluidRow(
            column(
              6,
              checkboxInput(
                "eb_class_symbol",
                label = helper(
                  type = "inline",
                  title = "Class Symbol",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Class Symbol</span>"
                  ),
                  content = "Use class symbols for the plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$eb_class_symbol) %||% FALSE
              )
            ),
            column(
              6,
              textInput(
                "eb_x_lab",
                label = helper(
                  type = "inline",
                  title = "X-Axis Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>X-Axis Label</span>"
                  ),
                  content = "The label to use on the X axis (e.g., 'Cytokine').",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$eb_x_lab) %||% "Cytokines"
              )
            )
          ),
          fluidRow(
            column(
              6,
              textInput(
                "eb_y_lab",
                label = helper(
                  type = "inline",
                  title = "Y-Axis Label",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Y-Axis Label</span>"
                  ),
                  content = "The label to use on the Y axis (e.g., 'Value').",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$eb_y_lab) %||% "Concentrations"
              )
            ),
            column(
              6,
              textInput(
                "eb_title",
                label = helper(
                  type = "inline",
                  title = "Plot Title",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Plot Title</span>"
                  ),
                  content = "The title to use for the plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$eb_title) %||% "Error-Bar Plot"
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
        cols <- names(df)

        ui_list <- tagList(
          fluidRow(
            column(
              4,
              selectInput(
                "df_group_var",
                label = helper(
                  type = "inline",
                  title = "Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Comparison Column</span>"
                  ),
                  content = "Column that contains the groups to compare in the dual-flashlight plot. This should be a categorical variable.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = isolate(userState$df_group_var) %||% cols[1]
              )
            ),
            column(
              4,
              uiOutput("df_conditions_ui")
            )
          ),
          fluidRow(
            column(
              4,
              numericInput(
                "df_ssmd_thresh",
                label = helper(
                  type = "inline",
                  title = "SSMD Threshold",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>SSMD Threshold</span>"
                  ),
                  content = HTML(paste0(
                    "The threshold for the SSMD (strictly standardized mean difference) value. 
                     SSMD is a measure of how different two groups are. A higher threshold means that 
                      only larger differences are considered significant.
                     Learn more about SSMD at <a href='https://doi.org/10.1177/1087057111405851' target='_blank' rel='noopener noreferrer'>link 1</a>, and 
                    <a href='https://doi.org/10.1177/1087057107300645' target='_blank' rel='noopener noreferrer'>link 2</a>"
                  )),

                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$df_ssmd_thresh) %||% 1
              )
            ),
            column(
              4,
              numericInput(
                "df_log2fc_thresh",
                label = helper(
                  type = "inline",
                  title = "Log2 FC Threshold",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Log2 Fold Change Threshold</span>"
                  ),
                  content = "Threshold for log2 fold change.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$df_log2fc_thresh) %||% 1
              )
            )
          ),
          fluidRow(
            column(
              4,
              numericInput(
                "df_top_labels",
                label = helper(
                  type = "inline",
                  title = "Top Labels",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Top Labels</span>"
                  ),
                  content = "Number of top features to label on the plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$df_top_labels) %||% 15
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
        ann_choices <- names(df)[sapply(df, function(x) {
          is.factor(x) || is.character(x)
        })]

        ui_list <- tagList(
          fluidRow(
            column(
              6,
              selectInput(
                "hm_annotation",
                label = helper(
                  type = "inline",
                  title = "Annotation Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Annotation Column</span>"
                  ),
                  content = "Categorical column to use for annotating rows/columns.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = ann_choices,
                selected = isolate(userState$hm_annotation) %||% ann_choices[1]
              )
            )
          )
        )
      },
      # ————————————————————
      # Correlation Plots
      # ————————————————————
      "Correlation Plots" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }

        cols <- names(df)
        cols <- cols[cols != "..cyto_id.."]
        num_cols <- cols[sapply(df[cols], is.numeric)]
        none_choice <- c("None (no grouping)" = "")

        ui_list <- tagList(
          fluidRow(
            column(
              6,
              selectInput(
                "corr_target",
                label = helper(
                  type = "inline",
                  title = "Target Variable",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Target Variable</span>"
                  ),
                  content = "Correlate this variable against all other numeric features.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = num_cols,
                selected = isolate(userState$corr_target) %||% num_cols[1]
              )
            ),
            column(
              6,
              selectInput(
                "corr_group_col",
                label = helper(
                  type = "inline",
                  title = "Grouping (optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Grouping Variable</span>"
                  ),
                  content = "If set, you can render per-group heatmaps (first two levels) for each method.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c(none_choice, setNames(cols, cols)),
                selected = isolate(userState$corr_group_col) %||% ""
              )
            )
          ),
          fluidRow(
            column(
              6,
              checkboxInput(
                "corr_by_group",
                label = helper(
                  type = "inline",
                  title = "Per-group Heatmaps",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Per-group Heatmaps</span>"
                  ),
                  content = "If a grouping column is selected, show one heatmap per (first two) levels.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$corr_by_group) %||% FALSE
              )
            )
          )
        )
      },

      # ————————————————————
      # PCA
      # ————————————————————
      "Principal Component Analysis (PCA)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- names(df)
        ui_list <- tagList(
          # Row 1: grouping columns
          fluidRow(
            column(
              6,
              selectInput(
                "pca_group_col",
                label = helper(
                  type = "inline",
                  title = "PCA Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>PCA Comparison Column</span>"
                  ),
                  content = "Column to use for grouping the data in PCA. This is typically a categorical variable that defines the groups you want to compare.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = isolate(userState$pca_group_col) %||% cols[1]
              )
            ),
            column(
              6,
              selectInput(
                "pca_group_col2",
                label = helper(
                  type = "inline",
                  title = "PCA Stratification Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>PCA Stratification Column</span>"
                  ),
                  content = "Optional second grouping column for PCA. This can be used to further stratify the data within the primary grouping column. If you don't want to use a second grouping column, select the same column as in the comparison column.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = isolate(userState$pca_group_col2) %||% cols[1]
              )
            )
          ),

          # Row 2: components & colors
          fluidRow(
            column(
              6,
              numericInput(
                "pca_comp_num",
                label = helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = "Specify the number of principal components to calculate and potentially visualize. For 2D plots, the first two components are used. For 3D plots, at least 3 components are required.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$pca_comp_num) %||% 2,
                min = 2
              )
            ),
            column(
              6,
              selectizeInput(
                "pca_colors",
                label = helper(
                  type = "inline",
                  title = "Select Colors for PCA Plot (Optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Select Colors for PCA Plot (Optional)</span>"
                  ),
                  content = "The color palette to use for the PCA plot. Select the number of colors to match the number of categories in comparison column.",
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
          fluidRow(
            column(
              6,
              checkboxInput(
                "pca_ellipse",
                label = helper(
                  type = "inline",
                  title = "Draw Ellipse",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Draw Ellipse</span>"
                  ),
                  content = "Draw an ellipse around the data points on the PCA plot. (Draws an ellipse covering 95% of the data points.)",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$pca_ellipse) %||% FALSE
              )
            )
          ),

          # Row 4: style & symbols
          fluidRow(
            column(
              6,
              selectInput(
                "pca_style",
                label = helper(
                  type = "inline",
                  title = "Plot Style",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Plot Style</span>"
                  ),
                  content = "The style of the PCA plot. Choose between 2D and 3D. Requires at least 3 components for 3D.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("2D", "3D"),
                selected = isolate(userState$pca_style) %||% "2D"
              )
            ),
            column(
              6,
              selectizeInput(
                "pca_pch",
                label = helper(
                  type = "inline",
                  title = "Plotting Symbols",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Plotting Symbols</span>"
                  ),
                  content = "Select plotting character (PCH) symbols for data points. If using a second grouping column, match the number of symbols to its categories.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = pch_choices,
                selected = isolate(
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

      # ——————————————————————————————
      # Partial Least Squares Regression
      # ——————————————————————————————
      "Partial Least Squares Regression (PLSR)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- names(df)
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
        ui_list <- tagList(
          fluidRow(
            column(
              6,
              selectInput(
                "plsr_group_col",
                label = helper(
                  type = "inline",
                  title = "Grouping Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Grouping Column</span>"
                  ),
                  content = "Optional: choose a column to colour points in the score plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = grp_choices,
                selected = isolate(userState$plsr_group_col) %||% ""
              )
            ),
            column(
              6,
              selectInput(
                "plsr_response_col",
                label = helper(
                  type = "inline",
                  title = "Response Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Response Column</span>"
                  ),
                  content = "Select the numeric column to be predicted.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = num_cols,
                selected = isolate(userState$plsr_response_col) %||%
                  num_cols[1]
              )
            )
          ),
          fluidRow(
            column(
              6,
              numericInput(
                "plsr_comp_num",
                label = helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = "How many latent components to extract.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$plsr_comp_num) %||% 2,
                min = 2
              )
            ),
            # Add a checkbox for sparse PLSR
            column(
              6,
              checkboxInput(
                "plsr_sparse",
                label = helper(
                  type = "inline",
                  title = "Sparse PLSR",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Sparse PLSR</span>"
                  ),
                  content = "Use sparse PLSR for variable selection.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$plsr_sparse) %||% FALSE
              )
            )
          ),
          fluidRow(
            column(
              6,
              # Conditional to show keepX only if sparse is checked
              conditionalPanel(
                condition = "input.plsr_sparse == true",
                numericInput(
                  "plsr_keepX",
                  label = helper(
                    type = "inline",
                    title = "Number of Variables",
                    icon = "fas fa-exclamation-circle",
                    shiny_tag = HTML(
                      "<span style='margin-right: 15px;'>Number of Variables</span>"
                    ),
                    content = "Select number of variables to keep per component.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = isolate(userState$plsr_keepX),
                  1
                )
              )
            )
          ),
          fluidRow(
            column(
              6,
              checkboxInput(
                "plsr_ellipse",
                label = helper(
                  type = "inline",
                  title = "Ellipse",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Ellipse</span>"
                  ),
                  content = "Draw 95% confidence ellipses on the Scores plot (if grouping provided).",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$plsr_ellipse) %||% FALSE
              )
            ),
            column(
              6,
              selectizeInput(
                "plsr_colors",
                label = helper(
                  type = "inline",
                  title = "Colors (optionnal)",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Colors (optional)</span>"
                  ),
                  content = "Optional palette for levels of the grouping column (recycled as needed).",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = allowed_colors, # already defined globally
                selected = isolate(userState$plsr_colors),
                multiple = TRUE,
                options = list(placeholder = "Pick colors (optional)")
              )
            )
          ),
          fluidRow(
            column(
              6,
              tagList(
                radioButtons(
                  "plsr_cv_opt",
                  label = helper(
                    type = "inline",
                    title = "Cross-validation",
                    icon = "fas fa-exclamation-circle",
                    shiny_tag = HTML(
                      "<span style='margin-right: 15px;'>Cross-validation</span>"
                    ),
                    content = "Choose Leave-One-Out Cross Validation (LOOCV) or M-fold with defined number of folds to assess predictive performance.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = c("None", "LOOCV", "Mfold"),
                  selected = isolate(userState$plsr_cv_opt) %||% "None",
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = "input.plsr_cv_opt == 'Mfold'",
                  numericInput(
                    "plsr_fold_num",
                    "Number of folds",
                    value = isolate(userState$plsr_fold_num) %||% 5,
                    min = 2
                  )
                )
              )
            )
          )
        )
      },
      # ——————————————————————————————
      # Random Forest
      # ——————————————————————————————
      "Random Forest" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- names(df)

        ui_list <- tagList(
          # Row 1: grouping & ntrees
          fluidRow(
            column(
              6,
              selectInput(
                "rf_group_col",
                label = helper(
                  type = "inline",
                  title = "Grouping Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Grouping Column</span>"
                  ),
                  content = "Select the column that contains the outcome or class labels you want the Random Forest model to predict.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = isolate(userState$rf_group_col) %||% cols[1]
              )
            ),
            column(
              6,
              numericInput(
                "rf_ntree",
                label = helper(
                  type = "inline",
                  title = "Number of Trees",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Number of Trees</span>"
                  ),
                  content = "The number of trees to grow in the random forest model. Each tree is a simple model; more trees can improve predictions but take longer to compute.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$rf_ntree) %||% 500,
                min = 1
              )
            )
          ),

          # Row 2: mtry & train fraction
          fluidRow(
            column(
              6,
              numericInput(
                "rf_mtry",
                label = helper(
                  type = "inline",
                  title = "Number of Variables to Split",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Number of Variables to Split</span>"
                  ),
                  content = "The number of variables to randomly select at each split.
                         At each decision point, the model randomly considers this number of variables, which helps improve model accuracy.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$rf_mtry) %||% 5,
                min = 1
              )
            ),
            column(
              6,
              numericInput(
                "rf_train_fraction",
                label = helper(
                  type = "inline",
                  title = "Train Fraction",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Train Fraction</span>"
                  ),
                  content = "The fraction of the data to use for training the random forest model. The remainder is used for testing the model.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$rf_train_fraction) %||% 0.7,
                min = 0.1,
                max = 0.9,
                step = 0.1
              )
            )
          ),

          # Row 3: ROC & RFCV toggle
          fluidRow(
            column(
              6,
              checkboxInput(
                "rf_plot_roc",
                label = helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Plot ROC (Binary Comparison Only)</span>"
                  ),
                  content = "Plot the Receiving Operating Characteristic (ROC) curve for the random forest model.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$rf_plot_roc) %||% FALSE
              )
            ),
            column(
              6,
              checkboxInput(
                "rf_run_rfcv",
                label = helper(
                  type = "inline",
                  title = "Run RFCV?",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Run RFCV</span>"
                  ),
                  content = "Perform cross-validation to evaluate model performance with a decreasing number of predictors.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$rf_run_rfcv) %||% FALSE
              )
            )
          ),

          # Row 4: RFCV parameters (conditional)
          conditionalPanel(
            condition = "input.rf_run_rfcv == true",
            fluidRow(
              column(
                6,
                numericInput(
                  "rf_k_folds",
                  label = helper(
                    type = "inline",
                    title = "Number of Folds",
                    icon = "fas fa-question-circle",
                    shiny_tag = HTML(
                      "<span style='margin-right:15px;'>Number of Folds</span>"
                    ),
                    content = "The number of folds to use for cross-validation in the RFCV process.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = isolate(userState$rf_k_folds) %||% 5,
                  min = 2
                )
              ),
              column(
                6,
                numericInput(
                  "rf_step",
                  label = helper(
                    type = "inline",
                    title = "Step Size",
                    icon = "fas fa-question-circle",
                    shiny_tag = HTML(
                      "<span style='margin-right:15px;'>Step Size</span>"
                    ),
                    content = "The step size to use for the RFCV process (0.1–0.9).",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = isolate(userState$rf_step) %||% 0.5,
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
        cols <- names(df)

        ui_list <- tagList(
          fluidRow(
            column(
              6,
              selectInput(
                "skku_group_cols",
                label = helper(
                  type = "inline",
                  title = "Grouping Columns",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Grouping Columns</span>"
                  ),
                  content = "Select one or more categorical columns to stratify the skewness/kurtosis.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = isolate(userState$skku_group_cols),
                multiple = TRUE
              )
            ),
            column(
              6,
              checkboxInput(
                "skku_print_raw",
                label = helper(
                  type = "inline",
                  title = "Print Raw Results",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Print Raw Results</span>"
                  ),
                  content = "Show the un-transformed skewness/kurtosis values.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$skku_print_raw) %||% FALSE
              )
            )
          ),
          fluidRow(
            column(
              6,
              checkboxInput(
                "skku_print_log",
                label = helper(
                  type = "inline",
                  title = "Print Log-Transformed Results",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Print Log-Transformed Results</span>"
                  ),
                  content = "Show skewness/kurtosis after log transformation.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$skku_print_log) %||% FALSE
              )
            ),
            column(6) # placeholder
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
        cols <- names(df)
        ui_list <- tagList(
          # Row 1: grouping columns
          fluidRow(
            column(
              6,
              selectInput(
                "splsda_group_col",
                label = helper(
                  type = "inline",
                  title = "sPLS-DA Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>sPLS-DA Comparison Column</span>"
                  ),
                  content = "Select the column containing the class labels or groups to discriminate between using sPLS-DA.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = isolate(userState$splsda_group_col) %||% cols[1]
              )
            ),
            column(
              6,
              selectInput(
                "splsda_group_col2",
                label = helper(
                  type = "inline",
                  title = "sPLS-DA Stratification Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>sPLS-DA Stratification Column</span>"
                  ),
                  content = "Optional: Select a second column for stratifying the sPLS-DA analysis. This can be used to further differentiate groups within the primary grouping column. If not needed, select the same column as in the comparison column.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = isolate(userState$splsda_group_col2) %||% cols[1]
              )
            )
          ),

          # batch_col column based on a conditional check to do batch correction similar to multilevel
          fluidRow(
            column(
              6,
              checkboxInput(
                "splsda_use_batch_corr",
                label = helper(
                  type = "inline",
                  title = "Perform Batch Correction?",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Perform Batch Correction?</span>"
                  ),
                  content = "Performing batch correction using z-score normalization. This is useful when your data has multiple 
                  batches or experimental runs that need to be corrected for.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$splsda_use_batch_corr) %||% FALSE
              )
            ),
            column(
              6,
              conditionalPanel(
                condition = "input.splsda_use_batch_corr == true",
                selectInput(
                  "splsda_batch_col",
                  label = helper(
                    type = "inline",
                    title = "Multilevel Column",
                    icon = "fas fa-question-circle",
                    shiny_tag = HTML(
                      "<span style='margin-right: 15px;'>Select Batch Column</span>"
                    ),
                    content = "Select the column that identifies batch categories.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = cols,
                  selected = isolate(userState$splsda_batch_col) %||% cols[1]
                )
              )
            )
          ),
          # Row 2: multilevel toggle + selector
          fluidRow(
            column(
              6,
              checkboxInput(
                "splsda_use_multilevel",
                label = helper(
                  type = "inline",
                  title = "Perform Multilevel Analysis?",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Perform Multilevel Analysis?</span>"
                  ),
                  content = "Check if your data has repeated measures and you want to account for that variation.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$splsda_use_multilevel) %||% FALSE
              )
            ),
            column(
              6,
              conditionalPanel(
                condition = "input.splsda_use_multilevel == true",
                selectInput(
                  "splsda_multilevel",
                  label = helper(
                    type = "inline",
                    title = "Multilevel Column",
                    icon = "fas fa-question-circle",
                    shiny_tag = HTML(
                      "<span style='margin-right: 15px;'>Select Repeated Measures Column</span>"
                    ),
                    content = "Select the column that identifies repeated measurements (e.g. Patient ID).",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = cols,
                  selected = isolate(userState$splsda_multilevel) %||% cols[1]
                )
              )
            )
          ),

          # Row 3: sparsity & colors
          fluidRow(
            column(
              6,
              numericInput(
                "splsda_var_num",
                label = helper(
                  type = "inline",
                  title = "Number of Variables",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Number of Variables</span>"
                  ),
                  content = "Specify the number of variables to select/retain on each component.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$splsda_var_num),
                min = 1
              )
            ),
            column(
              6,
              selectizeInput(
                "splsda_colors",
                label = helper(
                  type = "inline",
                  title = "Select Colors (Optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Select Colors for sPLS-DA Plot (Optional)</span>"
                  ),
                  content = "The color palette to use for the sPLS-DA plot. Match the number of colors to the number of categories in the comparison column.",
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
          fluidRow(
            column(
              6,
              selectInput(
                "splsda_cv_opt",
                label = helper(
                  type = "inline",
                  title = "Cross-Validation Option",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Cross-Validation Option</span>"
                  ),
                  content = "Choose None, LOOCV, or Mfold for model evaluation stability.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("None", "LOOCV", "Mfold"),
                selected = isolate(userState$splsda_cv_opt) %||% "None"
              )
            ),
            column(
              6,
              conditionalPanel(
                condition = "input.splsda_cv_opt == 'Mfold'",
                numericInput(
                  "splsda_fold_num",
                  label = helper(
                    type = "inline",
                    title = "Number of Folds",
                    icon = "fas fa-question-circle",
                    shiny_tag = HTML(
                      "<span style='margin-right: 15px;'>Number of Folds</span>"
                    ),
                    content = "The number of folds for M-Fold cross-validation.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = isolate(userState$splsda_fold_num) %||% 5,
                  min = 2
                )
              )
            )
          ),

          # Row 5: log2 & components
          fluidRow(
            column(
              6,
              numericInput(
                "splsda_comp_num",
                label = helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = "Specify how many latent variables to compute in the model.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$splsda_comp_num) %||% 2,
                min = 2
              )
            ),
            column(
              6,
              radioButtons(
                "splsda_ind_names_mode",
                label = helper(
                  type = "inline",
                  title = "Plot Individual Names?",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Plot Individual Names?</span>"
                  ),
                  content = "Choose Off (shapes only), Row names (use rownames), or a Column (choose a column to label points).",
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
                selected = isolate(userState$splsda_ind_names_mode) %||% "off",
                inline = TRUE
              )
            ),
            column(
              6,
              conditionalPanel(
                condition = "input.splsda_ind_names_mode === 'column'",
                selectInput(
                  "splsda_ind_names_col",
                  label = helper(
                    type = "inline",
                    title = "Label Column",
                    icon = "fas fa-question-circle",
                    shiny_tag = HTML(
                      "<span style='margin-right: 15px;'>Label Column</span>"
                    ),
                    content = "Column to use for individual labels.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  choices = cols,
                  selected = isolate(userState$splsda_ind_names_col) %||% NULL
                )
              )
            )
          ),
          # Row 6: symbols & plot style
          fluidRow(
            column(
              6,
              selectizeInput(
                "splsda_pch",
                label = helper(
                  type = "inline",
                  title = "Plotting Symbols",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Plotting Symbols</span>"
                  ),
                  content = "Select PCH symbols for data points. Match to your grouping columns.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = pch_choices,
                selected = isolate(
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
            column(
              6,
              selectInput(
                "splsda_style",
                label = helper(
                  type = "inline",
                  title = "Plot Style",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Plot Style</span>"
                  ),
                  content = "Choose 2D or 3D style for the sPLS-DA plot (3D needs ≥3 components).",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("2D", "3D"),
                selected = isolate(userState$splsda_style) %||% "2D"
              )
            )
          ),

          # Row 7: ROC & ellipse
          fluidRow(
            column(
              6,
              checkboxInput(
                "splsda_roc",
                label = helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Plot ROC</span>"
                  ),
                  content = "Plot ROC curves to evaluate classification performance.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$splsda_roc) %||% FALSE
              )
            ),
            column(
              6,
              checkboxInput(
                "splsda_ellipse",
                label = helper(
                  type = "inline",
                  title = "Draw Ellipse",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Draw Ellipse</span>"
                  ),
                  content = "Draw a 95% ellipse around the data points on the sPLS-DA plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$splsda_ellipse) %||% FALSE
              )
            )
          ),

          # Row 8: background & confusion matrix
          fluidRow(
            column(
              6,
              checkboxInput(
                "splsda_bg",
                label = helper(
                  type = "inline",
                  title = "Shaded Background",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Shaded Background Prediction</span>"
                  ),
                  content = "Draw shaded prediction regions on the sPLS-DA plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$splsda_bg) %||% FALSE
              )
            ),
            column(
              6,
              checkboxInput(
                "splsda_conf_mat",
                label = helper(
                  type = "inline",
                  title = "Confusion Matrix",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Confusion Matrix</span>"
                  ),
                  content = "Display the confusion matrix for the sPLS-DA model. This helps evaluate classification accuracy by showing true vs predicted values. Additional metrics are also provided such as sensitivity and specificity.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$splsda_conf_mat) %||% FALSE
              )
            )
          )
        )
      },
      "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- names(df)

        default_num_vars <- sum(sapply(df, is.numeric))
        if (!isTRUE(userState$mint_splsda_var_num_manual)) {
          userState$mint_splsda_var_num <- default_num_vars
        }

        ui_list <- tagList(
          # Row 1: Grouping Columns
          fluidRow(
            column(
              6,
              selectInput(
                "mint_splsda_group_col",
                label = helper(
                  type = "inline",
                  title = "MINT sPLS-DA Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>MINT sPLS-DA Comparison Column</span>"
                  ),
                  content = "Select the column containing the class labels you want to discriminate.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = isolate(userState$mint_splsda_group_col) %||% cols[1]
              )
            ),
            column(
              6,
              selectInput(
                "mint_splsda_group_col2",
                label = helper(
                  type = "inline",
                  title = "MINT sPLS-DA Stratification Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>MINT sPLS-DA Stratification Column</span>"
                  ),
                  content = "Optional. If specified, the MINT sPLS-DA analysis will be run separately for each level of this column. If not needed, select the same column as in the comparison column.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("None" = NULL, cols),
                selected = isolate(userState$mint_splsda_group_col2) %||% NULL
              )
            )
          ),
          # Row 2: Batch and Variable Number
          fluidRow(
            column(
              6,
              selectInput(
                "mint_splsda_batch_col",
                label = helper(
                  type = "inline",
                  title = "Batch Column (Study)",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Batch Column</span>"
                  ),
                  content = "Select the column that identifies the different batches or studies. This is crucial for the batch correction model.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = isolate(userState$mint_splsda_batch_col) %||% cols[2]
              )
            ),
            column(
              6,
              numericInput(
                "mint_splsda_var_num",
                label = helper(
                  type = "inline",
                  title = "Number of Variables",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Number of Variables to Select</span>"
                  ),
                  content = "Specify the number of variables (e.g., cytokines) to select on each component.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$mint_splsda_var_num),
                min = 1
              )
            )
          ),
          # Row 3: Components and Colors
          fluidRow(
            column(
              6,
              numericInput(
                "mint_splsda_comp_num",
                label = helper(
                  type = "inline",
                  title = "Number of Components",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Number of Components</span>"
                  ),
                  content = "Specify how many latent variables (components) to compute in the model.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$mint_splsda_comp_num) %||% 2,
                min = 2
              )
            ),
            column(
              6,
              selectizeInput(
                "mint_splsda_colors",
                label = helper(
                  type = "inline",
                  title = "Select Colors (Optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Select Colors for Groups</span>"
                  ),
                  content = "The color palette to use for the different groups in the plots. Match the number of colors to the number of categories in the comparison column.",
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
          fluidRow(
            column(
              6,
              checkboxInput(
                "mint_splsda_cim",
                label = helper(
                  type = "inline",
                  title = "Draw a Clustered Image Map (CIM)?",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Draw a Clustered Image Map?</span>"
                  ),
                  content = "Draw a Clustered Image Map (CIM) to visualize the data. This provides a heatmap-like view of the data with clustering.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$mint_splsda_cim) %||% FALSE
              )
            ),
            column(
              6,
              checkboxInput(
                "mint_splsda_ellipse",
                label = helper(
                  type = "inline",
                  title = "Draw Ellipse",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Draw Ellipse</span>"
                  ),
                  content = "Draw a 95% confidence ellipse around the groups on the sample plots.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$mint_splsda_ellipse) %||% FALSE
              )
            )
          ),
          # Row 5: Final Toggles
          fluidRow(
            column(
              6,
              checkboxInput(
                "mint_splsda_roc",
                label = helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Plot ROC Curve</span>"
                  ),
                  content = "Plot ROC curves to evaluate classification performance.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$mint_splsda_roc) %||% FALSE
              )
            ),
            column(
              6,
              checkboxInput(
                "mint_splsda_bg",
                label = helper(
                  type = "inline",
                  title = "Draw Prediction Background",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Draw Background</span>"
                  ),
                  content = "Draws a shaded background to visualize the prediction areas on the global sample plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$mint_splsda_bg) %||% FALSE
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
        cols <- names(df)

        ui_list <- tagList(
          fluidRow(
            column(
              6,
              selectInput(
                "volc_group_col",
                label = helper(
                  type = "inline",
                  title = "Comparison Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Comparison Column</span>"
                  ),
                  content = "The column that contains the comparison groups for the volcano plot. This should be a categorical column.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = isolate(userState$volc_group_col) %||% cols[1]
              )
            ),
            column(
              6,
              uiOutput(
                "volc_conditions_ui"
              )
            ),
            column(
              6,
              numericInput(
                "volc_fold_change_thresh",
                label = helper(
                  type = "inline",
                  title = "Log2 FC Threshold",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Log2 Fold Change Threshold</span>"
                  ),
                  content = "Absolute log2 fold-change cutoff for significance.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$volc_fold_change_thresh) %||% 1
              )
            )
          ),
          fluidRow(
            column(
              6,
              numericInput(
                "volc_p_value_thresh",
                label = helper(
                  type = "inline",
                  title = "P-Value Threshold",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>P-Value Threshold</span>"
                  ),
                  content = "Cutoff for adjusted p-value for significance.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$volc_p_value_thresh) %||% 0.05
              )
            ),
            column(
              6,
              numericInput(
                "volc_top_labels",
                label = helper(
                  type = "inline",
                  title = "Top Labels",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Top Labels</span>"
                  ),
                  content = "Number of top genes/features to annotate on the volcano plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$volc_top_labels) %||% 15
              )
            )
          )
        )
      },
      # ——————————————————————————————
      # Extreme Gradient Boosting (XGBoost)
      # ——————————————————————————————
      "Extreme Gradient Boosting (XGBoost)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- names(df)

        ui_list <- tagList(
          # Row 1: grouping & train frac
          fluidRow(
            column(
              6,
              selectInput(
                "xgb_group_col",
                label = helper(
                  type = "inline",
                  title = "Grouping Column",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Grouping Column</span>"
                  ),
                  content = "Select the column that contains the outcome or class labels you want the XGBoost model to predict.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cols,
                selected = isolate(userState$xgb_group_col) %||% cols[1]
              )
            ),
            column(
              6,
              numericInput(
                "xgb_train_fraction",
                label = helper(
                  type = "inline",
                  title = "Train Fraction",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Train Fraction</span>"
                  ),
                  content = "The fraction of the data to use for training the XGBoost model.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$xgb_train_fraction) %||% 0.7,
                min = 0.1,
                max = 0.9,
                step = 0.1
              )
            )
          ),
          # Row 2: rounds & max_depth
          fluidRow(
            column(
              6,
              numericInput(
                "xgb_nrounds",
                label = helper(
                  type = "inline",
                  title = "Number of Rounds",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Number of Rounds</span>"
                  ),
                  content = "The number of rounds (trees) to grow in the XGBoost model.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$xgb_nrounds) %||% 500,
                min = 100
              )
            ),
            column(
              6,
              numericInput(
                "xgb_max_depth",
                label = helper(
                  type = "inline",
                  title = "Maximum Depth",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Maximum Depth</span>"
                  ),
                  content = "The maximum depth of the trees in the XGBoost model.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$xgb_max_depth) %||% 6,
                min = 1
              )
            )
          ),
          # Row 3: eta & eval metric
          fluidRow(
            column(
              6,
              numericInput(
                "xgb_eta",
                label = helper(
                  type = "inline",
                  title = "Learning Rate",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Learning Rate</span>"
                  ),
                  content = "The learning rate of the XGBoost model (step size shrinkage).",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$xgb_eta) %||% 0.1,
                min = 0,
                step = 0.01
              )
            ),
            column(
              6,
              selectInput(
                "xgb_eval_metric",
                label = helper(
                  type = "inline",
                  title = "Evaluation Metric",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Evaluation Metric</span>"
                  ),
                  content = "Choose 'mlogloss' or 'auc' to evaluate model performance.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = c("mlogloss", "auc"),
                selected = isolate(userState$xgb_eval_metric) %||% "mlogloss"
              )
            )
          ),
          # Row 4: top features & ROC plot
          fluidRow(
            column(
              6,
              numericInput(
                "xgb_top_n_features",
                label = helper(
                  type = "inline",
                  title = "Top Number of Features",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Top Number of Features</span>"
                  ),
                  content = "Number of top features (by importance) to display.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$xgb_top_n_features) %||% 10,
                min = 1
              )
            ),
            column(
              6,
              checkboxInput(
                "xgb_plot_roc",
                label = helper(
                  type = "inline",
                  title = "Plot ROC",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Plot ROC (Binary Only)</span>"
                  ),
                  content = "Plot the ROC curve for the XGBoost model (binary outcome).",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$xgb_plot_roc) %||% FALSE
              )
            )
          ),
          # Row 5: CV toggle & folds (conditional)
          fluidRow(
            column(
              6,
              checkboxInput(
                "xgb_cv",
                label = helper(
                  type = "inline",
                  title = "Cross-Validation",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Cross-Validation</span>"
                  ),
                  content = "Perform cross-validation on the XGBoost model.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$xgb_cv) %||% FALSE
              )
            ),
            column(
              6,
              conditionalPanel(
                condition = "input.xgb_cv == true",
                numericInput(
                  "xgb_nfold",
                  label = helper(
                    type = "inline",
                    title = "Number of Folds",
                    icon = "fas fa-question-circle",
                    shiny_tag = HTML(
                      "<span style='margin-right:15px;'>Number of Folds</span>"
                    ),
                    content = "The number of folds to use for cross-validation.",
                    colour = if (
                      input$theme_choice %in% c("darkly", "cyborg")
                    ) {
                      "red"
                    } else {
                      "blue"
                    }
                  ),
                  value = isolate(userState$xgb_nfold) %||% 5,
                  min = 2
                )
              )
            )
          )
        )
      }
    )
    do.call(tagList, ui_list)
  })

  # 2) Auto‑sync in Step 3/4 based purely on what was checked in Step 2
  shiny::observe({
    # Grab exactly what the user checked in Step 2
    cols_checked <- input$selected_numerical_cols %||% character(0)
    default_num_vars <- length(cols_checked)

    # If they haven't manually typed a value, update it
    if (!isTRUE(userState$splsda_var_num_manual)) {
      updateNumericInput(
        session,
        "splsda_var_num",
        value = default_num_vars
      )
    }
    if (!isTRUE(userState$mint_splsda_var_num_manual)) {
      updateNumericInput(
        session,
        "mint_splsda_var_num",
        value = default_num_vars
      )
    }
    if (!isTRUE(userState$plsr_keepX_manual)) {
      updateNumericInput(
        session,
        "plsr_keepX",
        value = default_num_vars
      )
    }
  })

  # 3) As soon as they type their own number, stop auto‑syncing
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
    req(filteredData())
    req(input$df_group_var)
    df <- filteredData()
    choices <- unique(df[[input$df_group_var]])
    if (length(choices) < 2) {
      return(helpText("Not enough unique levels in grouping column"))
    }

    # Use fluidRow and column to place inputs side-by-side
    fluidRow(
      column(
        12,
        selectInput(
          "df_cond1",
          label = helper(
            type = "inline",
            title = "Condition 1",
            icon = "fas fa-question-circle",
            shiny_tag = HTML(
              "<span style='margin-right: 15px;'>Condition 1</span>"
            ),
            content = "The first condition to compare.",
            colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
              "red"
            } else {
              "blue"
            }
          ),
          choices = choices,
          selected = isolate(userState$df_cond1) %||% choices[1]
        ),
        selectInput(
          "df_cond2",
          label = helper(
            type = "inline",
            title = "Condition 2",
            icon = "fas fa-question-circle",
            shiny_tag = HTML(
              "<span style='margin-right: 15px;'>Condition 2</span>"
            ),
            content = "The second condition to compare.",
            colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
              "red"
            } else {
              "blue"
            }
          ),
          choices = choices,
          selected = isolate(userState$df_cond2) %||% choices[2]
        )
      )
    )
  })

  output$volc_conditions_ui <- shiny::renderUI({
    req(filteredData())
    req(input$volc_group_col)
    df <- filteredData()
    choices <- unique(df[[input$volc_group_col]])
    if (length(choices) >= 2) {
      # Use fluidRow and column to place inputs side-by-side
      fluidRow(
        column(
          12,
          selectInput(
            "volc_cond1",
            label = helper(
              type = "inline",
              title = "Condition 1",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Condition 1</span>"
              ),
              content = "The first condition to compare.",
              colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                "red"
              } else {
                "blue"
              }
            ),
            choices = choices,
            selected = isolate(userState$volc_cond1) %||% choices[1]
          ),
          selectInput(
            "volc_cond2",
            label = helper(
              type = "inline",
              title = "Condition 2",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Condition 2</span>"
              ),
              content = "The second condition to compare.",
              colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                "red"
              } else {
                "blue"
              }
            ),
            choices = choices,
            selected = isolate(userState$volc_cond2) %||% choices[2]
          )
        )
      )
    } else {
      helpText(
        "Selected comparison variable does not have at least two unique values."
      )
    }
  })

  ## ---------------------------
  ## Sidebar Navigation
  ## ---------------------------
  # 1) A new reactiveVal to track which “page” we’re on
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

  shiny::observeEvent(input$nav_start, {
    currentPage("step1")
    toggle("nav_submenu")
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
      show("nav_submenu")
    } else {
      hide("nav_submenu")
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

    # Built=in Data built‑in tracking:
    userState$use_builtin = FALSE
    userState$built_in_choice = NULL

    # Step 2 log2 checkbox
    userState$step2_log2 = FALSE

    # Boxplots options
    userState$bp_bin_size = NULL
    userState$bp_mf_row = NULL
    userState$bp_y_lim = NULL

    # Enhanced Boxplots options
    userState$bp2_mf_row = NULL
    userState$bp2_y_lim = NULL

    # Error-Bar Plot
    userState$eb_group_col = NULL
    userState$eb_p_lab = NULL
    userState$eb_es_lab = NULL
    userState$eb_class_symbol = NULL
    userState$eb_x_lab = NULL
    userState$eb_y_lab = NULL
    userState$eb_title = NULL

    # Dual-Flashlight Plot options
    userState$df_group_var = NULL
    userState$df_cond1 = NULL
    userState$df_cond2 = NULL
    userState$df_ssmd_thresh = NULL
    userState$df_log2fc_thresh = NULL
    userState$df_top_labels = NULL

    # Heatmap options
    userState$hm_annotation = NULL

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
    userState$spsda_batch_col = NULL
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
    userState$plsda_colors = NULL
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
  }
  shiny::observeEvent(input$new_fresh, {
    resetState()
    currentPage("step1")
    currentStep(1)
  })

  shiny::observeEvent(input$new_reuse, {
    isolate({
      # Boxplots options
      userState$bp_bin_size = NULL
      userState$bp_mf_row = NULL
      userState$bp_y_lim = NULL

      # Enhanced Boxplots options
      userState$bp2_mf_row = NULL
      userState$bp2_y_lim = NULL

      # Error-Bar Plot
      userState$eb_group_col = NULL
      userState$eb_p_lab = NULL
      userState$eb_es_lab = NULL
      userState$eb_class_symbol = NULL
      userState$eb_x_lab = NULL
      userState$eb_y_lab = NULL
      userState$eb_title = NULL

      # Dual-Flashlight Plot options
      userState$df_group_var = NULL
      userState$df_cond1 = NULL
      userState$df_cond2 = NULL
      userState$df_ssmd_thresh = NULL
      userState$df_log2fc_thresh = NULL
      userState$df_top_labels = NULL

      # Heatmap options
      userState$hm_annotation = NULL

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
      userState$plsr_comp_num = NULL
      userState$plsr_keepX = NULL
      userState$plsr_keepX_manual = FALSE
      userState$sparse = FALSE
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
      userState$spsda_batch_col = NULL
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
      userState$plsda_colors = NULL
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
    })
    currentPage("step2")
    currentStep(2)
  })

  output$page_content <- shiny::renderUI({
    switch(
      currentPage(),
      "home" = homeUI(), # Make a helper that shows big header & “Let’s get started!”
      "tutorials" = tutorialUI(), # Simple link out to docs
      "step1" = step1UI(), # existing upload‐data card + Next button
      "step2" = step2UI(), # existing select‐cols/filters + Next/Back
      "step3" = step3UI(), # existing analysis‐options grid + Back
      "step4" = step4UI(), # existing function‐args form + Run/Back
      "step5" = resultsUI(), # existing results page
      "news" = newsUI(), # Simple news page
      "contact" = contactUI(), # Simple contact page
      homeUI() # Fallback
    )
  })
  totalPages <- 5
  stepHeader <- function(step) {
    pct <- round((step - 1) / (totalPages - 1) * 100)
    shiny::tagList(
      div(
        class = "step-title",
        h3(switch(
          as.character(step),
          "1" = "Step 1: Upload Data",
          "2" = "Step 2: Select Columns & Apply Filters",
          "3" = "Step 3: Analysis Choices",
          "4" = paste0("Step 4: Options for ", selected_function()),
          "5" = "Analysis Results"
        ))
      ),
      div(
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
      h1("Welcome to CytokineProfile", style = "font-weight:300;"),
      p(HTML(paste0(
        "CytokineProfile is an R Shiny Application based on the CytoProfile R package available at ",
        "<a href='https://cran.r-project.org/package=CytoProfile'>CRAN</a>. ",
        "This application is designed for advanced cytokine data analysis. ",
        "It provides a comprehensive suite of functions for exploratory, univariate, ",
        "and multivariate analysis as well as machine learning methods tailored to your data."
      ))),
      tags$h3("Features we offer:", style = "margin-top:2rem;"),
      fluidRow(
        column(
          width = 3,
          div(
            class = "card h-100",
            div(
              class = "card-header bg-primary text-white",
              "Univariate Analysis"
            ),
            div(
              class = "card-body",
              tags$ul(
                tags$li("ANOVA"),
                tags$li("Two-Sample T-Test")
              )
            )
          )
        ),
        column(
          width = 3,
          div(
            class = "card h-100",
            div(
              class = "card-header bg-primary text-white",
              "Exploratory Analysis"
            ),
            div(
              class = "card-body",
              tags$ul(
                tags$li("Boxplots"),
                tags$li("Enhanced Boxplots"),
                tags$li("Correlation Plots"),
                tags$li("Error-Bar Plot"),
                tags$li("Dual-Flashlight Plot"),
                tags$li("Heatmap"),
                tags$li("Skewness/Kurtosis Plots"),
                tags$li("Volcano Plot")
              )
            )
          )
        ),
        column(
          width = 3,
          div(
            class = "card h-100",
            div(
              class = "card-header bg-primary text-white",
              "Multivariate Analysis"
            ),
            div(
              class = "card-body",
              tags$ul(
                tags$li("Principal Component Analysis (PCA)"),
                tags$li("Partial Least Squares Regression (PLSR)"),
                tags$li(
                  "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
                ),
                tags$li(
                  "Multivariate INTegrative Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
                )
              )
            )
          )
        ),
        column(
          width = 3,
          div(
            class = "card h-100",
            div(
              class = "card-header bg-primary text-white",
              "Machine Learning Methods"
            ),
            div(
              class = "card-body",
              tags$ul(
                tags$li("Random Forest"),
                tags$li("Extreme Gradient Boosting (XGBoost)")
              )
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 12,
          align = "center",
          actionButton(
            "nav_start",
            "Let's get started!",
            icon = icon("arrow-right"),
            class = "btn-primary btn-lg"
          )
        )
      )
    )
  }

  tutorialUI <- function() {
    shiny::tagList(
      includeMarkdown("TUTORIALS.md")
    )
  }
  newsUI <- function() {
    shiny::tagList(
      h1("News and Updates"),
      includeMarkdown("NEWS.md")
    )
  }
  contactUI <- function() {
    shiny::fluidPage(
      h1("About Us"),
      fluidRow(
        ## —— Column 1 —— ##
        column(
          width = 6,
          h2("Shubh Saraswat"),
          p(em("Creator and Author of CytokineProfile")),
          p("Biomedical Data Scientist"),
          p("PhD Student in Epidemiology & Biostatistics"),
          p("University of Kentucky"),
          tags$a(
            href = "mailto:shubh.saraswat@uky.edu",
            class = "btn btn-primary me-2",
            icon("envelope"),
            "Email"
          ),
          tags$a(
            href = "https://www.linkedin.com/in/ssaraswat22",
            class = "btn btn-info me-2",
            icon("linkedin"),
            "LinkedIn"
          ),
          tags$a(
            href = "https://github.com/saraswatsh",
            class = "btn btn-dark me-2",
            icon("github"),
            "GitHub"
          ),
          tags$a(
            href = "https://orcid.org/0009-0009-2359-1484",
            class = "btn btn-link",
            icon("orcid"),
            "ORCID"
          )
        ),

        ## —— Column 2 —— ##
        column(
          width = 6,
          h2("Xiaohua Douglas Zhang"),
          p(em("Author of CytokineProfile")),
          p("Professor, Department of Biostatistics"),
          p("University of Kentucky"),
          tags$a(
            href = "mailto:xiaohua.zhang@uky.edu",
            class = "btn btn-primary me-2",
            icon("envelope"),
            "Email"
          ),
          tags$a(
            href = "https://orcid.org/0000-0002-2486-7931",
            class = "btn btn-link",
            icon("orcid"),
            "ORCID"
          )
        )
      ),
      fluidRow(
        ## —— Column 3 —— ##
        column(
          width = 6,
          h2("Bira Arumndari Nurrahma"),
          p(em("Author of CytokineProfile")),
          p("PhD Student in Nutritional Sciences"),
          p("University of Kentucky"),
          tags$a(
            href = "mailto:biraarum@uky.edu",
            class = "btn btn-primary me-2",
            icon("envelope"),
            "Email"
          )
        )
      ),
      # Add a note about who to contact for application issues
      column(
        width = 12,
        br(),
        p(
          "For issues related to the application, submit an issue at the ",
          tags$a(
            href = "https://github.com/saraswatsh/CytokineProfileShinyApp/issues",
            "GitHub repository."
          ),
          "For additional questions or concerns, contact the creator Shubh Saraswat with the provided email above."
        )
      )
    )
  }

  step1UI <- function() {
    shiny::tagList(
      stepHeader(currentStep()),

      fluidRow(
        # --- Left Column: Upload Controls ---
        column(
          width = 5,
          card(
            card_header(h4(icon("upload"), "Step 1: Provide Your Data")),
            card_body(
              tags$h5("Option A: Upload a File"),
              fileInput(
                "datafile",
                label = NULL,
                accept = c(".csv", ".txt", ".xls", ".xlsx")
              ),
              helpText("Accepted Formats: '.csv', '.txt', '.xls', '.xlsx'"),
              uiOutput("sheet_selector"),
              hr(),
              tags$h5("Option B: Use Built-in Data"),
              checkboxInput(
                "use_builtin",
                "Use a built-in dataset?",
                value = isolate(userState$use_builtin) %||% FALSE
              ),
              uiOutput("built_in_selector"),
              hr(),
              uiOutput("viewSummaryCheckboxes")
            ),
            card_footer(
              div(
                style = "text-align: right;",
                actionButton(
                  "next1",
                  "Next Step",
                  icon = icon("arrow-right"),
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # --- Right Column: Data Preview & Summary ---
        column(
          width = 7,
          conditionalPanel(
            condition = "output.data_is_loaded == true",
            navset_card_tab(
              id = "step1_data_tabs",
              nav_panel(
                "Data Preview",
                conditionalPanel(
                  condition = "input.view_data",
                  uiOutput("data_summary"),
                  shinycssloaders::withSpinner(uiOutput("preview_ui"), type = 8)
                ),
                conditionalPanel(
                  condition = "!input.view_data",
                  p(
                    style = "padding: 1rem;",
                    "Check 'View Data Loaded?' to see a preview of your data here."
                  )
                )
              ),
              nav_panel(
                "Summary Statistics",
                conditionalPanel(
                  condition = "input.show_summary",
                  shinycssloaders::withSpinner(
                    DT::DTOutput("summary_stats_table"),
                    type = 8
                  )
                ),
                conditionalPanel(
                  condition = "!input.show_summary",
                  p(
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
      df <- userData()
      all_cols <- names(df)[names(df) != "..cyto_id.."]
      is_numeric_col <- sapply(df[all_cols], is.numeric)
      numeric_cols <- all_cols[is_numeric_col]
      categorical_cols <- all_cols[!is_numeric_col]

      shiny::tagList(
        stepHeader(currentStep()),
        fluidRow(
          # -- Left Column: Selections & Filters --
          column(
            width = 5,
            # 1) Categorical selector
            card(
              card_header(class = "bg-info", "1. Select Categorical Columns"),
              card_body(
                div(
                  style = "margin-bottom: 10px;",
                  actionButton(
                    "select_all_cat",
                    "Select All",
                    class = "btn-sm"
                  ),
                  actionButton(
                    "deselect_all_cat",
                    "Deselect All",
                    class = "btn-sm"
                  )
                ),
                div(
                  class = "scrollable-checkbox-group",
                  checkboxGroupInput(
                    inputId = "selected_categorical_cols",
                    label = NULL,
                    choices = categorical_cols,
                    selected = {
                      init_cat <- intersect(
                        userState$selected_columns,
                        categorical_cols
                      )
                      if (length(init_cat) == 0) {
                        categorical_cols
                      } else {
                        init_cat
                      }
                    },
                    inline = TRUE
                  )
                )
              )
            ),

            # 2) Numerical selector
            card(
              card_header(class = "bg-info", "2. Select Numerical Columns"),
              card_body(
                div(
                  style = "margin-bottom: 6px;",
                  actionButton(
                    "select_all_num",
                    "Select All",
                    class = "btn-sm"
                  ),
                  actionButton(
                    "deselect_all_num",
                    "Deselect All",
                    class = "btn-sm"
                  )
                ),
                div(
                  class = "scrollable-checkbox-group",
                  checkboxGroupInput(
                    inputId = "selected_numerical_cols",
                    label = NULL,
                    choices = numeric_cols,
                    selected = {
                      init_num <- intersect(
                        userState$selected_columns,
                        numeric_cols
                      )
                      if (length(init_num) == 0) numeric_cols else init_num
                    },
                    inline = TRUE
                  )
                )
              )
            ),
            # 3) Optional: Log₂ transformation
            card(
              card_header(
                class = "bg-info",
                "3. Optional: Log₂ Transformation"
              ),
              card_body(
                checkboxInput(
                  "step2_log2",
                  label = "Apply log₂ transformation to all selected numerical columns",
                  value = isolate(userState$step2_log2) %||% FALSE
                )
              )
            ),

            # 4) Conditional filters UI
            uiOutput("conditional_filter_ui")
          ),

          # -- Right Column: Data Preview & Deletion --
          column(
            width = 7,
            card(
              style = "height: 40vh; display: flex; flex-direction: column;",
              card_header(h4(icon("table"), "Filtered Data Explorer")),
              card_body(
                style = "flex: 1 1 auto; overflow-y: auto; padding: 1rem;",
                div(
                  style = "max-height: 40vh; overflow-y: auto; padding: 1rem;",
                  DT::DTOutput("filtered_data_preview")
                )
              ),
              card_footer(
                style = "
                           display: flex;
                          justify-content: center;
                          padding: 0.75rem;
                          border-top: 1px solid #444;
                           background: inherit;
                        ",
                actionButton(
                  "delete_selected_rows",
                  "Delete Selected",
                  icon = icon("trash"),
                  class = "btn-danger me-2"
                ),
                actionButton(
                  "restore_selected_rows",
                  "Restore Selected",
                  icon = icon("undo"),
                  class = "btn-secondary me-2"
                ),
                actionButton(
                  "restore_all_rows",
                  "Restore All",
                  icon = icon("undo"),
                  class = "btn-secondary"
                )
              )
            ),

            # wrap deleted‐samples in a card too
            card(
              style = "display: flex; flex-direction: column;",
              card_header(h4(icon("table"), "Deleted Samples")),
              card_body(
                style = "flex: 1 1 auto; overflow-y: auto; padding: 1rem;",
                div(
                  style = "max-height: 40vh; overflow-y: auto; padding: 1rem;",
                  DT::DTOutput("deleted_data_preview")
                )
              )
            )
          )
        ),
        # -- Navigation --
        br(),
        fluidRow(
          column(
            12,
            div(
              style = "margin-top:0.5rem;",
              actionButton("back2", "Back", icon = icon("arrow-left")),
              conditionalPanel(
                "input.step2_log2 == true",
                actionButton(
                  "preview_transform",
                  "Preview Transformation",
                  icon = icon("magnifying-glass"),
                  class = "btn-secondary"
                )
              ),
              actionButton(
                "next2",
                "Next",
                icon = icon("arrow-right"),
                class = "btn-primary"
              ),
              actionButton(
                "open_impute_modal",
                "Treat missing values",
                icon = icon("fas fa-eraser"),
                class = "btn-secondary"
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
      fluidRow(
        # Statistical Tests
        column(
          width = 3,
          card(
            card_header("Univariate Analysis", class = "bg-info"),
            card_body(
              actionButton("menu_ANOVA", "ANOVA", class = "menu-card"),
              actionButton(
                "menu_t_test",
                "Two-Sample T-Test",
                class = "menu-card"
              ),
            )
          )
        ),
        # Exploratory Analysis
        column(
          width = 3,
          card(
            card_header("Exploratory Analysis", class = "bg-info"),
            card_body(
              actionButton("menu_boxplots", "Boxplots", class = "menu-card"),
              actionButton(
                "menu_enhanced_boxplots",
                "Enhanced Boxplots",
                class = "menu-card"
              ),
              actionButton(
                "menu_correlation",
                "Correlation Plots",
                class = "menu-card"
              ),
              actionButton(
                "menu_skewkurt",
                "Skewness/Kurtosis",
                class = "menu-card"
              ),
              actionButton(
                "menu_errorbp",
                "Error-Bar Plot",
                class = "menu-card"
              ),
              actionButton(
                "menu_dualflash",
                "Dual-Flashlight Plot",
                class = "menu-card"
              ),
              actionButton("menu_heatmap", "Heatmap", class = "menu-card"),
              actionButton(
                "menu_volcano",
                "Volcano Plot",
                class = "menu-card"
              )
            )
          )
        ),
        # Multivariate
        column(
          width = 3,
          card(
            card_header("Multivariate Analysis", class = "bg-info"),
            card_body(
              actionButton(
                "menu_PCA",
                "Principal Component Analysis (PCA)",
                class = "menu-card"
              ),
              actionButton(
                "menu_PLSR",
                "Partial Least Squares Regression (PLSR)",
                class = "menu-card"
              ),
              actionButton(
                "menu_splsda",
                "Sparse Partial Least Squares - Discriminant Analysis (sPLS‑DA)",
                class = "menu-card"
              ),
              actionButton(
                "menu_mint_splsda",
                "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS‑DA)",
                class = "menu-card"
              )
            )
          )
        ),
        # Machine Learning
        column(
          width = 3,
          card(
            card_header("Machine Learning Methods", class = "bg-info"),
            card_body(
              actionButton("menu_rf", "Random Forest", class = "menu-card"),
              actionButton(
                "menu_xgb",
                "Extreme Gradient Boosting (XGBoost)",
                class = "menu-card"
              )
            )
          )
        ),

        # # Back/Next buttons for the wizard
        fluidRow(
          column(
            12,
            div(
              style = "margin-top: 1rem; display: flex; justify-content: flex-start;",
              actionButton(
                "back3",
                "Back",
                icon = icon("arrow-left"),
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
      uiOutput("function_options_ui"),
      div(
        style = "display: flex; justify-content: space-between; margin-top: 1rem;",
        actionButton(
          "back4",
          "Back",
          icon = icon("arrow-left"),
          class = "btn-secondary"
        ),
        actionButton(
          "next4",
          "Run Analysis",
          icon = icon("play"),
          class = "btn-success"
        )
      )
    )
  }

  resultsUI <- function() {
    shiny::tagList(
      stepHeader(currentStep()),
      fluidRow(
        column(
          12,
          uiOutput("result_display")
        )
      ),
      # Add the download UI output here
      fluidRow(
        column(
          6,
          uiOutput("download_ui") # Placeholder for download button
        )
      ),
      fluidRow(
        column(
          12,
          div(
            style = "display:flex; align-items:center; margin-top:1rem;",
            actionButton(
              "back5",
              "Back",
              icon = icon("arrow-left"),
              class = "btn-secondary"
            ),
            div(
              style = "margin-left:auto; display:flex; align-items:center; gap:.5rem;",
              actionButton(
                "new_fresh",
                "Start New (fresh)",
                icon = icon("play"),
                class = "btn-primary"
              ),
              actionButton(
                "new_reuse",
                "Start New (reuse data)",
                icon = icon("repeat"),
                class = "btn-secondary"
              )
            )
          )
        )
      )
    )
  }

  # --- render the “deleted” table (read‐only, can select rows to restore)
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

  # --- Restore selected from the deleted table
  shiny::observeEvent(input$restore_selected_rows, {
    sel <- input$deleted_data_preview_rows_selected
    if (length(sel)) {
      # map row‐indices in deleted table back to IDs
      all <- userData()
      del <- userState$deleted_row_ids %||% integer(0)
      df_del <- all[all$..cyto_id.. %in% del, , drop = FALSE]
      to_restore <- df_del$..cyto_id..[sel]
      userState$deleted_row_ids <- setdiff(del, to_restore)
    }
  })

  # --- “Restore All” button
  shiny::observeEvent(input$restore_all_rows, {
    userState$deleted_row_ids <- integer(0)
  })

  output$viewSummaryCheckboxes <- shiny::renderUI({
    # Require either built-in data is used OR a file has been successfully uploaded
    req(input$use_builtin || isTruthy(input$datafile))
    # If the condition above is met, render the checkboxes
    tagList(
      checkboxInput("view_data", "View Data Loaded?", FALSE),
      checkboxInput("show_summary", "Show summary statistics", FALSE)
    )
  })
  # --- Logic for Custom Button Group: Statistical Tests ---
  stat_choices <- c("ANOVA", "Two-Sample T-Test")
  output$stat_function_ui <- shiny::renderUI({
    lapply(stat_choices, function(choice) {
      actionButton(
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
    "Enhanced Boxplots",
    "Correlation Plots",
    "Error-Bar Plot",
    "Dual-Flashlight Plot",
    "Heatmap",
    "Skewness/Kurtosis",
    "Volcano Plot"
  )
  output$exploratory_function_ui <- shiny::renderUI({
    lapply(exploratory_choices, function(choice) {
      actionButton(
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
      actionButton(
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
  output$ml_function_ui <- shiny:::renderUI({
    lapply(ml_choices, function(choice) {
      actionButton(
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

    updateProgressBar(
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
    if (!isTruthy(input$datafile) && !isTruthy(input$use_builtin)) {
      # If neither is true, show a pop-up modal warning
      showModal(modalDialog(
        title = "Data Source Required",
        "Please either upload a data file or check the 'Use built-in data' option to continue.",
        easyClose = TRUE,
        footer = actionButton("ok_no_data", "OK")
      ))
    } else {
      currentPage("step2")
      currentStep(2)
    }
  })
  shiny::observeEvent(input$ok_no_data, {
    removeModal() # close the popup
    currentPage("step1") # navigate back to Upload Data step
  })

  # A) Combine the two checkboxGroupInputs
  selected_columns_combined <- shiny::reactive({
    c(input$selected_categorical_cols, input$selected_numerical_cols)
  })

  # B) “Select / Deselect All” observers
  shiny::observeEvent(input$select_all_cat, {
    df <- userData()
    all_cols <- setdiff(names(df), "..cyto_id..")
    cat_cols <- all_cols[!sapply(df[all_cols], is.numeric)]
    updateCheckboxGroupInput(
      session,
      "selected_categorical_cols",
      selected = cat_cols
    )
  })
  shiny::observeEvent(input$deselect_all_cat, {
    df <- userData()
    all_cols <- setdiff(names(df), "..cyto_id..")
    cat_cols <- all_cols[!sapply(df[all_cols], is.numeric)]
    updateCheckboxGroupInput(
      session,
      "selected_categorical_cols",
      selected = character(0)
    )
  })
  shiny::observeEvent(input$select_all_num, {
    df <- userData()
    all_cols <- setdiff(names(df), "..cyto_id..")
    numeric_cols <- all_cols[sapply(df[all_cols], is.numeric)]
    updateCheckboxGroupInput(
      session,
      "selected_numerical_cols",
      selected = numeric_cols
    )
  })
  shiny::observeEvent(input$deselect_all_num, {
    df <- userData()
    all_cols <- setdiff(names(df), "..cyto_id..")
    numeric_cols <- all_cols[sapply(df[all_cols], is.numeric)]
    updateCheckboxGroupInput(
      session,
      "selected_numerical_cols",
      selected = character(0)
    )
  })
  # C) Base reactive to apply row‐deletions and categorical filters
  # 1. Always keep the filtered-but-raw data
  raw_filtered <- shiny::reactive({
    df <- userData()
    # (1) apply any row‐deletions
    if (!is.null(userState$deleted_row_ids)) {
      df <- df[!df$..cyto_id.. %in% userState$deleted_row_ids, , drop = FALSE]
    }
    # (2) apply filters
    if (length(input$selected_categorical_cols)) {
      for (col in input$selected_categorical_cols) {
        fid <- paste0("filter_", col)
        if (fid %in% names(input)) {
          df <- df[df[[col]] %in% input[[fid]], , drop = FALSE]
        }
      }
    }
    df
  })
  data_after_filters <- shiny::reactive({
    df <- raw_filtered()
    req(df)
    if (isTRUE(input$step2_log2)) {
      num_cols <- intersect(input$selected_numerical_cols, names(df))
      df[num_cols] <- round(log2(df[num_cols]), 5)
    }
    df
  })

  data_after_imputation <- reactive({
    dat <- data_after_filters()
    imp <- imputed_data()
    if (!is.null(imp)) {
      return(imp)
    }
    dat
  })

  # D) The main filteredData() used by DT
  filteredData <- shiny::reactive({
    df <- data_after_imputation()
    req(df)
    cols_to_keep <-
      if (currentStep() >= 3) {
        req(userState$selected_columns)
        userState$selected_columns
      } else {
        req(selected_columns_combined())
        selected_columns_combined()
      }

    # Always keep the internal ID for deletes
    final_cols <- union(cols_to_keep, "..cyto_id..")
    df[, intersect(names(df), final_cols), drop = FALSE]
  })

  # Populate the sPLS-DA "Label column" choices whenever the working data changes
  observeEvent(data_after_filters(), {
    df <- data_after_filters()
    req(nrow(df) > 0)

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

    updateSelectInput(
      session,
      "splsda_ind_names_col",
      choices = cand,
      selected = isolate(userState$splsda_ind_names_col) %||%
        if (length(cand)) cand[1] else NULL
    )
  })
  # E) Render the table in Step 2
  output$filtered_data_preview <- DT::renderDT({
    df <- filteredData()
    req(nrow(df) > 0)

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
      card(
        card_header(class = "bg-primary", "3. Apply Filters (Optional)"),
        card_body(
          bslib::accordion(
            id = "filter_accordion",
            open = isTruthy(input$filter_accordion),
            bslib::accordion_panel(
              "Filter by Categorical Values",
              uiOutput("filter_ui"),
              icon = fontawesome::fa("filter")
            )
          )
        )
      )
    } else {
      NULL
    }
  })

  observeEvent(input$open_impute_modal, {
    showModal(modalDialog(
      title = "Treat Missing Values",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton("apply_impute", "Apply", class = "btn-primary"),
        modalButton("Cancel")
      ),
      # --- simple UI for 5 methods ---
      fluidRow(
        column(4, {
          df <- data_after_filters()
          cols <- setdiff(names(df), c(".cyto_id.", "..cyto_id.."))
          checkboxGroupInput(
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
        column(
          4,
          radioButtons(
            "imp_method",
            "Method",
            choices = c(
              "Mean (numeric)" = "mean",
              "Median (numeric)" = "median",
              "Mode (categorical)" = "mode",
              "k-NN (sample-wise / mixed types)" = "knn_sample",
              "k-NN (feature-wise / numeric only)" = "knn_feature"
            ),
            selected = userState$impute_meta$method %||% "mean"
          ),
          conditionalPanel(
            "input.imp_method == 'knn_sample' || input.imp_method == 'knn_feature'",
            numericInput(
              "imp_k",
              "k neighbors",
              value = userState$impute_meta$k %||% 5,
              min = 1,
              step = 1
            ),
            checkboxInput(
              "imp_scale",
              "Standardize numeric vars before k-NN",
              value = isTRUE(userState$impute_meta$scaled)
            )
          )
        ),
        column(
          4,
          verbatimTextOutput("imp_na_before"),
          verbatimTextOutput("imp_na_after")
        )
      )
    ))
  })
  output$imp_na_before <- renderPrint({
    d <- data_after_filters()
    c(NAs = sum(is.na(d)), Pct = round(100 * mean(is.na(d)), 2))
  })
  output$imp_na_after <- renderPrint({
    req(imputed_data())
    d <- imputed_data()
    c(NAs = sum(is.na(d)), Pct = round(100 * mean(is.na(d)), 2))
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

      rec <- recipe(~., data = dat)
      if (scale_for_knn && length(num_cols)) {
        rec <- rec %>% step_normalize(all_of(num_cols))
      }
      rec <- rec %>% step_impute_knn(all_of(include), neighbors = k)
      dat <- bake(prep(rec, training = dat), new_data = dat)
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
        out <- impute.knn(Ms, k = k)$data
        out <- sweep(out, 2, scalev, "*")
        out <- sweep(out, 2, center, "+")
      } else {
        out <- impute.knn(M, k = k)$data
      }
      dat[num_cols] <- as.data.frame(out)
    }
    dat
  }
  observeEvent(input$apply_impute, {
    df <- data_after_filters() # <-- impute the *filtered* data
    sel <- input$imp_cols
    if (!length(sel)) {
      showNotification("Select ≥1 column.", type = "error")
      return()
    }

    dat_imp <- impute_data(
      df,
      include = sel,
      method = input$imp_method,
      k = input$imp_k %||% 5,
      scale_for_knn = isTRUE(input$imp_scale)
    )
    imputed_data(dat_imp)
    userState$impute_meta <- list(
      method = input$imp_method,
      k = input$imp_k %||% 5,
      cols = sel,
      scaled = isTRUE(input$imp_scale)
    )
    removeModal()
  })

  # ============================================================================
  # Reactive holders for the before/after comparison
  # ============================================================================
  comparison_data <- shiny::reactiveVal(list(
    orig = NULL,
    trans = NULL,
    num_cols = character()
  ))

  show_comparison <- shiny::reactiveVal(FALSE)

  # 1) Define the comparison plot output up front:
  output$norm_compare <- shiny::renderPlot({
    req(show_comparison())
    cmp <- comparison_data()
    orig <- cmp$orig
    trans <- cmp$trans
    num_cols <- cmp$num_cols
    # long data for before & after
    df_before <- pivot_longer(
      orig[, num_cols, drop = FALSE],
      cols = everything(),
      names_to = "variable",
      values_to = "value"
    )
    df_after <- pivot_longer(
      trans[, num_cols, drop = FALSE],
      cols = everything(),
      names_to = "variable",
      values_to = "value"
    )
    # 1) Separate density plots
    dens_before <- ggplot(df_before, aes(x = value)) +
      geom_density(fill = "skyblue", alpha = 0.5) +
      labs(title = "Before log₂", x = "Value", y = "Density") +
      theme_minimal()

    dens_after <- ggplot(df_after, aes(x = value)) +
      geom_density(fill = "salmon", alpha = 0.5) +
      labs(title = "After log₂", x = "Value", y = "") +
      theme_minimal() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

    # 2) Separate boxplot grids
    box_before <- ggplot(df_before, aes(x = variable, y = value)) +
      geom_boxplot(fill = "skyblue", outlier.size = 0.5) +
      labs(title = "Before log₂", x = NULL, y = "Value") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text = element_text(size = 8)
      )

    box_after <- ggplot(df_after, aes(x = variable, y = value)) +
      geom_boxplot(fill = "salmon", outlier.size = 0.5) +
      labs(title = "After log₂", x = NULL, y = "") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )

    # 3) Combine with patchwork
    # Top row: two densities
    # Bottom row: two boxplot facets
    comparison_plot <- patchwork::wrap_plots(
      dens_before,
      dens_after,
      box_before,
      box_after,
      ncol = 2,
      nrow = 2,
      heights = c(0.3, 0.7),
      widths = c(0.5, 0.5)
    )
    print(comparison_plot)
  })

  # 2) When they click “Next” on Step 2, save the selected columns and log2 option
  shiny::observeEvent(input$next2, {
    # save state
    userState$selected_columns <- selected_columns_combined()
    userState$step2_log2 <- input$step2_log2
    currentPage("step3")
    currentStep(3)
  })

  shiny::observeEvent(input$preview_transform, {
    req(input$step2_log2) # only if they’ve asked for log2
    orig <- raw_filtered()
    num_cols <- intersect(input$selected_numerical_cols, names(orig))
    trans <- orig
    trans[num_cols] <- round(log2(orig[num_cols]), 5)

    comparison_data(list(
      orig = orig,
      trans = trans,
      num_cols = num_cols
    ))
    show_comparison(TRUE)

    showModal(
      modalDialog(
        title = "Before vs After log₂-Transformation",
        plotOutput("norm_compare", height = "600px"),
        footer = modalButton("Close"),
        size = "l"
      )
    )
  })

  # On moving from Step 3 to Step 4, save the selected function and function options
  shiny::observeEvent(input$next3, {
    req(currentStep() == 4, !is.null(selected_function()))
    userState$selected_function <- selected_function()
    if (currentStep() == 4 && !is.null(selected_function())) {
      if (selected_function() == "Boxplots") {
        userState$bp_mf_row <- input$bp_mf_row
        userState$bp_y_lim <- input$bp_y_lim
        userState$bp_bin_size <- input$bp_bin_size
      }
      if (selected_function() == "Enhanced Boxplots") {
        userState$bp2_mf_row <- input$bp2_mf_row
        userState$bp2_y_lim <- input$bp2_y_lim
      }
      if (selected_function() == "Error-Bar Plot") {
        userState$eb_group_col <- input$eb_group_col
        userState$eb_p_lab <- input$eb_p_lab
        userState$eb_es_lab <- input$eb_es_lab
        userState$eb_class_symbol <- input$eb_class_symbol
        userState$eb_x_lab <- input$eb_x_lab
        userState$eb_y_lab <- input$eb_y_lab
        userState$eb_title <- input$eb_title
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
        userState$splsda_group_col2 <- input$splsda_group_col
        userState$splsda_use_batch_corr <- input$splsda_use_batch_corr
        userState$splsda_batch_col <- input$splsda_batch_col
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
        userstate$mint_splsda_cim <- input$mint_splsda_cim
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
    }
    currentPage("step4")
    currentStep(4)
  })
  # For error message screen.
  shiny::observeEvent(input$back6, {
    currentPage("step4")
    currentStep(4)
  })
  ## ---------------------------
  ## Updating inputs by userState
  ## ---------------------------

  shiny::observeEvent(currentStep(), {
    req(currentStep(), !is.null(selected_function()))
    if (currentStep() == 1) {
      # restore the Use built‑in” toggle
      updateCheckboxInput(session, "use_builtin", value = userState$use_builtin)
      if (
        isTRUE(userState$use_builtin) && !is.null(userState$built_in_choice)
      ) {
        updateSelectInput(
          session,
          "built_in_choice",
          selected = userState$built_in_choice
        )
      }
    }
    if (currentStep() == 2) {
      # recompute the same choices used in step2UI()
      df <- data_after_filters()
      all_cols <- names(df)
      is_numeric_col <- vapply(df, is.numeric, logical(1))
      categorical_cols <- all_cols[!is_numeric_col]
      numerical_cols <- all_cols[is_numeric_col]

      # restore what they had selected
      if (!is.null(userState$selected_categorical_cols)) {
        updateCheckboxGroupInput(
          session,
          "selected_categorical_cols",
          choices = categorical_cols,
          selected = intersect(
            userState$selected_categorical_cols,
            categorical_cols
          )
        )
      }
      if (!is.null(userState$selected_numerical_cols)) {
        updateCheckboxGroupInput(
          session,
          "selected_numerical_cols",
          choices = numerical_cols,
          selected = intersect(
            userState$selected_numerical_cols,
            numerical_cols
          )
        )
      }
      # also restore the log2 box
      updateCheckboxInput(
        session,
        "step2_log2",
        value = userState$step2_log2
      )
    }
    if (currentStep() == 3 && !is.null(userState$selected_function)) {
      if (!is.null(userState$analysis_categories)) {
        updateRadioButtons(
          session,
          "analysis_categories",
          selected = userState$analysis_categories
        )
      }
      # re‐select the dropdowns
      if (!is.null(userState$stat_function)) {
        updateSelectInput(
          session,
          "stat_function",
          selected = userState$stat_function
        )
      }
      if (!is.null(userState$exploratory_function)) {
        updateSelectInput(
          session,
          "exploratory_function",
          selected = userState$exploratory_function
        )
      }
      if (!is.null(userState$multivariate_function)) {
        updateSelectInput(
          session,
          "multivariate_function",
          selected = userState$multivariate_function
        )
      }
      if (!is.null(userState$ml_function)) {
        updateSelectInput(
          session,
          "ml_function",
          selected = userState$ml_function
        )
      }
      # Boxplots
      if (userState$selected_function == "Boxplots") {
        updateSelectInput(session, "bp_mf_row", selected = userState$bp_mf_row)
        updateTextInput(session, "bp_y_lim", value = userState$bp_y_lim)
        updateNumericInput(
          session,
          "bp_bin_size",
          value = userState$bp_bin_size
        )
      }

      # Enhanced Boxplots
      if (userState$selected_function == "Enhanced Boxplots") {
        updateTextInput(session, "bp2_mf_row", value = userState$bp2_mf_row)
        updateTextInput(session, "bp2_y_lim", value = userState$bp2_y_lim)
      }
      if (userState$selected_function == "Error-Bar Plot") {
        updateSelectInput(
          session,
          "eb_group_col",
          selected = userState$eb_group_col
        )
        updateCheckboxInput(session, "eb_p_lab", value = userState$eb_p_lab)
        updateCheckboxInput(session, "eb_es_lab", value = userState$eb_es_lab)
        updateCheckboxInput(
          session,
          "eb_class_symbol",
          value = userState$eb_class_symbol
        )
        updateTextInput(session, "eb_x_lab", value = userState$eb_x_lab)
        updateTextInput(session, "eb_y_lab", value = userState$eb_y_lab)
        updateTextInput(session, "eb_title", value = userState$eb_title)
      }
      # Dual-Flashlight Plot
      if (userState$selected_function == "Dual-Flashlight Plot") {
        updateSelectInput(
          session,
          "df_group_var",
          selected = userState$df_group_var
        )
        updateSelectInput(
          session,
          "df_cond1",
          selected = userState$df_cond1
        )
        updateSelectInput(
          session,
          "df_cond2",
          selected = userState$df_cond2
        )
        updateNumericInput(
          session,
          "df_ssmd_thresh",
          value = userState$df_ssmd_thresh
        )
        updateNumericInput(
          session,
          "df_log2fc_thresh",
          value = userState$df_log2fc_thresh
        )
        updateNumericInput(
          session,
          "df_top_labels",
          value = userState$df_top_labels
        )
      }

      # Heatmap
      if (userState$selected_function == "Heatmap") {
        updateSelectInput(
          session,
          "hm_annotation",
          selected = userState$hm_annotation
        )
      }

      # Principal Component Analysis (PCA)
      if (userState$selected_function == "Principal Component Analysis (PCA)") {
        updateSelectInput(
          session,
          "pca_group_col",
          selected = userState$pca_group_col
        )
        updateSelectInput(
          session,
          "pca_group_col2",
          selected = userState$pca_group_col2
        )
        updateNumericInput(
          session,
          "pca_comp_num",
          value = userState$pca_comp_num
        )
        updateCheckboxInput(
          session,
          "pca_ellipse",
          value = userState$pca_ellipse
        )
        updateSelectInput(session, "pca_style", selected = userState$pca_style)
        updateSelectizeInput(session, "pca_pch", selected = userState$pca_pch)
        updateSelectizeInput(
          session,
          "pca_colors",
          selected = userState$pca_colors
        )
      }
      # Partial Least Squares Regression (PLSR)
      if (
        userState$selected_function == "Partial Least Squares Regression (PLSR)"
      ) {
        updateSelectInput(
          session,
          "plsr_group_col",
          selected = userState$plsr_group_col
        )
        updateSelectInput(
          session,
          "plsr_response_col",
          selected = userState$plsr_response_col
        )
        updateNumericInput(
          session,
          "plsr_comp_num",
          value = userState$plsr_comp_num
        )
        updateCheckboxInput(
          session,
          "plsr_sparse",
          value = userState$plsr_sparse
        )
        updateNumericInput(
          session,
          "plsr_keepX",
          value = userState$plsr_keepX
        )
        updateCheckboxInput(
          session,
          "plsr_cv_opt",
          value = userState$plsr_cv_opt
        )
        updateNumericInput(
          session,
          "plsr_fold_num",
          value = userState$plsr_fold_num
        )
        updateCheckboxInput(
          session,
          "plsr_ellipse",
          value = userState$plsr_ellipse
        )
        updateSelectizeInput(
          session,
          "plsr_colors",
          selected = userState$plsr_colors
        )
      }

      # Correlation Plots
      if (userState$selected_function == "Correlation Plots") {
        updateSelectInput(
          session,
          "corr_group_col",
          selected = userState$corr_group_col
        )
        updateSelectInput(
          session,
          "corr_target",
          selected = userState$corr_target
        )
        updateCheckboxInput(
          session,
          "corr_by_group",
          value = userState$corr_by_group
        )
      }

      # Random Forest
      if (userState$selected_function == "Random Forest") {
        updateSelectInput(
          session,
          "rf_group_col",
          selected = userState$rf_group_col
        )
        updateNumericInput(session, "rf_ntree", value = userState$rf_ntree)
        updateNumericInput(session, "rf_mtry", value = userState$rf_mtry)
        updateNumericInput(
          session,
          "rf_train_fraction",
          value = userState$rf_train_fraction
        )
        updateCheckboxInput(
          session,
          "rf_plot_roc",
          value = userState$rf_plot_roc
        )
        updateCheckboxInput(
          session,
          "rf_run_rfcv",
          value = userState$rf_run_rfcv
        )
        updateNumericInput(session, "rf_k_folds", value = userState$rf_k_folds)
        updateNumericInput(session, "rf_step", value = userState$rf_step)
      }

      # Skewness/Kurtosis
      if (userState$selected_function == "Skewness/Kurtosis") {
        updateSelectInput(
          session,
          "skku_group_cols",
          selected = userState$skku_group_cols
        )
        updateCheckboxInput(
          session,
          "skku_print_raw",
          value = userState$skku_print_raw
        )
        updateCheckboxInput(
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
        df_now <- isolate(data_after_filters())
        if (isTruthy(df_now)) {
          cand <- names(df_now)[!vapply(df_now, is.numeric, logical(1))]
          cand <- setdiff(cand, "..cyto_id..")
        } else {
          cand <- character(0)
        }
        updateSelectInput(
          session,
          "splsda_group_col",
          selected = userState$splsda_group_col
        )
        updateSelectInput(
          session,
          "splsda_group_col2",
          selected = userState$splsda_group_col2
        )
        updateCheckboxInput(
          session,
          "splsda_use_batch_corr",
          value = userState$splsda_use_batch_corr
        )
        updateSelectInput(
          session,
          "splsda_batch_col",
          selected = userState$splsda_batch_col
        )
        updateCheckboxInput(
          session,
          "splsda_use_multilevel",
          value = userState$splsda_use_multilevel
        )
        updateSelectInput(
          session,
          "splsda_multilevel",
          selected = userState$splsda_multilevel
        )
        updateNumericInput(
          session,
          "splsda_var_num",
          value = userState$splsda_var_num
        )
        updateCheckboxInput(
          session,
          "splsda_cv_opt",
          value = userState$splsda_cv_opt
        )
        updateNumericInput(
          session,
          "splsda_fold_num",
          value = userState$splsda_fold_num
        )
        updateNumericInput(
          session,
          "splsda_comp_num",
          value = userState$splsda_comp_num
        )
        updateRadioButtons(
          session,
          "splsda_ind_names_mode",
          selected = userState$splsda_ind_names_mode %||% "off"
        )
        updateSelectInput(
          session,
          "splsda_ind_names_col",
          choices = cand,
          selected = userState$splsda_ind_names_col %||%
            if (length(cand)) cand[1] else NULL
        )
        updateSelectizeInput(
          session,
          "splsda_pch",
          selected = userState$splsda_pch
        )
        updateSelectInput(
          session,
          "splsda_style",
          selected = userState$splsda_style
        )
        updateCheckboxInput(session, "splsda_roc", value = userState$splsda_roc)
        updateCheckboxInput(
          session,
          "splsda_ellipse",
          value = userState$splsda_ellipse
        )
        updateCheckboxInput(session, "splsda_bg", value = userState$splsda_bg)
        updateCheckboxInput(
          session,
          "splsda_conf_mat",
          value = userState$splsda_conf_mat
        )
        updateSelectizeInput(
          session,
          "splsda_colors",
          selected = userState$splsda_colors
        )
      }
      # Volcano Plot
      if (userState$selected_function == "Volcano Plot") {
        updateSelectInput(
          session,
          "volc_group_col",
          selected = userState$volc_group_col
        )
        updateSelectInput(
          session,
          "volc_cond1",
          selected = userState$volc_cond1
        )
        updateSelectInput(
          session,
          "volc_cond2",
          selected = userState$volc_cond2
        )
        updateNumericInput(
          session,
          "volc_fold_change_thresh",
          value = userState$volc_fold_change_thresh
        )
        updateNumericInput(
          session,
          "volc_p_value_thresh",
          value = userState$volc_p_value_thresh
        )
        updateNumericInput(
          session,
          "volc_top_labels",
          value = userState$volc_top_labels
        )
      }

      # Extreme Gradient Boosting (XGBoost)
      if (
        userState$selected_function == "Extreme Gradient Boosting (XGBoost)"
      ) {
        updateSelectInput(
          session,
          "xgb_group_col",
          selected = userState$xgb_group_col
        )
        updateNumericInput(
          session,
          "xgb_train_fraction",
          value = userState$xgb_train_fraction
        )
        updateNumericInput(
          session,
          "xgb_nrounds",
          value = userState$xgb_nrounds
        )
        updateNumericInput(
          session,
          "xgb_max_depth",
          value = userState$xgb_max_depth
        )
        updateNumericInput(session, "xgb_eta", value = userState$xgb_eta)
        updateSelectInput(
          session,
          "xgb_eval_metric",
          selected = userState$xgb_eval_metric
        )
        updateNumericInput(
          session,
          "xgb_top_n_features",
          value = userState$xgb_top_n_features
        )
        updateCheckboxInput(
          session,
          "xgb_plot_roc",
          value = userState$xgb_plot_roc
        )
        updateCheckboxInput(session, "xgb_cv", value = userState$xgb_cv)
        updateNumericInput(session, "xgb_nfold", value = userState$xgb_nfold)
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
        bp_mf_row = input$bp_mf_row,
        bp_y_lim = input$bp_y_lim,

        # Enhanced Boxplots
        bp2_mf_row = input$bp2_mf_row,
        bp2_y_lim = input$bp2_y_lim,

        # Error-Bar Plot
        eb_group_col = input$eb_group_col,
        eb_p_lab = input$eb_p_lab,
        eb_es_lab = input$eb_es_lab,
        eb_class_symbol = input$eb_class_symbol,
        eb_x_lab = input$eb_x_lab,
        eb_y_lab = input$eb_y_lab,
        eb_title = input$eb_title,

        # Dual-Flashlight Plot
        df_group_var = input$df_group_var,
        df_cond1 = input$df_cond1,
        df_cond2 = input$df_cond2,
        df_ssmd_thresh = input$df_ssmd_thresh,
        df_log2fc_thresh = input$df_log2fc_thresh,
        df_top_labels = input$df_top_labels,

        # Heatmap
        hm_annotation = input$hm_annotation,

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

  ## ---------------------------
  ## Analysis and Results
  ## ---------------------------
  errorMessage <- shiny::reactiveVal(NULL)
  warningMessage <- shiny::reactiveVal(NULL)

  analysisResult <- shiny::eventReactive(input$next4, {
    errorMessage(NULL)
    warningMessage(NULL)

    req(filteredData())
    prog <- Progress$new()
    on.exit(prog$close())

    tryCatch(
      {
        withCallingHandlers(
          {
            df <- filteredData()
            df$..cyto_id.. <- NULL

            # Simplified: always generate a file path for functions that can output a PDF

            func_to_run <- selected_function()
            req(func_to_run)

            results <- switch(
              func_to_run,

              # -- Statistical Tests --
              "ANOVA" = cyt_anova(
                data = df,
                progress = prog,
                format_output = TRUE
              ),

              "Two-Sample T-Test" = cyt_ttest(
                data = df,
                progress = prog,
                scale = NULL,
                format_output = TRUE
              ),

              # -- Exploratory Visualization --
              "Boxplots" = cyt_bp(
                data = df,

                progress = prog,
                bin_size = input$bp_bin_size,
                mf_row = if (nzchar(input$bp_mf_row)) {
                  as.numeric(strsplit(input$bp_mf_row, ",")[[1]])
                },
                y_lim = if (nzchar(input$bp_y_lim)) {
                  as.numeric(strsplit(input$bp_y_lim, ",")[[1]])
                },
                scale = NULL
              ),

              "Enhanced Boxplots" = cyt_bp2(
                data = df,

                progress = prog,
                mf_row = if (nzchar(input$bp2_mf_row)) {
                  as.numeric(strsplit(input$bp2_mf_row, ",")[[1]])
                },
                y_lim = if (nzchar(input$bp2_y_lim)) {
                  as.numeric(strsplit(input$bp2_y_lim, ",")[[1]])
                },
                scale = NULL
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
                log2 = FALSE
              ),

              "Dual-Flashlight Plot" = cyt_dualflashplot(
                data = df,
                group_var = input$df_group_var,
                group1 = input$df_cond1,
                group2 = input$df_cond2,

                progress = prog,
                ssmd_thresh = input$df_ssmd_thresh,
                log2fc_thresh = input$df_log2fc_thresh,
                top_labels = input$df_top_labels
              ),

              "Heatmap" = cyt_heatmap(
                data = df,

                progress = prog,
                scale = NULL,
                annotation_col_name = input$hm_annotation
              ),

              "Skewness/Kurtosis" = cyt_skku(
                data = df,

                progress = prog,
                group_cols = input$skku_group_cols,
                print_res_raw = input$skku_print_raw,
                print_res_log = input$skku_print_log
              ),

              "Volcano Plot" = cyt_volc(
                data = df,

                progress = prog,
                group_col = input$volc_group_col,
                cond1 = input$volc_cond1,
                cond2 = input$volc_cond2,
                fold_change_thresh = input$volc_fold_change_thresh,
                p_value_thresh = input$volc_p_value_thresh,
                top_labels = input$volc_top_labels
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
                  pca_colors = cols
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

                res <- cyt_plsr(
                  data = df,
                  group_col = grp,
                  response_col = input$plsr_response_col,
                  comp_num = input$plsr_comp_num,
                  sparse = input$plsr_sparse,
                  var_num = input$plsr_keepX,
                  scale = NULL,
                  ellipse = isTRUE(input$plsr_ellipse),
                  cv_opt = if (input$plsr_cv_opt == "None") {
                    NULL
                  } else {
                    tolower(input$plsr_cv_opt)
                  }, # "loocv" or "mfold"
                  fold_num = input$plsr_fold_num,
                  pls_colors = input$plsr_colors,
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
                  group_var = if (nzchar(grp)) grp else NULL,
                  compare_groups = FALSE,
                  progress = prog
                )

                # per-group heatmaps for each method
                group_mats <- NULL
                if (bygrp) {
                  g <- factor(df[[grp]])
                  levs <- levels(g)
                  levs <- levs[!is.na(levs)]

                  build_group_mats <- function(method) {
                    mats <- list()
                    for (lv in levs) {
                      sub <- df[g == lv, , drop = FALSE]
                      mats[[lv]] <- cyt_corr(
                        data = sub,
                        target = tgt,
                        methods = method,
                        progress = prog
                      )[[method]]$heat_mat
                    }
                    mats
                  }

                  group_mats <- list(
                    spearman = build_group_mats("spearman"),
                    pearson = build_group_mats("pearson")
                  )
                }

                list(
                  spearman = list(
                    table = both$spearman$table,
                    heat_mat = both$spearman$heat_mat
                  ),
                  pearson = list(
                    table = both$pearson$table,
                    heat_mat = both$pearson$heat_mat
                  ),
                  group_mats = group_mats,
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
                    req(input$splsda_ind_names_col)
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
                  multilevel = multilevel,
                  var_num = input$splsda_var_num,
                  cv_opt = if (input$splsda_cv_opt == "None") {
                    NULL
                  } else {
                    input$splsda_cv_opt
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
                  conf_mat = input$splsda_conf_mat
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
                  bg = input$mint_splsda_bg
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
                step = input$rf_step
              ),

              "Extreme Gradient Boosting (XGBoost)" = cyt_xgb(
                data = df,

                progress = prog,
                group_col = input$xgb_group_col,
                train_fraction = input$xgb_train_fraction,
                nrounds = input$xgb_nrounds,
                max_depth = input$xgb_max_depth,
                eta = input$xgb_eta,
                nfold = input$xgb_nfold,
                cv = isTRUE(input$xgb_cv),
                eval_metric = input$xgb_eval_metric,
                top_n_features = input$xgb_top_n_features,
                plot_roc = isTRUE(input$xgb_plot_roc)
              )
            ) # end switch

            results
          },
          warning = function(w) {
            warningMessage(conditionMessage(w))
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
    req(analysisResult())
    analysisResult()
  })

  output$errorText <- shiny::renderUI({
    req(errorMessage())
    div(
      style = "color:red; padding:5px; border:1px solid red;",
      strong("Error:"),
      tags$pre(errorMessage())
    )
  })

  output$warningText <- shiny::renderUI({
    req(warningMessage())
    div(
      style = "color:orange; padding:5px; border:1px solid orange;",
      strong("Warning:"),
      tags$pre(warningMessage())
    )
  })

  output$result_display <- shiny::renderUI({
    err <- errorMessage()
    if (isTruthy(err)) {
      return(tagList(
        uiOutput("errorText"),
        actionButton(
          "back6",
          "Change Inputs",
          icon = icon("arrow-left"),
          class = "btn-secondary"
        )
      ))
    }
    req(analysisResult())
    res <- analysisResult()
    func_name <- selected_function()
    req(func_name)

    # Main UI container
    tagList(
      uiOutput("warningText"), # Display warnings if any

      # Interactive Mode: Use a switch to render the correct UI
      switch(
        func_name,

        # --- sPLS-DA UI ---
        "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = {
          if ("overall_indiv_plot" %in% names(res)) {
            # Single analysis UI
            tagList(
              h4("sPLS-DA Results"),
              tabsetPanel(
                id = "splsda_tabs",
                type = "tabs",
                tabPanel(
                  "sPLS-DA Plot",
                  shinycssloaders::withSpinner(
                    plotOutput("splsda_overallIndivPlot", height = "500px"),
                    type = 8
                  )
                ),
                tabPanel(
                  "Loadings",
                  shinycssloaders::withSpinner(
                    uiOutput("splsda_loadingsUI"),
                    type = 8
                  )
                ),
                tabPanel(
                  "VIP Scores",
                  shinycssloaders::withSpinner(
                    uiOutput("splsda_vipScoresUI"),
                    type = 8
                  )
                ),
                if (!is.null(res$vip_indiv_plot)) {
                  tabPanel(
                    "VIP Model Plot",
                    shinycssloaders::withSpinner(
                      plotOutput("splsda_vipIndivPlot", height = "500px"),
                      type = 8
                    )
                  )
                },
                if (!is.null(res$vip_loadings)) {
                  tabPanel(
                    "VIP Loadings",
                    shinycssloaders::withSpinner(
                      uiOutput("splsda_vipLoadingsUI"),
                      type = 8
                    )
                  )
                },
                if (!is.null(res$overall_3D)) {
                  tabPanel(
                    "3D Plot",
                    shinycssloaders::withSpinner(
                      plotOutput("splsda_overall3DPlot", height = "500px"),
                      type = 8
                    )
                  )
                },
                if (!is.null(res$overall_ROC) || !is.null(res$overall_CV)) {
                  tabPanel(
                    "Performance",
                    if (!is.null(res$overall_ROC)) {
                      shinycssloaders::withSpinner(
                        plotOutput("splsda_overallRocPlot", height = "400px"),
                        type = 8
                      )
                    },
                    if (!is.null(res$overall_CV)) {
                      shinycssloaders::withSpinner(
                        plotOutput("splsda_overallCvPlot", height = "400px"),
                        type = 8
                      )
                    }
                  )
                },
                if (!is.null(res$conf_matrix)) {
                  tabPanel(
                    "Confusion Matrix",
                    shinycssloaders::withSpinner(
                      verbatimTextOutput("splsda_confMatrix"),
                      type = 8
                    )
                  )
                }
              )
            )
          } else {
            # Multi-group analysis UI (tabs for each group)
            do.call(
              tabsetPanel,
              c(
                list(id = "splsda_multigroup_tabs"),
                lapply(names(res), function(trt) {
                  tabPanel(
                    title = trt,
                    tabsetPanel(
                      type = "tabs",
                      tabPanel(
                        "sPLS-DA Plot",
                        shinycssloaders::withSpinner(
                          plotOutput(
                            paste0("splsda_overallIndivPlot_", trt),
                            height = "500px"
                          ),
                          type = 8
                        )
                      ),
                      tabPanel(
                        "Loadings",
                        shinycssloaders::withSpinner(
                          uiOutput(paste0("splsda_loadingsUI_", trt)),
                          type = 8
                        )
                      ),
                      tabPanel(
                        "VIP Scores",
                        shinycssloaders::withSpinner(
                          uiOutput(paste0("splsda_vipScoresUI_", trt)),
                          type = 8
                        )
                      ),
                      if (!is.null(res[[trt]]$vip_indiv_plot)) {
                        tabPanel(
                          "VIP Model Plot",
                          shinycssloaders::withSpinner(
                            plotOutput(
                              paste0("splsda_vipIndivPlot_", trt),
                              height = "500px"
                            ),
                            type = 8
                          )
                        )
                      },
                      if (!is.null(res[[trt]]$vip_loadings)) {
                        tabPanel(
                          "VIP Loadings",
                          shinycssloaders::withSpinner(
                            uiOutput(paste0("splsda_vipLoadingsUI_", trt)),
                            type = 8
                          )
                        )
                      },
                      if (!is.null(res[[trt]]$overall_3D)) {
                        tabPanel(
                          "3D Plot",
                          shinycssloaders::withSpinner(
                            plotOutput(
                              paste0("splsda_overall3DPlot_", trt),
                              height = "500px"
                            ),
                            type = 8
                          )
                        )
                      },
                      if (
                        !is.null(res[[trt]]$overall_ROC) ||
                          !is.null(res[[trt]]$overall_CV)
                      ) {
                        tabPanel(
                          "Performance",
                          if (!is.null(res[[trt]]$overall_ROC)) {
                            shinycssloaders::withSpinner(
                              plotOutput(
                                paste0(
                                  "splsda_overallRocPlot_",
                                  trt
                                ),
                                height = "400px",
                              ),
                              type = 8
                            )
                          },
                          if (!is.null(res[[trt]]$overall_CV)) {
                            shinycssloaders::withSpinner(
                              plotOutput(
                                paste0(
                                  "splsda_overallCvPlot_",
                                  trt
                                ),
                                height = "400px"
                              ),
                              type = 8
                            )
                          }
                        )
                      },
                      if (!is.null(res[[trt]]$conf_matrix)) {
                        tabPanel(
                          "Confusion Matrix",
                          shinycssloaders::withSpinner(
                            verbatimTextOutput(
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
            tagList(
              h4("MINT sPLS-DA Results"),
              tabsetPanel(
                id = "mint_splsda_tabs",
                tabPanel(
                  "Global Sample Plot",
                  withSpinner(plotOutput("mint_splsda_global_plot"), type = 8)
                ),
                tabPanel(
                  "Partial Sample Plots",
                  withSpinner(plotOutput("mint_splsda_partial_plot"), type = 8)
                ),
                tabPanel(
                  "Variable Loadings",
                  withSpinner(uiOutput("mint_splsda_loadings_ui"), type = 8)
                ),
                tabPanel(
                  "Correlation Circle",
                  withSpinner(
                    plotOutput("mint_splsda_corr_circle_plot"),
                    type = 8
                  )
                ),
                if (!is.null(res$cim_obj)) {
                  tabPanel(
                    "Heatmap (CIM)",
                    withSpinner(
                      plotOutput(
                        "mint_splsda_cim_plot",
                        height = "600px"
                      ),
                      type = 8
                    )
                  )
                },
                if (!is.null(res$roc_plot)) {
                  tabPanel(
                    "ROC Curve",
                    withSpinner(plotOutput("mint_splsda_roc_plot"), type = 8)
                  )
                }
              )
            )
          } else {
            # UI for multi-group analysis (nested tabs)
            do.call(
              tabsetPanel,
              c(
                list(id = "mint_splsda_multigroup_tabs"),
                lapply(names(res), function(trt) {
                  tabPanel(
                    title = trt,
                    tabsetPanel(
                      type = "tabs",
                      tabPanel(
                        "Global Plot",
                        withSpinner(
                          plotOutput(paste0(
                            "mint_splsda_global_",
                            trt
                          )),
                          type = 8
                        )
                      ),
                      tabPanel(
                        "Partial Plots",
                        withSpinner(
                          plotOutput(paste0(
                            "mint_splsda_partial_",
                            trt
                          )),
                          type = 8
                        )
                      ),
                      tabPanel(
                        "Variable Loadings",
                        withSpinner(
                          uiOutput(paste0(
                            "mint_splsda_loadings_",
                            trt
                          )),
                          type = 8
                        )
                      ),
                      tabPanel(
                        "Correlation",
                        withSpinner(
                          plotOutput(paste0(
                            "mint_splsda_corr_",
                            trt
                          )),
                          type = 8
                        )
                      ),
                      if (!is.null(res[[trt]]$cim_obj)) {
                        tabPanel(
                          "CIM",
                          withSpinner(
                            plotOutput(
                              paste0("mint_splsda_cim_", trt),
                              height = "600px"
                            ),
                            type = 8
                          )
                        )
                      },
                      if (!is.null(res[[trt]]$roc_plot)) {
                        tabPanel(
                          "ROC",
                          withSpinner(
                            plotOutput(paste0(
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
            tagList(
              h4("PCA Results"),
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  "PCA Plot",
                  shinycssloaders::withSpinner(
                    plotOutput("pca_indivPlot", height = "400px"),
                    type = 8
                  )
                ),
                tabPanel(
                  "Scree Plot",
                  shinycssloaders::withSpinner(
                    plotOutput("pca_screePlot", height = "400px"),
                    type = 8
                  )
                ),
                tabPanel(
                  "Loadings Plots",
                  shinycssloaders::withSpinner(
                    uiOutput("pca_loadingsUI"),
                    type = 8
                  )
                ),
                tabPanel(
                  "Biplot",
                  shinycssloaders::withSpinner(
                    plotOutput("pca_biplot", height = "400px"),
                    type = 8
                  )
                ),
                tabPanel(
                  "Correlation Circle",
                  shinycssloaders::withSpinner(
                    plotOutput("pca_corrCircle", height = "400px"),
                    type = 8
                  )
                ),
                if (!is.null(res$overall_3D)) {
                  tabPanel(
                    "3D Plot",
                    shinycssloaders::withSpinner(
                      plotOutput("pca_3DPlot", height = "400px"),
                      type = 8
                    )
                  )
                }
              )
            )
          } else {
            do.call(
              tabsetPanel,
              c(
                list(id = "pca_multigroup_tabs"),
                lapply(names(res), function(trt) {
                  tabPanel(
                    title = trt,
                    tabsetPanel(
                      type = "tabs",
                      tabPanel(
                        "PCA Plot",
                        shinycssloaders::withSpinner(
                          plotOutput(
                            paste0("pca_indivPlot_", trt),
                            height = "400px"
                          ),
                          type = 8
                        )
                      ),
                      tabPanel(
                        "Scree Plot",
                        shinycssloaders::withSpinner(
                          plotOutput(
                            paste0("pca_screePlot_", trt),
                            height = "400px"
                          ),
                          type = 8
                        )
                      ),
                      tabPanel(
                        "Loadings Plots",
                        shinycssloaders::withSpinner(
                          uiOutput(paste0("pca_loadingsUI_", trt)),
                          type = 8
                        )
                      ),
                      tabPanel(
                        "Biplot",
                        shinycssloaders::withSpinner(
                          plotOutput(
                            paste0("pca_biplot_", trt),
                            height = "400px"
                          ),
                          type = 8
                        )
                      ),
                      tabPanel(
                        "Correlation Circle",
                        shinycssloaders::withSpinner(
                          plotOutput(
                            paste0("pca_corrCircle_", trt),
                            height = "400px"
                          ),
                          type = 8
                        )
                      ),
                      if (!is.null(res[[trt]]$overall_3D)) {
                        tabPanel(
                          "3D Plot",
                          shinycssloaders::withSpinner(
                            plotOutput(
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
          tagList(
            h4("PLSR Results"),
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Scores Plot",
                shinycssloaders::withSpinner(
                  plotOutput("plsr_indivPlot", height = "400px"),
                  type = 8
                )
              ),
              tabPanel(
                "Predicted vs Observed",
                shinycssloaders::withSpinner(
                  plotOutput("plsr_predPlot", height = "400px"),
                  type = 8
                )
              ),
              tabPanel(
                "Residuals vs Fitted",
                shinycssloaders::withSpinner(
                  plotOutput("plsr_residPlot", height = "400px"),
                  type = 8
                )
              ),
              tabPanel(
                "Loadings Plots",
                shinycssloaders::withSpinner(
                  uiOutput("plsr_loadingsUI"),
                  type = 8
                )
              ),
              tabPanel(
                "VIP Scores",
                shinycssloaders::withSpinner(
                  uiOutput("plsr_vipUI"),
                  type = 8
                )
              ),
              if (!is.null(res$cv_plot)) {
                tabPanel(
                  "Cross-Validation",
                  shinycssloaders::withSpinner(
                    plotOutput("plsr_cvPlot", height = "400px"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$vip_scores_indiv)) {
                tabPanel(
                  "VIP > 1: Scores",
                  shinycssloaders::withSpinner(
                    plotOutput("plsr_vipIndivPlot", height = "400px"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$vip_cv_plot)) {
                tabPanel(
                  "VIP > 1: Cross-Validation",
                  shinycssloaders::withSpinner(
                    plotOutput("plsr_vipCVPlot", height = "400px"),
                    type = 8
                  )
                )
              }
            )
          )
        },

        # --- Correlation UI ---
        "Correlation Plots" = {
          tagList(
            h4("Correlation Results"),
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Spearman",
                tabsetPanel(
                  type = "tabs",
                  tabPanel(
                    "Table",
                    shinycssloaders::withSpinner(
                      DT::dataTableOutput("corr_tbl_spearman"),
                      type = 8
                    )
                  ),
                  tabPanel(
                    "Heatmap",
                    shinycssloaders::withSpinner(
                      plotOutput("corr_heatmap_spearman", height = "600px"),
                      type = 8
                    )
                  ),
                  if (!is.null(res$group_mats)) {
                    tabPanel(
                      "Per-Group Heatmaps",
                      shinycssloaders::withSpinner(
                        uiOutput("corr_group_heatmap_ui_spearman"),
                        type = 8
                      )
                    )
                  }
                )
              ),
              tabPanel(
                "Pearson",
                tabsetPanel(
                  type = "tabs",
                  tabPanel(
                    "Table",
                    shinycssloaders::withSpinner(
                      DT::dataTableOutput("corr_tbl_pearson"),
                      type = 8
                    )
                  ),
                  tabPanel(
                    "Heatmap",
                    shinycssloaders::withSpinner(
                      plotOutput("corr_heatmap_pearson", height = "600px"),
                      type = 8
                    )
                  ),
                  if (!is.null(res$group_mats)) {
                    tabPanel(
                      "Per-Group Heatmaps",
                      shinycssloaders::withSpinner(
                        uiOutput("corr_group_heatmap_ui_pearson"),
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
          tagList(
            h4("Random Forest Results"),
            tabsetPanel(
              type = "tabs",
              tabPanel("Summary", verbatimTextOutput("rf_summary")),
              tabPanel(
                "Variable Importance",
                shinycssloaders::withSpinner(
                  plotOutput("rf_vipPlot", height = "400px"),
                  type = 8
                )
              ),
              if (!is.null(res$roc_plot)) {
                tabPanel(
                  "ROC Curve",
                  shinycssloaders::withSpinner(
                    plotOutput("rf_rocPlot", height = "400px"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$rfcv_plot)) {
                tabPanel(
                  "Cross-Validation",
                  shinycssloaders::withSpinner(
                    plotOutput("rf_rfcvPlot", height = "400px"),
                    type = 8
                  )
                )
              }
            )
          )
        },

        # --- XGBoost UI ---
        "Extreme Gradient Boosting (XGBoost)" = {
          tagList(
            h4("XGBoost Results"),
            tabsetPanel(
              type = "tabs",
              tabPanel("Summary", verbatimTextOutput("xgb_summary")),
              tabPanel(
                "Variable Importance",
                shinycssloaders::withSpinner(
                  plotOutput("xgb_vipPlot", height = "400px"),
                  type = 8
                )
              ),
              if (!is.null(res$roc_plot)) {
                tabPanel(
                  "ROC Curve",
                  shinycssloaders::withSpinner(
                    plotOutput("xgb_rocPlot", height = "400px"),
                    type = 8
                  )
                )
              }
            )
          )
        },

        # --- Volcano Plot UI ---
        "Volcano Plot" = {
          tagList(
            h4("Volcano Plot Results"),
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Plot",
                shinycssloaders::withSpinner(
                  plotOutput("volcPlotOutput", height = "400px"),
                  type = 8
                )
              ),
              tabPanel(
                "Statistics Table",
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("volcStats"),
                  type = 8
                )
              )
            ),
            div(
              style = "margin-top: .75rem;",
              p(
                tags$a(
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
          tagList(
            h4("Heatmap"),
            shinycssloaders::withSpinner(
              imageOutput("heatmapImage", height = "400px"),
              type = 8
            ),
            verbatimTextOutput("textResults")
          )
        },

        # --- Dual-Flashlight Plot UI ---
        "Dual-Flashlight Plot" = {
          tagList(
            h4("Dual-Flashlight Plot Results"),
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Plot",
                shinycssloaders::withSpinner(
                  plotOutput("dualflashPlotOutput", height = "400px"),
                  type = 8
                )
              ),
              tabPanel(
                "Statistics Table",
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("dualflashStats"),
                  type = 8
                )
              )
            ),
            div(
              style = "margin-top: .75rem;",
              p(
                tags$a(
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
          tagList(
            h4("Skewness and Kurtosis Results"),
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Skewness Plot",
                shinycssloaders::withSpinner(
                  plotOutput("skku_skewPlot", height = "400px"),
                  type = 8
                )
              ),
              tabPanel(
                "Kurtosis Plot",
                shinycssloaders::withSpinner(
                  plotOutput("skku_kurtPlot", height = "400px"),
                  type = 8
                )
              ),
              if (input$skku_print_raw) {
                tabPanel(
                  "Raw Data",
                  shinycssloaders::withSpinner(
                    DT::dataTableOutput("skku_raw_results"),
                    type = 8
                  )
                )
              },
              if (input$skku_print_log) {
                tabPanel(
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
          tagList(
            h4("Error-Bar Plot Results"),
            shinycssloaders::withSpinner(
              plotOutput("errorBarPlotOutput", height = "400px"),
              type = 8
            )
          )
        },
        "ANOVA" = {
          tagList(
            h4("ANOVA Results"),
            shinycssloaders::withSpinner(
              DT::dataTableOutput("anovaResults"),
              type = 8
            )
          )
        },
        "Two-Sample T-Test" = {
          tagList(
            h4("Two-Sample T-Test Results"),
            shinycssloaders::withSpinner(
              DT::dataTableOutput("ttestResults"),
              type = 8
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
              tagList,
              lapply(seq_along(res), function(i) {
                shinycssloaders::withSpinner(
                  plotOutput(paste0("dynamicPlot_", i), height = "400px"),
                  type = 8
                )
              })
            )
          }
        }
      )
    )
  }) %>%
    shiny::bindCache(
      selected_function(),
      input$new_fresh,
      input$new_reuse,
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
  }) %>%
    shiny::bindCache(
      analysis_inputs()$df,
      analysis_inputs()$func_name,
      analysis_inputs()$args,
      input$new_fresh,
      input$new_reuse,
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
          output[[paste0("dynamicPlot_", my_i)]] <- renderPlot({
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
    req(res, func_name)

    # --- sPLS-DA Rendering Logic ---
    if (
      func_name ==
        "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
    ) {
      if ("overall_indiv_plot" %in% names(res)) {
        # --- Single Analysis Rendering ---
        output$splsda_overallIndivPlot <- renderPlot({
          replayPlot(res$overall_indiv_plot)
        })
        output$splsda_overall3DPlot <- renderPlot({
          if (!is.null(res$overall_3D)) replayPlot(res$overall_3D)
        })
        output$splsda_overallRocPlot <- renderPlot({
          if (!is.null(res$overall_ROC)) replayPlot(res$overall_ROC)
        })
        output$splsda_overallCvPlot <- renderPlot({
          if (!is.null(res$overall_CV)) print(res$overall_CV)
        })
        output$splsda_vipIndivPlot <- renderPlot({
          if (!is.null(res$vip_indiv_plot)) replayPlot(res$vip_indiv_plot)
        })
        output$splsda_confMatrix <- renderPrint({
          if (!is.null(res$conf_matrix)) {
            cat(paste(res$conf_matrix, collapse = "\n"))
          }
        })

        output$splsda_loadingsUI <- renderUI({
          req(res$loadings)
          lapply(seq_along(res$loadings), function(i) {
            plotOutput(paste0("splsda_loading_plot_", i), height = "400px")
          })
        })
        lapply(seq_along(res$loadings), function(i) {
          output[[paste0("splsda_loading_plot_", i)]] <- renderPlot({
            replayPlot(res$loadings[[i]])
          })
        })

        output$splsda_vipScoresUI <- renderUI({
          req(res$vip_scores)
          lapply(seq_along(res$vip_scores), function(i) {
            plotOutput(paste0("splsda_vip_plot_", i), height = "400px")
          })
        })
        lapply(seq_along(res$vip_scores), function(i) {
          output[[paste0("splsda_vip_plot_", i)]] <- renderPlot({
            print(res$vip_scores[[i]])
          })
        })

        output$splsda_vipLoadingsUI <- renderUI({
          req(res$vip_loadings)
          lapply(seq_along(res$vip_loadings), function(i) {
            plotOutput(paste0("splsda_vip_loading_plot_", i), height = "400px")
          })
        })
        lapply(seq_along(res$vip_loadings), function(i) {
          output[[paste0("splsda_vip_loading_plot_", i)]] <- renderPlot({
            replayPlot(res$vip_loadings[[i]])
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
            )]] <- renderPlot({
              replayPlot(sub_res$overall_indiv_plot)
            })
            output[[paste0("splsda_vipIndivPlot_", current_trt)]] <- renderPlot(
              {
                if (!is.null(sub_res$vip_indiv_plot)) {
                  replayPlot(sub_res$vip_indiv_plot)
                }
              }
            )

            output[[paste0("splsda_loadingsUI_", current_trt)]] <- renderUI({
              req(sub_res$loadings)
              lapply(seq_along(sub_res$loadings), function(i) {
                plotOutput(
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
              )]] <- renderPlot({
                replayPlot(sub_res$loadings[[i]])
              })
            })

            output[[paste0("splsda_vipScoresUI_", current_trt)]] <- renderUI({
              req(sub_res$vip_scores)
              lapply(seq_along(sub_res$vip_scores), function(i) {
                plotOutput(
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
              )]] <- renderPlot({
                print(sub_res$vip_scores[[i]])
              })
            })

            output[[paste0("splsda_vipLoadingsUI_", current_trt)]] <- renderUI({
              req(sub_res$vip_loadings)
              lapply(seq_along(sub_res$vip_loadings), function(i) {
                plotOutput(
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
              )]] <- renderPlot({
                replayPlot(sub_res$vip_loadings[[i]])
              })
            })
            output[[paste0(
              "splsda_overall3DPlot_",
              current_trt
            )]] <- renderPlot({
              if (!is.null(sub_res$overall_3D)) {
                replayPlot(sub_res$overall_3D)
              }
            })
            output[[paste0(
              "splsda_overallRocPlot_",
              current_trt
            )]] <- renderPlot({
              if (!is.null(sub_res$overall_ROC)) {
                replayPlot(sub_res$overall_ROC)
              }
            })
            output[[paste0(
              "splsda_overallCvPlot_",
              current_trt
            )]] <- renderPlot({
              if (!is.null(sub_res$overall_CV)) {
                print(sub_res$overall_CV)
              }
            })
            output[[paste0("splsda_confMatrix_", current_trt)]] <- renderPrint({
              if (!is.null(sub_res$conf_matrix)) {
                cat(paste(sub_res$conf_matrix, collapse = "\n"))
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
      req(res)
      is_nested <- is.list(res) &&
        !is.null(names(res)) &&
        is.list(res[[1]]) &&
        "global_indiv_plot" %in% names(res[[1]])

      if (!is_nested) {
        # --- Single Analysis Rendering ---
        output$mint_splsda_global_plot <- renderPlot({
          replayPlot(res$global_indiv_plot)
        })
        output$mint_splsda_partial_plot <- renderPlot({
          replayPlot(res$partial_indiv_plot)
        })
        if (!is.null(res$partial_loadings_plots)) {
          output$mint_splsda_loadings_ui <- renderUI({
            req(res$partial_loadings_plots)
            tagList(lapply(seq_along(res$partial_loadings_plots), function(i) {
              plotOutput(
                paste0("mint_splsda_loading_plot_", i),
                height = "400px"
              )
            }))
          })

          # Use a for loop to correctly scope the index 'i'
          for (i in seq_along(res$partial_loadings_plots)) {
            local({
              # Use local() to ensure 'i' is captured correctly
              local_i <- i
              output[[paste0(
                "mint_splsda_loading_plot_",
                local_i
              )]] <- renderPlot({
                replayPlot(res$partial_loadings_plots[[local_i]])
              })
            })
          }
        }
        output$mint_splsda_corr_circle_plot <- renderPlot({
          replayPlot(res$correlation_circle_plot)
        })
        output$mint_splsda_cim_plot <- renderPlot(
          {
            if (
              !is.null(res$cim_obj) && inherits(res$cim_obj, "recordedplot")
            ) {
              replayPlot(res$cim_obj)
            }
          },
          height = 600
        )
        output$mint_splsda_roc_plot <- renderPlot({
          if (!is.null(res$roc_plot)) replayPlot(res$roc_plot)
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
              )]] <- renderPlot({
                replayPlot(sub_res$global_indiv_plot)
              })
              output[[paste0(
                "mint_splsda_partial_",
                current_trt
              )]] <- renderPlot({
                replayPlot(sub_res$partial_indiv_plot)
              })
              output[[paste0(
                "mint_splsda_loadings_",
                current_trt
              )]] <- renderUI({
                req(sub_res$partial_loadings_plots)
                lapply(seq_along(sub_res$partial_loadings_plots), function(i) {
                  plotOutput(
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
                )]] <- renderPlot({
                  replayPlot(sub_res$partial_loadings_plots[[i]])
                })
              })
              output[[paste0("mint_splsda_corr_", current_trt)]] <- renderPlot({
                replayPlot(sub_res$correlation_circle_plot)
              })
              output[[paste0("mint_splsda_cim_", current_trt)]] <- renderPlot(
                {
                  if (
                    !is.null(sub_res$cim_obj) &&
                      inherits(sub_res$cim_obj, "recordedplot")
                  ) {
                    replayPlot(sub_res$cim_obj)
                  }
                },
                height = 600
              )
              output[[paste0("mint_splsda_roc_", current_trt)]] <- renderPlot({
                if (!is.null(sub_res$roc_plot)) replayPlot(sub_res$roc_plot)
              })
            }
          })
        }
      }
    }
    if (func_name == "Principal Component Analysis (PCA)" && is.list(res)) {
      if ("overall_indiv_plot" %in% names(res)) {
        output$pca_indivPlot <- renderPlot({
          replayPlot(res$overall_indiv_plot)
        })
        output$pca_3DPlot <- renderPlot({
          if (!is.null(res$overall_3D)) replayPlot(res$overall_3D)
        })
        output$pca_screePlot <- renderPlot({
          if (!is.null(res$overall_scree_plot)) {
            replayPlot(res$overall_scree_plot)
          }
        })
        output$pca_biplot <- renderPlot({
          if (!is.null(res$biplot)) replayPlot(res$biplot)
        })
        output$pca_corrCircle <- renderPlot({
          if (!is.null(res$correlation_circle)) {
            replayPlot(res$correlation_circle)
          }
        })

        output$pca_loadingsUI <- shiny::renderUI({
          if (!is.null(res$loadings)) {
            tagList(lapply(seq_along(res$loadings), function(i) {
              plotOutput(paste0("pca_loadings_", i), height = "300px")
            }))
          }
        })

        if (!is.null(res$loadings)) {
          for (i in seq_along(res$loadings)) {
            local({
              local_i <- i
              output[[paste0("pca_loadings_", local_i)]] <- renderPlot({
                replayPlot(res$loadings[[local_i]])
              })
            })
          }
        }
      } else {
        for (lvl in names(res)) {
          local({
            currentGroup <- lvl
            subres <- res[[currentGroup]]
            output[[paste0("pca_indivPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(subres$overall_indiv_plot)) {
                replayPlot(subres$overall_indiv_plot)
              }
            })
            output[[paste0("pca_3DPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(subres$overall_3D)) replayPlot(subres$overall_3D)
            })
            output[[paste0("pca_screePlot_", currentGroup)]] <- renderPlot({
              if (!is.null(subres$overall_scree_plot)) {
                replayPlot(subres$overall_scree_plot)
              }
            })
            output[[paste0("pca_biplot_", currentGroup)]] <- renderPlot({
              if (!is.null(subres$biplot)) replayPlot(subres$biplot)
            })
            output[[paste0("pca_corrCircle_", currentGroup)]] <- renderPlot({
              if (!is.null(subres$correlation_circle)) {
                replayPlot(subres$correlation_circle)
              }
            })

            output[[paste0(
              "pca_loadingsUI_",
              currentGroup
            )]] <- shiny::renderUI({
              if (!is.null(subres$loadings)) {
                tagList(lapply(seq_along(subres$loadings), function(i) {
                  safeGroup <- gsub("[^A-Za-z0-9_]+", "_", currentGroup)
                  plotOutput(
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
                  )]] <- renderPlot({
                    replayPlot(subres$loadings[[local_i]])
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
      output$plsr_indivPlot <- renderPlot({
        req(res$scores_plot)
        replayPlot(res$scores_plot)
      })

      # Predicted vs Observed (recordedplot)
      output$plsr_predPlot <- renderPlot({
        req(res$pred_vs_obs)
        replayPlot(res$pred_vs_obs)
      })

      # Residuals vs Fitted (recordedplot)
      output$plsr_residPlot <- renderPlot({
        req(res$residuals_plot)
        replayPlot(res$residuals_plot)
      })

      # CV plot (recordedplot) + metrics text
      output$plsr_cvPlot <- renderPlot({
        req(res$cv_plot)
        replayPlot(res$cv_plot)
      })

      # Per-component loadings (each element is a recordedplot)
      output$plsr_loadingsUI <- renderUI({
        req(res$loadings)
        tagList(lapply(seq_along(res$loadings), function(i) {
          plotOutput(paste0("plsr_loading_plot_", i), height = "350px")
        }))
      })
      if (!is.null(res$loadings)) {
        for (i in seq_along(res$loadings)) {
          local({
            ii <- i
            output[[paste0("plsr_loading_plot_", ii)]] <- renderPlot({
              replayPlot(res$loadings[[ii]])
            })
          })
        }
      }

      # VIP barplots (ggplot objects — do NOT use replayPlot)
      output$plsr_vipUI <- renderUI({
        req(res$vip_scores)
        tagList(lapply(seq_along(res$vip_scores), function(i) {
          plotOutput(paste0("plsr_vip_plot_", i), height = "350px")
        }))
      })
      if (!is.null(res$vip_scores)) {
        for (i in seq_along(res$vip_scores)) {
          local({
            ii <- i
            output[[paste0("plsr_vip_plot_", ii)]] <- renderPlot({
              # ggplot object; just return it
              res$vip_scores[[ii]]
            })
          })
        }
      }

      # VIP>1 preview plots (recordedplot, conditional)
      output$plsr_vipIndivPlot <- renderPlot({
        req(res$vip_scores_indiv)
        replayPlot(res$vip_scores_indiv)
      })
      output$plsr_vipCVPlot <- renderPlot({
        req(res$vip_cv_plot)
        replayPlot(res$vip_cv_plot)
      })
    }

    # --- Correlation Rendering Logic ---
    # Tables
    output$corr_tbl_spearman <- DT::renderDataTable({
      req(res$spearman$table)
      DT::datatable(
        res$spearman$table,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE
      )
    })
    output$corr_tbl_pearson <- DT::renderDataTable({
      req(res$pearson$table)
      DT::datatable(
        res$pearson$table,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # Heatmaps (overall)
    output$corr_heatmap_spearman <- renderPlot({
      req(res$spearman$heat_mat)
      ggcorrplot::ggcorrplot(
        res$spearman$heat_mat,
        hc.order = TRUE,
        type = "full",
        lab = FALSE,
        show.diag = TRUE,
        outline.col = "white",
        ggtheme = ggplot2::theme_minimal()
      ) +
        ggplot2::labs(
          title = paste0("Spearman: ", res$corr_target, " vs all features"),
          x = NULL,
          y = NULL
        )
    })

    output$corr_heatmap_pearson <- renderPlot({
      req(res$pearson$heat_mat)
      ggcorrplot::ggcorrplot(
        res$pearson$heat_mat,
        hc.order = TRUE,
        type = "full",
        lab = FALSE,
        show.diag = TRUE,
        outline.col = "white",
        ggtheme = ggplot2::theme_minimal()
      ) +
        ggplot2::labs(
          title = paste0("Pearson: ", res$corr_target, " vs all features"),
          x = NULL,
          y = NULL
        )
    })

    # Per-group heatmaps (if any) — Spearman
    output$corr_group_heatmap_ui_spearman <- renderUI({
      req(res$group_mats$spearman)
      tabs <- lapply(names(res$group_mats$spearman), function(lv) {
        tabPanel(
          title = lv,
          plotOutput(
            paste0("corr_heatmap_grp_spear_", gsub("\\W+", "_", lv)),
            height = "600px"
          )
        )
      })
      do.call(tabsetPanel, c(list(type = "tabs"), tabs))
    })

    if (!is.null(res$group_mats$spearman)) {
      for (lv in names(res$group_mats$spearman)) {
        local({
          lvl <- lv
          output[[paste0(
            "corr_heatmap_grp_spear_",
            gsub("\\W+", "_", lvl)
          )]] <- renderPlot({
            mat <- res$group_mats$spearman[[lvl]]
            ggcorrplot::ggcorrplot(
              mat,
              hc.order = TRUE,
              type = "full",
              lab = FALSE,
              show.diag = TRUE,
              outline.col = "white",
              ggtheme = ggplot2::theme_minimal()
            ) +
              ggplot2::labs(title = paste0("Group: ", lvl), x = NULL, y = NULL)
          })
        })
      }
    }

    # Per-group heatmaps (if any) — Pearson
    output$corr_group_heatmap_ui_pearson <- renderUI({
      req(res$group_mats$pearson)
      tabs <- lapply(names(res$group_mats$pearson), function(lv) {
        tabPanel(
          title = lv,
          plotOutput(
            paste0("corr_heatmap_grp_pear_", gsub("\\W+", "_", lv)),
            height = "600px"
          )
        )
      })
      do.call(tabsetPanel, c(list(type = "tabs"), tabs))
    })

    if (!is.null(res$group_mats$pearson)) {
      for (lv in names(res$group_mats$pearson)) {
        local({
          lvl <- lv
          output[[paste0(
            "corr_heatmap_grp_pear_",
            gsub("\\W+", "_", lvl)
          )]] <- renderPlot({
            mat <- res$group_mats$pearson[[lvl]]
            ggcorrplot::ggcorrplot(
              mat,
              hc.order = TRUE,
              type = "full",
              lab = FALSE,
              show.diag = TRUE,
              outline.col = "white",
              ggtheme = ggplot2::theme_minimal()
            ) +
              ggplot2::labs(title = paste0("Group: ", lvl), x = NULL, y = NULL)
          })
        })
      }
    }

    # --- Random Forest Rendering Logic ---
    if (func_name == "Random Forest") {
      output$rf_summary <- renderPrint({
        cat(res$summary_text)
      })
      output$rf_vipPlot <- renderPlot({
        print(res$vip_plot)
      })
      output$rf_rocPlot <- renderPlot({
        if (!is.null(res$roc_plot)) print(res$roc_plot)
      })
      output$rf_rfcvPlot <- renderPlot({
        if (!is.null(res$rfcv_plot)) print(res$rfcv_plot)
      })
    }

    # --- XGBoost Rendering Logic ---
    if (func_name == "Extreme Gradient Boosting (XGBoost)") {
      output$xgb_summary <- renderPrint({
        cat(res$summary_text)
      })
      output$xgb_vipPlot <- renderPlot({
        print(res$plot)
      })
      output$xgb_rocPlot <- renderPlot({
        if (!is.null(res$roc_plot)) print(res$roc_plot)
      })
    }

    # --- Volcano Plot Rendering Logic ---
    if (func_name == "Volcano Plot") {
      output$volcPlotOutput <- renderPlot({
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
      output$heatmapImage <- renderImage(
        {
          list(src = res, contentType = 'image/png', alt = "Heatmap")
        },
        deleteFile = FALSE
      )
    }
    # --- Dual-Flashlight Plot Rendering ---
    if (func_name == "Dual-Flashlight Plot") {
      output$dualflashPlotOutput <- renderPlot({
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
      output$skku_skewPlot <- renderPlot({
        print(res$p_skew)
      })
      output$skku_kurtPlot <- renderPlot({
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
          output[[paste0("dynamicPlot_", my_i)]] <- renderPlot({
            res[[my_i]]
          })
        })
      })
    }
  })

  output$textResults <- shiny::renderPrint({
    res <- analysisResult()
    print(res)
  })
  # --- ANOVA table ---
  output$anovaResults <- DT::renderDataTable(
    {
      res <- analysisResult()
      # Ensure this renderer only runs when ANOVA was the selected function
      req(selected_function() == "ANOVA")
      res$out_df
    },
    options = list(pageLength = 10, scrollX = TRUE)
  )

  # --- Two-Sample T-Test table ---
  output$ttestResults <- DT::renderDataTable(
    {
      res <- analysisResult()
      # Ensure this renderer only runs when T-Test was the selected function
      req(selected_function() == "Two-Sample T-Test")
      res$out_df
    },
    options = list(pageLength = 10, scrollX = TRUE)
  )

  output$errorBarPlotOutput <- shiny::renderPlot({
    req(analysisResult())
    res <- analysisResult()
    print(res)
  })

  # Render the download UI conditionally
  output$download_ui <- shiny::renderUI({
    res <- analysisResult()
    # Show download button only if the result is of a type that generates a downloadable plot
    if (
      !is.null(res) &&
        !selected_function() %in% c("ANOVA", "Two-Sample T-Test") &&
        !inherits(res, "data.frame") &&
        (is.list(res) || is.character(res))
    ) {
      tagList(
        textInput(
          "download_filename",
          "Enter filename for download:",
          value = "Cytokine_Analysis_Results"
        ),
        downloadButton("download_output", "Download Results as PDF")
      )
    }
  })

  # Download handler
  output$download_output <- shiny::downloadHandler(
    filename = function() {
      paste0(input$download_filename, ".pdf")
    },
    content = function(file) {
      pdf(file, width = 10, height = 8)
      plots <- reactivePlots()
      # print each ggplot in order
      for (p in plots) {
        print(p)
      }
      dev.off()
    }
  )

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
  shiny::observeEvent(selected_function(), {
    userState$selected_function <- selected_function()
  })
  # For Boxplots
  shiny::observeEvent(input$bp_bin_size, {
    userState$bp_bin_size <- input$bp_bin_size
  })
  shiny::observeEvent(input$bp_mf_row, {
    userState$bp_mf_row <- input$bp_mf_row
  })
  shiny::observeEvent(input$bp_y_lim, {
    userState$bp_y_lim <- input$bp_y_lim
  })
  # For Enhanced Boxplots
  shiny::observeEvent(input$bp2_mf_row, {
    userState$bp2_mf_row <- input$bp2_mf_row
  })
  shiny::observeEvent(input$bp2_y_lim, {
    userState$bp2_y_lim <- input$bp2_y_lim
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
  shiny::observeEvent(input$plsr_response_var, {
    userState$plsr_response_var <- input$plsr_response_var
  })
  shiny::observeEvent(input$plsr_comp_num, {
    userState$plsr_comp_num <- input$plsr_comp_num
  })
  shiny::observeEvent(input$plsr_sparse, {
    userState$plsr_sparse <- input$plsr_sparse
  })
  shiny::observeEvent(input$plsr_keepX, {
    df <- filteredData()
    req(df)
    computed_default <- sum(sapply(df, is.numeric))
    # Mark as manually changed if the input does not equal the computed default.
    userState$plsr_keepX_manual <- (input$plsr_keepX != computed_default)
    # Also, update the stored value so it’s available if the user has modified it.
    userState$plsr_keepX <- input$plsr_keepX
  })
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
  shiny::observeEvent(input$use_splsda_multilevel, {
    userState$splsda_multilevel <- input$use_splsda_multilevel
  })
  shiny::observeEvent(input$splsda_multilevel, {
    userState$splsda_multilevel <- input$splsda_multilevel
  })
  shiny::observeEvent(input$splsda_var_num, {
    df <- filteredData()
    req(df)
    computed_default <- sum(sapply(df, is.numeric))
    # Mark as manually changed if the input does not equal the computed default.
    userState$splsda_var_num_manual <- (input$splsda_var_num !=
      computed_default)
    # Also, update the stored value so it’s available if the user has modified it.
    userState$splsda_var_num <- input$splsda_var_num
  })

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
    req(df)
    computed_default <- sum(sapply(df, is.numeric))
    # Mark as manually changed if the input does not equal the computed default.
    userState$mint_splsda_var_num_manual <- (input$mint_splsda_var_num !=
      computed_default)
    # Also, update the stored value so it’s available if the user has modified it.
    userState$mint_splsda_var_num <- input$mint_splsda_var_num
  })
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
  # remember which function‐picker they chose
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

  shiny::observeEvent(input$menu_ANOVA, {
    selected_function("ANOVA")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_t_test, {
    selected_function("Two-Sample T-Test")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_boxplots, {
    selected_function("Boxplots")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_enhanced_boxplots, {
    selected_function("Enhanced Boxplots")
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
}
