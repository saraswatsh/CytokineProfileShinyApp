library(shiny)
library(mixOmics)
library(dplyr)
library(tidyr)
library(rlang)
library(broom)
library(ggplot2)
library(readxl)
library(bslib)
library(shinyhelper)
library(DT)
library(shinyFeedback)
library(skimr)
library(shinycssloaders)

## Define server logic
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
    bslib::bs_theme(bootswatch = "cyborg")
  )

  # Rebuild the theme when the user picks a new skin
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
  userState <- shiny::reactiveValues(
    # General state
    selected_columns = NULL,
    selected_function = NULL,

    # Built=in Data built‑in tracking:
    use_builtin = FALSE,
    built_in_choice = NULL,

    # ANOVA options
    anova_log2 = NULL,

    # Boxplots options
    bp_bin_size = NULL,
    bp_mf_row = NULL,
    bp_y_lim = NULL,
    bp_log2 = NULL,

    # Enhanced Boxplots options
    bp2_mf_row = NULL,
    bp2_log2 = NULL,
    bp2_y_lim = NULL,

    # Error-BarPlot
    eb_group_col = NULL,
    eb_p_lab = NULL,
    eb_es_lab = NULL,
    eb_class_symbol = NULL,
    eb_x_lab = NULL,
    eb_y_lab = NULL,
    eb_title = NULL,
    eb_log2 = NULL,

    # Dual-Flashlight Plot options
    df_group_var = NULL,
    df_cond1 = NULL,
    df_cond2 = NULL,
    df_ssmd_thresh = NULL,
    df_log2fc_thresh = NULL,
    df_top_labels = NULL,

    # Heatmap options
    hm_log2 = NULL,
    hm_annotation = NULL,

    # PCA options
    pca_group_col = NULL,
    pca_group_col2 = NULL,
    pca_comp_num = NULL,
    pca_log2 = NULL,
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

    # Skewness/Kurtosis options
    skku_group_cols = NULL,
    skku_print_raw = NULL,
    skku_print_log = NULL,

    # sPLS-DA options
    splsda_group_col = NULL,
    splsda_group_col2 = NULL,
    splsda_var_num = NULL,
    splsda_var_num_manual = FALSE,
    splsda_cv_opt = NULL,
    splsda_fold_num = NULL,
    splsda_log2 = NULL,
    splsda_comp_num = NULL,
    splsda_pch = NULL,
    splsda_style = NULL,
    splsda_roc = NULL,
    splsda_ellipse = NULL,
    splsda_bg = NULL,
    splsda_conf_mat = NULL,
    splsda_colors = NULL,
    splsda_multilevel = NULL,

    # Two-Sample T-Test options
    ttest_log2 = NULL,

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
    xgb_plot_roc = NULL
  )

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
      return(df)
    }
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
    selectInput(
      "built_in_choice",
      "Select Built-in Data:",
      choices = builtInList,
      selected = isolate(userState$built_in_choice) %||% builtInList[[1]]
    )
  })
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
      userData()
    },
    options = list(
      pageLength = 5, # initial page size
      lengthMenu = list(
        c(5, 10, 25, 50, 100), # values
        c("5", "10", "25", "50", "100") # labels
      ),
      scrollX = TRUE,
      scrollY = TRUE
    )
  )
  output$summary_stats_table <- DT::renderDT({
    req(userData())
    df <- userData()

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
        lengthMenu = c(5, 10, 25, 50, 100),
        scrollX = TRUE
      )
    )
  })
  # Simple validations & warnings
  shiny::observeEvent(userData(), {
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

  # Reactive values to store the selection from our new custom buttons
  selected_stat_func <- reactiveVal("ANOVA")
  selected_exploratory_func <- reactiveVal("Boxplots")
  selected_multivariate_func <- reactiveVal(
    "Principle Component Analysis (PCA)"
  )
  selected_ml_func <- reactiveVal("Random Forest")

  filteredData <- shiny::reactive({
    df <- userData()
    req(df)
    currentCols <- if (!is.null(userState$selected_columns)) {
      intersect(userState$selected_columns, names(df))
    } else {
      intersect(input$selected_columns, names(df))
    }

    if (!is.null(currentCols) && length(currentCols) > 0) {
      df <- df[, currentCols, drop = FALSE]
    }
    factor_cols <- names(df)[sapply(
      df,
      function(x) is.factor(x) || is.character(x)
    )]
    if (length(factor_cols) > 0) {
      for (col in factor_cols) {
        filter_vals <- input[[paste0("filter_", col)]]
        if (!is.null(filter_vals)) {
          df <- df[df[[col]] %in% filter_vals, ]
        }
      }
    }
    df
  })
  selected_function <- shiny::reactive({
    cats <- input$analysis_categories
    req(cats)
    switch(
      cats,
      "stat_tests" = selected_stat_func(),
      "exploratory" = selected_exploratory_func(),
      "multivariate" = selected_multivariate_func(),
      "machine" = selected_ml_func(),
      NULL
    )
  })
  output$column_selection_ui <- shiny::renderUI({
    df <- userData()
    if (is.null(df)) {
      return(NULL)
    }
    checkboxGroupInput(
      "selected_columns",
      "Select Columns:",
      choices = names(df),
      selected = if (!is.null(userState$selected_columns)) {
        userState$selected_columns
      } else {
        names(df)
      }
    )
  })

  output$select_buttons_ui <- shiny::renderUI({
    df <- userData()
    if (is.null(df)) {
      return(NULL)
    }
    tagList(
      fluidRow(
        div(
          style = "text-align: left;",
          div(
            style = "display: inline-block; margin-right: 10px;",
            actionButton("select_all", "Select All")
          ),
          div(
            style = "display: inline-block;",
            actionButton("deselect_all", "Deselect All")
          )
        )
      ),
      br()
    )
  })
  output$conditional_filter_ui <- renderUI({
    df <- userData()
    selected_cols <- input$selected_columns

    # Return NULL if no columns are selected yet
    if (is.null(selected_cols)) {
      return(NULL)
    }

    # Check if any of the *selected* columns are categorical
    sub_df <- df[, selected_cols, drop = FALSE]
    is_categorical <- sapply(sub_df, function(x) {
      is.character(x) || is.factor(x)
    })

    # Only if at least one categorical column is selected, show the filter UI
    if (any(is_categorical)) {
      column(
        6,
        card(
          card_header(
            class = "bg-primary",
            "2. Apply Filters (Optional)"
          ),
          card_body(
            bslib::accordion(
              open = FALSE, # Start closed
              bslib::accordion_panel(
                "Filter by Categorical Values",
                uiOutput("filter_ui"), # This is your existing filter UI output
                icon = fontawesome::fa("filter")
              )
            )
          )
        )
      )
    } else {
      # Otherwise, render nothing
      NULL
    }
  })
  shiny::observeEvent(input$select_all, {
    df <- userData()
    if (!is.null(df)) {
      updateCheckboxGroupInput(
        session,
        "selected_columns",
        choices = names(df),
        selected = names(df)
      )
    }
  })
  shiny::observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(
      session,
      "selected_columns",
      selected = character(0)
    )
  })

  output$filter_ui <- shiny::renderUI({
    df <- filteredData()
    if (is.null(df)) {
      return(NULL)
    }
    factor_cols <- names(df)[sapply(
      df,
      function(x) is.factor(x) || is.character(x)
    )]
    if (length(factor_cols) == 0) {
      return(NULL)
    }
    ui_list <- lapply(factor_cols, function(col) {
      all_levels <- sort(unique(userData()[[col]]))
      selected_levels <- sort(unique(df[[col]]))
      selectizeInput(
        inputId = paste0("filter_", col),
        label = paste("Filter", col, "(select levels)"),
        choices = all_levels,
        selected = selected_levels,
        multiple = TRUE,
        options = list(plugins = c("remove_button", "restore_on_backspace"))
      )
    })
    do.call(tagList, ui_list)
  })

  ## ---------------------------
  ## Function Options UI (Step 3)
  ## ---------------------------

  # Colors vector for specific functions
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

  # Function Options UI
  output$function_options_ui <- shiny::renderUI({
    func <- selected_function()
    req(func)

    userState$selected_function <- func
    func_name <- func
    ui_list <- list()

    userState$selected_function <- func
    func_name <- func
    ui_list <- list()

    switch(
      func_name,
      "ANOVA" = {
        ui_list <- tagList(
          checkboxInput(
            "anova_log2",
            label = helper(
              type = "inline",
              title = "Apply log2 transformation",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Apply log2 transformation</span>"
              ),
              content = "Apply a log2 transformation to the data before performing the ANOVA test.
                               This transformation can help manage data that span a wide range of values.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$anova_log2) %||% FALSE
          )
        )
      },
      "Boxplots" = {
        ui_list <- tagList(
          numericInput(
            "bp_bin_size",
            label = helper(
              type = "inline",
              title = "Bin size for boxplots",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Bin Size</span>"
              ),
              content = "Determines the number of columns (variables) to group together in each set of box plots. 
                        For example, a bin size of 25 will display box plots for up to 25 columns (variables) at a time. 
                        If there are more columns, multiple sets of box plots will be generated.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$bp_bin_size) %||% 25,
            min = 1
          ),

          textInput(
            "bp_mf_row",
            label = helper(
              type = "inline",
              title = "Graphs per Row and Column",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Graphs per Row and Columns</span><br>(rows, cols; comma-separated)"
              ),
              content = "Specify the layout of multiple box plots on the plotting area, defined as (rows, cols). For example, '2,2' will arrange up to 4 box plots in a 2x2 grid.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$bp_mf_row) %||% "1,1"
          ),

          textInput(
            "bp_y_lim",
            label = helper(
              type = "inline",
              title = "Y-axis Limits",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Y-axis Limits</span><br>(min, max; comma-separated; leave blank for auto)"
              ),
              content = "Set the minimum and maximum values for the Y-axis, entered as (min, max) e.g., '0,100'. 
              Leave blank for automatic scaling based on data range. 
              This controls the vertical range displayed on the plot.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$bp_y_lim) %||% ""
          ),

          checkboxInput(
            "bp_log2",
            label = helper(
              type = "inline",
              title = "Apply log2 transformation",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Apply log2 transformation</span>"
              ),
              content = "Apply a log2 transformation to the data before generating the boxplots. This transformation can
                                                    help manage data that span a wide range of values.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$bp_log2) %||% FALSE
          )
        )
      },
      "Enhanced Boxplots" = {
        ui_list <- tagList(
          textInput(
            "bp2_mf_row",
            label = helper(
              type = "inline",
              title = "Graphs per Row and Column",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Graphs per Row and Columns</span><br>(rows, cols; comma-separated)"
              ),
              content = "Specify the layout of multiple box plots on the plotting area, defined as (rows, cols). For example, '2,2' will arrange up to 4 box plots in a 2x2 grid.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$bp2_mf_row) %||% "1,1"
          ),
          textInput(
            "bp2_y_lim",
            label = helper(
              type = "inline",
              title = "Y-axis Limits",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Y-axis Limits</span><br>(min, max; comma-separated; leave blank for auto)"
              ),
              content = "Set the minimum and maximum values for the Y-axis, entered as (min, max) e.g., '0,100'. Leave blank for automatic scaling based on data range. This controls the vertical range displayed on the plot.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$bp2_y_lim) %||% ""
          ),
          checkboxInput(
            "bp2_log2",
            label = helper(
              type = "inline",
              title = "Apply log2 transformation",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Apply log2 transformation</span>"
              ),
              content = "Apply a log2 transformation to the data before generating the boxplots.
                                             This transformation can help manage data that span a wide range of values.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$bp2_log2) %||% FALSE
          )
        )
      },
      "Error-BarPlot" = {
        df <- filteredData()
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- names(df)
        # Identify candidate grouping columns (categorical variables)
        cat_vars <- names(df)[sapply(
          df,
          function(x) is.factor(x) || is.character(x)
        )]

        ui_list <- tagList(
          if (length(cat_vars) > 0) {
            selectInput(
              "eb_group_col",
              label = helper(
                type = "inline",
                title = "Grouping Variable",
                icon = "fas fa-question-circle",
                shiny_tag = HTML(
                  "<span style='margin-right: 15px;'>Grouping Variable</span>"
                ),
                content = "The column to use for grouping the data. For example, a column that
                        specifies categories such as 'Control' or 'Treatment'.",
                if (
                  input$theme_choice == "darkly" ||
                    input$theme_choice == "cyborg"
                ) {
                  colour = "red"
                } else {
                  colour = "blue"
                }
              ),
              ,
              choices = cols,
              selected = isolate(userState$eb_group_col) %||% cols[0]
            )
          },
          checkboxInput(
            "eb_log2",
            label = helper(
              type = "inline",
              title = "Apply log2 transformation",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Apply log2 transformation</span>"
              ),
              content = "Apply a log2 transformation to the numeric data before generating the error-bar plot. This can be useful for data with wide ranges or skewed distributions, helping to stabilize variances and make patterns more visible.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$eb_log2) %||% FALSE
          ),
          checkboxInput(
            "eb_p_lab",
            label = helper(
              type = "inline",
              title = "Display P-value Labels",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Display P-value Labels</span>"
              ),
              content = "Display P-value labels above the bar plots to indicate whether results are
                significant or not.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$eb_p_lab) %||% FALSE
          ),
          checkboxInput(
            "eb_es_lab",
            label = helper(
              type = "inline",
              title = "Display Effect Size Labels",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Display Effect Size Labels</span>"
              ),
              content = HTML(paste0(
                "Display effect size labels above the bar plots to indicate the effect observed by strictly
                standardized mean differences (SSMD).
                Learn more about SSMD at <a href='https://doi.org/10.1177/1087057111405851' target='_blank' rel='noopener noreferrer'>link 1</a>, and 
                <a href='https://doi.org/10.1198/sbr.2009.0074' target='_blank' rel='noopener noreferrer'>link 2</a>"
              )),
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$eb_es_lab) %||% FALSE
          ),
          checkboxInput(
            "eb_class_symbol",
            label = helper(
              type = "inline",
              title = "Display Class Symbol",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Display Class Symbol</span>"
              ),
              content = "If checked, P-values and effect sizes (if displayed) will be represented by symbols (e.g., asterisks for significance levels) instead of their numeric values.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$eb_class_symbol) %||% FALSE
          ),
          textInput(
            "eb_x_lab",
            label = helper(
              type = "inline",
              title = "X-Axis Label",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>X-Axis Label</span>"
              ),
              content = "The X-axis label you would like to have in the plot.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$eb_x_lab) %||% "Cytokine"
          ),
          textInput(
            "eb_y_lab",
            label = helper(
              type = "inline",
              title = "Y-Axis Label",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Y-Axis Label</span>"
              ),
              content = "The Y-axis label you would like to have in the plot.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$eb_y_lab) %||% "Concentration"
          ),
          textInput(
            "eb_title",
            label = helper(
              type = "inline",
              title = "Title",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Title</span>"
              ),
              content = "Title of the plot.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$eb_title) %||% "Error-BarPlot"
          )
        )
      },
      "Dual-Flashlight Plot" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- names(df)
        ui_list <- tagList(
          selectInput(
            "df_group_var",
            label = helper(
              type = "inline",
              title = "Grouping Variable",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Grouping Variable</span>"
              ),
              content = "The column to use for grouping the data. For example, a column that
                                          specifies categories such as 'Control' or 'Treatment'.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            ,
            choices = cols,
            selected = isolate(userState$df_group_var) %||% cols[1]
          ),
          uiOutput("df_conditions_ui"),
          numericInput(
            "df_ssmd_thresh",
            label = helper(
              type = "inline",
              title = "SSMD Threshold",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>SSMD Threshold</span>"
              ),
              content = "The threshold for the SSMD (strictly standardized mean difference) value. 
                                           SSMD is a measure of how different two groups are. A higher threshold means that 
                                           only larger differences are considered significant.
                                           Learn more about SSMD at <a href='https://doi.org/10.1177/1087057111405851' target='_blank' rel='noopener noreferrer'>link 1</a>, and 
                <a href='https://doi.org/10.1198/sbr.2009.0074' target='_blank' rel='noopener noreferrer'>link 2</a>",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$df_ssmd_thresh) %||% 1,
            min = 0
          ),
          numericInput(
            "df_log2fc_thresh",
            label = helper(
              type = "inline",
              title = "Log2 Fold Change Threshold",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Log2 Fold Change Threshold</span>"
              ),
              content = "The threshold for the log2 fold change value.
                                           Fold change shows the ratio of difference between groups on a logarithmic scale; 
                                           for example, a value of 1 represents a doubling or halving.
                                           Learn more about fold change at <a href='https://doi.org/10.1177/1087057110381919' target='_blank' rel='noopener noreferrer'>link 1</a>, and 
                <a href='https://doi.org/10.1177/1087057110381783' target='_blank' rel='noopener noreferrer'>link 2</a>",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$df_log2fc_thresh) %||% 1,
            min = 0
          ),
          numericInput(
            "df_top_labels",
            label = helper(
              type = "inline",
              title = "Top Labels",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Top Labels</span>"
              ),
              content = "The number of top labels to display on the plot. Usually, the top labels are 
                                           the most significant points.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$df_top_labels) %||% 15,
            min = 1
          )
        )
      },
      "Heatmap" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        ann_choices <- names(df)[sapply(
          df,
          function(x) is.factor(x) || is.character(x)
        )]
        ui_list <- tagList(
          checkboxInput(
            "hm_log2",
            label = helper(
              type = "inline",
              title = "Apply log2 transformation",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Apply log2 transformation</span>"
              ),
              content = "Apply a log2 transformation to the data before generating the heatmap.
                               This transformation can help manage data that span a wide range of values.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$hm_log2) %||% FALSE
          ),
          selectInput(
            "hm_annotation",
            label = helper(
              type = "inline",
              title = "Annotation Column",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Annotation Column</span>"
              ),
              content = "The column to use for annotating the heatmap.
                             This column will add color coding to help differentiate groups",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = ann_choices,
            selected = isolate(userState$hm_annotation) %||%
              (if (length(ann_choices) > 0) ann_choices[1] else NULL)
          )
        )
      },
      "Principle Component Analysis (PCA)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- names(df)
        ui_list <- tagList(
          selectInput(
            "pca_group_col",
            label = helper(
              type = "inline",
              title = "Grouping Column 1",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Grouping Column 1</span>"
              ),
              content = "The first column to use for grouping the data.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = cols,
            selected = isolate(userState$pca_group_col) %||% cols[1]
          ),
          selectInput(
            "pca_group_col2",
            label = helper(
              type = "inline",
              title = "Grouping Column 2",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Grouping Column 2</span>"
              ),
              content = "An optional second column to use for grouping the data.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = cols,
            selected = isolate(userState$pca_group_col2) %||% cols[1]
          ),
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
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$pca_comp_num) %||% 2,
            min = 2
          ),
          selectizeInput(
            "pca_colors",
            label = helper(
              type = "inline",
              title = "Select Colors for PCA Plot (Optional)",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Select Colors for PCA Plot (Optional)</span>"
              ),
              content = "The color palette to use for the PCA plot. Select the number of colors to match the number of categories in grouping column 1.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = allowed_colors,
            multiple = TRUE,
            options = list(
              placeholder = "Select Colors (Optional)",
              plugins = c("remove_button", "restore_on_backspace")
            )
          ),
          checkboxInput(
            "pca_log2",
            label = helper(
              type = "inline",
              title = "Apply log2 transformation",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Apply log2 transformation</span>"
              ),
              content = "Apply a log2 transformation to the data before generating the PCA plot.
                               This transformation can help manage data that span a wide range of values.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$pca_log2) %||% FALSE
          ),
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
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$pca_ellipse) %||% FALSE
          ),
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
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = c("2D", "3D"),
            selected = isolate(userState$pca_style) %||% "2D"
          ),
          selectizeInput(
            "pca_pch",
            label = helper(
              type = "inline",
              title = "Plotting Symbols",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Plotting Symbols</span>"
              ),
              content = "Select plotting character (PCH) symbols for data points. If using a second grouping column (Grouping Column 2), the number of symbols should match the number of categories in that column to assign unique shapes. If not, it can correspond to Grouping Column 1.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = pch_choices,
            selected = pch_choices[c(17, 5)],
            multiple = TRUE,
            options = list(
              placeholder = "Select Symbols",
              plugins = c("remove_button", "restore_on_backspace"),
              create = TRUE
            )
          )
        )
      },
      "Random Forest" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- names(df)
        ui_list <- tagList(
          selectInput(
            "rf_group_col",
            label = helper(
              type = "inline",
              title = "Grouping Column",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Grouping Column</span>"
              ),
              content = "Select the column that contains the outcome or class labels you want the Random Forest model to predict.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = cols,
            selected = isolate(userState$rf_group_col) %||% cols[1]
          ),
          numericInput(
            "rf_ntree",
            label = helper(
              type = "inline",
              title = "Number of Trees",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Number of Trees</span>"
              ),
              content = "The number of trees to grow in the random forest model.
                              Each tree is a simple model; more trees can improve predictions but take longer to compute.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$rf_ntree) %||% 500,
            min = 1
          ),
          numericInput(
            "rf_mtry",
            label = helper(
              type = "inline",
              title = "Number of Variables to Split",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Number of Variables to Split</span>"
              ),
              content = "The number of variables to randomly select at each split.
                              At each decision point, the model randomly considers this number of variables, which helps improve model accuracy.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$rf_mtry) %||% 5,
            min = 1
          ),
          numericInput(
            "rf_train_fraction",
            label = helper(
              type = "inline",
              title = "Train Fraction",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Train Fraction</span>"
              ),
              content = "The fraction of the data to use for training the random forest model.
                              The remainder is used for testing the model.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$rf_train_fraction) %||% 0.7,
            min = 0.1,
            max = 0.9,
            step = 0.1
          ),
          checkboxInput(
            "rf_plot_roc",
            label = helper(
              type = "inline",
              title = "Plot ROC",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Plot ROC (Binary Comparison Only)</span>"
              ),
              content = "Plot the Recieving Operating Characteristic (ROC) curve for the random forest model.
                               Requires comparison to be a binary comparison (at most 2 groups). The ROC curve helps evaluate 
                               the model’s performance in distinguishing between two groups. 
                               Learn more about ROC at <a href='https://doi.org/10.4097/kja.21209' target='_blank' rel='noopener noreferrer'> this link</a>.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$rf_plot_roc) %||% FALSE
          ),
          checkboxInput(
            "rf_run_rfcv",
            label = helper(
              type = "inline",
              title = "Run RFCV?",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Run RFCV</span>"
              ),
              content = "Perform cross-validation to evaluate model performance with a decreasing number of predictors (variables). This helps identify a smaller set of important variables without significant loss of predictive accuracy.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$rf_run_rfcv) %||% TRUE
          ),
          conditionalPanel(
            condition = "input.rf_run_rfcv == true",
            numericInput(
              "rf_k_folds",
              label = helper(
                type = "inline",
                title = "Number of Folds",
                icon = "fas fa-question-circle",
                shiny_tag = HTML(
                  "<span style='margin-right: 15px;'>Number of Folds</span>"
                ),
                content = "The number of folds to use for cross-validation in the RFCV process.
                                Folds are subsets of the data used to train and test the model. More folds can improve accuracy but take longer to compute.",
                if (
                  input$theme_choice == "darkly" ||
                    input$theme_choice == "cyborg"
                ) {
                  colour = "red"
                } else {
                  colour = "blue"
                }
              ),
              value = isolate(userState$rf_k_folds) %||% 5,
              min = 2
            ),
            numericInput(
              "rf_step",
              label = helper(
                type = "inline",
                title = "Step Size",
                icon = "fas fa-question-circle",
                shiny_tag = HTML(
                  "<span style='margin-right: 15px;'>Step Size</span>"
                ),
                content = "The step size to use for the RFCV process. Must be between 0.1 and 0.9.
                                The value controls how quickly the model eliminates variables. A smaller step size can help find the best variables.",
                if (
                  input$theme_choice == "darkly" ||
                    input$theme_choice == "cyborg"
                ) {
                  colour = "red"
                } else {
                  colour = "blue"
                }
              ),
              value = isolate(userState$rf_step) %||% 0.5,
              min = 0.1,
              max = 0.9,
              step = 0.1
            )
          )
        )
      },
      "Skewness/Kurtosis" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        ui_list <- tagList(
          selectInput(
            "skku_group_cols",
            label = helper(
              type = "inline",
              title = "Grouping Columns (Optional)",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Grouping Columns</span>"
              ),
              content = "Select one or more categorical columns to group the data by. Skewness and kurtosis will be calculated for numeric variables separately for each group.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = names(df),
            multiple = TRUE,
            selected = isolate(userState$skku_group_cols)
          ),
          # Checkbox for printing raw results
          checkboxInput(
            "skku_print_raw",
            label = helper(
              type = "inline",
              title = "Print Raw Results",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Print Raw Results</span>"
              ),
              content = "Display the calculated skewness and kurtosis values for the original numeric data.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$skku_print_raw) %||% FALSE
          ),
          # Checkbox for printing log transformed results
          checkboxInput(
            "skku_print_log",
            label = helper(
              type = "inline",
              title = "Print Log-Transformed Results",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Print Log-Transformed Results</span>"
              ),
              content = "Display the calculated skewness and kurtosis values after applying a log2 transformation to the numeric data. This can show how transformation affects these distribution shape measures.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$skku_print_log) %||% FALSE
          )
        )
      },
      "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        default_num_vars <- sum(sapply(df, is.numeric))

        # If the user hasn't manually changed the value, update it.
        if (!isTRUE(userState$splsda_var_num_manual)) {
          userState$splsda_var_num <- default_num_vars
        }

        cols <- names(df)
        ui_list <- tagList(
          selectInput(
            "splsda_group_col",
            label = helper(
              type = "inline",
              title = "Grouping Column 1",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Grouping Column 1</span>"
              ),
              content = "Select the column containing the class labels or groups that you want to discriminate between using sPLS-DA.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = cols,
            selected = isolate(userState$splsda_group_col) %||% cols[1]
          ),
          selectInput(
            "splsda_group_col2",
            label = helper(
              type = "inline",
              title = "Grouping Column 2",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Grouping Column 2</span>"
              ),
              content = "Optional: Select a second column for visualizing additional grouping information on the sPLS-DA plots distinct from the primary grouping column used for discrimination.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = cols,
            selected = isolate(userState$splsda_group_col2) %||% cols[1]
          ),
          selectInput(
            "splsda_multilevel",
            label = helper(
              type = "inline",
              title = "Multilevel Column",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Multilevel Column</span>"
              ),
              content = "Optional: Select a column that describes the categorizations of repeated measurements to be used for multilevel analysis.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = cols,
            selected = isolate(userState$splsda_multilevel) %||% cols[1]
          ),
          numericInput(
            "splsda_var_num",
            label = helper(
              type = "inline",
              title = "Number of Variables",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Number of Variables</span>"
              ),
              content = "Specify the number of variables to select/retain on each component during the sPLS-DA model building. This is a key parameter for achieving sparsity.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = userState$splsda_var_num,
            min = 1
          ),
          selectizeInput(
            "splsda_colors",
            label = helper(
              type = "inline",
              title = "Select Colors for sPLS-DA Plot (Optional)",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Select Colors for sPLS-DA Plot (Optional)</span>"
              ),
              content = "The color palette to use for the sPLS-DA plot. Select the number of colors to match the number of categories in grouping column 1.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = allowed_colors,
            multiple = TRUE,
            options = list(
              placeholder = "Select Colors (Optional)",
              plugins = c("remove_button", "restore_on_backspace")
            )
          ),
          selectInput(
            "splsda_cv_opt",
            label = helper(
              type = "inline",
              title = "Cross-Validation Option",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Cross-Validation Option</span>"
              ),
              content = "The cross-validation option to use for the sPLS-DA analysis.
                             Cross-validation helps evaluate the model's performance. Choose between None, LOOCV, and Mfold.
                             LOOCV = Leave-One-Out Cross-Validation (LOOCV) which works by training the model on all data except one sample and then testing on the left-out sample.
                              M-Fold Cross-Validation which works by splitting the data into M groups (folds), training the model on M-1 folds, and testing on the held-out fold. This is repeated M times until every fold has served as a test set. The overall performance can be averaged across multiple such M-fold procedures (repeats) for stability.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = c("None", "LOOCV", "Mfold"),
            selected = isolate(userState$splsda_cv_opt) %||% "None"
          ),
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
                content = "The number of folds to use for the M-Fold Cross-Validation process. More folds can improve 
                                accuracy but take longer to compute.",
                if (
                  input$theme_choice == "darkly" ||
                    input$theme_choice == "cyborg"
                ) {
                  colour = "red"
                } else {
                  colour = "blue"
                }
              ),
              value = isolate(userState$splsda_fold_num) %||% 5,
              min = 2
            )
          ),
          checkboxInput(
            "splsda_log2",
            label = helper(
              type = "inline",
              title = "Apply log2 transformation",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Apply log2 transformation</span>"
              ),
              content = "Apply a log2 transformation to the data before generating the sPLS-DA plot.
                               This transformation can help manage data that span a wide range of values.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$splsda_log2) %||% FALSE
          ),
          numericInput(
            "splsda_comp_num",
            label = helper(
              type = "inline",
              title = "Number of Components",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Number of Components</span>"
              ),
              content = "Specify the number of components (latent variables) to compute in the sPLS-DA model. Typically 1 to (number of groups - 1), or more if exploring data structure. For 2D plots, 2 components are used.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$splsda_comp_num) %||% 2,
            min = 2
          ),
          selectizeInput(
            "splsda_pch",
            label = helper(
              type = "inline",
              title = "Plotting Symbols",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Plotting Symbols</span>"
              ),
              content = "Select plotting character (PCH) symbols for data points. If using Grouping Column 2, match the number of symbols to its categories. Otherwise, it can correspond to Grouping Column 1.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = pch_choices,
            selected = pch_choices[c(17, 5)],
            multiple = TRUE,
            options = list(
              placeholder = "Select PCH Values",
              plugins = c("remove_button", "restore_on_backspace"),
              create = TRUE
            )
          ),
          selectInput(
            "splsda_style",
            label = helper(
              type = "inline",
              title = "Plot Style",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Plot Style</span>"
              ),
              content = "The style of the sPLS-DA plot. Choose between 2D and 3D. Requires at least 3 components for 3D.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = c("2D", "3D"),
            selected = isolate(userState$splsda_style) %||% "2D"
          ),
          checkboxInput(
            "splsda_roc",
            label = helper(
              type = "inline",
              title = "Plot ROC",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Plot ROC</span>"
              ),
              content = "Plot Receiver Operating Characteristic (ROC) curves to evaluate the classification performance of the sPLS-DA model.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$splsda_roc) %||% FALSE
          ),
          checkboxInput(
            "splsda_ellipse",
            label = helper(
              type = "inline",
              title = "Draw Ellipse",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Draw Ellipse</span>"
              ),
              content = "Draw an ellipse around the data points on the sPLS-DA plot. (Draws an ellipse covering 95% of the data points.)",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$splsda_ellipse) %||% FALSE
          ),
          checkboxInput(
            "splsda_bg",
            label = helper(
              type = "inline",
              title = "Background Color",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Shaded Background Prediction</span>"
              ),
              content = "Draws a shaded background prediction on the sPLS-DA plot. 
                               This shading indicates the model’s predicted classification regions",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$splsda_bg) %||% FALSE
          ),
          checkboxInput(
            "splsda_conf_mat",
            label = helper(
              type = "inline",
              title = "Confusion Matrix",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Confusion Matrix</span>"
              ),
              content = "Display the confusion matrix for the sPLS-DA model. This table shows how many
                               predictions were correct and provides key performance measures.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$splsda_conf_mat) %||% FALSE
          )
        )
      },
      "Two-Sample T-Test" = {
        ui_list <- tagList(
          checkboxInput(
            "ttest_log2",
            label = helper(
              type = "inline",
              title = "Apply log2 transformation",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Apply log2 transformation</span>"
              ),
              content = "Apply a log2 transformation to the data before performing the two-sample t-test.
                               This transformation can help manage data that span a wide range of values.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$ttest_log2) %||% FALSE
          )
        )
      },
      "Volcano Plot" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- names(df)
        ui_list <- tagList(
          selectInput(
            "volc_group_col",
            label = helper(
              type = "inline",
              title = "Grouping Column",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Grouping Column</span>"
              ),
              content = "Select the column that defines the two groups for comparison in the volcano plot (e.g., 'Control' vs 'Treatment').",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = cols,
            selected = isolate(userState$volc_group_col) %||% cols[1]
          ),
          uiOutput("volc_conditions_ui"),
          numericInput(
            "volc_fold_change_thresh",
            label = helper(
              type = "inline",
              title = "Fold Change Threshold",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Fold Change Threshold</span>"
              ),
              content = "Set the threshold for fold change. Points exceeding this fold change (and the p-value threshold) are typically will be shown as significant or not significant.
              Learn more about fold change at <a href='https://doi.org/10.1177/1087057110381919' target='_blank' rel='noopener noreferrer'>link 1</a>, and 
                <a href='https://doi.org/10.1177/1087057110381783' target='_blank' rel='noopener noreferrer'>link 2</a>",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$volc_fold_change_thresh) %||% 2,
            min = 0
          ),
          numericInput(
            "volc_p_value_thresh",
            label = helper(
              type = "inline",
              title = "P-Value Threshold",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>P-Value Threshold</span>"
              ),
              content = "Set the threshold for statistical significance (p-value). Points with p-values below this threshold (and exceeding the fold change threshold) are highlighted as significant.
              Learn more about fold change at <a href='https://doi.org/10.1142/S0219720012310038' target='_blank' rel='noopener noreferrer'>link 1</a>, and 
                <a href='https://doi.org/10.1038/s41467-024-45834-7' target='_blank' rel='noopener noreferrer'>link 2</a>",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$volc_p_value_thresh) %||% 0.05,
            min = 0,
            step = 0.01
          ),
          numericInput(
            "volc_top_labels",
            label = helper(
              type = "inline",
              title = "Top Labels",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Top Labels</span>"
              ),
              content = "Specify the number of significant points to label on the volcano plot.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$volc_top_labels) %||% 10,
            min = 1
          )
        )
      },
      "Extreme Gradient Boosting (XGBoost)" = {
        df <- filteredData()
        if (is.null(df)) {
          return(NULL)
        }
        cols <- names(df)
        ui_list <- tagList(
          selectInput(
            "xgb_group_col",
            label = helper(
              type = "inline",
              title = "Grouping Column",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Grouping Column</span>"
              ),
              content = "Select the column that contains the outcome or class labels you want the XGBoost model to predict (the target variable).",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = cols,
            selected = isolate(userState$xgb_group_col) %||% cols[1]
          ),
          numericInput(
            "xgb_train_fraction",
            label = helper(
              type = "inline",
              title = "Train Fraction",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Train Fraction</span>"
              ),
              content = "The fraction of the data to use for training the XGBoost model.
                              Remainder is used for testing the model.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$xgb_train_fraction) %||% 0.7,
            min = 0.1,
            max = 0.9,
            step = 0.1
          ),
          numericInput(
            "xgb_nrounds",
            label = helper(
              type = "inline",
              title = "Number of Rounds",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Number of Rounds</span>"
              ),
              content = "The number of rounds to grow the trees in the XGBoost model.
                              Each round adds a new tree to improve the model’s predictions.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$xgb_nrounds) %||% 500,
            min = 100
          ),
          numericInput(
            "xgb_max_depth",
            label = helper(
              type = "inline",
              title = "Maximum Depth",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Maximum Depth</span>"
              ),
              content = "The maximum depth of the trees in the XGBoost model.
                              Deeper trees can capture more complex relationships but may risk overfitting.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$xgb_max_depth) %||% 6,
            min = 1
          ),
          numericInput(
            "xgb_eta",
            label = helper(
              type = "inline",
              title = "Learning Rate",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Learning Rate</span>"
              ),
              content = "The learning rate of the XGBoost model.
                              This controls how much each new tree influences the model; 
                              lower rates make the model learn more slowly but can improve accuracy.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$xgb_eta) %||% 0.1,
            min = 0,
            step = 0.01
          ),
          selectInput(
            "xgb_eval_metric",
            label = helper(
              type = "inline",
              title = "Evaluation Metric",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Evaluation Metric</span>"
              ),
              content = "The evaluation metric to use for the XGBoost model.
                             Choose 'mlogloss' to measure classification error or 'auc' for the area under the ROC curve.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            choices = c("mlogloss", "auc"),
            selected = isolate(userState$xgb_eval_metric) %||% "mlogloss"
          ),
          numericInput(
            "xgb_top_n_features",
            label = helper(
              type = "inline",
              title = "Top Number of Features",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Top Number of Features</span>"
              ),
              content = "Specify the number of top features (based on importance scores from the XGBoost model) to display in a feature importance plot.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$xgb_top_n_features) %||% 10,
            min = 1
          ),
          checkboxInput(
            "xgb_plot_roc",
            label = helper(
              type = "inline",
              title = "Plot ROC",
              icon = "fas fa-question-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Plot ROC (Binary Comparison Only)</span>"
              ),
              content = "Plot the Recieving Operating Characteristic (ROC) curve for the XGBoost model.
                               Requires comparison to be a binary comparison (at most 2 groups). The ROC curve 
                               helps evaluate how well the model distinguishes between two groups.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$xgb_plot_roc) %||% FALSE
          ),
          checkboxInput(
            "xgb_cv",
            label = helper(
              type = "inline",
              title = "Cross-Validation",
              icon = "fas fa-exclamation-circle",
              shiny_tag = HTML(
                "<span style='margin-right: 15px;'>Cross-Validation</span>"
              ),
              content = "Perform cross-validation on the XGBoost model.
                               Cross-validation helps evaluate the model’s performance on different subsets of the data.",
              if (
                input$theme_choice == "darkly" || input$theme_choice == "cyborg"
              ) {
                colour = "red"
              } else {
                colour = "blue"
              }
            ),
            value = isolate(userState$xgb_cv) %||% FALSE
          ),
          conditionalPanel(
            condition = "input.xgb_cv == true",
            numericInput(
              "xgb_nfold",
              label = helper(
                type = "inline",
                title = "Number of Folds",
                icon = "fas fa-question-circle",
                shiny_tag = HTML(
                  "<span style='margin-right: 15px;'>Number of Folds</span>"
                ),
                content = "The number of folds to use for cross-validation in the XGBoost model.
                                The folds are used to train and test the model. More folds can improve accuracy but take longer to compute.",
                if (
                  input$theme_choice == "darkly" ||
                    input$theme_choice == "cyborg"
                ) {
                  colour = "red"
                } else {
                  colour = "blue"
                }
              ),
              value = isolate(userState$xgb_nfold) %||% 5,
              min = 2
            )
          )
        )
      }
    )
    do.call(tagList, ui_list)
  })

  shiny::observe({
    # Get the filtered data reactively
    df <- filteredData()
    req(df) # ensure df is available

    # Count numeric columns in the dataset
    numeric_cols <- sapply(df, is.numeric)
    default_num_vars <- sum(numeric_cols)

    # Always update the input with the new default value
    updateNumericInput(session, "splsda_var_num", value = default_num_vars)
  })

  output$df_conditions_ui <- shiny::renderUI({
    req(filteredData())
    req(input$df_group_var)
    df <- filteredData()
    choices <- unique(df[[input$df_group_var]])
    if (length(choices) < 2) {
      return(helpText("Not enough unique levels in grouping column"))
    }
    tagList(
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
          if (
            input$theme_choice == "darkly" || input$theme_choice == "cyborg"
          ) {
            colour = "red"
          } else {
            colour = "blue"
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
          if (
            input$theme_choice == "darkly" || input$theme_choice == "cyborg"
          ) {
            colour = "red"
          } else {
            colour = "blue"
          }
        ),
        choices = choices,
        selected = isolate(userState$df_cond2) %||% choices[2]
      )
    )
  })

  output$volc_conditions_ui <- shiny::renderUI({
    req(filteredData())
    req(input$volc_group_col)
    df <- filteredData()
    choices <- unique(df[[input$volc_group_col]])
    if (length(choices) >= 2) {
      tagList(
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
            if (
              input$theme_choice == "darkly" || input$theme_choice == "cyborg"
            ) {
              colour = "red"
            } else {
              colour = "blue"
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
            if (
              input$theme_choice == "darkly" || input$theme_choice == "cyborg"
            ) {
              colour = "red"
            } else {
              colour = "blue"
            }
          ),
          choices = choices,
          selected = isolate(userState$volc_cond2) %||% choices[2]
        )
      )
    } else {
      helpText(
        "Selected grouping variable does not have at least two unique values."
      )
    }
  })

  ## ---------------------------
  ## Wizard Navigation UI
  ## ---------------------------
  output$wizardUI <- shiny::renderUI({
    step <- currentStep()
    totalSteps <- 4
    pct <- round((step - 1) / (totalSteps - 1) * 100)

    # helper to inject title + bar
    stepHeader <- tagList(
      div(
        class = "step-title",
        h3(
          switch(
            as.character(step),
            "1" = "Step 1: Upload Data",
            "2" = "Step 2: Select Columns & Apply Filters",
            "3" = "Step 3: Analysis Options",
            "4" = "Analysis Results: "
          )
        )
      ),
      div(
        class = "progress-wrapper",
        # this div will shrink-wrap itself and pick up our #wizard_pb CSS
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
    switch(
      as.character(step),

      # STEP 1 ----
      "1" = tagList(
        stepHeader,
        fileInput(
          "datafile",
          HTML(
            "Upload Data File<br>Accepted Formats: ('.csv', '.txt', '.xls', '.xlsx')"
          ),
          accept = c(".csv", ".txt", ".xls", ".xlsx")
        ),
        checkboxInput("use_builtin", "Use built-in data?", FALSE),
        uiOutput("built_in_selector"),
        uiOutput("sheet_selector"),
        uiOutput("viewSummaryCheckboxes"),

        conditionalPanel(
          condition = "input.view_data",
          uiOutput("data_summary"),
          uiOutput("preview_ui")
        ),

        conditionalPanel(
          condition = "input.show_summary",
          h3("Summary Statistics"),
          shinycssloaders::withSpinner(
            DT::DTOutput("summary_stats_table"),
            type = 8
          )
        ),
        actionButton("next1", "Next")
      ),

      "2" = {
        # First, identify column types from the user's data
        df <- userData()
        is_numeric_col <- sapply(df, is.numeric)
        all_cols <- names(df)
        numeric_cols <- all_cols[is_numeric_col]
        categorical_cols <- all_cols[!is_numeric_col]

        tagList(
          stepHeader,
          fluidRow(
            # -- Column for Selections (now contains TWO cards) --
            column(
              6,
              # Card 1: Categorical Columns
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
                      "selected_categorical_cols",
                      label = NULL,
                      choices = categorical_cols,
                      selected = intersect(
                        userState$selected_columns,
                        categorical_cols
                      ) %||%
                        categorical_cols
                    )
                  )
                )
              ),

              # Card 2: Numerical Columns
              card(
                card_header(class = "bg-info", "2. Select Numerical Columns"),
                card_body(
                  div(
                    style = "margin-bottom: 10px;",
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
                      "selected_numerical_cols",
                      label = NULL,
                      choices = numeric_cols,
                      selected = intersect(
                        userState$selected_columns,
                        numeric_cols
                      ) %||%
                        numeric_cols
                    )
                  )
                )
              )
            ),

            # -- UI output for the CONDITIONAL filter panel --
            uiOutput("conditional_filter_ui")
          ),
          # -- Navigation --
          br(),
          fluidRow(
            column(
              12,
              div(
                style = "text-align: left;",
                actionButton("back2", "Back", icon = icon("arrow-left")),
                actionButton(
                  "next2",
                  "Next",
                  icon = icon("arrow-right"),
                  class = "btn-primary"
                )
              )
            )
          )
        )
      },
      "3" = tagList(
        stepHeader,
        bslib::navset_card_tab(
          id = "analysis_categories",
          title = "Select Analysis Type",
          bslib::nav_panel(
            "Statistical Tests",
            value = "stat_tests",
            icon = fontawesome::fa("calculator"),
            helpText("Choose Statistical Test:"),
            uiOutput("stat_function_ui")
          ),
          bslib::nav_panel(
            "Exploratory Analysis",
            value = "exploratory",
            icon = fontawesome::fa("chart-bar"),
            helpText("Choose Exploratory Function:"),
            uiOutput("exploratory_function_ui")
          ),
          bslib::nav_panel(
            "Multivariate",
            value = "multivariate",
            icon = fontawesome::fa("sitemap"),
            helpText("Choose Multivariate Function:"),
            uiOutput("multivariate_function_ui")
          ),
          bslib::nav_panel(
            "Machine Learning",
            value = "machine",
            icon = fontawesome::fa("robot"),
            helpText("Choose Machine Learning Function:"),
            uiOutput("ml_function_ui")
          )
        ),
        br(),
        div(
          class = "overflow-visible",
          fluidRow(
            column(12, h4("Analysis Options"), uiOutput("function_options_ui"))
          )
        ),
        fluidRow(
          column(
            2.5,
            card(
              card_header("Output Options"),
              card_body(
                radioButtons(
                  "output_mode",
                  "Output Mode:",
                  choices = c("Interactive", "Download"),
                  selected = "Interactive",
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = "input.output_mode == 'Download'",
                  textInput(
                    "output_file_name",
                    "Output File Name (no extension)",
                    ""
                  )
                )
              )
            )
          )
        ),
        br(),
        div(
          style = "text-align: left;",
          actionButton("back3", "Back", icon = icon("arrow-left")),
          actionButton(
            "next3",
            "Run Analysis",
            icon = icon("play"),
            class = "btn-success"
          )
        )
      ),

      # STEP 4 ----
      "4" = tagList(
        stepHeader,
        uiOutput("result_display"),
        actionButton("back4", "Back")
      )
    )
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
  output$stat_function_ui <- renderUI({
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
    observeEvent(input[[paste0("stat_func_", gsub("\\s|\\-", "_", choice))]], {
      selected_stat_func(choice)
    })
  })

  # --- Logic for Custom Button Group: Exploratory Vis ---
  exploratory_choices <- c(
    "Boxplots",
    "Enhanced Boxplots",
    "Error-BarPlot",
    "Dual-Flashlight Plot",
    "Heatmap",
    "Skewness/Kurtosis",
    "Volcano Plot"
  )
  output$exploratory_function_ui <- renderUI({
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
    observeEvent(input[[paste0("exp_func_", gsub("\\s|\\-", "_", choice))]], {
      selected_exploratory_func(choice)
    })
  })

  # --- Logic for Custom Button Group: Multivariate ---
  multivariate_choices <- c(
    "Principle Component Analysis (PCA)",
    "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
  )
  output$multivariate_function_ui <- renderUI({
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
    observeEvent(input[[paste0("multi_func_", gsub("\\s|\\-", "_", choice))]], {
      selected_multivariate_func(choice)
    })
  })

  # --- Logic for Custom Button Group: Machine Learning ---
  ml_choices <- c("Random Forest", "Extreme Gradient Boosting (XGBoost)")
  output$ml_function_ui <- renderUI({
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
    observeEvent(input[[paste0("ml_func_", gsub("\\s|\\-", "_", choice))]], {
      selected_ml_func(choice)
    })
  })
  # Calculating percentage for progress bar
  totalSteps <- 4
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
    currentStep(2)
  })
  shiny::observeEvent(input$back2, {
    currentStep(1)
  })

  # On moving from Step 2 to Step 3, save the selected columns
  shiny::observeEvent(input$next2, {
    if (
      !is.null(input$selected_columns) && length(input$selected_columns) > 0
    ) {
      userState$selected_columns <- input$selected_columns
    }
    currentStep(3)
  })

  shiny::observeEvent(input$back3, {
    currentStep(2)
  })

  # A. Create a new reactive to combine the selected columns
  selected_columns_combined <- reactive({
    # Use req() to ensure the inputs are available before combining
    req(input$selected_categorical_cols, input$selected_numerical_cols)
    c(input$selected_categorical_cols, input$selected_numerical_cols)
  })

  # B. Update the logic for the "Select/Deselect All" buttons
  #    (Remove the old observeEvents for "select_all" and "deselect_all")
  observeEvent(input$select_all_cat, {
    df <- userData()
    categorical_cols <- names(df)[!sapply(df, is.numeric)]
    updateCheckboxGroupInput(
      session,
      "selected_categorical_cols",
      selected = categorical_cols
    )
  })
  observeEvent(input$deselect_all_cat, {
    updateCheckboxGroupInput(
      session,
      "selected_categorical_cols",
      selected = character(0)
    )
  })
  observeEvent(input$select_all_num, {
    df <- userData()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    updateCheckboxGroupInput(
      session,
      "selected_numerical_cols",
      selected = numeric_cols
    )
  })
  observeEvent(input$deselect_all_num, {
    updateCheckboxGroupInput(
      session,
      "selected_numerical_cols",
      selected = character(0)
    )
  })

  # C. Update the 'filteredData' reactive to use the combined list
  filteredData <- shiny::reactive({
    df <- userData()
    req(df)

    # *** CHANGE THIS PART ***
    # Use the new combined reactive instead of input$selected_columns
    currentCols <- if (!is.null(userState$selected_columns)) {
      intersect(userState$selected_columns, names(df))
    } else {
      intersect(selected_columns_combined(), names(df)) # Use the reactive here
    }
    # ***********************

    if (!is.null(currentCols) && length(currentCols) > 0) {
      df <- df[, currentCols, drop = FALSE]
    }
    factor_cols <- names(df)[sapply(df, function(x) {
      is.factor(x) || is.character(x)
    })]
    if (length(factor_cols) > 0) {
      for (col in factor_cols) {
        filter_vals <- input[[paste0("filter_", col)]]
        if (!is.null(filter_vals)) {
          df <- df[df[[col]] %in% filter_vals, ]
        }
      }
    }
    df
  })

  # D. Update the logic for saving state when clicking "Next"
  shiny::observeEvent(input$next2, {
    # *** CHANGE THIS PART ***
    if (
      !is.null(selected_columns_combined()) &&
        length(selected_columns_combined()) > 0
    ) {
      userState$selected_columns <- selected_columns_combined()
    }
    # ***********************
    currentStep(3)
  })

  # E. Update the conditional_filter_ui to use the new categorical input
  output$conditional_filter_ui <- renderUI({
    # *** SIMPLIFIED LOGIC ***
    # The filter UI now depends only on the selection of categorical columns.
    if (
      !is.null(input$selected_categorical_cols) &&
        length(input$selected_categorical_cols) > 0
    ) {
      column(
        6,
        card(
          card_header(class = "bg-primary", "3. Apply Filters (Optional)"),
          card_body(
            bslib::accordion(
              open = FALSE,
              bslib::accordion_panel(
                "Filter by Categorical Values",
                uiOutput("filter_ui"),
                icon = fontawesome::fa("filter")
              )
            )
          )
        )
      )
    } else {
      NULL
    }
  })
  # On moving from Step 3 to Step 4, save the selected function and function options
  shiny::observeEvent(input$next3, {
    userState$selected_function <- selected_function()
    if (selected_function() == "Boxplots") {
      userState$bp_mf_row <- input$bp_mf_row
      userState$bp_y_lim <- input$bp_y_lim
      userState$bp_bin_size <- input$bp_bin_size
      userState$bp_log2 <- input$bp_log2
    }
    if (selected_function() == "Enhanced Boxplots") {
      userState$bp2_mf_row <- input$bp2_mf_row
      userState$bp2_log2 <- input$bp2_log2
      userState$bp2_y_lim <- input$bp2_y_lim
    }
    if (selected_function() == "Error-BarPlot") {
      userState$eb_group_col <- input$eb_group_col
      userState$eb_p_lab <- input$eb_p_lab
      userState$eb_es_lab <- input$eb_es_lab
      userState$eb_class_symbol <- input$eb_class_symbol
      userState$eb_x_lab <- input$eb_x_lab
      userState$eb_y_lab <- input$eb_y_lab
      userState$eb_title <- input$eb_title
      userState$eb_log2 <- input$eb_log2
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
      userState$hm_log2 <- input$hm_log2
      userState$hm_annotation <- input$hm_annotation
    }
    if (selected_function() == "Principle Component Analysis (PCA)") {
      userState$pca_group_col <- input$pca_group_col
      userState$pca_group_col2 <- input$pca_group_col2
      userState$pca_comp_num <- input$pca_comp_num
      userState$pca_log2 <- input$pca_log2
      userState$pca_ellipse <- input$pca_ellipse
      userState$pca_style <- input$pca_style
      userState$pca_pch <- as.numeric(input$pca_pch)
      userState$pca_colors <- input$pca_colors
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
      userState$splsda_multilevel <- input$splsda_multilevel
      userState$splsda_var_num <- input$splsda_var_num
      userState$splsda_cv_opt <- input$splsda_cv_opt
      userState$splsda_fold_num <- input$splsda_fold_num
      userState$splsda_log2 <- input$splsda_log2
      userState$splsda_comp_num <- input$splsda_comp_num
      userState$splsda_pch <- as.numeric(input$splsda_pch)
      userState$splsda_style <- input$splsda_style
      userState$splsda_roc <- input$splsda_roc
      userState$splsda_ellipse <- input$splsda_ellipse
      userState$splsda_bg <- input$splsda_bg
      userState$splsda_conf_mat <- input$splsda_conf_mat
      userState$splsda_colors <- input$splsda_colors
    }
    if (selected_function() == "Two-Sample T-Test") {
      userState$ttest_log2 <- input$ttest_log2
    }
    if (selected_function() == "ANOVA") {
      userState$anova_log2 <- input$anova_log2
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
    currentStep(4)
  })

  shiny::observeEvent(input$back4, {
    currentStep(3)
  })
  # For error message screen.
  shiny::observeEvent(input$back5, {
    currentStep(3)
  })
  ## ---------------------------
  ## Updating inputs by userState
  ## ---------------------------

  shiny::observeEvent(currentStep(), {
    if (currentStep() == 1) {
      # restore the “Use built‑in?” toggle
      updateCheckboxInput(session, "use_builtin", value = userState$use_builtin)

      # if they *had* chosen built‑in, make sure the selector
      # comes back with the right choice
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
        updateCheckboxInput(session, "bp_log2", value = userState$bp_log2)
      }

      # Enhanced Boxplots
      if (userState$selected_function == "Enhanced Boxplots") {
        updateTextInput(session, "bp2_mf_row", value = userState$bp2_mf_row)
        updateCheckboxInput(session, "bp2_log2", value = userState$bp2_log2)
        updateTextInput(session, "bp2_y_lim", value = userState$bp2_y_lim)
      }
      if (userState$selected_function == "Error-BarPlot") {
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
        updateCheckboxInput(session, "eb_log2", value = userState$eb_log2)
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
        updateCheckboxInput(session, "hm_log2", value = userState$hm_log2)
        updateSelectInput(
          session,
          "hm_annotation",
          selected = userState$hm_annotation
        )
      }

      # Principle Component Analysis (PCA)
      if (userState$selected_function == "Principle Component Analysis (PCA)") {
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
        updateCheckboxInput(session, "pca_log2", value = userState$pca_log2)
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
        updateCheckboxInput(
          session,
          "splsda_log2",
          value = userState$splsda_log2
        )
        updateNumericInput(
          session,
          "splsda_comp_num",
          value = userState$splsda_comp_num
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

      # Two-Sample T-Test
      if (userState$selected_function == "Two-Sample T-Test") {
        updateCheckboxInput(session, "ttest_log2", value = userState$ttest_log2)
      }
      # ANOVA
      if (userState$selected_function == "ANOVA") {
        updateCheckboxInput(session, "anova_log2", value = userState$anova_log2)
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

  ## ---------------------------
  ## Analysis and Results
  ## ---------------------------
  downloadPath <- shiny::reactiveVal(NULL)
  errorMessage <- shiny::reactiveVal(NULL)
  warningMessage <- shiny::reactiveVal(NULL)

  analysisResult <- shiny::eventReactive(input$next3, {
    errorMessage(NULL)
    warningMessage(NULL)

    req(filteredData())
    prog <- shiny::Progress$new()
    on.exit(prog$close())

    tryCatch(
      {
        withCallingHandlers(
          {
            df <- filteredData()
            mode <- input$output_mode
            out_file <- if (
              mode == "Download" && nzchar(input$output_file_name)
            ) {
              file.path(tempdir(), paste0(input$output_file_name, ".pdf"))
            } else {
              NULL
            }

            # Get the function to run from our corrected reactive
            func_to_run <- selected_function()
            req(func_to_run)

            # A single, consolidated switch statement drives all analysis
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
                scale = if (input$ttest_log2) "log2" else NULL,
                format_output = TRUE
              ),

              # -- Exploratory Visualization --
              "Boxplots" = cyt_bp(
                data = df,
                output_file = if (mode == "Download") out_file else NULL,
                progress = prog,
                bin_size = input$bp_bin_size,
                mf_row = if (nzchar(input$bp_mf_row)) {
                  as.numeric(strsplit(input$bp_mf_row, ",")[[1]])
                } else {
                  NULL
                },
                y_lim = if (nzchar(input$bp_y_lim)) {
                  as.numeric(strsplit(input$bp_y_lim, ",")[[1]])
                } else {
                  NULL
                },
                scale = if (input$bp_log2) "log2" else NULL
              ),
              "Enhanced Boxplots" = cyt_bp2(
                data = df,
                output_file = if (mode == "Download") out_file else NULL,
                progress = prog,
                mf_row = if (nzchar(input$bp2_mf_row)) {
                  as.numeric(strsplit(input$bp2_mf_row, ",")[[1]])
                } else {
                  NULL
                },
                y_lim = if (nzchar(input$bp2_y_lim)) {
                  as.numeric(strsplit(input$bp2_y_lim, ",")[[1]])
                } else {
                  NULL
                },
                scale = if (input$bp2_log2) "log2" else NULL
              ),
              "Error-BarPlot" = cyt_errbp(
                data = df,
                output_file = if (mode == "Download") out_file else NULL,
                progress = prog,
                group_col = input$eb_group_col,
                p_lab = input$eb_p_lab,
                es_lab = input$eb_es_lab,
                class_symbol = input$eb_class_symbol,
                x_lab = input$eb_x_lab,
                y_lab = input$eb_y_lab,
                title = input$eb_title,
                log2 = input$eb_log2
              ),
              "Dual-Flashlight Plot" = cyt_dualflashplot(
                data = df,
                group_var = input$df_group_var,
                group1 = input$df_cond1,
                group2 = input$df_cond2,
                output_file = if (mode == "Download") out_file else NULL,
                progress = prog,
                ssmd_thresh = input$df_ssmd_thresh,
                log2fc_thresh = input$df_log2fc_thresh,
                top_labels = input$df_top_labels
              ),
              "Heatmap" = cyt_heatmap(
                data = df,
                output_file = if (mode == "Download") out_file else NULL,
                progress = prog,
                scale = if (input$hm_log2) "log2" else NULL,
                annotation_col_name = input$hm_annotation
              ),
              "Skewness/Kurtosis" = cyt_skku(
                data = df,
                output_file = if (mode == "Download") out_file else NULL,
                progress = prog,
                group_cols = input$skku_group_cols,
                print_res_raw = input$skku_print_raw,
                print_res_log = input$skku_print_log
              ),
              "Volcano Plot" = cyt_volc(
                data = df,
                output_file = if (mode == "Download") out_file else NULL,
                progress = prog,
                group_col = input$volc_group_col,
                cond1 = input$volc_cond1,
                cond2 = input$volc_cond2,
                fold_change_thresh = input$volc_fold_change_thresh,
                p_value_thresh = input$volc_p_value_thresh,
                top_labels = input$volc_top_labels
              ),

              # -- Multivariate Analysis --
              "Principle Component Analysis (PCA)" = {
                pch_vals <- as.numeric(input$pca_pch)
                grp <- df[[input$pca_group_col]]
                uniq <- unique(grp)
                if (length(pch_vals) < length(uniq)) {
                  pch_vals <- rep(pch_vals, length.out = length(uniq))
                }
                cols <- if (length(input$pca_colors)) {
                  input$pca_colors
                } else {
                  rainbow(length(uniq))
                }
                cyt_pca(
                  data = df,
                  output_file = if (mode == "Download") out_file else NULL,
                  progress = prog,
                  group_col = input$pca_group_col,
                  group_col2 = input$pca_group_col2,
                  comp_num = input$pca_comp_num,
                  scale = if (input$pca_log2) "log2" else NULL,
                  ellipse = input$pca_ellipse,
                  style = if (input$pca_style == "3D") "3d" else NULL,
                  pch_values = pch_vals,
                  pca_colors = cols
                )
              },
              "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = {
                pch_vals <- as.numeric(input$splsda_pch)
                grp <- df[[input$splsda_group_col]]
                uniq <- unique(grp)
                if (length(pch_vals) < length(uniq)) {
                  pch_vals <- rep(pch_vals, length.out = length(uniq))
                }
                cols <- if (length(input$splsda_colors)) {
                  input$splsda_colors
                } else {
                  rainbow(length(uniq))
                }
                cyt_splsda(
                  data = df,
                  output_file = if (mode == "Download") out_file else NULL,
                  progress = prog,
                  group_col = input$splsda_group_col,
                  group_col2 = input$splsda_group_col2,
                  multilevel = input$splsda_multilevel,
                  var_num = input$splsda_var_num,
                  cv_opt = if (input$splsda_cv_opt == "None") {
                    NULL
                  } else {
                    input$splsda_cv_opt
                  },
                  fold_num = input$splsda_fold_num,
                  scale = if (input$splsda_log2) "log2" else NULL,
                  comp_num = input$splsda_comp_num,
                  style = if (input$splsda_style == "3D") "3d" else NULL,
                  pch_values = pch_vals,
                  splsda_colors = cols,
                  roc = input$splsda_roc,
                  ellipse = input$splsda_ellipse,
                  bg = input$splsda_bg,
                  conf_mat = input$splsda_conf_mat
                )
              },

              # -- Machine Learning --
              "Random Forest" = cyt_rf(
                data = df,
                output_file = if (mode == "Download") out_file else NULL,
                progress = prog,
                group_col = input$rf_group_col,
                ntree = input$rf_ntree,
                mtry = input$rf_mtry,
                train_fraction = input$rf_train_fraction,
                plot_roc = input$rf_plot_roc,
                run_rfcv = input$rf_run_rfcv,
                k_folds = input$rf_k_folds,
                step = input$rf_step
              ),
              "Extreme Gradient Boosting (XGBoost)" = cyt_xgb(
                data = df,
                output_file = if (mode == "Download") out_file else NULL,
                progress = prog,
                group_col = input$xgb_group_col,
                train_fraction = input$xgb_train_fraction,
                nrounds = input$xgb_nrounds,
                max_depth = input$xgb_max_depth,
                eta = input$xgb_eta,
                nfold = input$xgb_nfold,
                cv = input$xgb_cv,
                eval_metric = input$xgb_eval_metric,
                top_n_features = input$xgb_top_n_features,
                plot_roc = input$xgb_plot_roc
              )
            )

            if (mode == "Download" && nzchar(input$output_file_name)) {
              downloadPath(normalizePath(out_file))
              return(paste("Output file generated:", out_file))
            }

            return(results)
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
    )
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
        actionButton("back5", "Change inputs")
      ))
    }
    req(analysisResult())
    res <- analysisResult()
    mode <- input$output_mode
    func_name <- selected_function()
    req(func_name)
    tagList(
      uiOutput("warningText"),

      if (mode == "Download") {
        h3(if (is.character(res)) res else as.character(res))
      } else {
        if (
          func_name ==
            "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" &&
            is.list(res)
        ) {
          if ("overall_indiv_plot" %in% names(res)) {
            tagList(
              if (!is.null(res$overall_indiv_plot)) {
                shinycssloaders::withSpinner(
                  plotOutput("splsda_overallIndivPlot", height = "400px"),
                  type = 8
                )
              } else {
                NULL
              },
              if (!is.null(res$overall_3D)) {
                shinycssloaders::withSpinner(
                  plotOutput("splsda_overall3DPlot", height = "400px"),
                  type = 8
                )
              } else {
                NULL
              },
              if (!is.null(res$overall_ROC)) {
                shinycssloaders::withSpinner(
                  plotOutput("splsda_overallRocPlot", height = "400px"),
                  type = 8
                )
              } else {
                NULL
              },
              if (!is.null(res$overall_CV)) {
                shinycssloaders::withSpinner(
                  plotOutput("splsda_overallCvPlot", height = "400px"),
                  type = 8
                )
              } else {
                NULL
              },
              if (!is.null(res$loadings)) {
                shinycssloaders::withSpinner(
                  uiOutput("splsda_loadingsUI"),
                  type = 8
                )
              } else {
                NULL
              },
              if (!is.null(res$vip_scores)) {
                shinycssloaders::withSpinner(
                  uiOutput("splsda_vipScoresUI"),
                  type = 8
                )
              } else {
                NULL
              },
              if (!is.null(res$vip_indiv_plot)) {
                shinycssloaders::withSpinner(
                  plotOutput("splsda_vipIndivPlot", height = "400px"),
                  type = 8
                )
              } else {
                NULL
              },
              if (!is.null(res$vip_loadings)) {
                shinycssloaders::withSpinner(
                  uiOutput("splsda_vipLoadingsUI"),
                  type = 8
                )
              } else {
                NULL
              },
              if (!is.null(res$vip_3D)) {
                shinycssloaders::withSpinner(
                  plotOutput("splsda_vip3DPlot", height = "400px"),
                  type = 8
                )
              } else {
                NULL
              },
              if (!is.null(res$vip_ROC)) {
                shinycssloaders::withSpinner(
                  plotOutput("splsda_vipRocPlot", height = "400px"),
                  type = 8
                )
              } else {
                NULL
              },
              if (!is.null(res$vip_CV)) {
                shinycssloaders::withSpinner(
                  plotOutput("splsda_vipCvPlot", height = "400px"),
                  type = 8
                )
              } else {
                NULL
              },
              if (!is.null(res$conf_matrix)) {
                shinycssloaders::withSpinner(
                  verbatimTextOutput("splsda_confMatrix"),
                  type = 8
                )
              } else {
                NULL
              }
            )
          } else {
            do.call(
              tabsetPanel,
              lapply(names(res), function(trt) {
                tabPanel(
                  title = trt,
                  tagList(
                    if (!is.null(res[[trt]]$overall_indiv_plot)) {
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("splsda_overallIndivPlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    } else {
                      NULL
                    },
                    if (!is.null(res[[trt]]$overall_3D)) {
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("splsda_overall3DPlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    } else {
                      NULL
                    },
                    if (!is.null(res[[trt]]$overall_ROC)) {
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("splsda_overallRocPlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    } else {
                      NULL
                    },
                    if (!is.null(res[[trt]]$overall_CV)) {
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("splsda_overallCvPlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    } else {
                      NULL
                    },
                    if (!is.null(res[[trt]]$loadings)) {
                      shinycssloaders::withSpinner(
                        uiOutput(paste0("splsda_loadingsUI_", trt)),
                        type = 8
                      )
                    } else {
                      NULL
                    },
                    if (!is.null(res[[trt]]$vip_scores)) {
                      shinycssloaders::withSpinner(
                        uiOutput(paste0("splsda_vipScoresUI_", trt)),
                        type = 8
                      )
                    } else {
                      NULL
                    },
                    if (!is.null(res[[trt]]$vip_indiv_plot)) {
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("splsda_vipIndivPlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    } else {
                      NULL
                    },
                    if (!is.null(res[[trt]]$vip_loadings)) {
                      shinycssloaders::withSpinner(
                        uiOutput(paste0("splsda_vipLoadingsUI_", trt)),
                        type = 8
                      )
                    } else {
                      NULL
                    },
                    if (!is.null(res[[trt]]$vip_3D)) {
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("splsda_vip3DPlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    } else {
                      NULL
                    },
                    if (!is.null(res[[trt]]$vip_ROC)) {
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("splsda_vipRocPlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    } else {
                      NULL
                    },
                    if (!is.null(res[[trt]]$vip_CV)) {
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("splsda_vipCvPlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    } else {
                      NULL
                    },
                    if (!is.null(res[[trt]]$conf_matrix)) {
                      shinycssloaders::withSpinner(
                        verbatimTextOutput(paste0("splsda_confMatrix_", trt)),
                        type = 8
                      )
                    } else {
                      NULL
                    }
                  )
                )
              })
            )
          }
        } else if (
          func_name == "Heatmap" &&
            is.character(res) &&
            grepl("\\.png$", res, ignore.case = TRUE)
        ) {
          tagList(
            shinycssloaders::withSpinner(
              imageOutput("heatmapImage", height = "600px"),
              type = 8
            ),
            verbatimTextOutput("textResults")
          )
        } else if (func_name == "Volcano Plot" && is.list(res)) {
          tagList(
            shinycssloaders::withSpinner(
              plotOutput("volcPlotOutput", height = "400px"),
              type = 8
            ),
            shinycssloaders::withSpinner(tableOutput("volcStats"), type = 8)
          )
        } else if (
          func_name == "Principle Component Analysis (PCA)" && is.list(res)
        ) {
          if ("overall_indiv_plot" %in% names(res)) {
            tagList(
              shinycssloaders::withSpinner(
                plotOutput("pca_indivPlot", height = "400px"),
                type = 8
              ),
              if (!is.null(res$overall_3D)) {
                shinycssloaders::withSpinner(
                  plotOutput("pca_3DPlot", height = "400px"),
                  type = 8
                )
              },
              shinycssloaders::withSpinner(
                plotOutput("pca_screePlot", height = "400px"),
                type = 8
              ),
              shinycssloaders::withSpinner(
                uiOutput("pca_loadingsUI"),
                type = 8
              ),
              shinycssloaders::withSpinner(
                plotOutput("pca_biplot", height = "400px"),
                type = 8
              ),
              shinycssloaders::withSpinner(
                plotOutput("pca_corrCircle", height = "400px"),
                type = 8
              )
            )
          } else {
            do.call(
              tabsetPanel,
              lapply(names(res), function(lvl) {
                tabPanel(
                  title = lvl,
                  shinycssloaders::withSpinner(
                    plotOutput(paste0("pca_indivPlot_", lvl), height = "400px"),
                    type = 8
                  ),
                  if (!is.null(res[[lvl]]$overall_3D)) {
                    shinycssloaders::withSpinner(
                      plotOutput(paste0("pca_3DPlot_", lvl), height = "400px"),
                      type = 8
                    )
                  } else {
                    NULL
                  },
                  shinycssloaders::withSpinner(
                    plotOutput(paste0("pca_screePlot_", lvl), height = "400px"),
                    type = 8
                  ),
                  shinycssloaders::withSpinner(
                    uiOutput(paste0("pca_loadingsUI_", lvl)),
                    type = 8
                  ),
                  shinycssloaders::withSpinner(
                    plotOutput(paste0("pca_biplot_", lvl), height = "400px"),
                    type = 8
                  ),
                  shinycssloaders::withSpinner(
                    plotOutput(
                      paste0("pca_corrCircle_", lvl),
                      height = "400px"
                    ),
                    type = 8
                  )
                )
              })
            )
          }
        } else if (func_name == "Error-BarPlot" && inherits(res, "ggplot")) {
          tagList(
            shinycssloaders::withSpinner(
              plotOutput("errorBarPlotOutput", height = "400px"),
              type = 8
            )
          )
        } else if (func_name == "Random Forest" && is.list(res)) {
          tagList(
            verbatimTextOutput("rf_summary"),
            shinycssloaders::withSpinner(
              plotOutput("rf_vipPlot", height = "400px"),
              type = 8
            ),
            if (!is.null(res$roc_plot)) {
              shinycssloaders::withSpinner(
                plotOutput("rf_rocPlot", height = "400px"),
                type = 8
              )
            },
            if (!is.null(res$rfcv_plot)) {
              shinycssloaders::withSpinner(
                plotOutput("rf_rfcvPlot", height = "400px"),
                type = 8
              )
            }
          )
        } else if (
          func_name == "Extreme Gradient Boosting (XGBoost)" && is.list(res)
        ) {
          tagList(
            verbatimTextOutput("xgb_summary"),
            shinycssloaders::withSpinner(
              plotOutput("xgb_vipPlot", height = "400px"),
              type = 8
            ),
            conditionalPanel(
              condition = "output.xgb_hasROC == true",
              shinycssloaders::withSpinner(
                plotOutput("xgb_rocPlot", height = "400px"),
                type = 8
              )
            )
          )
        } else if (func_name == "Skewness/Kurtosis") {
          tagList(
            shinycssloaders::withSpinner(
              plotOutput("skku_skewPlot", height = "400px"),
              type = 8
            ),
            shinycssloaders::withSpinner(
              plotOutput("skku_kurtPlot", height = "400px"),
              type = 8
            ),
            conditionalPanel(
              condition = "input.skku_print_raw == true",
              shinycssloaders::withSpinner(
                tableOutput("skku_raw_results")
              ),
              type = 8
            ),
            conditionalPanel(
              condition = "input.skku_print_log == true",
              shinycssloaders::withSpinner(
                tableOutput("skku_log_results")
              ),
              type = 8
            )
          )
        } else if (func_name == "Dual-Flashlight Plot") {
          tagList(
            shinycssloaders::withSpinner(
              plotOutput("dualflashPlotOutput", height = "400px"),
              type = 8
            ),
            shinycssloaders::withSpinner(
              tableOutput("dualflashStats"),
              type = 8
            )
          )
        } else if (
          is.list(res) &&
            length(res) > 0 &&
            all(sapply(res, function(x) inherits(x, "ggplot")))
        ) {
          tagList(
            lapply(seq_along(res), function(i) {
              shinycssloaders::withSpinner(
                plotOutput(paste0("dynamicPlot_", i), height = "400px"),
                type = 8
              )
            })
          )
        } else {
          tagList(
            DT::dataTableOutput("statResults")
          )
        }
      }
    )
  })

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
  })

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
    req(selected_function())
    if (input$output_mode != "Interactive") {
      return(NULL)
    }
    res <- analysisResult()
    func_name <- selected_function()

    if (
      selected_function() ==
        "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" &&
        is.list(res)
    ) {
      if ("overall_indiv_plot" %in% names(res)) {
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
        output$splsda_loadingsUI <- shiny::renderUI({
          if (!is.null(res$loadings)) {
            tagList(lapply(seq_along(res$loadings), function(i) {
              plotOutput(
                paste0("splsda_loadings_overall_", i),
                height = "300px"
              )
            }))
          }
        })
        if (!is.null(res$loadings)) {
          for (i in seq_along(res$loadings)) {
            local({
              ii <- i
              output[[paste0("splsda_loadings_overall_", ii)]] <- renderPlot({
                replayPlot(res$loadings[[ii]])
              })
            })
          }
        }

        output$splsda_vipScoresUI <- shiny::renderUI({
          if (!is.null(res$vip_scores)) {
            tagList(lapply(seq_along(res$vip_scores), function(i) {
              plotOutput(
                paste0("splsda_vipScore_overall_", i),
                height = "300px"
              )
            }))
          }
        })
        if (!is.null(res$vip_scores)) {
          for (i in seq_along(res$vip_scores)) {
            local({
              ii <- i
              output[[paste0("splsda_vipScore_overall_", ii)]] <- renderPlot({
                print(res$vip_scores[[ii]])
              })
            })
          }
        }
        output$splsda_vipLoadingsUI <- shiny::renderUI({
          if (!is.null(res$vip_loadings)) {
            tagList(lapply(seq_along(res$vip_loadings), function(i) {
              plotOutput(
                paste0("splsda_vipLoadings_overall_", i),
                height = "300px"
              )
            }))
          }
        })

        if (!is.null(res$vip_loadings)) {
          for (i in seq_along(res$vip_loadings)) {
            local({
              ii <- i
              output[[paste0("splsda_vipLoadings_overall_", ii)]] <- renderPlot(
                {
                  replayPlot(res$vip_loadings[[ii]])
                }
              )
            })
          }
        }

        output$splsda_vipIndivPlot <- renderPlot({
          if (!is.null(res$vip_indiv_plot)) replayPlot(res$vip_indiv_plot)
        })
        output$splsda_vip3DPlot <- renderPlot({
          if (!is.null(res$vip_3D)) replayPlot(res$vip_3D)
        })
        output$splsda_vipRocPlot <- renderPlot({
          if (!is.null(res$vip_ROC)) replayPlot(res$vip_ROC)
        })
        output$splsda_vipCvPlot <- renderPlot({
          if (!is.null(res$vip_CV)) print(res$vip_CV)
        })
        output$splsda_confMatrix <- renderPrint({
          if (!is.null(res$conf_matrix)) {
            cat(paste(res$conf_matrix, collapse = "\n"))
          }
        })
      } else {
        for (grp in names(res)) {
          local({
            currentGroup <- grp
            currentSubres <- res[[currentGroup]]
            output[[paste0(
              "splsda_overallIndivPlot_",
              currentGroup
            )]] <- renderPlot({
              if (!is.null(currentSubres$overall_indiv_plot)) {
                replayPlot(currentSubres$overall_indiv_plot)
              }
            })
            output[[paste0(
              "splsda_overall3DPlot_",
              currentGroup
            )]] <- renderPlot({
              if (!is.null(currentSubres$overall_3D)) {
                replayPlot(currentSubres$overall_3D)
              }
            })
            output[[paste0(
              "splsda_overallRocPlot_",
              currentGroup
            )]] <- renderPlot({
              if (!is.null(currentSubres$overall_ROC)) {
                replayPlot(currentSubres$overall_ROC)
              }
            })
            output[[paste0(
              "splsda_overallCvPlot_",
              currentGroup
            )]] <- renderPlot({
              if (!is.null(currentSubres$overall_CV)) {
                print(currentSubres$overall_CV)
              }
            })
            if (!is.null(currentSubres$loadings)) {
              output[[paste0(
                "splsda_loadingsUI_",
                currentGroup
              )]] <- shiny::renderUI({
                tagList(lapply(seq_along(currentSubres$loadings), function(i) {
                  plotOutput(
                    paste0("splsda_loadings_", currentGroup, "_", i),
                    height = "300px"
                  )
                }))
              })
              for (i in seq_along(currentSubres$loadings)) {
                local({
                  ii <- i
                  output[[paste0(
                    "splsda_loadings_",
                    currentGroup,
                    "_",
                    ii
                  )]] <- renderPlot({
                    replayPlot(currentSubres$loadings[[ii]])
                  })
                })
              }
            }
            if (!is.null(currentSubres$vip_scores)) {
              output[[paste0(
                "splsda_vipScoresUI_",
                currentGroup
              )]] <- shiny::renderUI(
                {
                  tagList(lapply(
                    seq_along(currentSubres$vip_scores),
                    function(i) {
                      plotOutput(
                        paste0("splsda_vipScore_", currentGroup, "_", i),
                        height = "300px"
                      )
                    }
                  ))
                }
              )
              for (i in seq_along(currentSubres$vip_scores)) {
                local({
                  ii <- i
                  output[[paste0(
                    "splsda_vipScore_",
                    currentGroup,
                    "_",
                    ii
                  )]] <- renderPlot({
                    print(currentSubres$vip_scores[[ii]])
                  })
                })
              }
            }
            output[[paste0(
              "splsda_vipIndivPlot_",
              currentGroup
            )]] <- renderPlot({
              if (!is.null(currentSubres$vip_indiv_plot)) {
                replayPlot(currentSubres$vip_indiv_plot)
              }
            })
            if (!is.null(currentSubres$vip_loadings)) {
              output[[paste0(
                "splsda_vipLoadingsUI_",
                currentGroup
              )]] <- shiny::renderUI({
                tagList(lapply(
                  seq_along(currentSubres$vip_loadings),
                  function(i) {
                    plotOutput(
                      paste0("splsda_vipLoadings_", currentGroup, "_", i),
                      height = "300px"
                    )
                  }
                ))
              })
              for (i in seq_along(currentSubres$vip_loadings)) {
                local({
                  ii <- i
                  output[[paste0(
                    "splsda_vipLoadings_",
                    currentGroup,
                    "_",
                    ii
                  )]] <- renderPlot({
                    replayPlot(currentSubres$vip_loadings[[ii]])
                  })
                })
              }
            }
            output[[paste0("splsda_vip3DPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(currentSubres$vip_3D)) {
                replayPlot(currentSubres$vip_3D)
              }
            })
            output[[paste0("splsda_vipRocPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(currentSubres$vip_ROC)) {
                replayPlot(currentSubres$vip_ROC)
              }
            })
            output[[paste0("splsda_vipCvPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(currentSubres$vip_CV)) print(currentSubres$vip_CV)
            })
            output[[paste0("splsda_confMatrix_", currentGroup)]] <- renderPrint(
              {
                if (!is.null(currentSubres$conf_matrix)) {
                  cat(paste(currentSubres$conf_matrix, collapse = "\n"))
                }
              }
            )
          })
        }
      }
    }

    if (func_name == "Extreme Gradient Boosting (XGBoost)" && is.list(res)) {
      output$xgb_summary <- renderPrint({
        cat(res$summary_text)
      })
      output$xgb_vipPlot <- renderPlot({
        req(res$plot)
        print(res$plot)
      })
      output$xgb_rocPlot <- renderPlot({
        req(res$roc_plot)
        print(res$roc_plot)
      })
      output$xgb_hasROC <- shiny::reactive({
        !is.null(res$roc_plot)
      })

      outputOptions(output, "xgb_rocPlot", suspendWhenHidden = FALSE)
      outputOptions(output, "xgb_hasROC", suspendWhenHidden = FALSE)
    }

    if (func_name == "Random Forest" && is.list(res)) {
      output$rf_summary <- renderPrint({
        cat(res$summary_text)
      })
      output$rf_vipPlot <- renderPlot({
        req(res$vip_plot)
        print(res$vip_plot)
      })
      output$rf_rocPlot <- renderPlot({
        req(res$roc_plot)
        print(res$roc_plot)
      })
      output$rf_rfcvPlot <- renderPlot({
        req(res$rfcv_plot)
        print(res$rfcv_plot)
      })
    }

    if (func_name == "Principle Component Analysis (PCA)" && is.list(res)) {
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
  })

  output$volcPlotOutput <- shiny::renderPlot({
    req(analysisResult())
    if (input$output_mode != "Interactive") {
      return(NULL)
    }
    res <- analysisResult()
    req(res$plot)
    print(res$plot)
  })

  output$volcStats <- shiny::renderTable(
    {
      req(analysisResult())
      res <- analysisResult()
      req(res$stats)
      res$stats
    },
    rownames = FALSE
  )
  output$statResults <- DT::renderDT(
    {
      res <- analysisResult()
      func <- selected_function()

      if (func == "ANOVA") {
        if (is.data.frame(res)) {
          df <- res
        } else if (is.list(res)) {
          df <- dplyr::bind_rows(
            lapply(names(res), function(key) {
              parts <- strsplit(key, "_")[[1]]
              tibble::tibble(
                Outcome = parts[1],
                Categorical = parts[2],
                Comparison = names(res[[key]]),
                P_adj = unname(res[[key]])
              )
            })
          )
        } else {
          df <- tibble::tibble(Message = as.character(res))
        }
      } else if (func == "Two-Sample T-Test") {
        if (is.data.frame(res)) {
          df <- res
        } else if (
          is.list(res) && all(vapply(res, inherits, logical(1), "htest"))
        ) {
          df <- dplyr::bind_rows(
            lapply(names(res), function(key) {
              tt <- res[[key]]
              parts <- strsplit(key, "_")[[1]]

              lvl <- tryCatch(
                levels(filteredData()[[parts[2]]]),
                error = function(e) c("Level1", "Level2")
              )
              tibble::tibble(
                Outcome = parts[1],
                Categorical = parts[2],
                Comparison = paste(lvl[1], "vs", lvl[2]),
                Test = tt$method,
                Estimate = if ("estimate" %in% names(tt)) {
                  unname(tt$estimate)[1]
                } else {
                  NA_real_
                },
                Statistic = unname(tt$statistic)[1],
                P_value = tt$p.value
              )
            })
          )
        } else {
          df <- tibble::tibble(Message = as.character(res))
        }
      } else {
        df <- if (is.data.frame(res)) {
          res
        } else {
          tibble::tibble(Result = as.character(res))
        }
      }

      df
    },
    options = list(pageLength = 10, scrollX = TRUE),
    rownames = FALSE
  )
  output$dualflashPlotOutput <- shiny::renderPlot({
    req(analysisResult())
    if (input$output_mode != "Interactive") {
      return(NULL)
    }
    res <- analysisResult()
    req(res$plot)
    print(res$plot)
  })
  output$dualflashStats <- shiny::renderTable(
    {
      req(analysisResult())
      res <- analysisResult()
      req(res$stats)
      res$stats
    },
    rownames = FALSE
  )

  output$heatmapImage <- shiny::renderImage(
    {
      req(analysisResult())
      filepath <- analysisResult()
      list(
        src = filepath,
        contentType = "image/png",
        width = "600",
        height = "600",
        alt = "Heatmap"
      )
    },
    deleteFile = FALSE
  )

  output$textResults <- shiny::renderPrint({
    res <- analysisResult()
    print(res)
  })

  output$skku_skewPlot <- shiny::renderPlot({
    req(analysisResult())
    if (input$output_mode != "Interactive") {
      return(NULL)
    }
    res <- analysisResult()
    if (is.list(res) && !is.null(res$p_skew)) {
      print(res$p_skew)
    }
  })
  output$skku_kurtPlot <- shiny::renderPlot({
    req(analysisResult())
    if (input$output_mode != "Interactive") {
      return(NULL)
    }
    res <- analysisResult()
    if (is.list(res) && !is.null(res$p_kurt)) {
      print(res$p_kurt)
    }
  })
  output$skku_raw_results <- shiny::renderTable(
    {
      req(analysisResult())
      res <- analysisResult()
      req(res$raw_results)
      res$raw_results
    },
    rownames = FALSE
  )
  output$skku_log_results <- shiny::renderTable(
    {
      req(analysisResult())
      res <- analysisResult()
      req(res$log_results)
      res$log_results
    },
    rownames = FALSE
  )
  output$errorBarPlotOutput <- shiny::renderPlot({
    req(analysisResult())
    res <- analysisResult()
    print(res)
  })

  output$download_output <- shiny::downloadHandler(
    filename = function() {
      if (nzchar(input$output_file_name)) {
        paste0(input$output_file_name, ".pdf")
      } else {
        "output.pdf"
      }
    },
    content = function(file) {
      req(input$output_mode == "Download")
      req(downloadPath())

      file.copy(downloadPath(), file, overwrite = TRUE)
    }
  )

  ## ---------------------------
  ## Save key inputs using shiny::reactiveValues (alternative approach)
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
  shiny::observeEvent(input$bp_log2, {
    userState$bp_log2 <- input$bp_log2
  })
  # For Enhanced Boxplots
  shiny::observeEvent(input$bp2_mf_row, {
    userState$bp2_mf_row <- input$bp2_mf_row
  })
  shiny::observeEvent(input$bp2_log2, {
    userState$bp2_log2 <- input$bp2_log2
  })
  shiny::observeEvent(input$bp2_y_lim, {
    userState$bp2_y_lim <- input$bp2_y_lim
  })
  # For Error-BarPlot
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
  shiny::observeEvent(input$eb_log2, {
    userState$eb_log2 <- input$eb_log2
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
  shiny::observeEvent(input$hm_log2, {
    userState$hm_log2 <- input$hm_log2
  })
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
  shiny::observeEvent(input$pca_log2, {
    userState$pca_log2 <- input$pca_log2
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
  # For sPLS-DA
  shiny::observeEvent(input$splsda_group_col, {
    userState$splsda_group_col <- input$splsda_group_col
  })
  shiny::observeEvent(input$splsda_group_col2, {
    userState$splsda_group_col2 <- input$splsda_group_col2
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
  shiny::observeEvent(input$splsda_log2, {
    userState$splsda_log2 <- input$splsda_log2
  })
  shiny::observeEvent(input$splsda_comp_num, {
    userState$splsda_comp_num <- input$splsda_comp_num
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
  # For Two-Sample T-Test
  shiny::observeEvent(input$ttest_log2, {
    userState$ttest_log2 <- input$ttest_log2
  })
  # ANOVA
  shiny::observeEvent(input$ttest_log2, {
    userState$anova_log2 <- input$anova_log2
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
}
