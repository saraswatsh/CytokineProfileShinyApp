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

    # MINT sPLS-DA options
    mint_splsda_group_col = NULL,
    mint_splsda_group_col2 = NULL,
    mint_splsda_batch_col = NULL,
    mint_splsda_var_num = NULL,
    mint_splsda_var_num_manual = FALSE,
    mint_splsda_comp_num = NULL,
    mint_splsda_log2 = NULL,
    mint_splsda_ellipse = NULL,
    mint_splsda_bg = NULL,
    mint_splsda_roc = NULL,
    mint_splsda_colors = NULL,
    mint_splsda_pch = NULL,

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
  # Reactive values to store the selection from our new custom buttons
  selected_stat_func <- reactiveVal("ANOVA")
  selected_exploratory_func <- reactiveVal("Boxplots")
  selected_multivariate_func <- reactiveVal(
    "Principle Component Analysis (PCA)"
  )
  selected_ml_func <- reactiveVal("Random Forest")
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

    # Use radioButtons to display all options inside the card
    radioButtons(
      inputId = "built_in_choice",
      label = "Select a built-in dataset:",
      choices = builtInList, # Assumes builtInList is your list of choices
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
                  content = "Determines the number of columns (variables) to group together in each set of box plots. 
                         For example, a bin size of 25 will display box plots for up to 25 columns (variables) at a time. 
                         If there are more columns, multiple sets of box plots will be generated.",
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
                  content = "Set the minimum and maximum values for the Y-axis, entered as (min, max) e.g., '0,100'. 
                         Leave blank for automatic scaling based on data range. 
                         This controls the vertical range displayed on the plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$bp_y_lim) %||% ""
              )
            ),
            column(
              width = 6,
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$bp_log2) %||% FALSE
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
          ),
          fluidRow(
            column(
              6,
              checkboxInput(
                "bp2_log2",
                label = helper(
                  type = "inline",
                  title = "Apply log2 transformation",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Apply log2 transformation</span>"
                  ),
                  content = "Transform data by log2 before plotting enhanced boxplots.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$bp2_log2) %||% FALSE
              )
            ),
            column(6) # placeholder to balance the grid
          )
        )
      },
      # ------------------------
      # Error-BarPlot
      # ------------------------
      "Error-BarPlot" = {
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
                  title = "Grouping Variable",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Grouping Variable</span>"
                  ),
                  content = "The column to use for grouping the data (e.g. 'Control' vs 'Treatment').",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                choices = cat_vars,
                selected = isolate(userState$eb_group_col) %||% cat_vars[1]
              )
            ),
            column(
              6,
              checkboxInput(
                "eb_log2",
                label = helper(
                  type = "inline",
                  title = "Apply log2 transformation",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Apply log2 transformation</span>"
                  ),
                  content = "Apply log2 transformation to the data before plotting.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$eb_log2) %||% FALSE
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
                  content = "Label for the effect-size annotation on the plot.",
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
                  title = "Grouping Variable",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Grouping Variable</span>"
                  ),
                  content = "Column that contains the group labels (e.g. 'Control', 'Treated').",
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
                  content = "Threshold for Strictly Standardized Mean Difference.",
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
            ),
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
              checkboxInput(
                "hm_log2",
                label = helper(
                  type = "inline",
                  title = "Apply log2 transformation",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Apply log2 transformation</span>"
                  ),
                  content = "Apply log2 to the data before drawing the heatmap.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$hm_log2) %||% FALSE
              )
            ),
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
      # PCA
      # ————————————————————
      "Principle Component Analysis (PCA)" = {
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
                  title = "Grouping Column 1",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Grouping Column 1</span>"
                  ),
                  content = "The first column to use for grouping the data.",
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
                  title = "Grouping Column 2",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Grouping Column 2</span>"
                  ),
                  content = "An optional second column to use for grouping the data.",
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
                  content = "The color palette to use for the PCA plot. Select the number of colors to match the number of categories in grouping column 1.",
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
                "pca_log2",
                label = helper(
                  type = "inline",
                  title = "Apply log2 transformation",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Apply log2 transformation</span>"
                  ),
                  content = "Apply a log2 transformation to the data before generating the PCA plot. This transformation can help manage data that span a wide range of values.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$pca_log2) %||% FALSE
              )
            ),
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
                selected = pch_choices[c(17, 5)],
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
                  content = "The number of trees to grow in the random forest model.
                         Each tree is a simple model; more trees can improve predictions but take longer to compute.",
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
                  content = "The fraction of the data to use for training the random forest model.
                         The remainder is used for testing the model.",
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
                  title = "Grouping Column 1",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Grouping Column 1</span>"
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
                  title = "Grouping Column 2",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Grouping Column 2</span>"
                  ),
                  content = "Optional: Select a second column for additional grouping information on the plots.",
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
                  content = "The color palette to use for the sPLS-DA plot. Match the number of colors to the number of groups.",
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
              checkboxInput(
                "splsda_log2",
                label = helper(
                  type = "inline",
                  title = "Apply log2 transformation",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Apply log2 transformation</span>"
                  ),
                  content = "Apply log2 to the data before generating the sPLS-DA plot.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$splsda_log2) %||% FALSE
              )
            ),
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
                selected = pch_choices[c(17, 5)],
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
                  content = "Display the confusion matrix for the sPLS-DA model.",
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
                  title = "Grouping Column (Outcome)",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Grouping Column</span>"
                  ),
                  content = "Select the column containing the class labels you want to discriminate (e.g., Treatment, Condition).",
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
                  title = "Second Grouping Column (Optional)",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Second Grouping Column</span>"
                  ),
                  content = "Optional. If specified, the MINT sPLS-DA analysis will be run separately for each level of this column.",
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
                  content = "The color palette to use for the different groups in the plots.",
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
              selectizeInput(
                "mint_splsda_pch",
                label = helper(
                  type = "inline",
                  title = "Plotting Symbols for Batches",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Plotting Symbols</span>"
                  ),
                  content = "Select PCH symbols to represent the different batches on the sample plots.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
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
              )
            ),
            column(
              6,
              checkboxInput(
                "mint_splsda_log2",
                label = helper(
                  type = "inline",
                  title = "Apply log2 transformation",
                  icon = "fas fa-exclamation-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right: 15px;'>Apply log2 transformation</span>"
                  ),
                  content = "Apply log2 to the data before analysis.",
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                    "red"
                  } else {
                    "blue"
                  }
                ),
                value = isolate(userState$mint_splsda_log2) %||% FALSE
              )
            )
          ),
          # Row 5: Final Toggles
          fluidRow(
            column(
              4,
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
            ),
            column(
              4,
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
              4,
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
                  title = "Grouping Variable",
                  icon = "fas fa-question-circle",
                  shiny_tag = HTML(
                    "<span style='margin-right:15px;'>Grouping Variable</span>"
                  ),
                  content = "Column to color points by (e.g. 'Treatment').",
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

  shiny::observe({
    # Get the filtered data reactively
    df <- filteredData()
    req(df) # ensure df is available

    # Count numeric columns in the dataset
    numeric_cols <- sapply(df, is.numeric)
    default_num_vars <- sum(numeric_cols)

    # Always update the input with the new default value
    if (!isTRUE(userState$splsda_var_num_manual)) {
      default_num_vars <- sum(sapply(df, is.numeric))
      updateNumericInput(session, "splsda_var_num", value = default_num_vars)
    }

    if (!isTRUE(userState$mint_splsda_var_num_manual)) {
      default_num_vars <- sum(sapply(df, is.numeric))
      updateNumericInput(
        session,
        "mint_splsda_var_num",
        value = default_num_vars
      )
    }
  })

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
        card(
          # Use the card_header for a bold title
          card_header(h4(icon("upload"), "Step 1: Provide Your Data")),

          card_body(
            # Use columns to neatly separate the two ways a user can provide data
            fluidRow(
              column(
                width = 6,
                tags$h5("Option A: Upload a File"),
                fileInput(
                  "datafile",
                  label = NULL, # The h5 above acts as the label
                  accept = c(".csv", ".txt", ".xls", ".xlsx")
                ),
                helpText("Accepted Formats: '.csv', '.txt', '.xls', '.xlsx'"),
                uiOutput("sheet_selector") # Your existing UI for sheet selection
              ),
              column(
                width = 6,
                tags$h5("Option B: Use Built-in Data"),
                checkboxInput("use_builtin", "Use a built-in dataset?", FALSE),
                uiOutput("built_in_selector") # Your existing UI for built-in data
              )
            )
          ),
          card_footer(
            # Place the "Next" button in the footer for a clear call to action
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
        ),

        # Your conditional panels for viewing the data can remain outside the card
        br(),
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
        )
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
    "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)",
    "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
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
    # Check if a file has been uploaded OR if the "use built-in" box is checked
    if (!isTruthy(input$datafile) && !isTruthy(input$use_builtin)) {
      # If neither is true, show a pop-up modal warning
      showModal(modalDialog(
        title = "Data Source Required",
        "Please either upload a data file or check the 'Use built-in data' option to continue.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    } else {
      # Otherwise, if data is present, proceed to the next step
      currentStep(2)
    }
  })
  shiny::observeEvent(input$back2, {
    currentStep(1)
  })

  # On moving from Step 2 to Step 3, save the selected columns
  shiny::observeEvent(input$next2, {
    userState$selected_columns <- selected_columns_combined()
    currentStep(3)
  })

  shiny::observeEvent(input$back3, {
    currentStep(2)
  })

  # A. Create a new reactive to combine the selected columns
  selected_columns_combined <- reactive({
    c(input$selected_categorical_cols, input$selected_numerical_cols)
  })

  # B. Update the logic for the "Select/Deselect All" buttons
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

    currentCols <- if (currentStep() == 2) {
      # On Step 2, ALWAYS use the live selections from the checkboxes.
      req(selected_columns_combined())
      intersect(selected_columns_combined(), names(df))
    } else {
      # On all other steps (like Step 3 & 4 analysis), use the saved state.
      req(userState$selected_columns)
      intersect(userState$selected_columns, names(df))
    }

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
  # D. Update the logic for saving state when clicking "Next"
  shiny::observeEvent(input$next2, {
    userState$selected_columns <- selected_columns_combined()

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
    if (
      selected_function() ==
        "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
    ) {
      userState$mint_splsda_group_col <- input$mint_splsda_group_col
      userState$mint_splsda_group_col2 <- input$mint_splsda_group_col2
      userState$mint_splsda_batch_col <- input$mint_splsda_batch_col
      userState$mint_splsda_var_num <- input$mint_splsda_var_num
      userState$mint_splsda_comp_num <- input$mint_splsda_comp_num
      userState$mint_splsda_log2 <- input$mint_splsda_log2
      userState$mint_splsda_ellipse <- input$mint_splsda_ellipse
      userState$mint_splsda_bg <- input$mint_splsda_bg
      userState$mint_splsda_roc <- input$mint_splsda_roc
      userState$mint_splsda_colors <- input$mint_splsda_colors
      userState$mint_splsda_pch <- as.numeric(input$mint_splsda_pch)
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
                  group_col2 = if (nzchar(input$mint_splsda_group_col2)) {
                    input$mint_splsda_group_col2
                  } else {
                    NULL
                  },
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
                multilevel_column_to_pass <- if (
                  isTRUE(input$splsda_use_multilevel)
                ) {
                  input$splsda_multilevel
                } else {
                  NULL
                }
                cyt_splsda(
                  data = df,
                  output_file = if (mode == "Download") out_file else NULL,
                  progress = prog,
                  group_col = input$splsda_group_col,
                  group_col2 = input$splsda_group_col2,
                  multilevel = multilevel_column_to_pass,
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
              "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)" = {
                # Ensure pch values are numeric
                pch_vals <- as.numeric(input$mint_splsda_pch)

                # Get number of unique groups to set colors
                grp <- df[[input$mint_splsda_group_col]]
                uniq_groups <- unique(grp)
                cols <- if (length(input$mint_splsda_colors)) {
                  input$mint_splsda_colors
                } else {
                  rainbow(length(uniq_groups))
                }

                # Call the new function
                cyt_mint_splsda(
                  data = df,
                  output_file = if (mode == "Download") out_file else NULL,
                  progress = prog,
                  group_col = input$mint_splsda_group_col,
                  group_col2 = input$mint_splsda_group_col2,
                  batch_col = input$mint_splsda_batch_col,
                  var_num = input$mint_splsda_var_num,
                  comp_num = input$mint_splsda_comp_num,
                  scale = if (input$mint_splsda_log2) "log2" else NULL,
                  pch_values = pch_vals,
                  colors = cols,
                  roc = input$mint_splsda_roc,
                  ellipse = input$mint_splsda_ellipse,
                  bg = input$mint_splsda_bg
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

    # Main UI container
    tagList(
      uiOutput("warningText"), # Display warnings if any

      if (mode == "Download") {
        # Simple message for download mode
        h3(if (is.character(res)) res else as.character(res))
      } else {
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
                    withSpinner(plotOutput("mint_splsda_global_plot"))
                  ),
                  tabPanel(
                    "Partial Sample Plots",
                    withSpinner(plotOutput("mint_splsda_partial_plot"))
                  ),
                  tabPanel(
                    "Variable Loadings",
                    withSpinner(uiOutput("mint_splsda_loadings_ui")) # Changed to uiOutput
                  ),
                  tabPanel(
                    "Correlation Circle",
                    withSpinner(plotOutput("mint_splsda_corr_circle_plot"))
                  ),
                  tabPanel(
                    "Heatmap (CIM)",
                    withSpinner(plotOutput(
                      "mint_splsda_cim_plot",
                      height = "600px"
                    ))
                  ),
                  if (!is.null(res$roc_plot)) {
                    tabPanel(
                      "ROC Curve",
                      withSpinner(plotOutput("mint_splsda_roc_plot"))
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
                          withSpinner(plotOutput(paste0(
                            "mint_splsda_global_",
                            trt
                          )))
                        ),
                        tabPanel(
                          "Partial Plots",
                          withSpinner(plotOutput(paste0(
                            "mint_splsda_partial_",
                            trt
                          )))
                        ),
                        tabPanel(
                          "Variable Loadings",
                          withSpinner(uiOutput(paste0(
                            "mint_splsda_loadings_",
                            trt
                          )))
                        ),
                        tabPanel(
                          "Correlation",
                          withSpinner(plotOutput(paste0(
                            "mint_splsda_corr_",
                            trt
                          )))
                        ),
                        tabPanel(
                          "CIM",
                          withSpinner(plotOutput(
                            paste0("mint_splsda_cim_", trt),
                            height = "600px"
                          ))
                        ),
                        if (!is.null(res[[trt]]$roc_plot)) {
                          tabPanel(
                            "ROC",
                            withSpinner(plotOutput(paste0(
                              "mint_splsda_roc_",
                              trt
                            )))
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
          "Principle Component Analysis (PCA)" = {
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
          "Error-BarPlot" = {
            tagList(
              h4("Error-Bar Plot Results"),
              shinycssloaders::withSpinner(
                plotOutput("errorBarPlotOutput", height = "400px"),
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
            } else {
              # Handle functions that return a data frame for a table
              shinycssloaders::withSpinner(
                DT::dataTableOutput("statResults"),
                type = 8
              )
            }
          }
        )
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
    # Ensure this runs only in interactive mode
    if (input$output_mode != "Interactive") {
      return(NULL)
    }

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
        output$mint_splsda_loadings_ui <- renderUI({
          req(res$partial_loadings_plots)
          lapply(seq_along(res$partial_loadings_plots), function(i) {
            plotOutput(paste0("mint_splsda_loading_plot_", i), height = "400px")
          })
        })
        lapply(seq_along(res$partial_loadings_plots), function(i) {
          output[[paste0("mint_splsda_loading_plot_", i)]] <- renderPlot({
            replayPlot(res$partial_loadings_plots[[i]])
          })
        })
        output$mint_splsda_corr_circle_plot <- renderPlot({
          replayPlot(res$correlation_circle_plot)
        })
        output$mint_splsda_cim_plot <- renderPlot(
          {
            replayPlot(res$cim_plot)
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
                req(sub_res$partial_loadings_plots) # Use plural 'plots'
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
                  replayPlot(sub_res$cim_plot)
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
    # --- Skewness/Kurtosis Rendering (FIXED) ---
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
    # --- Generic Table Rendering Logic (for ANOVA, T-Test, etc.) ---
    if (func_name %in% c("ANOVA", "Two-Sample T-Test")) {
      output$statResults <- DT::renderDataTable(
        {
          res
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
  shiny::observeEvent(input$mint_splsda_log2, {
    userState$mint_splsda_log2 <- input$mint_splsda_log2
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
  shiny::observeEvent(input$mint_splsda_pch, {
    userState$mint_splsda_pch <- input$mint_splsda_pch
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
