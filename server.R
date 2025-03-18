# Loading libraries
library(base64enc)
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(readxl)
library(bslib)
library(shinyhelper)

## Define server logic
server <- function(input, output, session) {
  # Helpers
  observe_helpers()
  ## ---------------------------
  ## Theme Toggle (unchanged)
  ## ---------------------------
  observeEvent(input$theme_mode, {
    if (input$theme_mode == "Dark") {
      shinyjs::addClass(selector = "body", class = "dark-theme-fix")
      session$setCurrentTheme(
        bs_theme(
          version = 5,
          bootswatch = "darkly",
          primary = "#0F7BED",
          secondary = "#0F7BED",
          "body-bg" = "#1f1f1f",
          "body-color" = "#e0e0e0",
          "card-bg" = "#2f2f2f",
          "card-border-color" = "#444444",
          "card-color" = "#ffffff",
          "navbar-bg" = "#2f2f2f",
          "navbar-color" = "#e0e0e0",
          font_scale = 1.0
        )
      )
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-theme-fix")
      session$setCurrentTheme(
        bs_theme(
          version = 5,
          bootswatch = "flatly",
          primary = "#0F7BED",
          secondary = "#0F7BED",
          "body-bg" = "#ffffff",
          "body-color" = "#000000",
          "card-bg" = "#ffffff",
          "card-border-color" = "#dddddd",
          "card-color" = "#000000",
          font_scale = 1.0
        )
      )
    }
  })
  
  ## ---------------------------
  ## Wizard Step Control
  ## ---------------------------
  currentStep <- reactiveVal(1)
  output$currentStep <- reactive({ currentStep() })
  outputOptions(output, "currentStep", suspendWhenHidden = FALSE)
  
  ## ---------------------------
  ## Persistent State: Create a reactiveValues object
  ## ---------------------------
  userState <- reactiveValues(
    # General state
    selected_columns = NULL,
    selected_function = NULL,
    
    # ANOVA options
    anova_format_output = NULL,
    
    # Boxplots options
    bp_bin_size = NULL,
    bp_mf_row = NULL,
    bp_y_lim = NULL,
    bp_log2 = NULL,
    
    # Enhanced Boxplots options
    bp2_mf_row = NULL,
    bp2_log2 = NULL,
    bp2_y_lim = NULL,
    
    # Dual-Flashlight Plot options
    df_group_var = NULL,
    df_ssmd_thresh = NULL,
    df_log2fc_thresh = NULL,
    df_top_labels = NULL,
    
    # Heatmap options
    hm_log2 = NULL,
    hm_annotation = NULL,
    
    # PCA options
    pca_group_col = NULL,
    pca_trt_col = NULL,
    pca_comp_num = NULL,
    pca_log2 = NULL,
    pca_ellipse = NULL,
    pca_style = NULL,
    pca_pch = NULL,
    
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
    splsda_trt_col = NULL,
    splsda_var_num = NULL,
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
    
    # Two-Sample T-Test options
    ttest_log2 = NULL,
    ttest_format_output = NULL,
    
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
  userData <- reactive({
    if (isTRUE(input$use_builtin)) {
      req(input$built_in_choice)
      return(get(input$built_in_choice))
    } else if (!is.null(input$datafile)) {
      ext <- tolower(tools::file_ext(input$datafile$name))
      if (ext == "csv") {
        df <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
      } else if (ext == "txt") {
        df <- read.table(input$datafile$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      } else if (ext %in% c("xls", "xlsx")) {
        sheet_num <- ifelse(is.null(input$sheet), 1, input$sheet)
        df <- read_excel(input$datafile$datapath, sheet = sheet_num)
        df <- as.data.frame(df)
      } else {
        stop("Unsupported file type.")
      }
      return(df)
    } else {
      return(NULL)
    }
  })
  
  ## ---------------------------
  ## UI for Sheet Selector and Built-in Data Choice
  ## ---------------------------
  output$sheet_selector <- renderUI({
    req(input$datafile)
    ext <- tolower(tools::file_ext(input$datafile$name))
    if (ext %in% c("xls", "xlsx")) {
      numericInput("sheet", "Sheet Number", value = 1, min = 1)
    } else {
      return(NULL)
    }
  })
  output$built_in_selector <- renderUI({
    if (isTRUE(input$use_builtin)) {
      selectInput("built_in_choice", "Select Built-in Data:",
                  choices = builtInList,
                  selected = builtInList[1])
    } else {
      return(NULL)
    }
  })
  
  ## ---------------------------
  ## Data Filtering and Column Selection
  ## ---------------------------
  filteredData <- reactive({
    df <- userData()
    req(df)
    # Use stored columns if available
    currentCols <- if (!is.null(userState$selected_columns)) {
      userState$selected_columns
    } else {
      input$selected_columns
    }
    if (!is.null(currentCols) && length(currentCols) > 0) {
      df <- df[, currentCols, drop = FALSE]
    }
    factor_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
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
  
  output$column_selection_ui <- renderUI({
    df <- userData()
    if (is.null(df)) return(NULL)
    checkboxGroupInput("selected_columns", "Select Columns:",
                       choices = names(df),
                       selected = if (!is.null(userState$selected_columns)) {
                         userState$selected_columns
                       } else {
                         names(df)
                       })
  })
  
  output$select_buttons_ui <- renderUI({
    df <- userData()
    if (is.null(df)) return(NULL)
    tagList(
      fluidRow(
        div(style = "text-align: left;",
            div(style = "display: inline-block; margin-right: 10px;",
                actionButton("select_all", "Select All")
            ),
            div(style = "display: inline-block;",
                actionButton("deselect_all", "Deselect All")
            )
        )
      ),
      # Put the <br> after the fluidRow, so you get a blank line below the buttons
      br()
    )
  })
  
  observeEvent(input$select_all, {
    df <- userData()
    if (!is.null(df)) {
      updateCheckboxGroupInput(session, "selected_columns",
                               choices = names(df),
                               selected = names(df))
    }
  })
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "selected_columns",
                             selected = character(0))
  })
  
  output$filter_ui <- renderUI({
    df <- filteredData()
    if (is.null(df)) return(NULL)
    factor_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    if (length(factor_cols) == 0) return(NULL)
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
  output$function_options_ui <- renderUI({
    req(input$selected_function)
    userState$selected_function <- input$selected_function
    func_name <- input$selected_function
    ui_list <- list()
    switch(func_name,
           "ANOVA" = {
             ui_list <- tagList(
               checkboxInput("anova_format_output", "Format output as tidy table", 
                             value = isolate(userState$anova_format_output) %||% FALSE)
             )
           },
           "Boxplots" = {
             ui_list <- tagList(
               numericInput("bp_bin_size", label = helper(type = "inline", 
                                                          title = "Bin size for boxplots", 
                                                          icon = "question-circle",
                                                          shiny_tag = "Bin Size‎ ‎ ‎ ‎ ‎  ",
                                                          content = "Determines the number of columns (variables) to group together in each set of box plots. 
                        For example, a bin size of 25 will display box plots for up to 25 columns (variables) at a time. 
                        If there are more columns, multiple sets of box plots will be generated.", 
                                                        if(input$theme_mode == "Dark"){
                                                          colour = "red"
                                                        }else{
                                                          colour = "blue"
                                                        }),
                            value = isolate(userState$bp_bin_size) %||% 25, min = 1),
               
               textInput("bp_mf_row", label = helper(type = "inline", 
                                                          title = "Graphs per Row and Column", 
                                                          icon = "question-circle",
                                                          shiny_tag = HTML("Graphs per Row and Columns<br>(rows, cols; comma-separated) ‎ ‎ ‎ ‎ ‎ ‎ ‎ "),
                                                          content = "The number of rows and columns for the boxplots. 
                        For example, '2,2' will display 4 boxplots in a 2x2 grid.", 
                                                     if(input$theme_mode == "Dark"){
                                                       colour = "red"
                                                     }else{
                                                       colour = "blue"
                                                     }),
                         value = isolate(userState$bp_mf_row) %||% "1,1"),
               
               textInput("bp_y_lim", label = helper(type = "inline", 
                                                    title = "Y-axis Limits", 
                                                    icon = "question-circle",
                                                    shiny_tag = HTML("Y-axis Limits<br>(min, max; comma-separated; leave blank for auto) ‎ ‎ ‎ ‎ ‎ ‎ ‎ ‎ "),
                                                    content = "The minimum and maximum values for the y-axis. Leave blank to automatically determine the limits.
                                                    This controls the vertical scale of the plot.", 
                                                    if(input$theme_mode == "Dark"){
                                                      colour = "red"
                                                    }else{
                                                      colour = "blue"
                                                    }), 
                         value = isolate(userState$bp_y_lim) %||% ""), 
                         
               checkboxInput("bp_log2", label = helper(type = "inline", 
                                                    title = "Apply log2 transformation", 
                                                    icon = "exclamation",
                                                    shiny_tag = "Apply log2 transformation ‎ ‎ ‎ ‎ ‎ ",
                                                    content = "Apply a log2 transformation to the data before generating the boxplots. This transformation can
                                                    help manage data that span a wide range of values.", 
                                                    if(input$theme_mode == "Dark"){
                                                      colour = "red"
                                                    }else{
                                                      colour = "blue"
                                                    }), 
                             value = isolate(userState$bp_log2) %||% FALSE)
             )
           },
           "Enhanced Boxplots" = {
             ui_list <- tagList(
               textInput("bp2_mf_row", 
                         label = helper(type = "inline", 
                                        title = "Graphs per Row and Column", 
                                        icon = "question-circle",
                                        shiny_tag = HTML("Graphs per Row and Columns<br>(rows, cols; comma-separated) ‎ ‎ ‎ ‎ ‎ ‎ ‎ "),
                                        content = "The number of rows and columns for the boxplots. 
                                        For example, '2,2' will display 4 boxplots in a 2x2 grid.", 
                                        if(input$theme_mode == "Dark"){
                                          colour = "red"
                                        }else{
                                          colour = "blue"
                                        }),
                         value = isolate(userState$bp2_mf_row) %||% "1,1"),
               textInput("bp2_y_lim",
                         label = helper(type = "inline", 
                                        title = "Y-axis Limits", 
                                        icon = "question-circle",
                                        shiny_tag = HTML("Y-axis Limits<br>(min, max; comma-separated; leave blank for auto) ‎ ‎ ‎ ‎ ‎ ‎ ‎ ‎ "),
                                        content = "The minimum and maximum values for the y-axis. Leave blank to automatically determine the limits.
                                        This controls the vertical scale of the plot.", 
                                        if(input$theme_mode == "Dark"){
                                          colour = "red"
                                        }else{
                                          colour = "blue"
                                        }),
                         value = isolate(userState$bp2_y_lim) %||% ""),
               checkboxInput("bp2_log2", 
                             label = helper(type = "inline", 
                                            title = "Apply log2 transformation", 
                                            icon = "exclamation",
                                            shiny_tag = "Apply log2 transformation ‎ ‎ ‎ ‎ ‎ ",
                                            content = "Apply a log2 transformation to the data before generating the boxplots.
                                             This transformation can help manage data that span a wide range of values.", 
                                            if(input$theme_mode == "Dark"){
                                              colour = "red"
                                            }else{
                                              colour = "blue"
                                            }),
                             value = isolate(userState$bp2_log2) %||% FALSE)
             )
           },
           "Dual-Flashlight Plot" = {
             df <- filteredData()
             if (is.null(df)) return(NULL)
             cols <- names(df)
             ui_list <- tagList(
               selectInput("df_group_var",
                           label = helper(type = "inline", 
                                          title = "Grouping Variable", 
                                          icon = "question-circle",
                                          shiny_tag = "Grouping Variable ‎ ‎ ‎ ‎ ‎ ",
                                          content = "The column to use for grouping the data. For example, a column that
                                          specifies categories such as 'Control' or 'Treatment'.", 
                                          if(input$theme_mode == "Dark"){
                                            colour = "red"
                                          }else{
                                            colour = "blue"
                                          }), 
                             , choices = cols, 
                           selected = isolate(userState$df_group_var) %||% cols[1]),
               uiOutput("df_conditions_ui"),
               numericInput("df_ssmd_thresh", 
                            label = helper(type = "inline", 
                                           title = "SSMD Threshold", 
                                           icon = "question-circle",
                                           shiny_tag = "SSMD Threshold ‎ ‎ ‎ ‎ ‎ ",
                                           content = "The threshold for the SSMD (strictly standardized mean difference) value. 
                                           SSMD is a measure of how different two groups are. A higher threshold means that 
                                           only larger differences are considered significant.",
                                           if(input$theme_mode == "Dark"){
                                             colour = "red"
                                           }else{
                                             colour = "blue"
                                           }), 
                            value = isolate(userState$df_ssmd_thresh) %||% 1, min = 0),
               numericInput("df_log2fc_thresh",
                            label = helper(type = "inline", 
                                           title = "Log2 Fold Change Threshold", 
                                           icon = "question-circle",
                                           shiny_tag = "Log2 Fold Change Threshold ‎ ‎ ‎ ‎ ‎ ",
                                           content = "The threshold for the log2 fold change value.
                                           Fold change shows the ratio of difference between groups on a logarithmic scale; 
                                           for example, a value of 1 represents a doubling or halving.",
                                           if(input$theme_mode == "Dark"){
                                             colour = "red"
                                           }else{
                                             colour = "blue"
                                           }), 
                            value = isolate(userState$df_log2fc_thresh) %||% 1, min = 0),
               numericInput("df_top_labels",
                            label = helper(type = "inline", 
                                           title = "Top Labels", 
                                           icon = "exclamation",
                                           shiny_tag = "Top Labels ‎ ‎ ‎ ‎ ‎ ",
                                           content = "The number of top labels to display on the plot. Usually, the top labels are 
                                           the most significant points.",
                                           if(input$theme_mode == "Dark"){
                                             colour = "red"
                                           }else{
                                             colour = "blue"
                                           }), 
                            value = isolate(userState$df_top_labels) %||% 15, min = 1)
             )
           },
           "Heatmap" = {
             df <- filteredData()
             if (is.null(df)) return(NULL)
             ann_choices <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
             ui_list <- tagList(
               checkboxInput("hm_log2",
                             label = helper(
                               type = "inline", 
                               title = "Apply log2 transformation", 
                               icon = "exclamation",
                               shiny_tag = "Apply log2 transformation ‎ ‎ ‎ ‎ ‎ ",
                               content = "Apply a log2 transformation to the data before generating the heatmap.
                               This transformation can help manage data that span a wide range of values.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }),
                             value = isolate(userState$hm_log2) %||% FALSE),
               selectInput("hm_annotation",
                           label = helper(
                             type = "inline", 
                             title = "Annotation Column", 
                             icon = "question-circle",
                             shiny_tag = "Annotation Column ‎ ‎ ‎ ‎ ‎ ",
                             content = "The column to use for annotating the heatmap.
                             This column will add color coding to help differentiate groups", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), 
                           choices = ann_choices, 
                           selected = isolate(userState$hm_annotation) %||% (if(length(ann_choices)>0) ann_choices[1] else NULL))
             )
           },
           "Principle Component Analysis (PCA)" = {
             df <- filteredData()
             if (is.null(df)) return(NULL)
             cols <- names(df)
             ui_list <- tagList(
               selectInput("pca_group_col", 
                           label = helper(
                             type = "inline", 
                             title = "Grouping Column 1", 
                             icon = "question-circle",
                             shiny_tag = "Grouping Column 1 ‎ ‎ ‎ ‎ ‎ ",
                             content = "The first column to use for grouping the data.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), choices = cols, 
                           selected = isolate(userState$pca_group_col) %||% cols[1]),
               selectInput("pca_trt_col",
                           label = helper(
                             type = "inline", 
                             title = "Grouping Column 2", 
                             icon = "question-circle",
                             shiny_tag = "Grouping Column 2 ‎ ‎ ‎ ‎ ‎ ",
                             content = "The second column to use for grouping the data.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), choices = cols, 
                           selected = isolate(userState$pca_trt_col) %||% cols[1]),
               numericInput("pca_comp_num",
                            label = helper(
                              type = "inline", 
                              title = "Number of Components", 
                              icon = "question-circle",
                              shiny_tag = "Number of Components ‎ ‎ ‎ ‎ ‎ ",
                              content = "The number of components to use for the PCA analysis. Must be at least 2.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }), 
                            value = isolate(userState$pca_comp_num) %||% 2, min = 2),
               checkboxInput("pca_log2",
                             label = helper(
                               type = "inline", 
                               title = "Apply log2 transformation", 
                               icon = "exclamation",
                               shiny_tag = "Apply log2 transformation ‎ ‎ ‎ ‎ ‎ ",
                               content = "Apply a log2 transformation to the data before generating the PCA plot.
                               This transformation can help manage data that span a wide range of values.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }), 
                             value = isolate(userState$pca_log2) %||% FALSE),
               checkboxInput("pca_ellipse",
                             label = helper(
                               type = "inline", 
                               title = "Draw Ellipse", 
                               icon = "exclamation",
                               shiny_tag = "Draw Ellipse ‎ ‎ ‎ ‎ ‎ ",
                               content = "Draw an ellipse around the data points on the PCA plot. (Draws an ellipse covering 95% of the data points.)", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }), 
                             value = isolate(userState$pca_ellipse) %||% FALSE),
               selectInput("pca_style",
                           label = helper(
                             type = "inline", 
                             title = "Plot Style", 
                             icon = "question-circle",
                             shiny_tag = "Plot Style ‎ ‎ ‎ ‎ ‎ ",
                             content = "The style of the PCA plot. Choose between 2D and 3D. Requires at least 3 components for 3D.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), choices = c("2D", "3D"), 
                           selected = isolate(userState$pca_style) %||% "2D"),
               textInput("pca_pch",
                         label = helper(
                           type = "inline", 
                           title = "PCH Values", 
                           icon = "question-circle",
                           shiny_tag = "PCH Values ‎ ‎ ‎ ‎ ‎ ",
                           content = "The plotting character (PCH) values to use for plotting the data points. Separate multiple values with a comma.
                           Click <a href='https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/points'>here</a> for more information on PCH values.", 
                           if(input$theme_mode == "Dark"){
                             colour = "red"
                           }else{
                             colour = "blue"
                           }), 
                         value = isolate(userState$pca_pch) %||% "16,4")
             )
           },
           "Random Forest" = {
             df <- filteredData()
             if (is.null(df)) return(NULL)
             cols <- names(df)
             ui_list <- tagList(
               selectInput("rf_group_col",
                           label = helper(
                             type = "inline", 
                             title = "Grouping Column", 
                             icon = "question-circle",
                             shiny_tag = "Grouping Column ‎ ‎ ‎ ‎ ‎ ",
                             content = "The column to use for grouping the data.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), choices = cols, 
                           selected = isolate(userState$rf_group_col) %||% cols[1]),
               numericInput("rf_ntree",
                            label = helper(
                              type = "inline", 
                              title = "Number of Trees", 
                              icon = "question-circle",
                              shiny_tag = "Number of Trees ‎ ‎ ‎ ‎ ‎ ",
                              content = "The number of trees to grow in the random forest model.
                              Each tree is a simple model; more trees can improve predictions but take longer to compute.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }),
                            value = isolate(userState$rf_ntree) %||% 500, min = 1),
               numericInput("rf_mtry",
                            label = helper(
                              type = "inline", 
                              title = "Number of Variables to Split", 
                              icon = "question-circle",
                              shiny_tag = "Number of Variables to Split ‎ ‎ ‎ ‎ ‎ ",
                              content = "The number of variables to randomly select at each split.
                              At each decision point, the model randomly considers this number of variables, which helps improve model accuracy.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }),
                            value = isolate(userState$rf_mtry) %||% 5, min = 1),
               numericInput("rf_train_fraction",
                            label = helper(
                              type = "inline", 
                              title = "Train Fraction", 
                              icon = "question-circle",
                              shiny_tag = "Train Fraction ‎ ‎ ‎ ‎ ‎ ",
                              content = "The fraction of the data to use for training the random forest model.
                              The remainder is used for testing the model.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }),
                            value = isolate(userState$rf_train_fraction) %||% 0.7, min = 0.1, max = 0.9, step = 0.1),
               checkboxInput("rf_plot_roc",
                             label = helper(
                               type = "inline", 
                               title = "Plot ROC", 
                               icon = "exclamation",
                               shiny_tag = "Plot ROC (Binary Comparison Only) ‎ ‎ ‎ ‎ ‎ ",
                               content = "Plot the Recieving Operating Characteristic (ROC) curve for the random forest model.
                               Requires comparison to be a binary comparison (at most 2 groups). The ROC curve helps evaluate 
                               the model’s performance in distinguishing between two groups.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }),
                             value = isolate(userState$rf_plot_roc) %||% FALSE),
               checkboxInput("rf_run_rfcv", 
                             label = helper(
                               type = "inline", 
                               title = "Run RFCV?", 
                               icon = "exclamation",
                               shiny_tag = "Run RFCV ‎ ‎ ‎ ‎ ‎ ",
                               content = "Run Recursive Feature Elimination (RFE) with Cross-Validation (CV) to determine 
                               the optimal number of variables to include in the model.This process automatically tests 
                               different combinations of variables to find the ones that best predict the outcome.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }), 
                             value = isolate(userState$rf_run_rfcv) %||% TRUE),
               conditionalPanel(
                 condition = "input.rf_run_rfcv == true",
                 numericInput("rf_k_folds",
                              label = helper(
                                type = "inline", 
                                title = "Number of Folds", 
                                icon = "question-circle",
                                shiny_tag = "Number of Folds ‎ ‎ ‎ ‎ ‎ ",
                                content = "The number of folds to use for cross-validation in the RFCV process.
                                Folds are subsets of the data used to train and test the model. More folds can improve accuracy but take longer to compute.", 
                                if(input$theme_mode == "Dark"){
                                  colour = "red"
                                }else{
                                  colour = "blue"
                                }), 
                              value = isolate(userState$rf_k_folds) %||% 5, min = 2),
                 numericInput("rf_step", 
                              label = helper(
                                type = "inline", 
                                title = "Step Size", 
                                icon = "question-circle",
                                shiny_tag = "Step Size ‎ ‎ ‎ ‎ ‎ ",
                                content = "The step size to use for the RFCV process. Must be between 0.1 and 0.9.
                                The value controls how quickly the model eliminates variables. A smaller step size can help find the best variables.", 
                                if(input$theme_mode == "Dark"){
                                  colour = "red"
                                }else{
                                  colour = "blue"
                                }), 
                              value = isolate(userState$rf_step) %||% 0.5, min = 0.1, max = 0.9, step = 0.1)
               )
             )
           },
           "Skewness/Kurtosis" = {
             df <- filteredData()
             if (is.null(df)) return(NULL)
             ui_list <- tagList(
               selectInput("skku_group_cols", 
                           label = helper(
                             type = "inline", 
                             title = "Grouping Columns", 
                             icon = "question-circle",
                             shiny_tag = "Grouping Columns ‎ ‎ ‎ ‎ ‎ ",
                             content = "The columns to use for grouping the data.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), 
                           choices = names(df), multiple = TRUE,
                           selected = isolate(userState$skku_group_cols)),
               # Checkbox for printing raw results
               checkboxInput("skku_print_raw", 
                             label = helper(
                               type = "inline", 
                               title = "Print Raw Results", 
                               icon = "exclamation",
                               shiny_tag = "Print Raw Results ‎ ‎ ‎ ‎ ‎ ",
                               content = "Print the raw skewness and kurtosis values for each column in the data.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }), 
                             value = isolate(userState$skku_print_raw) %||% FALSE),
               # Checkbox for printing log transformed results
               checkboxInput("skku_print_log", 
                             label = helper(
                               type = "inline", 
                               title = "Print Log-Transformed Results", 
                               icon = "exclamation",
                               shiny_tag = "Print Log-Transformed Results ‎ ‎ ‎ ‎ ‎ ",
                               content = "Print the skewness and kurtosis values for each column after applying a log2 transformation.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }), 
                             value = isolate(userState$skku_print_log) %||% FALSE)
             )
           },
           "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = {
             df <- filteredData()
             if (is.null(df)) return(NULL)
             cols <- names(df)
             ui_list <- tagList(
               selectInput("splsda_group_col", 
                           label = helper(
                             type = "inline", 
                             title = "Grouping Column 1", 
                             icon = "question-circle",
                             shiny_tag = "Grouping Column 1 ‎ ‎ ‎ ‎ ‎ ",
                             content = "The first column to use for grouping the data.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), choices = cols, 
                           selected = isolate(userState$splsda_group_col) %||% cols[1]),
               selectInput("splsda_trt_col", 
                           label = helper(
                             type = "inline", 
                             title = "Grouping Column 2", 
                             icon = "question-circle",
                             shiny_tag = "Grouping Column 2 ‎ ‎ ‎ ‎ ‎ ",
                             content = "The second column to use for grouping the data.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), choices = cols, 
                           selected = isolate(userState$splsda_trt_col) %||% cols[1]),
               numericInput("splsda_var_num", 
                            label = helper(
                              type = "inline", 
                              title = "Number of Variables", 
                              icon = "question-circle",
                              shiny_tag = "Number of Variables ‎ ‎ ‎ ‎ ‎ ",
                              content = "The number of variables to use for the sPLS-DA analysis. It should match
                              the number of variables selected for the analysis.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }),
                            value = isolate(userState$splsda_var_num) %||% 25, min = 1),
               selectInput("splsda_cv_opt",
                           label = helper(
                             type = "inline", 
                             title = "Cross-Validation Option", 
                             icon = "question-circle",
                             shiny_tag = "Cross-Validation Option ‎ ‎ ‎ ‎ ‎ ",
                             content = "The cross-validation option to use for the sPLS-DA analysis.
                             Cross-validation helps evaluate the model's performance. Choose between None, LOOCV, and Mfold.
                             LOOCV = Leave-One-Out Cross-Validation (LOOCV) which works by training the model on all data except one sample and then testing on the left-out sample.
                             Mfold = M-Fold Cross-Validation which works by splitting the data into M groups and training the model on M-1 groups and testing on the left-out group. This process is repeated 1000 times.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), 
                           choices = c("None", "LOOCV", "Mfold"), 
                           selected = isolate(userState$splsda_cv_opt) %||% "None"),
               conditionalPanel(
                 condition = "input.splsda_cv_opt == 'Mfold'",
                 numericInput("splsda_fold_num", 
                              label = helper(
                                type = "inline", 
                                title = "Number of Folds", 
                                icon = "question-circle",
                                shiny_tag = "Number of Folds ‎ ‎ ‎ ‎ ‎ ",
                                content = "The number of folds to use for the M-Fold Cross-Validation process. More folds can improve 
                                accuracy but take longer to compute.",
                                if(input$theme_mode == "Dark"){
                                  colour = "red"
                                }else{
                                  colour = "blue"
                                }), 
                              value = isolate(userState$splsda_fold_num) %||% 5, min = 2)
               ),
               checkboxInput("splsda_log2", 
                             label = helper(
                               type = "inline", 
                               title = "Apply log2 transformation", 
                               icon = "exclamation",
                               shiny_tag = "Apply log2 transformation ‎ ‎ ‎ ‎ ‎ ",
                               content = "Apply a log2 transformation to the data before generating the sPLS-DA plot.
                               This transformation can help manage data that span a wide range of values.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }), 
                             value = isolate(userState$splsda_log2) %||% FALSE),
               numericInput("splsda_comp_num",
                            label = helper(
                              type = "inline", 
                              title = "Number of Components", 
                              icon = "question-circle",
                              shiny_tag = "Number of Components ‎ ‎ ‎ ‎ ‎ ",
                              content = "The number of components to use for the sPLS-DA analysis. Must be at least 2.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }), 
                            value = isolate(userState$splsda_comp_num) %||% 2, min = 2),
               textInput("splsda_pch", 
                         label = helper(
                           type = "inline", 
                           title = "PCH Values", 
                           icon = "question-circle",
                           shiny_tag = "PCH Values ‎ ‎ ‎ ‎ ‎ ",
                           content = "The plotting character (PCH) values to use for plotting the data points. Separate multiple values with a comma.
                           Click <a href='https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/points'>here</a> for more information on PCH values.", 
                           if(input$theme_mode == "Dark"){
                             colour = "red"
                           }else{
                             colour = "blue"
                           }), 
                         value = isolate(userState$splsda_pch) %||% "16,4"),
               selectInput("splsda_style", 
                           label = helper(
                             type = "inline", 
                             title = "Plot Style", 
                             icon = "question-circle",
                             shiny_tag = "Plot Style ‎ ‎ ‎ ‎ ‎ ",
                             content = "The style of the sPLS-DA plot. Choose between 2D and 3D. Requires at least 3 components for 3D.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), choices = c("2D", "3D"), 
                           selected = isolate(userState$splsda_style) %||% "2D"),
               checkboxInput("splsda_roc", 
                             label = helper(
                               type = "inline", 
                               title = "Plot ROC", 
                               icon = "question-circle",
                               shiny_tag = "Plot ROC  ‎ ‎ ‎ ‎ ‎ ",
                               content = "Plot the Recieving Operating Characteristic (ROC) curve for the sPLS-DA model. This curve shows
                               how well the model distinguishes between classes.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }), 
                             value = isolate(userState$splsda_roc) %||% FALSE),
               checkboxInput("splsda_ellipse", 
                             label = helper(
                               type = "inline", 
                               title = "Draw Ellipse", 
                               icon = "exclamation",
                               shiny_tag = "Draw Ellipse ‎ ‎ ‎ ‎ ‎ ",
                               content = "Draw an ellipse around the data points on the sPLS-DA plot. (Draws an ellipse covering 95% of the data points.)", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }), 
                             value = isolate(userState$splsda_ellipse) %||% FALSE),
               checkboxInput("splsda_bg",
                             label = helper(
                               type = "inline", 
                               title = "Background Color", 
                               icon = "exclamation",
                               shiny_tag = "Shaded Background Prediction ‎ ‎ ‎ ‎ ‎ ",
                               content = "Draws a shaded background prediction on the sPLS-DA plot. 
                               This shading indicates the model’s predicted classification regions", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }),
                             value = isolate(userState$splsda_bg) %||% FALSE),
               checkboxInput("splsda_conf_mat", 
                             label = helper(
                               type = "inline", 
                               title = "Confusion Matrix", 
                               icon = "exclamation",
                               shiny_tag = "Confusion Matrix ‎ ‎ ‎ ‎ ‎ ",
                               content = "Display the confusion matrix for the sPLS-DA model. This table shows how many
                               predictions were correct and provides key performance measures.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }), 
                             value = isolate(userState$splsda_conf_mat) %||% FALSE)
             )
           },
           "Two-Sample T-Test" = {
             ui_list <- tagList(
               checkboxInput("ttest_log2",
                             label = helper(
                               type = "inline", 
                               title = "Apply log2 transformation", 
                               icon = "exclamation",
                               shiny_tag = "Apply log2 transformation ‎ ‎ ‎ ‎ ‎ ",
                               content = "Apply a log2 transformation to the data before performing the two-sample t-test.
                               This transformation can help manage data that span a wide range of values.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }), 
                             value = isolate(userState$ttest_log2) %||% FALSE),
               checkboxInput("ttest_format_output", "Format output as tidy table", 
                             value = isolate(userState$ttest_format_output) %||% FALSE)
             )
           },
           "Volcano Plot" = {
             df <- filteredData()
             if (is.null(df)) return(NULL)
             cols <- names(df)
             ui_list <- tagList(
               selectInput("volc_group_col", 
                           label = helper(
                             type = "inline", 
                             title = "Grouping Column", 
                             icon = "question-circle",
                             shiny_tag = "Grouping Column ‎ ‎ ‎ ‎ ‎ ",
                             content = "The column to use for grouping the data.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), choices = cols, 
                           selected = isolate(userState$volc_group_col) %||% cols[1]),
               uiOutput("volc_conditions_ui"),
               numericInput("volc_fold_change_thresh",
                            label = helper(
                              type = "inline", 
                              title = "Fold Change Threshold", 
                              icon = "question-circle",
                              shiny_tag = "Fold Change Threshold ‎ ‎ ‎ ‎ ‎ ",
                              content = "The threshold for the fold change value.
                              Fold change indicates the ratio of differences between groups; 
                              values above this threshold are considered significant.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }), 
                            value = isolate(userState$volc_fold_change_thresh) %||% 2, min = 0),
               numericInput("volc_p_value_thresh", 
                            label = helper(
                              type = "inline", 
                              title = "P-Value Threshold", 
                              icon = "question-circle",
                              shiny_tag = "P-Value Threshold ‎ ‎ ‎ ‎ ‎ ",
                              content = "The threshold for the p-value. P-values indicate the probability that the 
                              observed difference is due to chance; lower values suggest more significant differences.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }), 
                            value = isolate(userState$volc_p_value_thresh) %||% 0.05, min = 0, step = 0.01),
               numericInput("volc_top_labels", 
                            label = helper(
                              type = "inline", 
                              title = "Top Labels", 
                              icon = "exclamation",
                              shiny_tag = "Top Labels ‎ ‎ ‎ ‎ ‎ ",
                              content = "The number of top labels to display on the plot.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }), 
                            value = isolate(userState$volc_top_labels) %||% 10, min = 1)
             )
           },
           "Extreme Gradient Boosting (XGBoost)" = {
             df <- filteredData()
             if (is.null(df)) return(NULL)
             cols <- names(df)
             ui_list <- tagList(
               selectInput("xgb_group_col", 
                           label = helper(
                             type = "inline", 
                             title = "Grouping Column", 
                             icon = "question-circle",
                             shiny_tag = "Grouping Column ‎ ‎ ‎ ‎ ‎ ",
                             content = "The column to use for grouping the data.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), choices = cols, 
                           selected = isolate(userState$xgb_group_col) %||% cols[1]),
               numericInput("xgb_train_fraction", 
                            label = helper(
                              type = "inline", 
                              title = "Train Fraction", 
                              icon = "question-circle",
                              shiny_tag = "Train Fraction ‎ ‎ ‎ ‎ ‎ ",
                              content = "The fraction of the data to use for training the XGBoost model.
                              Remainder is used for testing the model.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }), 
                            value = isolate(userState$xgb_train_fraction) %||% 0.7, min = 0.1, max = 0.9, step = 0.1),
               numericInput("xgb_nrounds", 
                            label = helper(
                              type = "inline", 
                              title = "Number of Rounds", 
                              icon = "question-circle",
                              shiny_tag = "Number of Rounds ‎ ‎ ‎ ‎ ‎ ",
                              content = "The number of rounds to grow the trees in the XGBoost model.
                              Each round adds a new tree to improve the model’s predictions.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }),
                            value = isolate(userState$xgb_nrounds) %||% 500, min = 100),
               numericInput("xgb_max_depth", 
                            label = helper(
                              type = "inline", 
                              title = "Maximum Depth", 
                              icon = "question-circle",
                              shiny_tag = "Maximum Depth ‎ ‎ ‎ ‎ ‎ ",
                              content = "The maximum depth of the trees in the XGBoost model.
                              Deeper trees can capture more complex relationships but may risk overfitting.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }), 
                            value = isolate(userState$xgb_max_depth) %||% 6, min = 1),
               numericInput("xgb_eta", 
                            label = helper(
                              type = "inline", 
                              title = "Learning Rate", 
                              icon = "question-circle",
                              shiny_tag = "Learning Rate ‎ ‎ ‎ ‎ ‎ ",
                              content = "The learning rate of the XGBoost model.
                              This controls how much each new tree influences the model; 
                              lower rates make the model learn more slowly but can improve accuracy.", 
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }), 
                            value = isolate(userState$xgb_eta) %||% 0.1, min = 0, step = 0.01),
               selectInput("xgb_eval_metric", 
                           label = helper(
                             type = "inline", 
                             title = "Evaluation Metric", 
                             icon = "question-circle",
                             shiny_tag = "Evaluation Metric ‎ ‎ ‎ ‎ ‎ ",
                             content = "The evaluation metric to use for the XGBoost model.
                             Choose 'mlogloss' to measure classification error or 'auc' for the area under the ROC curve.", 
                             if(input$theme_mode == "Dark"){
                               colour = "red"
                             }else{
                               colour = "blue"
                             }), 
                           choices = c("mlogloss", "auc"), 
                           selected = isolate(userState$xgb_eval_metric) %||% "mlogloss"),
               numericInput("xgb_top_n_features", 
                            label = helper(
                              type = "inline",
                              title = "Top Number of Features",
                              icon = "exclamation",
                              shiny_tag = "Top Number of Features ‎ ‎ ‎ ‎ ‎ ",
                              content = "The number of top features to display on the plot.",
                              if(input$theme_mode == "Dark"){
                                colour = "red"
                              }else{
                                colour = "blue"
                              }), 
                            value = isolate(userState$xgb_top_n_features) %||% 10, min = 1),
               checkboxInput("xgb_plot_roc",
                             label = helper(
                               type = "inline", 
                               title = "Plot ROC", 
                               icon = "question-circle",
                               shiny_tag = "Plot ROC (Binary Comparison Only) ‎ ‎ ‎ ‎ ‎ ",
                               content = "Plot the Recieving Operating Characteristic (ROC) curve for the XGBoost model.
                               Requires comparison to be a binary comparison (at most 2 groups). The ROC curve 
                               helps evaluate how well the model distinguishes between two groups.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }), 
                             value = isolate(userState$xgb_plot_roc) %||% FALSE),
               checkboxInput("xgb_cv", 
                             label = helper(
                               type = "inline", 
                               title = "Cross-Validation", 
                               icon = "exclamation",
                               shiny_tag = "Cross-Validation ‎ ‎ ‎ ‎ ‎ ",
                               content = "Perform cross-validation on the XGBoost model.
                               Cross-validation helps evaluate the model’s performance on different subsets of the data.", 
                               if(input$theme_mode == "Dark"){
                                 colour = "red"
                               }else{
                                 colour = "blue"
                               }),
                             value = isolate(userState$xgb_cv) %||% FALSE),
               conditionalPanel(
                 condition = "input.xgb_cv == true",
                 numericInput("xgb_nfold", 
                              label = helper(
                                type = "inline", 
                                title = "Number of Folds", 
                                icon = "question-circle",
                                shiny_tag = "Number of Folds ‎ ‎ ‎ ‎ ‎ ",
                                content = "The number of folds to use for cross-validation in the XGBoost model.
                                The folds are used to train and test the model. More folds can improve accuracy but take longer to compute.", 
                                if(input$theme_mode == "Dark"){
                                  colour = "red"
                                }else{
                                  colour = "blue"
                                }), 
                              value = isolate(userState$xgb_nfold) %||% 5, min = 2)
               )
             )
           }
    )
    do.call(tagList, ui_list)
  })
  
  output$df_conditions_ui <- renderUI({
    req(filteredData())
    req(input$df_group_var)
    df <- filteredData()
    choices <- unique(df[[input$df_group_var]])
    if (length(choices) < 2) {
      return(helpText("Not enough unique levels in grouping column"))
    }
    tagList(
      selectInput("df_cond1", 
                  label = helper(
                    type = "inline", 
                    title = "Condition 1", 
                    icon = "question-circle",
                    shiny_tag = "Condition 1 ‎ ‎ ‎ ‎ ‎ ",
                    content = "The first condition to compare.", 
                    if(input$theme_mode == "Dark"){
                      colour = "red"
                    }else{
                      colour = "blue"
                    }), choices = choices, selected = choices[1]),
      selectInput("df_cond2", 
                  label = helper(
                    type = "inline", 
                    title = "Condition 2", 
                    icon = "question-circle",
                    shiny_tag = "Condition 2 ‎ ‎ ‎ ‎ ‎ ",
                    content = "The second condition to compare.", 
                    if(input$theme_mode == "Dark"){
                      colour = "red"
                    }else{
                      colour = "blue"
                    }), choices = choices, selected = choices[2])
    )
  })
  
  output$volc_conditions_ui <- renderUI({
    req(filteredData())
    req(input$volc_group_col)
    df <- filteredData()
    choices <- unique(df[[input$volc_group_col]])
    if (length(choices) >= 2) {
      tagList(
        selectInput("volc_cond1",
                    label = helper(
                      type = "inline", 
                      title = "Condition 1", 
                      icon = "question-circle",
                      shiny_tag = "Condition 1 ‎ ‎ ‎ ‎ ‎ ",
                      content = "The first condition to compare.", 
                      if(input$theme_mode == "Dark"){
                        colour = "red"
                      }else{
                        colour = "blue"
                      }), choices = choices, selected = choices[1]),
        selectInput("volc_cond2",
                    label = helper(
                      type = "inline", 
                      title = "Condition 2", 
                      icon = "question-circle",
                      shiny_tag = "Condition 2 ‎ ‎ ‎ ‎ ‎ ",
                      content = "The second condition to compare.", 
                      if(input$theme_mode == "Dark"){
                        colour = "red"
                      }else{
                        colour = "blue"
                      }), choices = choices, selected = choices[2])
      )
    } else {
      helpText("Selected grouping variable does not have at least two unique values.")
    }
  })
  
  ## ---------------------------
  ## Wizard Navigation UI
  ## ---------------------------
  output$wizardUI <- renderUI({
    step <- currentStep()
    if (step == 1) {
      tagList(
        h3("Step 1: Upload Data"),
        fileInput("datafile", 
                  HTML("Upload Data File<br>Accepted Formats: ('.csv', '.txt', '.xls', '.xlsx')"), 
                  accept = c(".csv", ".txt", ".xls", ".xlsx")),
        checkboxInput("use_builtin", "Use built-in data?", FALSE),
        uiOutput("built_in_selector"),
        uiOutput("sheet_selector"),
        actionButton("next1", "Next")
      )
    } else if (step == 2) {
      tagList(
        h3("Step 2: Select Columns & Apply Filters"),
        uiOutput("column_selection_ui"),
        uiOutput("select_buttons_ui"),
        uiOutput("filter_ui"),
        fluidRow(
          div(style = "text-align: left;",
              div(style = "display: inline-block; margin-right: 10px;",
                  actionButton("back2", "Back")
              ),
              div(style = "display: inline-block;",
                  actionButton("next2", "Next")
              )
          )
        )
      )
    } else if (step == 3) {
      tagList(
        h3("Step 3: Analysis Options"),
        selectInput("selected_function", "Select Analysis Function:",
                    choices = c("ANOVA",
                                "Boxplots",
                                "Enhanced Boxplots",
                                "Dual-Flashlight Plot",
                                "Heatmap",
                                "Principle Component Analysis (PCA)",
                                "Random Forest",
                                "Skewness/Kurtosis",
                                "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)",
                                "Two-Sample T-Test",
                                "Volcano Plot",
                                "Extreme Gradient Boosting (XGBoost)"),
                    selected = isolate(userState$selected_function) %||% "ANOVA"),
        uiOutput("function_options_ui"),
        radioButtons("output_mode", "Output Mode:",
                     choices = c("Interactive", "Download"),
                     selected = "Interactive"),
        conditionalPanel(
          condition = "input.output_mode == 'Download'",
          textInput("output_file_name", "Output File Name (optional, no extension)", "")
        ),
        fluidRow(
          div(style = "text-align: left;",
              div(style = "display: inline-block; margin-right: 10px;",
                  actionButton("back3", "Back")
              ),
              div(style = "display: inline-block;",
                  actionButton("next3", "Run Analysis")
              )
          )
        )
      )
    } else if (step == 4) {
      tagList(
        h3("Step 4: Analysis Results"),
        uiOutput("result_display"),
        actionButton("back4", "Back")
      )
    }
  })
  
  ## ---------------------------
  ## Navigation: Next/Back Buttons & Save User State
  ## ---------------------------
  observeEvent(input$next1, { currentStep(2) })
  observeEvent(input$back2, { currentStep(1) })
  
  # On moving from Step 2 to Step 3, save the selected columns
  observeEvent(input$next2, {
    if (!is.null(input$selected_columns) && length(input$selected_columns) > 0) {
      userState$selected_columns <- input$selected_columns
    }
    currentStep(3)
  })
  
  observeEvent(input$back3, { currentStep(2) })
  
  # On moving from Step 3 to Step 4, save the selected function and function options
  observeEvent(input$next3, {
    userState$selected_function <- input$selected_function
    if (input$selected_function == "Boxplots") {
      userState$bp_mf_row <- input$bp_mf_row
      userState$bp_y_lim <- input$bp_y_lim
      userState$bp_bin_size <- input$bp_bin_size
      userState$bp_log2 <- input$bp_log2
    }
    if (input$selected_function == "Enhanced Boxplots") {
      userState$bp2_mf_row <- input$bp2_mf_row
      userState$bp2_log2 <- input$bp2_log2
      userState$bp2_y_lim <- input$bp2_y_lim
    }
    if (input$selected_function == "Dual-Flashlight Plot") {
      userState$df_group_var <- input$df_group_var
      userState$df_ssmd_thresh <- input$df_ssmd_thresh
      userState$df_log2fc_thresh <- input$df_log2fc_thresh
      userState$df_top_labels <- input$df_top_labels
    }
    if (input$selected_function == "Heatmap") {
      userState$hm_log2 <- input$hm_log2
      userState$hm_annotation <- input$hm_annotation
    }
    if (input$selected_function == "Principle Component Analysis (PCA)") {
      userState$pca_group_col <- input$pca_group_col
      userState$pca_trt_col <- input$pca_trt_col
      userState$pca_comp_num <- input$pca_comp_num
      userState$pca_log2 <- input$pca_log2
      userState$pca_ellipse <- input$pca_ellipse
      userState$pca_style <- input$pca_style
      userState$pca_pch <- input$pca_pch
    }
    if (input$selected_function == "Random Forest") {
      userState$rf_group_col <- input$rf_group_col
      userState$rf_ntree <- input$rf_ntree
      userState$rf_mtry <- input$rf_mtry
      userState$rf_train_fraction <- input$rf_train_fraction
      userState$rf_plot_roc <- input$rf_plot_roc
      userState$rf_run_rfcv <- input$rf_run_rfcv
      userState$rf_k_folds <- input$rf_k_folds
      userState$rf_step <- input$rf_step
    }
    if (input$selected_function == "Skewness/Kurtosis") {
      userState$skku_group_cols <- input$skku_group_cols
      userState$skku_print_raw <- input$skku_print_raw
      userState$skku_print_log <- input$skku_print_log
    }
    if (input$selected_function == "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)") {
      userState$splsda_group_col <- input$splsda_group_col
      userState$splsda_trt_col <- input$splsda_trt_col
      userState$splsda_var_num <- input$splsda_var_num
      userState$splsda_cv_opt <- input$splsda_cv_opt
      userState$splsda_fold_num <- input$splsda_fold_num
      userState$splsda_log2 <- input$splsda_log2
      userState$splsda_comp_num <- input$splsda_comp_num
      userState$splsda_pch <- input$splsda_pch
      userState$splsda_style <- input$splsda_style
      userState$splsda_roc <- input$splsda_roc
      userState$splsda_ellipse <- input$splsda_ellipse
      userState$splsda_bg <- input$splsda_bg
      userState$splsda_conf_mat <- input$splsda_conf_mat
    }
    if (input$selected_function == "Two-Sample T-Test") {
      userState$ttest_log2 <- input$ttest_log2
      userState$ttest_format_output <- input$ttest_format_output
    }
    if (input$selected_function == "Volcano Plot") {
      userState$volc_group_col <- input$volc_group_col
      userState$volc_cond1 <- input$volc_cond1
      userState$volc_cond2 <- input$volc_cond2
      userState$volc_fold_change_thresh <- input$volc_fold_change_thresh
      userState$volc_p_value_thresh <- input$volc_p_value_thresh
      userState$volc_top_labels <- input$volc_top_labels
    }
    if (input$selected_function == "Extreme Gradient Boosting (XGBoost)") {
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
  
  observeEvent(input$back4, { currentStep(3) })
  
  ## ---------------------------
  ## Updating inputs by userState
  ## ---------------------------
  
  observeEvent(currentStep(),{
    if (currentStep() == 3 && !is.null(userState$selected_function)) {
      # Boxplots
      if (userState$selected_function == "Boxplots") {
        updateSelectInput(session, "bp_mf_row", selected = userState$bp_mf_row)
        updateTextInput(session, "bp_y_lim", value = userState$bp_y_lim)
        updateNumericInput(session, "bp_bin_size", value = userState$bp_bin_size)
        updateCheckboxInput(session, "bp_log2", value = userState$bp_log2)
      }
      
      # Enhanced Boxplots
      if (userState$selected_function == "Enhanced Boxplots") {
        updateTextInput(session, "bp2_mf_row", value = userState$bp2_mf_row)
        updateCheckboxInput(session, "bp2_log2", value = userState$bp2_log2)
        updateTextInput(session, "bp2_y_lim", value = userState$bp2_y_lim)
      }
      
      # Dual-Flashlight Plot
      if (userState$selected_function == "Dual-Flashlight Plot") {
        updateSelectInput(session, "df_group_var", selected = userState$df_group_var)
        updateNumericInput(session, "df_ssmd_thresh", value = userState$df_ssmd_thresh)
        updateNumericInput(session, "df_log2fc_thresh", value = userState$df_log2fc_thresh)
        updateNumericInput(session, "df_top_labels", value = userState$df_top_labels)
      }
      
      # Heatmap
      if (userState$selected_function == "Heatmap") {
        updateCheckboxInput(session, "hm_log2", value = userState$hm_log2)
        updateSelectInput(session, "hm_annotation", selected = userState$hm_annotation)
      }
      
      # Principle Component Analysis (PCA)
      if (userState$selected_function == "Principle Component Analysis (PCA)") {
        updateSelectInput(session, "pca_group_col", selected = userState$pca_group_col)
        updateSelectInput(session, "pca_trt_col", selected = userState$pca_trt_col)
        updateNumericInput(session, "pca_comp_num", value = userState$pca_comp_num)
        updateCheckboxInput(session, "pca_log2", value = userState$pca_log2)
        updateCheckboxInput(session, "pca_ellipse", value = userState$pca_ellipse)
        updateSelectInput(session, "pca_style", selected = userState$pca_style)
        updateTextInput(session, "pca_pch", value = userState$pca_pch)
      }
      
      # Random Forest
      if (userState$selected_function == "Random Forest") {
        updateSelectInput(session, "rf_group_col", selected = userState$rf_group_col)
        updateNumericInput(session, "rf_ntree", value = userState$rf_ntree)
        updateNumericInput(session, "rf_mtry", value = userState$rf_mtry)
        updateNumericInput(session, "rf_train_fraction", value = userState$rf_train_fraction)
        updateCheckboxInput(session, "rf_plot_roc", value = userState$rf_plot_roc)
        updateCheckboxInput(session, "rf_run_rfcv", value = userState$rf_run_rfcv)
        updateNumericInput(session, "rf_k_folds", value = userState$rf_k_folds)
        updateNumericInput(session, "rf_step", value = userState$rf_step)
      }
      
      # Skewness/Kurtosis
      if (userState$selected_function == "Skewness/Kurtosis") {
        updateSelectInput(session, "skku_group_cols", selected = userState$skku_group_cols)
        updateCheckboxInput(session, "skku_print_raw", value = userState$skku_print_raw)
        updateCheckboxInput(session, "skku_print_log", value = userState$skku_print_log)
      }
      
      # Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)
      if (userState$selected_function == "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)") {
        updateSelectInput(session, "splsda_group_col", selected = userState$splsda_group_col)
        updateSelectInput(session, "splsda_trt_col", selected = userState$splsda_trt_col)
        updateNumericInput(session, "splsda_var_num", value = userState$splsda_var_num)
        updateCheckboxInput(session, "splsda_cv_opt", value = userState$splsda_cv_opt)
        updateNumericInput(session, "splsda_fold_num", value = userState$splsda_fold_num)
        updateCheckboxInput(session, "splsda_log2", value = userState$splsda_log2)
        updateNumericInput(session, "splsda_comp_num", value = userState$splsda_comp_num)
        updateTextInput(session, "splsda_pch", value = userState$splsda_pch)
        updateSelectInput(session, "splsda_style", selected = userState$splsda_style)
        updateCheckboxInput(session, "splsda_roc", value = userState$splsda_roc)
        updateCheckboxInput(session, "splsda_ellipse", value = userState$splsda_ellipse)
        updateCheckboxInput(session, "splsda_bg", value = userState$splsda_bg)
        updateCheckboxInput(session, "splsda_conf_mat", value = userState$splsda_conf_mat)
      }
      
      # Two-Sample T-Test
      if (userState$selected_function == "Two-Sample T-Test") {
        updateCheckboxInput(session, "ttest_log2", value = userState$ttest_log2)
        updateCheckboxInput(session, "ttest_format_output", value = userState$ttest_format_output)
      }
      
      # Volcano Plot
      if (userState$selected_function == "Volcano Plot") {
        updateSelectInput(session, "volc_group_col", selected = userState$volc_group_col)
        updateSelectInput(session, "volc_cond1", selected = userState$volc_cond1)
        updateSelectInput(session, "volc_cond2", selected = userState$volc_cond2)
        updateNumericInput(session, "volc_fold_change_thresh", value = userState$volc_fold_change_thresh)
        updateNumericInput(session, "volc_p_value_thresh", value = userState$volc_p_value_thresh)
        updateNumericInput(session, "volc_top_labels", value = userState$volc_top_labels)
      }
      
      # Extreme Gradient Boosting (XGBoost)
      if (userState$selected_function == "Extreme Gradient Boosting (XGBoost)") {
        updateSelectInput(session, "xgb_group_col", selected = userState$xgb_group_col)
        updateNumericInput(session, "xgb_train_fraction", value = userState$xgb_train_fraction)
        updateNumericInput(session, "xgb_nrounds", value = userState$xgb_nrounds)
        updateNumericInput(session, "xgb_max_depth", value = userState$xgb_max_depth)
        updateNumericInput(session, "xgb_eta", value = userState$xgb_eta)
        updateSelectInput(session, "xgb_eval_metric", selected = userState$xgb_eval_metric)
        updateNumericInput(session, "xgb_top_n_features", value = userState$xgb_top_n_features)
        updateCheckboxInput(session, "xgb_plot_roc", value = userState$xgb_plot_roc)
        updateCheckboxInput(session, "xgb_cv", value = userState$xgb_cv)
        updateNumericInput(session, "xgb_nfold", value = userState$xgb_nfold)
      }
    }
  })
  
  
  ## ---------------------------
  ## Analysis and Results
  ## ---------------------------
  downloadPath <- reactiveVal(NULL)
  
  analysisResult <- eventReactive(input$next3, {
    req(filteredData())
    prog <- shiny::Progress$new()
    on.exit(prog$close())
    
    df <- filteredData()
    mode <- input$output_mode
    out_file <- NULL
    if (mode == "Download" && nzchar(input$output_file_name)) {
      out_file <- file.path(tempdir(), paste0(input$output_file_name, ".pdf"))
    }
    
    func_name <- input$selected_function  # Using simpler names now
    opts <- list()
    if (func_name == "ANOVA") {
      opts$format_output <- input$anova_format_output
    }
    if (func_name == "Boxplots") {
      opts$bin_size <- input$bp_bin_size
      if (nzchar(input$bp_mf_row)) {
        opts$mf_row <- as.numeric(unlist(strsplit(input$bp_mf_row, ",")))
      }
      if (nzchar(input$bp_y_lim)) {
        opts$y_lim <- as.numeric(unlist(strsplit(input$bp_y_lim, ",")))
      }
      opts$scale <- if (input$bp_log2) "log2" else NULL
    }
    if (func_name == "Enhanced Boxplots") {
      if (nzchar(input$bp2_mf_row)) {
        opts$mf_row <- as.numeric(unlist(strsplit(input$bp2_mf_row, ",")))
      }
      opts$scale <- if (input$bp2_log2) "log2" else NULL
      if (nzchar(input$bp2_y_lim)) {
        opts$y_lim <- as.numeric(unlist(strsplit(input$bp2_y_lim, ",")))
      }
    }
    if (func_name == "Dual-Flashlight Plot") {
      opts$ssmd_thresh <- input$df_ssmd_thresh
      opts$log2fc_thresh <- input$df_log2fc_thresh
      opts$top_labels <- input$df_top_labels
    }
    if (func_name == "Heatmap") {
      opts$scale <- if (input$hm_log2) "log2" else NULL
      opts$annotation_col_name <- input$hm_annotation
    }
    if (func_name == "Principle Component Analysis (PCA)") {
      opts$group_col <- input$pca_group_col
      opts$trt_col <- input$pca_trt_col
      opts$comp_num <- input$pca_comp_num
      opts$scale <- if (input$pca_log2) "log2" else NULL
      opts$ellipse <- input$pca_ellipse
      opts$style <- if (input$pca_style == "3D") "3d" else NULL
      if (nzchar(input$pca_pch)) {
        pch_vals <- as.numeric(unlist(strsplit(input$pca_pch, ",")))
        uniq_groups <- sort(unique(filteredData()[[input$pca_group_col]]))
        if (length(pch_vals) < length(uniq_groups)) {
          pch_vals <- rep(pch_vals, length.out = length(uniq_groups))
        }
        opts$pch_values <- pch_vals
      }
    }
    if (func_name == "Random Forest") {
      opts$group_col <- input$rf_group_col
      opts$ntree <- input$rf_ntree
      opts$mtry <- input$rf_mtry
      opts$train_fraction <- input$rf_train_fraction
      opts$plot_roc <- input$rf_plot_roc
      opts$run_rfcv <- input$rf_run_rfcv
      opts$k_folds <- input$rf_k_folds
      opts$step <- input$rf_step
    }
    if (func_name == "Skewness/Kurtosis") {
      opts$group_cols <- input$skku_group_cols
      opts$print_res_raw <- input$skku_print_raw
      opts$print_res_log <- input$skku_print_log
    }
    if (func_name == "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)") {
      opts$group_col <- input$splsda_group_col
      opts$trt_col <- input$splsda_trt_col
      opts$var_num <- input$splsda_var_num
      opts$cv_opt <- if (input$splsda_cv_opt == "None") NULL else input$splsda_cv_opt
      opts$fold_num <- input$splsda_fold_num
      opts$scale <- if (input$splsda_log2) "log2" else NULL
      opts$comp_num <- input$splsda_comp_num
      if (nzchar(input$splsda_pch)) {
        opts$pch_values <- as.numeric(unlist(strsplit(input$splsda_pch, ",")))
      }
      opts$style <- if (input$splsda_style == "3D") "3d" else NULL
      opts$roc <- input$splsda_roc
      opts$ellipse <- input$splsda_ellipse
      opts$bg <- input$splsda_bg
      opts$conf_mat <- input$splsda_conf_mat
    }
    if (func_name == "Two-Sample T-Test") {
      opts$scale <- if (input$ttest_log2) "log2" else NULL
      opts$format_output <- input$ttest_format_output
    }
    if (func_name == "Volcano Plot") {
      opts$group_col <- input$volc_group_col
      opts$cond1 <- input$volc_cond1
      opts$cond2 <- input$volc_cond2
      opts$fold_change_thresh <- input$volc_fold_change_thresh
      opts$p_value_thresh <- input$volc_p_value_thresh
      opts$top_labels <- input$volc_top_labels
    }
    if (func_name == "Extreme Gradient Boosting (XGBoost)") {
      opts$group_col <- input$xgb_group_col
      opts$train_fraction <- input$xgb_train_fraction
      opts$nrounds <- input$xgb_nrounds
      opts$max_depth <- input$xgb_max_depth
      opts$eta <- input$xgb_eta
      opts$nfold <- input$xgb_nfold
      opts$cv <- input$xgb_cv
      opts$eval_metric <- input$xgb_eval_metric
      opts$top_n_features <- input$xgb_top_n_features
      opts$plot_roc <- input$xgb_plot_roc
    }
    
    res <- switch(func_name,
                  "ANOVA" = do.call(cyt_anova, c(list(data = df, progress = prog), opts)),
                  "Boxplots" = do.call(cyt_bp, c(list(data = df, output_file = if(mode == "Download") out_file else NULL, progress = prog), opts)),
                  "Enhanced Boxplots" = do.call(cyt_bp2, c(list(data = df, output_file = if(mode == "Download") out_file else NULL, progress = prog), opts)),
                  "Dual-Flashlight Plot" = do.call(cyt_dualflashplot, c(list(data = df, group_var = input$df_group_var,
                                                                             group1 = input$df_cond1, group2 = input$df_cond2,
                                                                             output_file = if(mode == "Download") out_file else NULL,
                                                                             progress = prog), opts)),
                  "Heatmap" = do.call(cyt_heatmap, c(list(data = df, output_file = if(mode == "Download") out_file else NULL,
                                                          progress = prog), opts)),
                  "Principle Component Analysis (PCA)" = do.call(cyt_pca, c(list(data = df, output_file = if(mode == "Download") out_file else NULL, progress = prog), opts)),
                  "Random Forest" = do.call(cyt_rf, c(list(data = df, output_file = if(mode == "Download") out_file else NULL, progress = prog), opts)),
                  "Skewness/Kurtosis" = do.call(cyt_skku, c(list(data = df, output_file = if(mode == "Download") out_file else NULL, progress = prog), opts)),
                  "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = do.call(cyt_splsda, c(list(data = df, pdf_title = if(mode == "Download") out_file else NULL, progress = prog), opts)),
                  "Two-Sample T-Test" = do.call(cyt_ttest, c(list(data = df, progress = prog), opts)),
                  "Volcano Plot" = do.call(cyt_volc, c(list(data = df, output_file = if(mode == "Download") out_file else NULL, progress = prog), opts)),
                  "Extreme Gradient Boosting (XGBoost)" = do.call(cyt_xgb, c(list(data = df, output_file = if(mode == "Download") out_file else NULL, progress = prog), opts))
    )
    
    if (mode == "Download" && nzchar(input$output_file_name)) {
      out_file <- file.path(tempdir(), paste0(input$output_file_name, ".pdf"))
      downloadPath(normalizePath(out_file))
      return(paste("Output file generated:", out_file))
    }
    
    return(res)
  })
  
  output$result_display <- renderUI({
    req(analysisResult())
    res <- analysisResult()
    mode <- input$output_mode
    func_name <- input$selected_function  # simpler names now
    
    if (mode == "Download") {
      h3(if (is.character(res)) res else as.character(res))
    } else {
      if (func_name == "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" && is.list(res)) {
        if ("overall_indiv_plot" %in% names(res)) {
          # Overall (single-level) analysis exists
          tagList(
            h3("Results (sPLS-DA) - Overall Analysis"),
            if (!is.null(res$overall_indiv_plot)) plotOutput("splsda_overallIndivPlot", height = "400px") else NULL,
            if (!is.null(res$overall_3D)) plotOutput("splsda_overall3DPlot", height = "400px") else NULL,
            if (!is.null(res$overall_ROC)) plotOutput("splsda_overallRocPlot", height = "400px") else NULL,
            if (!is.null(res$overall_CV)) plotOutput("splsda_overallCvPlot", height = "400px") else NULL,
            if (!is.null(res$loadings)) uiOutput("splsda_loadingsUI") else NULL,
            if (!is.null(res$vip_scores)) uiOutput("splsda_vipScoresUI") else NULL,
            if (!is.null(res$vip_indiv_plot)) plotOutput("splsda_vipIndivPlot", height = "400px") else NULL,
            if (!is.null(res$vip_3D)) plotOutput("splsda_vip3DPlot", height = "400px") else NULL,
            if (!is.null(res$vip_ROC)) plotOutput("splsda_vipRocPlot", height = "400px") else NULL,
            if (!is.null(res$vip_CV)) plotOutput("splsda_vipCvPlot", height = "400px") else NULL,
            if (!is.null(res$conf_matrix)) verbatimTextOutput("splsda_confMatrix") else NULL
          )
        } else {
          # Multi-level analysis: create a tab for each treatment
          do.call(tabsetPanel, lapply(names(res), function(trt) {
            tabPanel(
              title = trt,
              tagList(
                h3(paste("Results (sPLS-DA) for", trt)),
                if (!is.null(res[[trt]]$overall_indiv_plot))
                  plotOutput(paste0("splsda_overallIndivPlot_", trt), height = "400px") else NULL,
                if (!is.null(res[[trt]]$overall_3D))
                  plotOutput(paste0("splsda_overall3DPlot_", trt), height = "400px") else NULL,
                if (!is.null(res[[trt]]$overall_ROC))
                  plotOutput(paste0("splsda_overallRocPlot_", trt), height = "400px") else NULL,
                if (!is.null(res[[trt]]$overall_CV))
                  plotOutput(paste0("splsda_overallCvPlot_", trt), height = "400px") else NULL,
                if (!is.null(res[[trt]]$loadings))
                  uiOutput(paste0("splsda_loadingsUI_", trt)) else NULL,
                if (!is.null(res[[trt]]$vip_scores))
                  uiOutput(paste0("splsda_vipScoresUI_", trt)) else NULL,
                if (!is.null(res[[trt]]$vip_indiv_plot))
                  plotOutput(paste0("splsda_vipIndivPlot_", trt), height = "400px") else NULL,
                if (!is.null(res[[trt]]$vip_3D))
                  plotOutput(paste0("splsda_vip3DPlot_", trt), height = "400px") else NULL,
                if (!is.null(res[[trt]]$vip_ROC))
                  plotOutput(paste0("splsda_vipRocPlot_", trt), height = "400px") else NULL,
                if (!is.null(res[[trt]]$vip_CV))
                  plotOutput(paste0("splsda_vipCvPlot_", trt), height = "400px") else NULL,
                if (!is.null(res[[trt]]$conf_matrix))
                  verbatimTextOutput(paste0("splsda_confMatrix_", trt)) else NULL
              )
            )
          }))
        }
      } else if (func_name == "Heatmap" &&
                 is.character(res) &&
                 grepl("\\.png$", res, ignore.case = TRUE)) {
        tagList(
          h3("Results (Heatmap)"),
          imageOutput("heatmapImage", height = "600px"),
          verbatimTextOutput("textResults")
        )
      } else if (func_name == "Principle Component Analysis (PCA)" && is.list(res)) {
        if ("overall_indiv_plot" %in% names(res)) {
          tagList(
            h3("Results (PCA - Single-Level)"),
            plotOutput("pca_indivPlot", height = "400px"),
            if (!is.null(res$overall_3D)) plotOutput("pca_3DPlot", height = "400px"),
            plotOutput("pca_screePlot", height = "400px"),
            uiOutput("pca_loadingsUI"),
            plotOutput("pca_biplot", height = "400px"),
            plotOutput("pca_corrCircle", height = "400px")
          )
        } else {
          do.call(tabsetPanel, lapply(names(res), function(lvl) {
            tabPanel(
              title = lvl,
              h3(paste("Results (PCA) for", lvl)),
              plotOutput(paste0("pca_indivPlot_", lvl), height = "400px"),
              if (!is.null(res[[lvl]]$overall_3D)) plotOutput(paste0("pca_3DPlot_", lvl), height = "400px") else NULL,
              plotOutput(paste0("pca_screePlot_", lvl), height = "400px"),
              uiOutput(paste0("pca_loadingsUI_", lvl)),
              plotOutput(paste0("pca_biplot_", lvl), height = "400px"),
              plotOutput(paste0("pca_corrCircle_", lvl), height = "400px")
            )
          }))
        }
      } else if (func_name == "Random Forest" && is.list(res)) {
        tagList(
          h3("Random Forest Results"),
          verbatimTextOutput("rf_summary"),
          plotOutput("rf_vipPlot", height = "400px"),
          if (!is.null(res$roc_plot)) {
            plotOutput("rf_rocPlot", height = "400px")
          },
          if (!is.null(res$rfcv_plot)) {
            plotOutput("rf_rfcvPlot", height = "400px")
          }
        )
      } else if (func_name == "Extreme Gradient Boosting (XGBoost)" && is.list(res)) {
        tagList(
          h3("XGBoost Results"),
          verbatimTextOutput("xgb_summary"),
          plotOutput("xgb_vipPlot", height = "400px"),
          conditionalPanel(
            condition = "output.xgb_hasROC == true",
            plotOutput("xgb_rocPlot", height = "400px")
          )
        )
      } else if (func_name == "Skewness/Kurtosis") {
        tagList(
          h3("Skewness/Kurtosis Results"),
          plotOutput("skku_skewPlot", height = "400px"),
          plotOutput("skku_kurtPlot", height = "400px"),
          conditionalPanel(
           # Conditional panel to see if print raw results or print log results
            condition = "input.skku_print_raw == true",
            verbatimTextOutput("skku_raw_results")
          ),
          conditionalPanel(
            condition = "input.skku_print_log == true",
            verbatimTextOutput("skku_log_results")
          )
        )
      }else if (is.list(res) && length(res) > 0 && all(sapply(res, function(x) inherits(x, "ggplot")))) {
        tagList(
          h3("Results:"),
          lapply(seq_along(res), function(i) {
            plotOutput(paste0("dynamicPlot_", i), height = "400px")
          }),
          verbatimTextOutput("textResults")
        )
      } else if (inherits(res, "ggplot")) {
        tagList(
          h3("Results (Dual-Flash Plot)"),
          plotOutput("dualflashPlotOutput", height = "400px"),
          verbatimTextOutput("textResults")
        )
      } else {
        tagList(
          h3("Results:"),
          verbatimTextOutput("textResults")
        )
      }
    }
  })
  
  observe({
    res <- analysisResult()
    if (is.list(res) && length(res) > 0 && all(sapply(res, function(x) inherits(x, "ggplot")))) {
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
  
  observeEvent(analysisResult(), {
    req(input$selected_function)
    if (input$output_mode != "Interactive") return(NULL)
    res <- analysisResult()
    func_name <- input$selected_function
    if (input$selected_function == "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" && is.list(res)) {
      if ("overall_indiv_plot" %in% names(res)) {
        # Single-level (overall) mode
        output$splsda_overallIndivPlot <- renderPlot({ replayPlot(res$overall_indiv_plot) })
        output$splsda_overall3DPlot   <- renderPlot({ if (!is.null(res$overall_3D)) replayPlot(res$overall_3D) })
        output$splsda_overallRocPlot  <- renderPlot({ if (!is.null(res$overall_ROC)) replayPlot(res$overall_ROC) })
        output$splsda_overallCvPlot   <- renderPlot({ if (!is.null(res$overall_CV)) print(res$overall_CV) })
        
        output$splsda_loadingsUI <- renderUI({
          if (!is.null(res$loadings)) {
            tagList(lapply(seq_along(res$loadings), function(i) {
              plotOutput(paste0("splsda_loadings_overall_", i), height = "300px")
            }))
          }
        })
        for (i in seq_along(res$loadings)) {
          local({
            ii <- i
            output[[paste0("splsda_loadings_overall_", ii)]] <- renderPlot({
              replayPlot(res$loadings[[ii]])
            })
          })
        }
        output$splsda_vipScoresUI <- renderUI({
          if (!is.null(res$vip_scores)) {
            tagList(lapply(seq_along(res$vip_scores), function(i) {
              plotOutput(paste0("splsda_vipScore_overall_", i), height = "300px")
            }))
          }
        })
        for (i in seq_along(res$vip_scores)) {
          local({
            ii <- i
            output[[paste0("splsda_vipScore_overall_", ii)]] <- renderPlot({
              print(res$vip_scores[[ii]])
            })
          })
        }
        output$splsda_vipIndivPlot <- renderPlot({ if (!is.null(res$vip_indiv_plot)) replayPlot(res$vip_indiv_plot) })
        output$splsda_vip3DPlot   <- renderPlot({ if (!is.null(res$vip_3D)) replayPlot(res$vip_3D) })
        output$splsda_vipRocPlot  <- renderPlot({ if (!is.null(res$vip_ROC)) replayPlot(res$vip_ROC) })
        output$splsda_vipCvPlot   <- renderPlot({ if (!is.null(res$vip_CV)) print(res$vip_CV) })
        output$splsda_confMatrix  <- renderPrint({ if (!is.null(res$conf_matrix)) cat(paste(res$conf_matrix, collapse = "\n")) })
        
      } else {
        # Multi-level mode: create a tab for each treatment group
        do.call(tabsetPanel, lapply(names(res), function(grp) {
          local({
            currentGroup <- grp
            currentSubres <- res[[currentGroup]]
            tabPanel(
              title = currentGroup,
              tagList(
                h3(paste("Results (sPLS-DA) for", currentGroup)),
                if (!is.null(currentSubres$overall_indiv_plot))
                  plotOutput(paste0("splsda_overallIndivPlot_", currentGroup), height = "400px"),
                if (!is.null(currentSubres$overall_3D))
                  plotOutput(paste0("splsda_overall3DPlot_", currentGroup), height = "400px"),
                if (!is.null(currentSubres$overall_ROC))
                  plotOutput(paste0("splsda_overallRocPlot_", currentGroup), height = "400px"),
                if (!is.null(currentSubres$overall_CV))
                  plotOutput(paste0("splsda_overallCvPlot_", currentGroup), height = "400px"),
                # Loadings UI container
                if (!is.null(currentSubres$loadings))
                  uiOutput(paste0("splsda_loadingsUI_", currentGroup)),
                # VIP scores UI container
                if (!is.null(currentSubres$vip_scores))
                  uiOutput(paste0("splsda_vipScoresUI_", currentGroup)),
                if (!is.null(currentSubres$vip_indiv_plot))
                  plotOutput(paste0("splsda_vipIndivPlot_", currentGroup), height = "400px"),
                if (!is.null(currentSubres$vip_3D))
                  plotOutput(paste0("splsda_vip3DPlot_", currentGroup), height = "400px"),
                if (!is.null(currentSubres$vip_ROC))
                  plotOutput(paste0("splsda_vipRocPlot_", currentGroup), height = "400px"),
                if (!is.null(currentSubres$vip_CV))
                  plotOutput(paste0("splsda_vipCvPlot_", currentGroup), height = "400px"),
                if (!is.null(currentSubres$conf_matrix))
                  verbatimTextOutput(paste0("splsda_confMatrix_", currentGroup))
              )
            )
          })
        }))
        # Then, separately create the observers for each treatment group:
        for (grp in names(res)) {
          local({
            currentGroup <- grp
            currentSubres <- res[[currentGroup]]
            output[[paste0("splsda_overallIndivPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(currentSubres$overall_indiv_plot))
                replayPlot(currentSubres$overall_indiv_plot)
            })
            output[[paste0("splsda_overall3DPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(currentSubres$overall_3D))
                replayPlot(currentSubres$overall_3D)
            })
            output[[paste0("splsda_overallRocPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(currentSubres$overall_ROC))
                replayPlot(currentSubres$overall_ROC)
            })
            output[[paste0("splsda_overallCvPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(currentSubres$overall_CV))
                print(currentSubres$overall_CV)
            })
            # Loadings: create a UI container and observers for each loadings plot
            if (!is.null(currentSubres$loadings)) {
              output[[paste0("splsda_loadingsUI_", currentGroup)]] <- renderUI({
                tagList(lapply(seq_along(currentSubres$loadings), function(i) {
                  plotOutput(paste0("splsda_loadings_", currentGroup, "_", i), height = "300px")
                }))
              })
              for (i in seq_along(currentSubres$loadings)) {
                local({
                  ii <- i
                  output[[paste0("splsda_loadings_", currentGroup, "_", ii)]] <- renderPlot({
                    replayPlot(currentSubres$loadings[[ii]])
                  })
                })
              }
            }
            # VIP scores:
            if (!is.null(currentSubres$vip_scores)) {
              output[[paste0("splsda_vipScoresUI_", currentGroup)]] <- renderUI({
                tagList(lapply(seq_along(currentSubres$vip_scores), function(i) {
                  plotOutput(paste0("splsda_vipScore_", currentGroup, "_", i), height = "300px")
                }))
              })
              for (i in seq_along(currentSubres$vip_scores)) {
                local({
                  ii <- i
                  output[[paste0("splsda_vipScore_", currentGroup, "_", ii)]] <- renderPlot({
                    print(currentSubres$vip_scores[[ii]])
                  })
                })
              }
            }
            output[[paste0("splsda_vipIndivPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(currentSubres$vip_indiv_plot))
                replayPlot(currentSubres$vip_indiv_plot)
            })
            output[[paste0("splsda_vip3DPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(currentSubres$vip_3D))
                replayPlot(currentSubres$vip_3D)
            })
            output[[paste0("splsda_vipRocPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(currentSubres$vip_ROC))
                replayPlot(currentSubres$vip_ROC)
            })
            output[[paste0("splsda_vipCvPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(currentSubres$vip_CV))
                print(currentSubres$vip_CV)
            })
            output[[paste0("splsda_confMatrix_", currentGroup)]] <- renderPrint({
              if (!is.null(currentSubres$conf_matrix))
                cat(paste(currentSubres$conf_matrix, collapse = "\n"))
            })
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
      outputOptions(output, "xgb_rocPlot", suspendWhenHidden = FALSE)
      output$xgb_hasROC <- reactive({
        !is.null(res$roc_plot)
      })
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
      # Single-level PCA branch
      if ("overall_indiv_plot" %in% names(res)) {
        output$pca_indivPlot <- renderPlot({
          replayPlot(res$overall_indiv_plot)
        })
        output$pca_3DPlot <- renderPlot({
          if (!is.null(res$overall_3D)) replayPlot(res$overall_3D)
        })
        output$pca_screePlot <- renderPlot({
          if (!is.null(res$overall_scree_plot)) replayPlot(res$overall_scree_plot)
        })
        output$pca_biplot <- renderPlot({
          if (!is.null(res$biplot)) replayPlot(res$biplot)
        })
        output$pca_corrCircle <- renderPlot({
          if (!is.null(res$correlation_circle)) replayPlot(res$correlation_circle)
        })
        
        output$pca_loadingsUI <- renderUI({
          if (!is.null(res$loadings)) {
            tagList(lapply(seq_along(res$loadings), function(i) {
              plotOutput(paste0("pca_loadings_", i), height = "300px")
            }))
          }
        })
        # Capture each loading plot in its own local environment:
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
        
        # Multi-level PCA branch
      } else {
        for (lvl in names(res)) {
          # Capture the current group (treatment level) in a local environment:
          local({
            currentGroup <- lvl
            subres <- res[[currentGroup]]
            
            output[[paste0("pca_indivPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(subres$overall_indiv_plot)) replayPlot(subres$overall_indiv_plot)
            })
            output[[paste0("pca_3DPlot_", currentGroup)]] <- renderPlot({
              if (!is.null(subres$overall_3D)) replayPlot(subres$overall_3D)
            })
            output[[paste0("pca_screePlot_", currentGroup)]] <- renderPlot({
              if (!is.null(subres$overall_scree_plot)) replayPlot(subres$overall_scree_plot)
            })
            output[[paste0("pca_biplot_", currentGroup)]] <- renderPlot({
              if (!is.null(subres$biplot)) replayPlot(subres$biplot)
            })
            output[[paste0("pca_corrCircle_", currentGroup)]] <- renderPlot({
              if (!is.null(subres$correlation_circle)) replayPlot(subres$correlation_circle)
            })
            
            # For loadings, create a UI container with unique IDs:
            output[[paste0("pca_loadingsUI_", currentGroup)]] <- renderUI({
              if (!is.null(subres$loadings)) {
                tagList(lapply(seq_along(subres$loadings), function(i) {
                  # Sanitize the group name if necessary:
                  safeGroup <- gsub("[^A-Za-z0-9_]+", "_", currentGroup)
                  plotOutput(paste0("pca_loadings_", safeGroup, "_", i), height = "300px")
                }))
              }
            })
            # Now assign each loadings plot a unique output:
            if (!is.null(subres$loadings)) {
              for (i in seq_along(subres$loadings)) {
                local({
                  local_i <- i
                  safeGroup <- gsub("[^A-Za-z0-9_]+", "_", currentGroup)
                  output[[paste0("pca_loadings_", safeGroup, "_", local_i)]] <- renderPlot({
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
  
  output$dualflashPlotOutput <- renderPlot({
    req(analysisResult())
    if (input$output_mode != "Interactive") return(NULL)
    res <- analysisResult()
    if (inherits(res, "ggplot")) print(res)
  })
  
  output$heatmapImage <- renderImage({
    req(analysisResult())
    filepath <- analysisResult()
    list(src = filepath,
         contentType = "image/png",
         width = "600",
         height = "600",
         alt = "Heatmap")
  }, deleteFile = FALSE)
  
  output$textResults <- renderPrint({
    res <- analysisResult()
    print(res)
  })
  
  output$skku_skewPlot <- renderPlot({
    req(analysisResult())
    if (input$output_mode != "Interactive") return(NULL)
    res <- analysisResult()
    if (is.list(res) && !is.null(res$p_skew)) {
      print(res$p_skew)
    }
  })
  output$skku_kurtPlot <- renderPlot({
    req(analysisResult())
    if (input$output_mode != "Interactive") return(NULL)
    res <- analysisResult()
    if (is.list(res) && !is.null(res$p_kurt)) {
      print(res$p_kurt)
    }
  })
  output$skku_raw_results <- renderPrint({
    req(analysisResult())
    if (input$skku_print_raw) {
      raw_results <- analysisResult()$raw_results
      print(raw_results)
    }
  })
  
  output$skku_log_results <- renderPrint({
    req(analysisResult())
    if (input$skku_print_log) {
      log_results <- analysisResult()$log_results
      print(log_results)
    }
  })
  
  output$download_output <- downloadHandler(
    filename = function() {
      if (nzchar(input$output_file_name))
        paste0(input$output_file_name, ".pdf")
      else
        "output.pdf"
    },
    content = function(file) {
      req(input$output_mode == "Download")
      req(downloadPath())
      file.copy(downloadPath(), file, overwrite = TRUE)
    }
  )
  
  ## ---------------------------
  ## Save key inputs using reactiveValues (alternative approach)
  ## ---------------------------
  observeEvent(input$selected_columns, {
    if (!is.null(input$selected_columns) && length(input$selected_columns) > 0) {
      userState$selected_columns <- input$selected_columns
    }
  }, ignoreNULL = TRUE)
  observeEvent(input$selected_function, {
    userState$selected_function <- input$selected_function
  })
  # For Boxplots
  observeEvent(input$bp_bin_size, { userState$bp_bin_size <- input$bp_bin_size })
  observeEvent(input$bp_mf_row, { userState$bp_mf_row <- input$bp_mf_row })
  observeEvent(input$bp_y_lim, { userState$bp_y_lim <- input$bp_y_lim })
  observeEvent(input$bp_log2, { userState$bp_log2 <- input$bp_log2 })
  # For Enhanced Boxplots
  observeEvent(input$bp2_mf_row, { userState$bp2_mf_row <- input$bp2_mf_row })
  observeEvent(input$bp2_log2, { userState$bp2_log2 <- input$bp2_log2 })
  observeEvent(input$bp2_y_lim, { userState$bp2_y_lim <- input$bp2_y_lim })
  # For Dual-Flashlight Plot
  observeEvent(input$df_group_var, { userState$df_group_var <- input$df_group_var })
  observeEvent(input$df_ssmd_thresh, { userState$df_ssmd_thresh <- input$df_ssmd_thresh })
  observeEvent(input$df_log2fc_thresh, { userState$df_log2fc_thresh <- input$df_log2fc_thresh })
  observeEvent(input$df_top_labels, { userState$df_top_labels <- input$df_top_labels })
  # For Heatmap
  observeEvent(input$hm_log2, { userState$hm_log2 <- input$hm_log2 })
  observeEvent(input$hm_annotation, { userState$hm_annotation <- input$hm_annotation })
  # For PCA
  observeEvent(input$pca_group_col, { userState$pca_group_col <- input$pca_group_col })
  observeEvent(input$pca_trt_col, { userState$pca_trt_col <- input$pca_trt_col })
  observeEvent(input$pca_comp_num, { userState$pca_comp_num <- input$pca_comp_num })
  observeEvent(input$pca_log2, { userState$pca_log2 <- input$pca_log2 })
  observeEvent(input$pca_ellipse, { userState$pca_ellipse <- input$pca_ellipse })
  observeEvent(input$pca_style, { userState$pca_style <- input$pca_style })
  observeEvent(input$pca_pch, { userState$pca_pch <- input$pca_pch })
  # For Random Forest
  observeEvent(input$rf_group_col, { userState$rf_group_col <- input$rf_group_col })
  observeEvent(input$rf_ntree, { userState$rf_ntree <- input$rf_ntree })
  observeEvent(input$rf_mtry, { userState$rf_mtry <- input$rf_mtry })
  observeEvent(input$rf_train_fraction, { userState$rf_train_fraction <- input$rf_train_fraction })
  observeEvent(input$rf_plot_roc, { userState$rf_plot_roc <- input$rf_plot_roc })
  observeEvent(input$rf_run_rfcv, { userState$rf_run_rfcv <- input$rf_run_rfcv })
  observeEvent(input$rf_k_folds, { userState$rf_k_folds <- input$rf_k_folds })
  observeEvent(input$rf_step, { userState$rf_step <- input$rf_step })
  # For Skewness/Kurtosis
  observeEvent(input$skku_group_cols, { userState$skku_group_cols <- input$skku_group_cols })
  observeEvent(input$skku_print_raw, { userState$skku_print_raw <- input$skku_print_raw })
  observeEvent(input$skku_print_log, { userState$skku_print_log <- input$skku_print_log })
  # For sPLS-DA
  observeEvent(input$splsda_group_col, { userState$splsda_group_col <- input$splsda_group_col })
  observeEvent(input$splsda_trt_col, { userState$splsda_trt_col <- input$splsda_trt_col })
  observeEvent(input$splsda_var_num, { userState$splsda_var_num <- input$splsda_var_num })
  observeEvent(input$splsda_cv_opt, { userState$splsda_cv_opt <- input$splsda_cv_opt })
  observeEvent(input$splsda_fold_num, { userState$splsda_fold_num <- input$splsda_fold_num })
  observeEvent(input$splsda_log2, { userState$splsda_log2 <- input$splsda_log2 })
  observeEvent(input$splsda_comp_num, { userState$splsda_comp_num <- input$splsda_comp_num })
  observeEvent(input$splsda_pch, { userState$splsda_pch <- input$splsda_pch })
  observeEvent(input$splsda_style, { userState$splsda_style <- input$splsda_style })
  observeEvent(input$splsda_roc, { userState$splsda_roc <- input$splsda_roc })
  observeEvent(input$splsda_ellipse, { userState$splsda_ellipse <- input$splsda_ellipse })
  observeEvent(input$splsda_bg, { userState$splsda_bg <- input$splsda_bg })
  observeEvent(input$splsda_conf_mat, { userState$splsda_conf_mat <- input$splsda_conf_mat })
  # For Two-Sample T-Test
  observeEvent(input$ttest_log2, { userState$ttest_log2 <- input$ttest_log2 })
  observeEvent(input$ttest_format_output, { userState$ttest_format_output <- input$ttest_format_output })
  # For Volcano Plot
  observeEvent(input$volc_group_col, { userState$volc_group_col <- input$volc_group_col })
  observeEvent(input$volc_cond1, { userState$volc_cond1 <- input$volc_cond1 })
  observeEvent(input$volc_cond2, { userState$volc_cond2 <- input$volc_cond2 })
  observeEvent(input$volc_fold_change_thresh, { userState$volc_fold_change_thresh <- input$volc_fold_change_thresh })
  observeEvent(input$volc_p_value_thresh, { userState$volc_p_value_thresh <- input$volc_p_value_thresh })
  observeEvent(input$volc_top_labels, { userState$volc_top_labels <- input$volc_top_labels })
  # For XGBoost
  observeEvent(input$xgb_group_col, { userState$xgb_group_col <- input$xgb_group_col })
  observeEvent(input$xgb_train_fraction, { userState$xgb_train_fraction <- input$xgb_train_fraction })
  observeEvent(input$xgb_nrounds, { userState$xgb_nrounds <- input$xgb_nrounds })
  observeEvent(input$xgb_max_depth, { userState$xgb_max_depth <- input$xgb_max_depth })
  observeEvent(input$xgb_eta, { userState$xgb_eta <- input$xgb_eta })
  observeEvent(input$xgb_nfold, { userState$xgb_nfold <- input$xgb_nfold })
  observeEvent(input$xgb_cv, { userState$xgb_cv <- input$xgb_cv })
  observeEvent(input$xgb_eval_metric, { userState$xgb_eval_metric <- input$xgb_eval_metric })
  observeEvent(input$xgb_top_n_features, { userState$xgb_top_n_features <- input$xgb_top_n_features })
  observeEvent(input$xgb_plot_roc, { userState$xgb_plot_roc <- input$xgb_plot_roc })
}
