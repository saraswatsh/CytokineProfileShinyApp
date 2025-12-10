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
      cols <- safe_names(df)

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

      cols <- safe_names(df)
      ann_choices <- c("None" = "", cols)

      ui_list <- tagList(
        # Row 1: Scale + Annotation col
        fluidRow(
          column(
            width = 6,
            selectInput(
              "hm_scale",
              label = helper(
                type = "inline",
                title = "Scaling / Transform",
                icon = "fas fa-question-circle",
                shiny_tag = HTML(
                  "<span style='margin-right: 15px;'>Scale</span>"
                ),
                content = paste(
                  "Choose none (raw), log2 transform, or Z-scores across rows/columns.",
                  "Z-scores standardize to mean 0, SD 1."
                ),
                colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                  "red"
                } else {
                  "blue"
                }
              ),
              choices = c(
                "None" = "none",
                "log2" = "log2",
                "Row Z-score" = "row_zscore",
                "Column Z-score" = "col_zscore"
              ),
              selected = isolate(userState$hm_scale) %||% "none"
            )
          ),
          column(
            width = 6,
            selectizeInput(
              "hm_annotation",
              label = helper(
                type = "inline",
                title = "Annotation Column",
                icon = "fas fa-question-circle",
                shiny_tag = HTML(
                  "<span style='margin-right:15px;'>Annotation Column</span>"
                ),
                content = "Categorical column to use for annotating rows/columns. Choose 'None' to skip.",
                colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                  "red"
                } else {
                  "blue"
                }
              ),
              choices = ann_choices,
              multiple = FALSE,
              selected = isolate(userState$hm_annotation) %||% ""
            )
          )
        ),
        # Row 2: Annotation side + Title/Filename
        fluidRow(
          column(
            width = 6,
            selectInput(
              "hm_ann_side",
              label = helper(
                type = "inline",
                title = "Annotation Side",
                icon = "fas fa-question-circle",
                shiny_tag = HTML(
                  "<span style='margin-right: 15px;'>Annotation Side</span>"
                ),
                content = "Auto chooses row/column based on length match; override if desired.",
                colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                  "red"
                } else {
                  "blue"
                }
              ),
              choices = c("Auto" = "auto", "Row" = "row", "Column" = "col"),
              selected = isolate(userState$hm_ann_side) %||% "auto"
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

      cols <- safe_names(df)
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
                title = "Response Variable",
                icon = "fas fa-question-circle",
                shiny_tag = HTML(
                  "<span style='margin-right: 15px;'>Response Variable</span>"
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
                title = "Stratification (optional)",
                icon = "fas fa-question-circle",
                shiny_tag = HTML(
                  "<span style='margin-right: 15px;'>Stratification Variable</span>"
                ),
                content = "If set, you can render per-group heatmaps for each method.",
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
                content = "If a grouping column is selected, show one heatmap per categories.",
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
      cols <- safe_names(df)
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
      cols <- safe_names(df)

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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
      cols <- safe_names(df)

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
      cols <- safe_names(df)
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
        ),
        # Row 9: Font scale multiplier
        fluidRow(
          column(
            6,
            sliderInput(
              "splsda_fontsize",
              label = helper(
                type = "inline",
                title = "Font Scale Multiplier",
                icon = "fas fa-exclamation-circle",
                shiny_tag = HTML(
                  "<span style='margin-right: 15px;'>Font Scale Multiplier</span>"
                ),
                content = "Increase Font Size",
                colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                  "red"
                } else {
                  "blue"
                }
              ),
              min = 0.5,
              max = 2.5,
              step = 0.1,
              value = isolate(userState$splsda_fontsize) %||% 1.0
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
      cols <- safe_names(df)

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
      cols <- safe_names(df)

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
      cols <- safe_names(df)

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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
