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
                "Choose how the app compares two groups for each numeric outcome. Use Auto if you are unsure. The app will pick between the t-test and Wilcoxon test based on the data, while the other options force one method for every outcome."
              ),
              choices = c(
                "Auto" = "auto",
                "T-test" = "ttest",
                "Wilcoxon" = "wilcox"
              ),
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
              selected = shiny::isolate(userState$uv2_p_adjust_method) %||% ""
            )
          )
        )
      )
    },
    # ------------------------
    # Univariate Tests (ANOVA, Kruskal-Wallis)
    # ------------------------
    "Univariate Tests (ANOVA, Kruskal-Wallis)" = {
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
                  ""
              )
            }
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
      eb_fill_palette_selected <- shiny::isolate(userState$eb_fill_palette) %||%
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
              selected = shiny::isolate(userState$eb_group_col) %||% cat_vars[1]
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              "eb_method",
              label = helper_label(
                "Test Method",
                "Error-Bar Statistical Test",
                "Choose which statistical test is used when the plot adds pairwise significance labels. Use Auto if you are unsure. The app will choose between the t-test and Wilcoxon test, while the other options force one method across all outcomes."
              ),
              choices = c(
                "Auto" = "auto",
                "T-test" = "ttest",
                "Wilcoxon" = "wilcox"
              ),
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
              selected = shiny::isolate(userState$eb_p_adjust_method) %||% ""
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::numericInput(
              "eb_label_size",
              label = helper_label(
                "Label Size",
                "Annotation Label Size",
                "Controls the size of the p-value or effect-size labels drawn on the plot. Increase this if the annotations are hard to read. Decrease it if the text overlaps or crowds the figure."
              ),
              value = shiny::isolate(userState$eb_label_size) %||% 4,
              min = 1,
              step = 0.5
            )
          ),
          shiny::column(
            6,
            shiny::sliderInput(
              "eb_base_size",
              label = helper_label(
                "Base Font Size",
                "Plot Font Size",
                "Controls the base text size used across the plot, including axes, facet titles, and labels. Increase this for presentations or dense figures. Decrease it if long labels are being cut off."
              ),
              min = 8,
              max = 20,
              value = shiny::isolate(userState$eb_base_size) %||% 11,
              step = 1
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
              selected = shiny::isolate(userState$corr_target) %||% num_cols[1]
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

    # ————————————————————
    # PCA
    # ————————————————————
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
    # ——————————————————————————————
    # Random Forest
    # ——————————————————————————————
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
              selected = shiny::isolate(userState$splsda_group_col) %||% cols[1]
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
              value = shiny::isolate(userState$splsda_use_batch_corr) %||% FALSE
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
              value = shiny::isolate(userState$splsda_use_multilevel) %||% FALSE
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
        # Row 9: Font scale multiplier
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::sliderInput(
              "splsda_fontsize",
              label = shinyhelper::helper(
                type = "inline",
                title = "Font Scale Multiplier",
                icon = "fas fa-exclamation-circle",
                shiny_tag = shiny::HTML(
                  "<span style='margin-right: 15px;'>Font Scale Multiplier</span>"
                ),
                content = "Scale the text size used in the sPLS-DA figures. Increase this if labels are hard to read in the plot output. Decrease it if the figure feels crowded.",
                colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
                  "red"
                } else {
                  "blue"
                }
              ),
              min = 0.5,
              max = 2.5,
              step = 0.1,
              value = shiny::isolate(userState$splsda_fontsize) %||% 1.0
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
    # ——————————————————————————————
    # Extreme Gradient Boosting (XGBoost)
    # ——————————————————————————————
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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
  do.call(shiny::tagList, ui_list)
})

# 2) Auto‑sync in Step 3/4 based purely on what was checked in Step 2
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
