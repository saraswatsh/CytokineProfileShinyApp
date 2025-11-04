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
  userState$hm_scale = NULL
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
  session$reload()
})
shiny::observeEvent(input$fresh_start, {
  shiny::showModal(
    shiny::modalDialog(
      title = "Start fresh?",
      "This will clear uploaded/built-in data and all saved options.",
      footer = tagList(
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
    userState$hm_scale = NULL
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
        p(em("Maintainer, Co-Creator, and Author of CytokineProfile")),
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
        p(em("Co-Creator and Author of CytokineProfile")),
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
    br(),
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
        "For additional questions or concerns, contact the maintainer Shubh Saraswat with the provided email above."
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
            # Conditional panel to show the editor button once data is uploaded
            uiOutput("open_editor_btn"),
            hr(),
            tags$h5("Option B: Use Built-in Data"),
            checkboxInput(
              "use_builtin",
              "Use a built-in dataset?",
              value = isolate(userState$use_builtin) %||% FALSE
            ),
            uiOutput("built_in_selector"),
            uiOutput("step1_bottom_block")
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
          card(
            class = "mb-3", # nice spacing below
            card_header(
              class = "bg-info",
              "2a. Treat Columns as Categorical"
            ),
            card_body(
              uiOutput("step2_type_ui") # your dynamic controls render here
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
              "open_impute_modal",
              "Treat missing values",
              icon = icon("fas fa-eraser"),
              class = "btn-secondary"
            ),
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
  req(input$use_builtin || isTRUE(bioplex$active))
  # If the condition above is met, render the checkboxes
  tagList(
    checkboxInput("view_data", "View Data Loaded?", FALSE),
    checkboxInput("show_summary", "Show summary statistics", FALSE)
  )
})

# Logic for the “Fresh Start” button
output$fresh_start_ui <- shiny::renderUI({
  has_data <- isTRUE(bioplex$active) ||
    (isTRUE(userState$use_builtin) && !is.null(userState$built_in_choice))

  if (!has_data) {
    return(NULL)
  }

  div(
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

  tagList(
    # show the view/summary toggles only after confirmed data (built-in OR Save & Use)
    if (has_confirmed_data) hr(class = "my-2"),
    if (has_confirmed_data) uiOutput("viewSummaryCheckboxes"),
    if (has_confirmed_data) hr(class = "my-2"),
    if (has_confirmed_data) uiOutput("fresh_start_ui")
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
output$ml_function_ui <- shiny::renderUI({
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
  if (!isTRUE(input$use_builtin) && !isTRUE(bioplex$active)) {
    showModal(modalDialog(
      title = "Confirm your data first",
      "Please open the Data Editor and click 'Save & Use' (or choose a built-in dataset) before continuing.",
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
}) %>%
  shiny::debounce(500)

data_after_filters <- shiny::reactive({
  df <- raw_filtered()
  req(df)
  if (isTRUE(input$step2_log2)) {
    num_cols <- intersect(input$selected_numerical_cols, names(df))
    df[num_cols] <- round(log2(df[num_cols]), 5)
  }
  df
}) %>%
  shiny::debounce(500)

data_after_imputation <- shiny::reactive({
  dat <- data_after_filters()
  imp <- imputed_data()
  if (!is.null(imp)) {
    return(imp)
  }
  dat
}) %>%
  shiny::debounce(500)

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
}) %>%
  shiny::debounce(500)

# Populate the sPLS-DA "Label column" choices whenever the working data changes
shiny::observeEvent(data_after_filters(), {
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
      card_header(class = "bg-info", "4. Optional: Apply Filters"),
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

shiny::observeEvent(input$open_impute_modal, {
  shiny::showModal(modalDialog(
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
          "impute_method",
          label = tagList(
            span("Method", style = "margin-right:10px;"),
            bslib::popover(
              actionLink("mv_help", NULL, icon = icon("question-circle")),
              div(
                tags$h4("How missing values are treated", class = "mb-3"),
                p(
                  tags$b("Mean:"),
                  " this method calculates the average of all non-missing values in a column and uses this mean to fill in any missing entries."
                ),
                p(
                  tags$b("Median:"),
                  " this method calculates the middle value of all non-missing entries in a column and uses this median to replace any missing values."
                ),
                p(
                  tags$b("Mode:"),
                  " this method identifies the most frequently occurring value in a column and uses it to fill in the missing data. Mode imputation is the only one of these three that can be used for categorical (non-numeric) data."
                ),
                p(
                  tags$b("kNN (sample-wise):"),
                  " this method for each missing cell, finds the k nearest rows (samples) using the other features and impute from those neighbors’ values in the same column"
                ),
                p(
                  tags$b("kNN (feature-wise):"),
                  " this method for each missing cell, finds the k most similar columns (features) based on their patterns across samples and impute from those features’ values in the same row"
                )
              ),
              placement = "right",
            )
          ),
          choices = c(
            "Mean",
            "Median",
            "Mode",
            "kNN (sample-wise)",
            "kNN (feature-wise)"
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

shiny::observe({
  df <- data_after_filters()
  req(df)
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
  req(imputed_data())
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
      rec <- rec %>% recipes::step_normalize(all_of(num_cols))
    }
    rec <- rec %>% recipes::step_impute_knn(all_of(include), neighbors = k)
    dat <- recipes::bake(prep(rec, training = dat), new_data = dat)
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
      userState$hm_scale <- input$hm_scale
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
