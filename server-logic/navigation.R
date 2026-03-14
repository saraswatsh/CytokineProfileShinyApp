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

  # Built=in Data built‑in tracking:
  userState$use_builtin = FALSE
  userState$built_in_choice = NULL

  # Step 2 preprocessing
  userState$step2_scale = "none"

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

# Development-notice modal: show once per session when user lands on Home.
shiny::observe({
  cfg <- tryCatch(config::get(), error = function(e) list(build_type = NULL))
  is_devel <- identical(cfg$build_type, "development") ||
    identical(cfg$build_type, "devel")

  if (
    is_devel && identical(currentPage(), "home") && !isTRUE(dev_notice_shown())
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
    # and medium width. Requires shinyjs (loaded in ui.R).
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
      ## —— Column 1 —— ##
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

      ## —— Column 2 —— ##
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
      ## —— Column 3 —— ##
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
    df <- userData()
    all_cols <- names(df)[names(df) != "..cyto_id.."]
    is_numeric_col <- sapply(df[all_cols], is.numeric)
    numeric_cols <- all_cols[is_numeric_col]
    categorical_cols <- all_cols[!is_numeric_col]

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
          bslib::card(
            class = "mb-3", # nice spacing below
            bslib::card_header(
              class = "bg-info",
              "2a. Treat Columns as Categorical"
            ),
            bslib::card_body(
              shiny::uiOutput("step2_type_ui") # your dynamic controls render here
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
                  colour = if (input$theme_choice %in% c("darkly", "cyborg")) {
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

          # wrap deleted‐samples in a card too
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
            shiny::actionButton("menu_heatmap", "Heatmap", class = "menu-card"),
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
              "Sparse Partial Least Squares - Discriminant Analysis (sPLS‑DA)",
              class = "menu-card"
            ),
            shiny::actionButton(
              "menu_mint_splsda",
              "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS‑DA)",
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

# Logic for the “Fresh Start” button
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
  c(input$selected_categorical_cols, input$selected_numerical_cols)
})

# B) “Select / Deselect All” observers
shiny::observeEvent(input$select_all_cat, {
  df <- userData()
  all_cols <- setdiff(names(df), "..cyto_id..")
  cat_cols <- all_cols[!sapply(df[all_cols], is.numeric)]
  shiny::updateCheckboxGroupInput(
    session,
    "selected_categorical_cols",
    selected = cat_cols
  )
})
shiny::observeEvent(input$deselect_all_cat, {
  df <- userData()
  all_cols <- setdiff(names(df), "..cyto_id..")
  cat_cols <- all_cols[!sapply(df[all_cols], is.numeric)]
  shiny::updateCheckboxGroupInput(
    session,
    "selected_categorical_cols",
    selected = character(0)
  )
})
shiny::observeEvent(input$select_all_num, {
  df <- userData()
  all_cols <- setdiff(names(df), "..cyto_id..")
  numeric_cols <- all_cols[sapply(df[all_cols], is.numeric)]
  shiny::updateCheckboxGroupInput(
    session,
    "selected_numerical_cols",
    selected = numeric_cols
  )
})
shiny::observeEvent(input$deselect_all_num, {
  df <- userData()
  all_cols <- setdiff(names(df), "..cyto_id..")
  numeric_cols <- all_cols[sapply(df[all_cols], is.numeric)]
  shiny::updateCheckboxGroupInput(
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
    num_cols <- intersect(input$selected_numerical_cols, names(df))
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
    title = "Filtered Data Explorer – Full View",
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
          "impute_method",
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
                  shiny::tags$li("If missingness is low and you want a simple numeric fill, start with median for skewed cytokine data and mean for roughly symmetric data."),
                  shiny::tags$li("If preserving multivariate structure matters, consider kNN instead of a simple fill."),
                  shiny::tags$li("Use sample-wise kNN when you expect biologically similar samples or subjects."),
                  shiny::tags$li("Use feature-wise kNN when correlated cytokines are expected and only numeric features are being imputed."),
                  shiny::tags$li("If missingness is substantial or clearly differs by group, interpret downstream results more cautiously.")
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
          choices = c(
            "Mean",
            "Median",
            "Mode",
            "kNN (sample-wise)",
            "kNN (feature-wise)"
          ),
          selected = userState$impute_meta$method %||% "mean"
        ),
        shiny::conditionalPanel(
          "input.imp_method == 'knn_sample' || input.imp_method == 'knn_feature'",
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
  df <- data_after_filters() # <-- impute the *filtered* data
  sel <- input$imp_cols
  if (!length(sel)) {
    shiny::showNotification("Select ≥1 column.", type = "error")
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

  # ── Cap variables shown in boxplots for readability ──────────────────────
  MAX_BOX_VARS <- 40
  box_cols <- if (length(num_cols) > MAX_BOX_VARS) {
    num_cols[seq_len(MAX_BOX_VARS)]
  } else {
    num_cols
  }

  # ── Long-format data ──────────────────────────────────────────────────────
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

  # ── Shared theme ──────────────────────────────────────────────────────────
  base_theme <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 8, colour = "gray50"),
      panel.grid.minor = ggplot2::element_blank()
    )
  # Helper: trim x-axis to 1st–99th percentile for display only
  trim_limits <- function(x, lo = 0.01, hi = 0.99) {
    qs <- quantile(x, probs = c(lo, hi), na.rm = TRUE)
    list(lo = qs[[1]], hi = qs[[2]])
  }
  before_lims <- trim_limits(df_before_hist$value)
  after_lims <- trim_limits(df_after_hist$value)
  # ── 1. Density plots (top row) ────────────────────────────────────────────
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

  # ── 2. Horizontal boxplots (bottom row) ───────────────────────────────────
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

  # ── 3. Compose ────────────────────────────────────────────────────────────
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

# 2) When they click “Next” on Step 2, save the selected columns and log2 option
shiny::observeEvent(input$next2, {
  # save state
  userState$selected_columns <- selected_columns_combined()
  userState$selected_categorical_cols <- input$selected_categorical_cols
  userState$selected_numerical_cols <- input$selected_numerical_cols
  userState$step2_scale <- input$step2_scale
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
