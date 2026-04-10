test_that("step 1 restoration updates built-in dataset controls", {
  local_mocked_browser_side_effects()
  messages <- NULL

  shiny::testServer(app_server, {
    messages <- capture_session_input_messages(session)
    app_ctx$currentStep(2)
    test_server_flush(session)

    app_ctx$userState$use_builtin <- TRUE
    app_ctx$userState$built_in_choice <- "ExampleData1"

    messages$clear()
    app_ctx$currentStep(1)
    test_server_flush(session)

    expect_true(isTRUE(messages$last("use_builtin")$message$value))
    expect_equal(
      messages$last("built_in_choice")$message$value,
      "ExampleData1"
    )
  })
})

test_that("step 2 restoration rehydrates bucket selections and defaults factor order", {
  local_mocked_browser_side_effects()
  messages <- NULL

  shiny::testServer(app_server, {
    messages <- capture_session_input_messages(session)
    prepare_app_server_step2(session)

    app_ctx$userState$selected_columns <- c("Group", "IL.10")
    app_ctx$userState$step2_scale <- "log2"
    app_ctx$userState$step2_factor_cols <- c("Time", "MissingFactor")
    app_ctx$userState$step2_numeric_override_cols <- c("IL.6", "GhostNumeric")
    app_ctx$userState$step2_factor_order_enable <- TRUE
    app_ctx$userState$step2_factor_order_col <- "MissingFactor"
    app_ctx$userState$step2_factor_levels_csv <- "20, 72"

    app_ctx$currentStep(3)
    test_server_flush(session)

    messages$clear()
    app_ctx$currentStep(2)
    test_server_flush(session)

    expect_equal(
      messages$last("selected_categorical_cols")$message$value,
      "Group"
    )
    expect_equal(
      messages$last("selected_numerical_cols")$message$value,
      "IL.10"
    )
    expect_equal(
      messages$last("step2_scale")$message$value,
      "log2"
    )
    expect_equal(
      messages$last("factor_cols")$message$value,
      "Time"
    )
    expect_equal(
      messages$last("numeric_override_cols")$message$value,
      "IL.6"
    )
    expect_true(isTRUE(messages$last("factor_order_enable")$message$value))
    expect_match(messages$last("factor_order_col")$message$options, "Time")
    expect_equal(
      messages$last("factor_order_col")$message$value,
      "Time"
    )
    expect_equal(
      messages$last("factor_levels_csv")$message$value,
      "20, 72"
    )
  })
})

test_that("step 3 restoration re-selects analysis category and menu choices", {
  local_mocked_browser_side_effects()
  messages <- NULL

  shiny::testServer(app_server, {
    messages <- capture_session_input_messages(session)
    prepare_app_server_step3(session, app_ctx = app_ctx)

    app_ctx$userState$analysis_categories <- "machine_learning"
    app_ctx$userState$stat_function <- "Univariate Tests (T-test, Wilcoxon)"
    app_ctx$userState$exploratory_function <- "Heatmap"
    app_ctx$userState$multivariate_function <- "Principal Component Analysis (PCA)"
    app_ctx$userState$ml_function <- "Random Forest"

    app_ctx$currentStep(2)
    test_server_flush(session)

    messages$clear()
    app_ctx$currentStep(3)
    test_server_flush(session)

    expect_equal(
      messages$last("analysis_categories")$message$value,
      "machine_learning"
    )
    expect_equal(
      messages$last("stat_function")$message$value,
      "Univariate Tests (T-test, Wilcoxon)"
    )
    expect_equal(
      messages$last("exploratory_function")$message$value,
      "Heatmap"
    )
    expect_equal(
      messages$last("multivariate_function")$message$value,
      "Principal Component Analysis (PCA)"
    )
    expect_equal(
      messages$last("ml_function")$message$value,
      "Random Forest"
    )
  })
})

test_that("step 4 restoration normalizes representative defaults", {
  local_mocked_browser_side_effects()
  messages <- NULL

  shiny::testServer(app_server, {
    messages <- capture_session_input_messages(session)
    prepare_app_server_step3(session, app_ctx = app_ctx)

    app_ctx$selected_function("Multi-level Univariate Tests (Anova, Kruskal-Wallis)")
    app_ctx$userState$selected_function <-
      "Multi-level Univariate Tests (Anova, Kruskal-Wallis)"
    app_ctx$userState$uvm_method <- "kruskal"
    app_ctx$userState$uvm_p_adjust_method <- NULL

    messages$clear()
    app_ctx$currentPage("step4")
    app_ctx$currentStep(4)
    test_server_flush(session)

    expect_equal(
      messages$last("uvm_method")$message$value,
      "kruskal"
    )
    expect_equal(
      messages$last("uvm_p_adjust_method")$message$value,
      "BH"
    )

    app_ctx$currentStep(3)
    test_server_flush(session)

    app_ctx$selected_function("Error-Bar Plot")
    app_ctx$userState$selected_function <- "Error-Bar Plot"
    app_ctx$userState$eb_group_col <- "Group"
    app_ctx$userState$eb_p_adjust_method <- NULL
    app_ctx$userState$eb_n_col <- NULL
    app_ctx$userState$eb_fill_palette <- "grey"

    messages$clear()
    app_ctx$currentStep(4)
    test_server_flush(session)

    expect_equal(
      messages$last("eb_group_col")$message$value,
      "Group"
    )
    expect_equal(
      messages$last("eb_p_adjust_method")$message$value,
      "BH"
    )
    expect_equal(
      as.numeric(messages$last("eb_n_col")$message$value),
      3
    )
    expect_equal(
      messages$last("eb_fill_palette")$message$value,
      "gray"
    )
  })
})

test_that("step 4 restoration covers representative PLSR and sPLS-DA branches", {
  local_mocked_browser_side_effects()
  messages <- NULL

  shiny::testServer(app_server, {
    messages <- capture_session_input_messages(session)
    prepare_app_server_step3(session, app_ctx = app_ctx)

    app_ctx$selected_function("Partial Least Squares Regression (PLSR)")
    app_ctx$userState$selected_function <-
      "Partial Least Squares Regression (PLSR)"
    app_ctx$userState$plsr_group_col <- "Group"
    app_ctx$userState$plsr_response_col <- "IL.10"
    app_ctx$userState$plsr_predictor_cols <- c("IL.10", "IL.6")
    app_ctx$userState$plsr_comp_num <- 2
    app_ctx$userState$plsr_sparse <- TRUE
    app_ctx$userState$plsr_keepX <- 1
    app_ctx$userState$plsr_cv_opt <- "Mfold"
    app_ctx$userState$plsr_fold_num <- 4
    app_ctx$userState$plsr_ellipse <- TRUE
    app_ctx$userState$plsr_colors <- c("red", "blue")
    app_ctx$userState$plsr_font_settings <- list(
      use_custom = TRUE,
      plot_title = 19,
      x_text = 15
    )

    messages$clear()
    app_ctx$currentPage("step4")
    app_ctx$currentStep(4)
    test_server_flush(session)

    expect_equal(
      messages$last("plsr_group_col")$message$value,
      "Group"
    )
    expect_equal(
      messages$last("plsr_response_col")$message$value,
      "IL.10"
    )
    expect_equal(
      messages$last("plsr_predictor_cols")$message$value,
      c("IL.10", "IL.6")
    )
    expect_equal(
      as.numeric(messages$last("plsr_keepX")$message$value),
      1
    )
    expect_equal(
      as.numeric(messages$last("plsr_font_plot_title")$message$value),
      19
    )
  })
})

test_that("step 4 restoration covers representative sPLS-DA restore defaults", {
  local_mocked_browser_side_effects()

  splsda_server <- function(input, output, session) {
    df <- data.frame(
      Group = factor(c("Case", "Control")),
      Time = factor(c("T1", "T2")),
      IL.10 = c(1, 2),
      IL.6 = c(3, 4),
      check.names = FALSE
    )
    app_ctx <- new_test_app_ctx()
    app_ctx$userState <- shiny::reactiveValues(
      selected_function = NULL,
      splsda_group_col = NULL,
      splsda_group_col2 = NULL,
      splsda_use_batch_corr = FALSE,
      splsda_batch_col = NULL,
      splsda_use_multilevel = FALSE,
      splsda_multilevel = NULL,
      splsda_var_num = NULL,
      splsda_cv_opt = NULL,
      splsda_fold_num = NULL,
      splsda_comp_num = NULL,
      splsda_ind_names_mode = NULL,
      splsda_ind_names_col = NULL,
      splsda_pch = NULL,
      splsda_style = NULL,
      splsda_roc = FALSE,
      splsda_ellipse = FALSE,
      splsda_bg = FALSE,
      splsda_conf_mat = FALSE,
      splsda_colors = NULL,
      splsda_font_settings = NULL
    )
    app_ctx$currentStep <- shiny::reactiveVal(0L)
    app_ctx$selected_function <- shiny::reactiveVal(NULL)
    app_ctx$step2_typed_col_info <- shiny::reactive({
      list(
        all = names(df),
        categorical = c("Group", "Time"),
        numerical = c("IL.10", "IL.6")
      )
    })
    app_ctx$data_after_filters <- shiny::reactive(df)
    app_ctx$filteredData <- shiny::reactive(df)
    app_ctx$get_analysis_font_spec <- function(func_name) {
      if (!identical(
        func_name,
        "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
      )) {
        return(NULL)
      }

      list(
        prefix = "splsda",
        state_key = "splsda_font_settings",
        supported_fields = c("plot_title", "point_labels"),
        default_font_settings = list(plot_title = 18, point_labels = 11)
      )
    }
    app_ctx$restore_font_settings_inputs <- function(
      session,
      prefix,
      supported_fields,
      state = NULL,
      default_font_settings = list(plot_title = 18, point_labels = 11)
    ) {
      for (field in supported_fields) {
        value <- state[[field]]
        if (is.null(value)) {
          value <- default_font_settings[[field]]
        }
        shiny::updateSliderInput(
          session,
          paste0(prefix, "_font_", field),
          value = value
        )
      }
      invisible(NULL)
    }

    mod_update_inputs_server(input, output, session, app_ctx)
    session$userData$app_ctx <- app_ctx
    invisible(app_ctx)
  }

  shiny::testServer(splsda_server, {
    messages <- capture_session_input_messages(session)
    app_ctx <- session$userData$app_ctx

    app_ctx$userState$selected_function <-
      "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
    app_ctx$userState$splsda_group_col <- "Group"
    app_ctx$userState$splsda_group_col2 <- "Time"
    app_ctx$userState$splsda_use_batch_corr <- TRUE
    app_ctx$userState$splsda_batch_col <- "Time"
    app_ctx$userState$splsda_use_multilevel <- TRUE
    app_ctx$userState$splsda_multilevel <- "Time"
    app_ctx$userState$splsda_var_num <- 2
    app_ctx$userState$splsda_cv_opt <- "Mfold"
    app_ctx$userState$splsda_fold_num <- 5
    app_ctx$userState$splsda_comp_num <- 2
    app_ctx$userState$splsda_ind_names_mode <- NULL
    app_ctx$userState$splsda_ind_names_col <- NULL
    app_ctx$userState$splsda_pch <- c(16, 17)
    app_ctx$userState$splsda_style <- "graphics"
    app_ctx$userState$splsda_roc <- TRUE
    app_ctx$userState$splsda_ellipse <- TRUE
    app_ctx$userState$splsda_bg <- FALSE
    app_ctx$userState$splsda_conf_mat <- TRUE
    app_ctx$userState$splsda_colors <- c("red", "blue")
    app_ctx$userState$splsda_font_settings <- list(
      use_custom = TRUE,
      plot_title = 21,
      point_labels = 12
    )

    messages$clear()
    app_ctx$currentStep(4)
    test_server_flush(session)

    expect_equal(
      as.numeric(messages$last("splsda_var_num")$message$value),
      2
    )
    expect_equal(
      messages$last("splsda_ind_names_mode")$message$value,
      "off"
    )
    expect_match(messages$last("splsda_ind_names_col")$message$options, "Group")
    expect_match(messages$last("splsda_ind_names_col")$message$options, "Time")
    expect_equal(
      messages$last("splsda_ind_names_col")$message$value,
      "Group"
    )
    expect_equal(
      as.numeric(messages$last("splsda_pch")$message$value),
      c(16, 17)
    )
    expect_equal(
      as.numeric(messages$last("splsda_font_plot_title")$message$value),
      21
    )
  })
})
