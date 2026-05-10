app_server <- getFromNamespace("app_server", "CytokineProfileShinyApp")

test_that("app server boots with default navigation and workflow state", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    expect_equal(app_ctx$currentStep(), 1)
    expect_equal(app_ctx$currentPage(), "home")
    expect_null(app_ctx$selected_function())
    expect_equal(app_ctx$userState$step2_scale, "none")
    expect_false(isTRUE(app_ctx$userState$use_builtin))
  })
})

test_that("app server blocks advancing from step 1 until data is available", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    enter_app_workflow(session)
    click_test_input(session, "next1")

    expect_equal(app_ctx$currentStep(), 1)
    expect_equal(app_ctx$currentPage(), "step1")
  })
})

test_that("built-in data and type overrides populate step 2 state", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    prepare_app_server_step2(session)

    typed_data <- app_ctx$step2_typed_data()
    col_info <- app_ctx$step2_typed_col_info()

    expect_s3_class(app_ctx$userData(), "data.frame")
    expect_true("..cyto_id.." %in% names(app_ctx$userData()))
    expect_true(all(example1_test_categorical_cols() %in% col_info$categorical))
    expect_false("Time" %in% col_info$numerical)
    expect_equal(app_ctx$userState$step2_applied_factor_cols, "Time")
    expect_true(all(example1_test_numerical_cols() %in% names(typed_data)))
  })
})

test_that("step 2 filtering and next2 persist selected workflow state", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    prepare_app_server_step3(
      session,
      app_ctx = app_ctx,
      apply_filters = TRUE,
      scale_choice = "log2"
    )

    filtered <- app_ctx$filteredData()

    expect_equal(app_ctx$currentStep(), 3)
    expect_equal(app_ctx$currentPage(), "step3")
    expect_equal(app_ctx$userState$step2_scale, "log2")
    expect_true(all(c("Group", "Time") %in% app_ctx$userState$selected_columns))
    expect_true(all(filtered$Group %in% c("PreT2D", "T2D")))
    expect_true(all(as.character(filtered$Time) %in% c("20", "72")))
  })
})

test_that("preview transform stays inactive until a preprocessing method is chosen", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    prepare_app_server_step2(session)

    click_test_input(session, "preview_transform")

    expect_false(app_ctx$show_comparison())
  })
})

test_that("save-key-input observers keep selected columns in sync before analysis", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    prepare_app_server_step2(session)

    expect_setequal(
      app_ctx$userState$selected_categorical_cols,
      example1_test_categorical_cols()
    )
    expect_setequal(
      app_ctx$userState$selected_numerical_cols,
      example1_test_numerical_cols()
    )
    expect_true(all(
      example1_test_categorical_cols() %in% app_ctx$userState$selected_columns
    ))
  })
})

test_that("imputation notifications explain missing and unsupported feature-wise selections", {
  local_mocked_browser_side_effects()
  notifications <- local_capture_shiny_notifications()

  shiny::testServer(app_server, {
    prepare_app_server_step3(session, app_ctx = app_ctx)

    set_test_input(session, ui_imputation_method_input_id(), "mean")
    click_test_input(session, "apply_impute")
    expect_match(
      notifications$last()$ui,
      "Choose at least one column before applying imputation.",
      fixed = TRUE
    )

    notifications$clear()
    set_test_input(session, ui_imputation_method_input_id(), "knn_feature")
    set_test_input(session, "imp_cols", "Group")
    click_test_input(session, "apply_impute")
    expect_match(
      notifications$last()$ui,
      "Feature-wise kNN imputation needs at least one numeric column.",
      fixed = TRUE
    )

    notifications$clear()
    set_test_input(session, "imp_cols", example1_test_numerical_cols()[1])
    click_test_input(session, "apply_impute")
    expect_match(
      notifications$last()$ui,
      "Feature-wise kNN imputation needs at least two numeric columns.",
      fixed = TRUE
    )
  })
})

test_that("Bio-Plex editor notifications explain invalid header and column actions", {
  local_mocked_browser_side_effects()
  notifications <- local_capture_shiny_notifications()

  shiny::testServer(app_server, {
    app_ctx$bioplex$df <- data.frame(A = 1:2, B = 3:4)
    app_ctx$bioplex$editor_mode <- "persisted"
    app_ctx$bioplex$deleted_idx <- integer(0)
    app_ctx$bioplex$user_columns <- character(0)

    click_test_input(session, "bioplex_set_header")
    expect_match(
      notifications$last()$ui,
      "Select exactly one row, then choose Set Header.",
      fixed = TRUE
    )

    notifications$clear()
    click_test_input(session, "bioplex_modal_delete_cols")
    expect_match(
      notifications$last()$ui,
      "Select one or more columns from the footer, then choose Delete.",
      fixed = TRUE
    )

    notifications$clear()
    set_test_input(session, "bioplex_newcol_name", "")
    click_test_input(session, "bioplex_newcol_confirm")
    expect_match(
      notifications$last()$ui,
      "Enter a column name before choosing Add.",
      fixed = TRUE
    )
  })
})
