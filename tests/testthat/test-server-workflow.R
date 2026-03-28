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
