app_server <- getFromNamespace("app_server", "CytokineProfileShinyApp")

test_that("app server supports an end-to-end built-in workflow with server-side assertions", {
  local_mocked_browser_side_effects()

  shiny::testServer(CytokineProfileShinyApp:::app_server, {
    enter_app_workflow(session)
    expect_equal(app_ctx$currentPage(), "step1")

    run_app_server_analysis(
      session,
      app_ctx,
      "menu_univariate_2lvl",
      apply_filters = TRUE,
      scale_choice = "log2"
    )

    expect_equal(app_ctx$userState$step2_scale, "log2")
    expect_equal(
      app_ctx$selected_function(),
      "Univariate Tests (T-test, Wilcoxon)"
    )
    expect_equal(app_ctx$currentStep(), 5)
    expect_equal(app_ctx$currentPage(), "step5")
    expect_true(all(app_ctx$filteredData()$Group %in% c("PreT2D", "T2D")))
    expect_true(all(
      as.character(app_ctx$filteredData()$Time) %in% c("20", "72")
    ))
    expect_equal(app_ctx$userState$uv2_method %||% "auto", "auto")
    expect_null(app_ctx$errorMessage())
  })
})
