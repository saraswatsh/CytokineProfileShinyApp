app_server <- getFromNamespace(
  "app_server",
  "CytokineProfileShinyApp"
)
font_helper_specs <- getFromNamespace(
  "ui_analysis_font_helper_specs",
  "CytokineProfileShinyApp"
)

font_helper_spec <- getFromNamespace(
  "ui_analysis_font_helper_spec",
  "CytokineProfileShinyApp"
)
font_test_prepare_step4 <- function(session, app_ctx, func_name) {
  set_test_input(session, "theme_choice", "flatly")
  prepare_app_server_step3(session, app_ctx = app_ctx)
  font_test_open_step4(session, app_ctx, func_name)
  invisible(NULL)
}

font_test_open_step4 <- function(session, app_ctx, func_name) {
  app_ctx$selected_function(func_name)
  app_ctx$userState$selected_function <- func_name
  app_ctx$currentPage("step4")
  app_ctx$currentStep(4)
  test_server_flush(session)
  invisible(NULL)
}

font_test_ui_html <- function(output) {
  htmltools::renderTags(output[["function_options_ui"]])$html
}

test_that("font helper metadata covers every configured analysis font field", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    helper_specs <- font_helper_specs()

    expect_true(length(helper_specs) > 0)

    for (func_name in names(app_ctx$analysis_font_specs)) {
      spec <- app_ctx$analysis_font_specs[[func_name]]

      expect_true(func_name %in% names(helper_specs), info = func_name)

      for (field in spec$supported_fields) {
        helper_entry <- font_helper_spec(func_name, field)

        expect_false(
          is.null(helper_entry),
          info = paste(func_name, field, sep = " / ")
        )
        expect_true(
          all(c("title", "content") %in% names(helper_entry)),
          info = paste(func_name, field, sep = " / ")
        )
        expect_true(
          nzchar(helper_entry$title),
          info = paste(func_name, field, sep = " / ")
        )
        expect_true(
          nzchar(helper_entry$content),
          info = paste(func_name, field, sep = " / ")
        )
      }
    }
  })
})

test_that("font settings stay local until apply button is clicked", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    font_test_prepare_step4(session, app_ctx, "Boxplots")

    expect_null(app_ctx$userState$bp_font_settings)

    set_test_input(session, "bp_font_use_custom", TRUE)
    set_test_input(session, "bp_font_plot_title", 22)

    expect_null(app_ctx$userState$bp_font_settings)

    click_test_input(session, "bp_font_apply")

    expect_true(isTRUE(app_ctx$userState$bp_font_settings$use_custom))
    expect_equal(app_ctx$userState$bp_font_settings$plot_title, 22)
  })
})

test_that("applied font settings restore after leaving and re-entering step 4", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    font_test_prepare_step4(session, app_ctx, "Boxplots")

    set_test_input(session, "bp_font_use_custom", TRUE)
    set_test_input(session, "bp_font_plot_title", 21)
    click_test_input(session, "bp_font_apply")

    app_ctx$currentPage("step3")
    app_ctx$currentStep(3)
    test_server_flush(session)

    font_test_open_step4(session, app_ctx, "Boxplots")

    expect_true(isTRUE(input$bp_font_use_custom))
    expect_equal(input$bp_font_plot_title, 21)
  })
})

test_that("next4 implicitly persists current font inputs", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    font_test_prepare_step4(session, app_ctx, "Boxplots")

    set_test_input(session, "bp_font_use_custom", TRUE)
    set_test_input(session, "bp_font_plot_title", 23)

    click_test_input(session, "next4")

    expect_equal(app_ctx$currentStep(), 5)
    expect_equal(app_ctx$currentPage(), "step5")
    expect_true(isTRUE(app_ctx$userState$bp_font_settings$use_custom))
    expect_equal(app_ctx$userState$bp_font_settings$plot_title, 23)
    expect_null(app_ctx$errorMessage())
    expect_equal(
      app_ctx$font_settings_state_to_backend(
        app_ctx$userState$bp_font_settings,
        default_font_settings = app_ctx$get_analysis_font_spec(
          "Boxplots"
        )$default_font_settings
      )$plot_title,
      23
    )
  })
})

test_that("every font-enabled analysis renders custom font and apply controls", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    set_test_input(session, "theme_choice", "flatly")
    prepare_app_server_step3(session, app_ctx = app_ctx)

    expect_true(length(app_ctx$analysis_font_specs) > 0)

    for (func_name in names(app_ctx$analysis_font_specs)) {
      spec <- app_ctx$analysis_font_specs[[func_name]]
      font_test_open_step4(session, app_ctx, func_name)
      html <- font_test_ui_html(output)
      first_field <- spec$supported_fields[[1]]
      first_helper <- font_helper_spec(func_name, first_field)

      expect_match(
        html,
        paste0(spec$prefix, "_font_use_custom"),
        fixed = TRUE,
        info = func_name
      )
      expect_match(
        html,
        paste0(spec$prefix, "_font_apply"),
        fixed = TRUE,
        info = func_name
      )
      expect_match(
        html,
        first_helper$title,
        fixed = TRUE,
        info = func_name
      )
    }
  })
})

test_that("representative font sliders render helper modal titles and content", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    set_test_input(session, "theme_choice", "flatly")
    prepare_app_server_step3(session, app_ctx = app_ctx)

    representative_fields <- list(
      list(func_name = "Boxplots", field = "plot_title"),
      list(func_name = "Heatmap", field = "row_names"),
      list(
        func_name = "Principal Component Analysis (PCA)",
        field = "point_labels"
      ),
      list(func_name = "Volcano Plot", field = "annotation_text")
    )

    for (case in representative_fields) {
      font_test_open_step4(session, app_ctx, case$func_name)
      html <- font_test_ui_html(output)
      helper_entry <- font_helper_spec(case$func_name, case$field)

      expect_match(
        html,
        helper_entry$title,
        fixed = TRUE,
        info = paste(case$func_name, case$field, sep = " / ")
      )
      expect_match(
        html,
        helper_entry$content,
        fixed = TRUE,
        info = paste(case$func_name, case$field, sep = " / ")
      )
    }
  })
})
