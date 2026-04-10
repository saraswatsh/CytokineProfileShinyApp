test_that("font-setting helpers preserve defaults and custom values", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    prepare_app_server_step4(session, app_ctx, "Boxplots")

    spec <- app_ctx$get_analysis_font_spec("Boxplots")
    fields <- c("plot_title", "x_text")

    expect_null(
      app_ctx$font_settings_state_from_inputs(
        input = list(),
        prefix = spec$prefix,
        supported_fields = fields,
        default_font_settings = spec$default_font_settings
      )
    )

    state_with_defaults <- app_ctx$font_settings_state_from_inputs(
      input = list(bp_font_use_custom = FALSE),
      prefix = spec$prefix,
      supported_fields = fields,
      default_font_settings = spec$default_font_settings
    )
    expect_false(isTRUE(state_with_defaults$use_custom))
    expect_equal(
      state_with_defaults$plot_title,
      spec$default_font_settings$plot_title
    )
    expect_equal(state_with_defaults$x_text, spec$default_font_settings$x_text)

    set_test_input(session, "bp_font_use_custom", TRUE)
    set_test_input(session, "bp_font_plot_title", 22)
    set_test_input(session, "bp_font_x_text", 15)

    custom_state <- app_ctx$font_settings_state_from_inputs(
      input = input,
      prefix = spec$prefix,
      supported_fields = fields,
      default_font_settings = spec$default_font_settings
    )
    expect_true(isTRUE(custom_state$use_custom))
    expect_equal(custom_state$plot_title, 22)
    expect_equal(custom_state$x_text, 15)

    default_backend <- app_ctx$font_settings_state_to_backend(
      state = list(use_custom = FALSE, plot_title = 99, x_text = 77),
      default_font_settings = spec$default_font_settings
    )
    expect_equal(default_backend$plot_title, spec$default_font_settings$plot_title)
    expect_equal(default_backend$x_text, spec$default_font_settings$x_text)

    custom_backend <- app_ctx$font_settings_state_to_backend(
      state = list(use_custom = TRUE, plot_title = 99, x_text = 77),
      default_font_settings = spec$default_font_settings
    )
    expect_equal(custom_backend$plot_title, 99)
    expect_equal(custom_backend$x_text, 77)
  })
})

new_options_test_app_ctx <- function(initial_df = NULL) {
  if (is.null(initial_df)) {
    initial_df <- data.frame(
      Group = factor(c("Case", "Control", "Case")),
      Volcano = factor(c("A", "B", "A")),
      IL.10 = c(1, 2, 3),
      IL.6 = c(4, 5, 6),
      TNF = c(7, 8, 9),
      check.names = FALSE
    )
  }

  app_ctx <- new_test_app_ctx()
  filtered_df <- shiny::reactiveVal(initial_df)

  app_ctx$userState <- shiny::reactiveValues(
    splsda_var_num_manual = FALSE,
    mint_splsda_var_num_manual = FALSE,
    plsr_keepX_manual = FALSE,
    df_cond1 = NULL,
    df_cond2 = NULL,
    volc_cond1 = NULL,
    volc_cond2 = NULL
  )
  app_ctx$selected_function <- shiny::reactiveVal(NULL)
  app_ctx$filteredData <- shiny::reactive(filtered_df())
  app_ctx$safe_names <- function(df) names(df)

  list(app_ctx = app_ctx, filtered_df = filtered_df)
}

test_that("auto-sync updates defaults until manual overrides are set", {
  local_mocked_browser_side_effects()
  ctx <- new_options_test_app_ctx()

  shiny::testServer(wrap_server_with_app_ctx(mod_options_server, ctx$app_ctx), {
    messages <- capture_session_input_messages(session)
    messages$clear()
    set_test_input(session, "selected_numerical_cols", c("IL.10", "IL.6", "TNF"))

    expect_equal(
      as.numeric(messages$last("splsda_var_num")$message$value),
      3
    )
    expect_equal(
      as.numeric(messages$last("mint_splsda_var_num")$message$value),
      3
    )
    expect_equal(
      as.numeric(messages$last("plsr_keepX")$message$value),
      3
    )

    messages$clear()
    set_test_input(session, "splsda_var_num", 7)
    set_test_input(session, "mint_splsda_var_num", 6)
    set_test_input(session, "plsr_keepX", 5)

    expect_true(isTRUE(ctx$app_ctx$userState$splsda_var_num_manual))
    expect_true(isTRUE(ctx$app_ctx$userState$mint_splsda_var_num_manual))
    expect_true(isTRUE(ctx$app_ctx$userState$plsr_keepX_manual))

    messages$clear()
    set_test_input(session, "selected_numerical_cols", "IL.10")

    expect_length(messages$find("splsda_var_num"), 0L)
    expect_length(messages$find("mint_splsda_var_num"), 0L)
    expect_length(messages$find("plsr_keepX"), 0L)
  })
})

test_that("dual-flashlight and volcano condition UIs handle valid and insufficient groups", {
  local_mocked_browser_side_effects()
  ctx <- new_options_test_app_ctx()

  shiny::testServer(wrap_server_with_app_ctx(mod_options_server, ctx$app_ctx), {
    set_test_input(session, "theme_choice", "flatly")
    set_test_input(session, "df_group_var", "Group")
    set_test_input(session, "volc_group_col", "Volcano")

    expect_match(
      test_server_output_html(output, "df_conditions_ui"),
      "df_cond1",
      fixed = TRUE
    )
    expect_match(
      test_server_output_html(output, "volc_conditions_ui"),
      "volc_cond1",
      fixed = TRUE
    )

    ctx$filtered_df(data.frame(Group = factor("Only"), Volcano = factor("Only")))
    test_server_flush(session)

    expect_match(
      test_server_output_html(output, "df_conditions_ui"),
      "Not enough unique levels in grouping column",
      fixed = TRUE
    )
    expect_match(
      test_server_output_html(output, "volc_conditions_ui"),
      "does not have at least two unique values",
      fixed = TRUE
    )
  })
})
