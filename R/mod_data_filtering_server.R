mod_data_filtering_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  userState <- app_ctx$userState
  step2_typed_data <- app_ctx$step2_typed_data
  step2_typed_col_info <- app_ctx$step2_typed_col_info
  safe_names <- app_ctx$safe_names
  deleted_row_ids <- app_ctx$deleted_row_ids
  imputed_data <- app_ctx$imputed_data
  ## ---------------------------
  ## Data Filtering and Column Selection
  ## ---------------------------

  # Decoupled UI generation to prevent reactive loops
  output$filter_ui <- shiny::renderUI({
    df <- step2_typed_data()
    shiny::req(df, input$selected_categorical_cols)
    col_info <- step2_typed_col_info()

    # Only create filters for the categorical columns the user has selected
    factor_cols <- intersect(
      col_info$categorical,
      input$selected_categorical_cols
    )

    if (length(factor_cols) == 0) {
      return(NULL)
    }

    lapply(factor_cols, function(col) {
      all_levels <- sort(unique(df[[col]]))
      filter_id <- paste0("filter_", col)

      # Use isolate() to read the input value without creating a dependency
      # When the UI is rebuilt, prefer the saved selection from userState.
      # Fall back to the live input value only when no saved state exists yet.
      selected_now <- userState$step2_filter_values[[col]]
      if (is.null(selected_now)) {
        selected_now <- shiny::isolate(input[[filter_id]])
      }
      if (is.null(selected_now)) {
        selected_now <- all_levels
      } else {
        selected_now <- intersect(selected_now, all_levels)
      }

      shiny::selectizeInput(
        inputId = filter_id,
        label = paste("Filter", col, "(select levels)"),
        choices = all_levels,
        selected = selected_now,
        multiple = TRUE,
        options = list(plugins = c("remove_button", "restore_on_backspace"))
      )
    })
  })

  # ---- Step 2: Type controls ----
  output$step2_type_override_ui <- shiny::renderUI({
    df <- step2_typed_data()
    shiny::req(df)
    cols <- safe_names(df)
    selected_factor_cols <- intersect(
      shiny::isolate(input$factor_cols) %||%
        userState$step2_factor_cols %||%
        character(0),
      cols
    )
    selected_numeric_cols <- intersect(
      shiny::isolate(input$numeric_override_cols) %||%
        userState$step2_numeric_override_cols %||%
        character(0),
      cols
    )
    factor_order_choices <- selected_factor_cols
    factor_order_selected <- shiny::isolate(input$factor_order_col) %||%
      userState$step2_factor_order_col
    factor_order_selected <- intersect(
      factor_order_selected %||% character(0),
      factor_order_choices
    )
    if (!length(factor_order_selected) && length(factor_order_choices)) {
      factor_order_selected <- factor_order_choices[1]
    }
    factor_order_enabled <- isTRUE(
      shiny::isolate(input$factor_order_enable) %||%
        userState$step2_factor_order_enable
    )
    factor_levels_value <- shiny::isolate(input$factor_levels_csv) %||%
      userState$step2_factor_levels_csv %||%
      ""

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectizeInput(
            "factor_cols",
            "Treat these columns as categorical:",
            choices = cols,
            selected = selected_factor_cols,
            multiple = TRUE,
            options = list(plugins = "remove_button")
          )
        ),
        shiny::column(
          6,
          shiny::selectizeInput(
            "numeric_override_cols",
            "Treat these columns as numeric:",
            choices = cols,
            selected = selected_numeric_cols,
            multiple = TRUE,
            options = list(plugins = "remove_button")
          )
        )
      ),
      shiny::checkboxInput(
        "factor_order_enable",
        "Specify level order for one selected categorical column (optional)",
        value = factor_order_enabled
      ),
      shiny::conditionalPanel(
        "input.factor_order_enable",
        shiny::selectInput(
          "factor_order_col",
          "Column to order:",
          choices = factor_order_choices,
          selected = factor_order_selected
        ),
        shiny::textInput(
          "factor_levels_csv",
          "Level order (comma-separated):",
          value = factor_levels_value,
          placeholder = "e.g. Control, Case, Unknown"
        )
      ),
      shiny::helpText(
        "Values that cannot be parsed as numbers will become missing (NA) while the column stays numeric."
      ),
      shiny::actionButton("apply_types", "Apply types")
    )
  })

  invisible(app_stage_commit(app_ctx, stage_env))
}
