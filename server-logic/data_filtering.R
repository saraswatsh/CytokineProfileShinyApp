## ---------------------------
## Data Filtering and Column Selection
## ---------------------------

# Decoupled UI generation to prevent reactive loops
output$filter_ui <- shiny::renderUI({
  df <- userData()
  shiny::req(df, input$selected_categorical_cols)

  # Only create filters for the categorical columns the user has selected
  factor_cols <- intersect(
    names(df)[sapply(df, function(x) is.factor(x) || is.character(x))],
    input$selected_categorical_cols
  )

  if (length(factor_cols) == 0) {
    return(NULL)
  }

  lapply(factor_cols, function(col) {
    all_levels <- sort(unique(df[[col]]))

    # Use isolate() to read the input value without creating a dependency
    # On first load, default to all levels being selected
    selected_now <- shiny::isolate(input[[paste0("filter_", col)]]) %||%
      all_levels

    shiny::selectizeInput(
      inputId = paste0("filter_", col),
      label = paste("Filter", col, "(select levels)"),
      choices = all_levels,
      selected = selected_now,
      multiple = TRUE,
      options = list(plugins = c("remove_button", "restore_on_backspace"))
    )
  })
})

# ---- Step 2: Type controls ----
output$step2_factor_type_ui <- shiny::renderUI({
  df <- userData()
  shiny::req(df)
  cols <- safe_names(df)
  selected_factor_cols <- intersect(
    userState$step2_factor_cols %||%
      shiny::isolate(input$factor_cols) %||%
      character(0),
    cols
  )
  factor_order_choices <- selected_factor_cols
  factor_order_selected <- userState$step2_factor_order_col %||%
    shiny::isolate(input$factor_order_col)
  factor_order_selected <- intersect(
    factor_order_selected %||% character(0),
    factor_order_choices
  )
  if (!length(factor_order_selected) && length(factor_order_choices)) {
    factor_order_selected <- factor_order_choices[1]
  }

  shiny::tagList(
    shiny::selectizeInput(
      "factor_cols",
      "Treat these columns as categorical:",
      choices = cols,
      selected = selected_factor_cols,
      multiple = TRUE,
      options = list(plugins = "remove_button")
    ),
    shiny::checkboxInput(
      "factor_order_enable",
      "Specify level order for one selected categorical column (optional)",
      value = isTRUE(userState$step2_factor_order_enable)
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
        value = userState$step2_factor_levels_csv %||% "",
        placeholder = "e.g. Control, Case, Unknown"
      )
    )
  )
})

output$step2_numeric_type_ui <- shiny::renderUI({
  df <- userData()
  shiny::req(df)
  cols <- safe_names(df)
  selected_numeric_cols <- intersect(
    userState$step2_numeric_override_cols %||%
      shiny::isolate(input$numeric_override_cols) %||%
      character(0),
    cols
  )

  shiny::tagList(
    shiny::selectizeInput(
      "numeric_override_cols",
      "Treat these columns as numeric:",
      choices = cols,
      selected = selected_numeric_cols,
      multiple = TRUE,
      options = list(plugins = "remove_button")
    ),
    shiny::helpText(
      "Values that cannot be parsed as numbers will become missing (NA) while the column stays numeric."
    ),
    shiny::actionButton("apply_types", "Apply types")
  )
})
