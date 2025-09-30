  ## ---------------------------
  ## Data Filtering and Column Selection
  ## ---------------------------

  # Decoupled UI generation to prevent reactive loops
  output$filter_ui <- shiny::renderUI({
    df <- userData()
    req(df, input$selected_categorical_cols)

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
      selected_now <- isolate(input[[paste0("filter_", col)]]) %||% all_levels

      selectizeInput(
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
  output$step2_type_ui <- shiny::renderUI({
    df <- userData()
    req(df)
    cols <- safe_names(df)
    tagList(
      selectizeInput(
        "factor_cols",
        "Treat these columns as categorical:",
        choices = cols,
        selected = isolate(input$factor_cols),
        multiple = TRUE,
        options = list(plugins = "remove_button")
      ),
      checkboxInput(
        "factor_order_enable",
        "Specify level order for one column (optional)",
        FALSE
      ),
      conditionalPanel(
        "input.factor_order_enable",
        selectInput("factor_order_col", "Column to order:", choices = cols),
        textInput(
          "factor_levels_csv",
          "Level order (comma-separated):",
          placeholder = "e.g. Control, Case, Unknown"
        )
      ),
      actionButton("apply_types", "Apply types")
    )
  })

