local_mocked_browser_side_effects <- function() {
  mock_env <- parent.frame()

  testthat::local_mocked_bindings(
    runjs = function(...) invisible(NULL),
    show = function(...) invisible(NULL),
    hide = function(...) invisible(NULL),
    toggle = function(...) invisible(NULL),
    addClass = function(...) invisible(NULL),
    removeClass = function(...) invisible(NULL),
    .env = mock_env,
    .package = "shinyjs"
  )
  testthat::local_mocked_bindings(
    feedbackWarning = function(...) invisible(NULL),
    hideFeedback = function(...) invisible(NULL),
    .env = mock_env,
    .package = "shinyFeedback"
  )
  testthat::local_mocked_bindings(
    showNotification = function(...) invisible(NULL),
    showModal = function(...) invisible(NULL),
    removeModal = function(...) invisible(NULL),
    debounce = function(
      r,
      millis,
      priority = 100,
      domain = shiny::getDefaultReactiveDomain()
    ) {
      r
    },
    .env = mock_env,
    .package = "shiny"
  )
  testthat::local_mocked_bindings(
    observe_helpers = function(...) invisible(NULL),
    .env = mock_env,
    .package = "shinyhelper"
  )
  testthat::local_mocked_bindings(
    app_server_stage_runners = function() {
      runners <- real_app_server_stage_runners()
      runners$navigation <- test_navigation_server
      runners
    },
    .env = mock_env,
    .package = "CytokineProfileShinyApp"
  )

  invisible(NULL)
}

test_server_flush <- function(session) {
  if (is.function(session$flushReact)) {
    session$flushReact()
  } else {
    shiny::flushReact()
  }

  invisible(NULL)
}

test_server_wait <- function(session, seconds = 0.6, cycles = 4L) {
  for (i in seq_len(cycles)) {
    Sys.sleep(seconds)
    test_server_flush(session)
  }
  invisible(NULL)
}

wait_for_analysis_settlement <- function(
  session,
  app_ctx,
  seconds = 0.05,
  cycles = 20L
) {
  for (i in seq_len(cycles)) {
    test_server_flush(session)

    if (is.function(app_ctx$analysisResult)) {
      tryCatch(
        app_ctx$analysisResult(),
        error = function(e) invisible(NULL)
      )
      test_server_flush(session)
    }

    step_now <- app_ctx$currentStep()
    page_now <- app_ctx$currentPage()
    err_now <- app_ctx$errorMessage()

    if (identical(step_now, 5) && identical(page_now, "step5")) {
      return(invisible("success"))
    }

    if (
      identical(step_now, 4) &&
        identical(page_now, "step4") &&
        shiny::isTruthy(err_now)
    ) {
      return(invisible("failure"))
    }

    Sys.sleep(seconds)
  }

  step_now <- app_ctx$currentStep()
  page_now <- app_ctx$currentPage()
  err_now <- app_ctx$errorMessage()
  testthat::fail(sprintf(
    paste(
      "Timed out waiting for analysis to settle after clicking next4.",
      "currentStep=%s, currentPage=%s, errorPresent=%s"
    ),
    as.character(step_now %||% NA),
    as.character(page_now %||% NA),
    if (shiny::isTruthy(err_now)) "TRUE" else "FALSE"
  ))
}

test_server_output_html <- function(output, id) {
  htmltools::renderTags(output[[id]])$html
}

local_capture_shiny_updates <- function() {
  mock_env <- parent.frame()
  calls <- list()

  record_update <- function(fun_name) {
    force(fun_name)

    function(session, inputId, ...) {
      calls[[length(calls) + 1L]] <<- c(
        list(fun = fun_name, inputId = inputId),
        list(...)
      )
      invisible(NULL)
    }
  }

  testthat::local_mocked_bindings(
    updateCheckboxInput = record_update("updateCheckboxInput"),
    updateCheckboxGroupInput = record_update("updateCheckboxGroupInput"),
    updateSelectInput = record_update("updateSelectInput"),
    updateSelectizeInput = record_update("updateSelectizeInput"),
    updateRadioButtons = record_update("updateRadioButtons"),
    updateTextInput = record_update("updateTextInput"),
    updateNumericInput = record_update("updateNumericInput"),
    updateSliderInput = record_update("updateSliderInput"),
    .env = mock_env,
    .package = "shiny"
  )

  filter_calls <- function(fun = NULL, inputId = NULL) {
    Filter(
      function(call) {
        (is.null(fun) || identical(call$fun, fun)) &&
          (is.null(inputId) || identical(call$inputId, inputId))
      },
      calls
    )
  }

  list(
    calls = function() calls,
    clear = function() {
      calls <<- list()
      invisible(NULL)
    },
    find = filter_calls,
    last = function(fun = NULL, inputId = NULL) {
      matches <- filter_calls(fun = fun, inputId = inputId)
      if (!length(matches)) {
        return(NULL)
      }

      matches[[length(matches)]]
    }
  )
}

capture_session_input_messages <- function(session) {
  calls <- list()

  session$sendInputMessage <- function(inputId, message) {
    calls[[length(calls) + 1L]] <<- list(inputId = inputId, message = message)
    invisible(NULL)
  }

  filter_calls <- function(inputId = NULL) {
    Filter(
      function(call) {
        is.null(inputId) || identical(call$inputId, inputId)
      },
      calls
    )
  }

  list(
    calls = function() calls,
    clear = function() {
      calls <<- list()
      invisible(NULL)
    },
    find = filter_calls,
    last = function(inputId = NULL) {
      matches <- filter_calls(inputId = inputId)
      if (!length(matches)) {
        return(NULL)
      }

      matches[[length(matches)]]
    }
  )
}

set_test_input <- function(session, id, value) {
  do.call(session$setInputs, stats::setNames(list(value), id))
  test_server_flush(session)
  invisible(NULL)
}

click_test_input <- function(session, id) {
  current <- tryCatch(session$input[[id]], error = function(e) NULL)
  if (is.null(current)) {
    current <- 0
  }
  set_test_input(session, id, current + 1)
}

app_server <- getFromNamespace("app_server", "CytokineProfileShinyApp")
app_server_stage_runners <- getFromNamespace(
  "app_server_stage_runners",
  "CytokineProfileShinyApp"
)
real_app_server_stage_runners <- app_server_stage_runners
mod_navigation_server <- getFromNamespace(
  "mod_navigation_server",
  "CytokineProfileShinyApp"
)
real_mod_navigation_server <- mod_navigation_server
mod_update_inputs_server <- getFromNamespace(
  "mod_update_inputs_server",
  "CytokineProfileShinyApp"
)
mod_options_server <- getFromNamespace(
  "mod_options_server",
  "CytokineProfileShinyApp"
)
apply_scale <- getFromNamespace(
  "apply_scale",
  "CytokineProfileShinyApp"
)

new_test_app_ctx <- function() {
  new.env(parent = environment(app_server))
}
wrap_server_with_app_ctx <- function(server_fun, app_ctx) {
  force(server_fun)
  force(app_ctx)

  function(input, output, session) {
    server_fun(input, output, session, app_ctx)
  }
}

prepare_app_server_step4 <- function(session, app_ctx, func_name) {
  set_test_input(session, "theme_choice", "flatly")
  prepare_app_server_step3(session, app_ctx = app_ctx)
  app_ctx$selected_function(func_name)
  app_ctx$userState$selected_function <- func_name
  app_ctx$currentPage("step4")
  app_ctx$currentStep(4)
  test_server_flush(session)
  invisible(NULL)
}

enter_app_workflow <- function(session) {
  click_test_input(session, "nav_start_home")
  invisible(NULL)
}

example1_test_categorical_cols <- function() {
  c("Group", "Time")
}

example1_test_numerical_cols <- function() {
  setdiff(names(ex1_full), c(names(ex1_full)[1:3], "Time"))
}

materialize_test_filtered_data <- function(app_ctx, inputs) {
  df <- app_ctx$step2_typed_data()
  col_info <- app_ctx$step2_typed_col_info()
  filter_cols <- intersect(
    inputs$selected_categorical_cols %||% character(0),
    col_info$categorical
  )

  if (length(filter_cols)) {
    for (col in filter_cols) {
      filter_id <- paste0("filter_", col)
      selected_levels <- inputs[[filter_id]]
      if (!is.null(selected_levels)) {
        df <- df[df[[col]] %in% selected_levels, , drop = FALSE]
      }
    }
  }

  scale_choice <- inputs$step2_scale %||% "none"
  if (!identical(scale_choice, "none")) {
    num_cols <- intersect(
      inputs$selected_numerical_cols %||% character(0),
      col_info$numerical
    )
    if (length(num_cols)) {
      df <- apply_scale(
        data = df,
        columns = num_cols,
        scale = scale_choice
      )
    }
  }

  df
}

stabilize_test_step3_state <- function(app_ctx, inputs) {
  app_ctx$data_after_filters <- shiny::reactive({
    materialize_test_filtered_data(app_ctx, inputs)
  })
  app_ctx$data_after_imputation <- shiny::reactive({
    imputed <- app_ctx$imputed_data()
    if (!is.null(imputed)) {
      return(imputed)
    }
    app_ctx$data_after_filters()
  })
  app_ctx$filteredData <- shiny::reactive({
    df <- app_ctx$data_after_imputation()
    selected_cols <- unique(c(
      inputs$selected_categorical_cols %||% character(0),
      inputs$selected_numerical_cols %||% character(0)
    ))
    cols_to_keep <- if (app_ctx$currentStep() >= 3) {
      app_ctx$userState$selected_columns %||% selected_cols
    } else {
      selected_cols
    }
    final_cols <- union(cols_to_keep, "..cyto_id..")
    df[, intersect(names(df), final_cols), drop = FALSE]
  })

  invisible(NULL)
}

test_navigation_server <- function(input, output, session, app_ctx) {
  real_mod_navigation_server(input, output, session, app_ctx)
  stabilize_test_step3_state(app_ctx, session$input)
  invisible(app_ctx)
}

prepare_app_server_step2 <- function(session) {
  enter_app_workflow(session)
  set_test_input(session, "use_builtin", TRUE)
  set_test_input(session, "built_in_choice", "ExampleData1")
  click_test_input(session, "next1")

  set_test_input(
    session,
    "selected_categorical_cols",
    example1_test_categorical_cols()
  )
  set_test_input(
    session,
    "selected_numerical_cols",
    example1_test_numerical_cols()
  )
  set_test_input(session, "factor_cols", "Time")
  click_test_input(session, "apply_types")

  invisible(NULL)
}

prepare_app_server_step3 <- function(
  session,
  app_ctx = NULL,
  apply_filters = FALSE,
  scale_choice = "none"
) {
  prepare_app_server_step2(session)

  if (!identical(scale_choice, "none")) {
    set_test_input(session, "step2_scale", scale_choice)
  }

  if (apply_filters) {
    set_test_input(session, "filter_Time", c("20", "72"))
    set_test_input(session, "filter_Group", c("PreT2D", "T2D"))
  }

  click_test_input(session, "next2")
  invisible(NULL)
}

run_app_server_analysis <- function(
  session,
  app_ctx,
  menu_id,
  inputs = list(),
  apply_filters = FALSE,
  scale_choice = "none"
) {
  prepare_app_server_step3(
    session,
    app_ctx = app_ctx,
    apply_filters = apply_filters,
    scale_choice = scale_choice
  )
  click_test_input(session, menu_id)

  if (length(inputs)) {
    for (nm in names(inputs)) {
      set_test_input(session, nm, inputs[[nm]])
    }
  }

  click_test_input(session, "next4")
  invisible(wait_for_analysis_settlement(session, app_ctx))
}

expect_analysis_success <- function(app_ctx, session = NULL) {
  if (!is.null(session)) {
    wait_for_analysis_settlement(session, app_ctx)
  }
  testthat::expect_null(app_ctx$errorMessage())
  testthat::expect_equal(app_ctx$currentStep(), 5)
  testthat::expect_equal(app_ctx$currentPage(), "step5")
}
