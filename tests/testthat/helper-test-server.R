local_mocked_browser_side_effects <- function() {
  testthat::local_mocked_bindings(
    runjs = function(...) invisible(NULL),
    show = function(...) invisible(NULL),
    hide = function(...) invisible(NULL),
    toggle = function(...) invisible(NULL),
    addClass = function(...) invisible(NULL),
    removeClass = function(...) invisible(NULL),
    .package = "shinyjs"
  )
  testthat::local_mocked_bindings(
    feedbackWarning = function(...) invisible(NULL),
    hideFeedback = function(...) invisible(NULL),
    .package = "shinyFeedback"
  )
  testthat::local_mocked_bindings(
    showNotification = function(...) invisible(NULL),
    showModal = function(...) invisible(NULL),
    removeModal = function(...) invisible(NULL),
    debounce = function(r, millis, priority = 100, domain = shiny::getDefaultReactiveDomain()) r,
    .package = "shiny"
  )
  testthat::local_mocked_bindings(
    observe_helpers = function(...) invisible(NULL),
    .package = "shinyhelper"
  )
  testthat::local_mocked_bindings(
    app_server_stage_runners = function() {
      runners <- real_app_server_stage_runners()
      runners$navigation <- test_navigation_server
      runners
    },
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

new_test_app_ctx <- function() {
  new.env(parent = environment(CytokineProfileShinyApp:::app_server))
}

real_app_server_stage_runners <- CytokineProfileShinyApp:::app_server_stage_runners
real_mod_navigation_server <- CytokineProfileShinyApp:::mod_navigation_server

wrap_server_with_app_ctx <- function(server_fun, app_ctx) {
  force(server_fun)
  force(app_ctx)

  function(input, output, session) {
    server_fun(input, output, session, app_ctx)
  }
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
      df <- CytokineProfileShinyApp:::apply_scale(
        data = df,
        columns = num_cols,
        scale = scale_choice
      )
    }
  }

  df
}

stabilize_test_step3_state <- function(app_ctx, inputs) {
  data_after_filters <- materialize_test_filtered_data(app_ctx, inputs)

  app_ctx$data_after_filters <- shiny::reactive({
    data_after_filters
  })
  app_ctx$data_after_imputation <- shiny::reactive({
    imputed <- app_ctx$imputed_data()
    if (!is.null(imputed)) {
      return(imputed)
    }
    data_after_filters
  })
  app_ctx$filteredData <- shiny::reactive({
    df <- app_ctx$data_after_imputation()
    cols_to_keep <- if (app_ctx$currentStep() >= 3) {
      app_ctx$userState$selected_columns %||% character(0)
    } else {
      unique(c(
        inputs$selected_categorical_cols %||% character(0),
        inputs$selected_numerical_cols %||% character(0)
      ))
    }
    final_cols <- union(cols_to_keep, "..cyto_id..")
    df[, intersect(names(df), final_cols), drop = FALSE]
  })

  invisible(NULL)
}

test_navigation_server <- function(input, output, session, app_ctx) {
  real_mod_navigation_server(input, output, session, app_ctx)
  stabilize_test_step3_state(app_ctx, input)
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
  if (!is.null(app_ctx)) {
    stabilize_test_step3_state(app_ctx, session$input)
  }
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
  test_server_flush(session)
  invisible(NULL)
}

expect_analysis_success <- function(app_ctx, session = NULL) {
  if (!is.null(session)) {
    test_server_flush(session)
  }
  testthat::expect_null(app_ctx$errorMessage())
  testthat::expect_false(is.null(app_ctx$analysisResult()))
}
