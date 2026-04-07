app_resolve_ns <- function(id = NULL, ns = NULL) {
  if (is.function(ns)) {
    return(ns)
  }

  if (is.null(id) || !nzchar(id)) {
    return(function(x) x)
  }

  shiny::NS(id)
}

app_step_title_text <- function(step, selected_function = NULL) {
  switch(
    as.character(step),
    "1" = "Step 1: Upload Data",
    "2" = "Step 2: Select Columns & Apply Filters",
    "3" = "Step 3: Analysis Choices",
    "4" = paste0(
      "Step 4: Options for ",
      selected_function %||% "Selected Analysis"
    ),
    "5" = "Analysis Results",
    NULL
  )
}

app_step_title_ui <- function(step, selected_function = NULL) {
  title <- app_step_title_text(
    step = step,
    selected_function = selected_function
  )

  if (is.null(title)) {
    return(NULL)
  }

  shiny::div(
    class = "step-title",
    shiny::h3(title)
  )
}

app_step_progress_ui <- function(step, id = NULL, ns = NULL) {
  ns <- app_resolve_ns(id = id, ns = ns)
  total_steps <- 5
  pct <- if (step >= total_steps) {
    100
  } else {
    round((step - 1) / (total_steps - 1) * 100)
  }

  shiny::div(
    class = "progress-wrapper",
    shinyWidgets::progressBar(
      id = ns("wizard_pb"),
      value = pct,
      title = if (step >= total_steps) {
        "Finished!"
      } else {
        paste("Step", step, "of", total_steps)
      },
      display_pct = TRUE,
      striped = TRUE,
      size = "xs",
      status = "info"
    )
  )
}

app_step_header_ui <- function(
  step,
  selected_function = NULL,
  id = NULL,
  ns = NULL
) {
  shiny::tagList(
    app_step_title_ui(
      step = step,
      selected_function = selected_function
    ),
    app_step_progress_ui(
      step = step,
      id = id,
      ns = ns
    )
  )
}
