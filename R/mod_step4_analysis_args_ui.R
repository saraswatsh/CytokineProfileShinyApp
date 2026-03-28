mod_step4_analysis_args_ui <- function(
  id = NULL,
  ns = NULL,
  selected_function = NULL
) {
  ns <- app_resolve_ns(id = id, ns = ns)

  shiny::tagList(
    shiny::uiOutput(ns("function_options_ui")),
    shiny::div(
      style = "display: flex; justify-content: space-between; margin-top: 1rem;",
      shiny::actionButton(
        ns("back4"),
        "Back",
        icon = shiny::icon("arrow-left"),
        class = "btn-secondary"
      ),
      shiny::actionButton(
        ns("next4"),
        "Run Analysis",
        icon = shiny::icon("play"),
        class = "btn-success"
      )
    )
  )
}
