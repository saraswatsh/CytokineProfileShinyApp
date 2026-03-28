mod_step5_results_ui <- function(
  id = NULL,
  ns = NULL,
  selected_function = NULL
) {
  ns <- app_resolve_ns(id = id, ns = ns)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::uiOutput(ns("result_display"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::uiOutput(ns("download_ui"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          style = "display:flex; align-items:center; margin-top:1rem;",
          shiny::actionButton(
            ns("back5"),
            "Back",
            icon = shiny::icon("arrow-left"),
            class = "btn-secondary"
          ),
          shiny::div(
            style = "margin-left:auto; display:flex; align-items:center; gap:.5rem;",
            shiny::actionButton(
              ns("new_fresh"),
              "Start New (fresh)",
              icon = shiny::icon("play"),
              class = "btn-primary"
            ),
            shiny::actionButton(
              ns("new_reuse"),
              "Start New (reuse data)",
              icon = shiny::icon("repeat"),
              class = "btn-secondary"
            )
          )
        )
      )
    )
  )
}
