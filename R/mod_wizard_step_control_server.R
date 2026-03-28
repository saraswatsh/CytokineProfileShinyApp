mod_wizard_step_control_server <- function(input, output, session, app_ctx) {
  stage_env <- app_stage_init(app_ctx)
  ## ---------------------------
  ## Wizard Step Control
  ## ---------------------------
  currentStep <- shiny::reactiveVal(1)
  output$currentStep <- shiny::reactive({
    currentStep()
  })
  shiny::outputOptions(output, "currentStep", suspendWhenHidden = FALSE)

  invisible(app_stage_commit(app_ctx, stage_env))
}
