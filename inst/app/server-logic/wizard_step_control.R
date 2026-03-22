  ## ---------------------------
  ## Wizard Step Control
  ## ---------------------------
  currentStep <- shiny::reactiveVal(1)
  output$currentStep <- shiny::reactive({
    currentStep()
  })
  shiny::outputOptions(output, "currentStep", suspendWhenHidden = FALSE)

