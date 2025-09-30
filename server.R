# Define server logic
server <- function(input, output, session) {
  # Creating a temp dir for data uploads
  upload_dir <- file.path(tempdir(), "uploads")
  dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)
  builtins_dir <- file.path(tempdir(), "builtins")
  dir.create(builtins_dir, recursive = TRUE, showWarnings = FALSE)

  # Helpers
  shinyhelper::observe_helpers()
  shinyFeedback::useShinyFeedback()
  source("server-logic/theme_toggle.R", local = TRUE)
  source("server-logic/wizard_step_control.R", local = TRUE)
  source("server-logic/persistent_state.R", local = TRUE)
  source("server-logic/data_handling.R", local = TRUE)
  source("server-logic/data_filtering.R", local = TRUE)
  source("server-logic/options_ui.R", local = TRUE)
  source("server-logic/navigation.R", local = TRUE)
  source("server-logic/update_inputs.R", local = TRUE)
  source("server-logic/analysis_results.R", local = TRUE)
  source("server-logic/save_key_inputs.R", local = TRUE)
}
