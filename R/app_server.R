app_session_temp_dir <- function(name) {
  path <- file.path(tempdir(), name)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

init_theme_server <- function(input, output, session, app_ctx) {
  app_logic_exec("theme_toggle.R", input, output, session, app_ctx)
}

init_wizard_step_control_server <- function(input, output, session, app_ctx) {
  app_logic_exec("wizard_step_control.R", input, output, session, app_ctx)
}

init_persistent_state_server <- function(input, output, session, app_ctx) {
  app_logic_exec("persistent_state.R", input, output, session, app_ctx)
}

init_data_handling_server <- function(input, output, session, app_ctx) {
  app_logic_exec("data_handling.R", input, output, session, app_ctx)
}

init_data_filtering_server <- function(input, output, session, app_ctx) {
  app_logic_exec("data_filtering.R", input, output, session, app_ctx)
}

init_options_server <- function(input, output, session, app_ctx) {
  app_logic_exec("options_ui.R", input, output, session, app_ctx)
}

init_navigation_server <- function(input, output, session, app_ctx) {
  app_logic_exec("navigation.R", input, output, session, app_ctx)
}

init_update_inputs_server <- function(input, output, session, app_ctx) {
  app_logic_exec("update_inputs.R", input, output, session, app_ctx)
}

init_analysis_results_server <- function(input, output, session, app_ctx) {
  app_logic_exec("analysis_results.R", input, output, session, app_ctx)
}

init_save_key_inputs_server <- function(input, output, session, app_ctx) {
  app_logic_exec("save_key_inputs.R", input, output, session, app_ctx)
}

app_server <- function(input, output, session) {
  app_ctx <- new.env(parent = emptyenv())
  app_ctx$upload_dir <- app_session_temp_dir("uploads")
  app_ctx$builtins_dir <- app_session_temp_dir("builtins")
  app_ctx$builtInList <- app_builtin_dataset_names()

  shinyhelper::observe_helpers()
  session$userData$stored_theme <- NULL

  init_theme_server(input, output, session, app_ctx)
  init_wizard_step_control_server(input, output, session, app_ctx)
  init_persistent_state_server(input, output, session, app_ctx)
  init_data_handling_server(input, output, session, app_ctx)
  init_data_filtering_server(input, output, session, app_ctx)
  init_options_server(input, output, session, app_ctx)
  init_navigation_server(input, output, session, app_ctx)
  init_update_inputs_server(input, output, session, app_ctx)
  init_analysis_results_server(input, output, session, app_ctx)
  init_save_key_inputs_server(input, output, session, app_ctx)
}
