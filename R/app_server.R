app_server_stage_runners <- function() {
  list(
    theme = mod_theme_server,
    wizard_step_control = mod_wizard_step_control_server,
    persistent_state = mod_persistent_state_server,
    data_handling = mod_data_handling_server,
    data_filtering = mod_data_filtering_server,
    options = mod_options_server,
    update_inputs = mod_update_inputs_server,
    navigation = mod_navigation_server,
    analysis_results = mod_analysis_results_server,
    save_key_inputs = mod_save_key_inputs_server
  )
}

app_server <- function(input, output, session) {
  app_ctx <- new.env(parent = environment())
  stage_runners <- app_server_stage_runners()
  session_storage <- app_session_storage(session, stale_hours = 24)

  run_stage <- function(stage_name) {
    stage_fun <- stage_runners[[stage_name]]
    stage_fun(input, output, session, app_ctx)
  }

  app_ctx$temp_root <- session_storage$temp_root
  app_ctx$session_temp_root <- session_storage$session_root
  app_ctx$session_heartbeat_path <- session_storage$heartbeat_path
  app_ctx$upload_dir <- session_storage$upload_dir
  app_ctx$builtins_dir <- session_storage$builtins_dir
  app_ctx$builtInList <- app_builtin_dataset_names()
  app_ctx$staged_files <- new.env(parent = emptyenv())
  shinyhelper::observe_helpers(session = session)

  shiny::observe({
    shiny::invalidateLater(5 * 60 * 1000, session)
    app_touch_file(app_ctx$session_heartbeat_path)
  })

  session$onSessionEnded(function() {
    app_delete_path(app_ctx$session_temp_root)
  })

  run_stage("theme")
  run_stage("wizard_step_control")
  run_stage("persistent_state")
  run_stage("data_handling")
  run_stage("data_filtering")
  run_stage("options")
  run_stage("navigation")
  run_stage("update_inputs")
  run_stage("analysis_results")
  run_stage("save_key_inputs")

  invisible(app_ctx)
}
