app_source_runtime_env <- function(source_root = getwd(), env = parent.frame()) {
  options(
    cytokineprofile.app_source_root = normalizePath(
      source_root,
      winslash = "/",
      mustWork = TRUE
    )
  )

  if (
    exists("app_ui", envir = env, inherits = FALSE) &&
      exists("app_server", envir = env, inherits = FALSE)
  ) {
    return(env)
  }

  assign("%||%", getFromNamespace("%||%", "rlang"), envir = env)
  assign(".data", getFromNamespace(".data", "rlang"), envir = env)

  r_files <- sort(list.files(
    file.path(source_root, "R"),
    full.names = TRUE,
    pattern = "\\.R$"
  ))
  invisible(lapply(r_files, source, local = env))

  env
}

app_env <- app_source_runtime_env()
ui <- base::get("app_ui", envir = app_env)()
server <- base::get("app_server", envir = app_env)

shiny::shinyApp(ui = ui, server = server)
