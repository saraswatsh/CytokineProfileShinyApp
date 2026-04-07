app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
runtime_file <- normalizePath(
  file.path(app_dir, "R", "app_runtime.R"),
  winslash = "/",
  mustWork = FALSE
)

if (!file.exists(runtime_file)) {
  stop("Could not locate R/app_runtime.R from the repo-root app launcher.")
}

source(runtime_file, local = TRUE)
components <- app_runtime_components(app_dir = app_dir)
shiny::shinyApp(ui = components$ui, server = components$server)
