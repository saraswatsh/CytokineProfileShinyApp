app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
runtime_file <- normalizePath(
  file.path(app_dir, "..", "..", "R", "app_runtime.R"),
  winslash = "/",
  mustWork = FALSE
)

if (file.exists(runtime_file)) {
  source(runtime_file, local = TRUE)
  components <- app_runtime_components(app_dir = app_dir)
} else {
  app_namespace <- asNamespace("CytokineProfileShinyApp")
  components <- base::get(
    "app_runtime_components",
    envir = app_namespace
  )(app_dir = app_dir)
}

shiny::shinyApp(ui = components$ui, server = components$server)
