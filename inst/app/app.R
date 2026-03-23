app_package_name <- function() {
  "CytokineProfileShinyApp"
}

app_source_checkout_root <- function(app_dir = getwd()) {
  candidate_root <- normalizePath(
    file.path(app_dir, "..", ".."),
    winslash = "/",
    mustWork = FALSE
  )

  if (
    file.exists(file.path(candidate_root, "DESCRIPTION")) &&
      dir.exists(file.path(candidate_root, "R"))
  ) {
    return(candidate_root)
  }

  ""
}

app_source_runtime_env <- function(source_root) {
  options(
    cytokineprofile.app_source_root = normalizePath(
      source_root,
      winslash = "/",
      mustWork = TRUE
    )
  )

  app_env <- new.env(parent = globalenv())
  assign("%||%", getFromNamespace("%||%", "rlang"), envir = app_env)
  assign(".data", getFromNamespace(".data", "rlang"), envir = app_env)

  r_files <- sort(list.files(
    file.path(source_root, "R"),
    full.names = TRUE,
    pattern = "\\.R$"
  ))
  invisible(lapply(r_files, source, local = app_env))

  app_env
}

app_runtime_env <- function() {
  source_root <- app_source_checkout_root()
  if (nzchar(source_root)) {
    return(app_source_runtime_env(source_root))
  }

  if (!requireNamespace(app_package_name(), quietly = TRUE)) {
    stop("Could not load the CytokineProfileShinyApp package namespace.")
  }

  asNamespace(app_package_name())
}

app_env <- app_runtime_env()
ui <- base::get("app_ui", envir = app_env)()
server <- base::get("app_server", envir = app_env)

shiny::shinyApp(ui = ui, server = server)
