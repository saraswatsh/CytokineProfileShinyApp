app_package_name <- function() {
  "CytokineProfileShinyApp"
}

app_namespace_loaded <- function() {
  app_package_name() %in% loadedNamespaces()
}

app_installed_file <- function(...) {
  system.file("app", ..., package = app_package_name())
}

app_using_source_root <- function() {
  nzchar(getOption("cytokineprofile.app_source_root", default = ""))
}

app_source_root <- function() {
  root <- getOption("cytokineprofile.app_source_root", default = "")
  if (nzchar(root)) {
    return(normalizePath(root, winslash = "/", mustWork = FALSE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

app_source_app_file <- function(...) {
  file.path(app_source_root(), "inst", "app", ...)
}

app_content_file <- function(...) {
  rel_path <- file.path(...)
  candidates <- unique(c(
    file.path(app_source_root(), rel_path),
    file.path(getwd(), rel_path),
    app_source_app_file(rel_path)
  ))

  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop("Could not locate app content file: ", rel_path)
}

app_config_path <- function() {
  config_file <- app_installed_file("config.yml")
  if (nzchar(config_file)) {
    return(config_file)
  }

  config_file <- app_source_app_file("config.yml")
  if (!file.exists(config_file)) {
    stop("Could not find the app config file.")
  }
  config_file
}

app_config <- function() {
  config::get(file = app_config_path())
}

app_www_dir <- function() {
  installed_dir <- app_installed_file("www")
  if (nzchar(installed_dir)) {
    return(installed_dir)
  }

  source_dir <- app_source_app_file("www")
  if (!dir.exists(source_dir)) {
    stop("Could not find the app asset directory.")
  }
  source_dir
}

app_resource_prefix <- function() {
  "app-www"
}

app_register_resources <- function() {
  prefix <- app_resource_prefix()
  resource_paths <- shiny::resourcePaths()
  if (!prefix %in% names(resource_paths)) {
    shiny::addResourcePath(prefix, normalizePath(app_www_dir(), winslash = "/"))
  }
  invisible(prefix)
}

app_asset_href <- function(filename) {
  gsub("\\\\", "/", file.path(app_resource_prefix(), filename))
}

app_builtin_dataset_names <- function() {
  c(
    "ExampleData1",
    "ExampleData2",
    "ExampleData3",
    "ExampleData4",
    "ExampleData5"
  )
}

app_builtin_dataset <- function(name) {
  if (!name %in% app_builtin_dataset_names()) {
    stop("Unknown built-in dataset: ", name)
  }

  data_env <- new.env(parent = emptyenv())

  if (app_namespace_loaded() || nzchar(system.file(package = app_package_name()))) {
    utils::data(list = name, package = app_package_name(), envir = data_env)
    if (exists(name, envir = data_env, inherits = FALSE)) {
      return(base::get(name, envir = data_env, inherits = FALSE))
    }
  }

  data_file <- file.path(app_source_root(), "data", paste0(name, ".rda"))
  if (!file.exists(data_file)) {
    stop("Could not locate built-in dataset file for ", name)
  }

  load(data_file, envir = data_env)
  base::get(name, envir = data_env, inherits = FALSE)
}

app_description_field <- function(field, default = NULL) {
  if (app_namespace_loaded()) {
    desc <- tryCatch(
      utils::packageDescription(app_package_name()),
      error = function(...) NULL
    )
    if (!is.null(desc) && !is.null(desc[[field]]) && !is.na(desc[[field]])) {
      return(desc[[field]])
    }
  }

  desc_file <- file.path(app_source_root(), "DESCRIPTION")
  if (!file.exists(desc_file)) {
    return(default)
  }

  desc <- read.dcf(desc_file)
  if (!field %in% colnames(desc)) {
    return(default)
  }

  value <- desc[1, field]
  if (is.na(value) || !nzchar(value)) {
    return(default)
  }
  value
}

app_version_string <- function() {
  as.character(app_description_field("Version", default = "0.0.0.9000"))
}

app_session_temp_dir <- function(name) {
  path <- file.path(tempdir(), name)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

app_source_checkout_root <- function(app_dir = getwd()) {
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = FALSE)
  candidates <- unique(c(
    app_dir,
    normalizePath(file.path(app_dir, "..", ".."), winslash = "/", mustWork = FALSE)
  ))

  is_source_root <- function(root) {
    file.exists(file.path(root, "DESCRIPTION")) &&
      dir.exists(file.path(root, "R")) &&
      dir.exists(file.path(root, "inst", "app"))
  }

  for (candidate in candidates) {
    if (is_source_root(candidate)) {
      return(candidate)
    }
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

app_runtime_env <- function(app_dir = getwd()) {
  source_root <- app_source_checkout_root(app_dir = app_dir)
  if (nzchar(source_root)) {
    return(app_source_runtime_env(source_root))
  }

  if (!requireNamespace(app_package_name(), quietly = TRUE)) {
    stop("Could not load the CytokineProfileShinyApp package namespace.")
  }

  asNamespace(app_package_name())
}

app_runtime_components <- function(app_dir = getwd()) {
  app_env <- app_runtime_env(app_dir = app_dir)

  list(
    app_env = app_env,
    ui = base::get("app_ui", envir = app_env)(),
    server = base::get("app_server", envir = app_env)
  )
}

app_build_shiny_app <- function(app_dir = getwd()) {
  components <- app_runtime_components(app_dir = app_dir)
  shiny::shinyApp(ui = components$ui, server = components$server)
}
