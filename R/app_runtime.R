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

app_config_path <- function() {
  config_file <- app_installed_file("config.yml")
  if (nzchar(config_file)) {
    return(config_file)
  }
  file.path(app_source_root(), "config.yml")
}

app_config <- function() {
  config::get(file = app_config_path())
}

app_www_dir <- function() {
  installed_dir <- app_installed_file("www")
  if (nzchar(installed_dir)) {
    return(installed_dir)
  }

  source_candidates <- file.path(app_source_root(), c("www", "WWW"))
  source_dir <- source_candidates[dir.exists(source_candidates)][1]
  if (is.na(source_dir) || !nzchar(source_dir)) {
    stop("Could not find the app asset directory.")
  }
  source_dir
}

app_server_logic_file <- function(filename) {
  logic_file <- app_installed_file("server-logic", filename)
  if (nzchar(logic_file)) {
    return(logic_file)
  }

  logic_file <- file.path(app_source_root(), "server-logic", filename)
  if (!file.exists(logic_file)) {
    stop("Could not find server logic file: ", filename)
  }
  logic_file
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

app_logic_parent_env <- function() {
  if (app_using_source_root()) {
    return(globalenv())
  }
  if (app_namespace_loaded()) {
    return(asNamespace(app_package_name()))
  }
  globalenv()
}

app_logic_exec <- function(filename, input, output, session, app_ctx) {
  parent_env <- app_logic_parent_env()
  if (!identical(parent.env(app_ctx), parent_env)) {
    parent.env(app_ctx) <- parent_env
  }

  # Keep app_ctx as the shared lexical parent so closures created in one
  # server-logic stage can resolve bindings added by later stages.
  logic_env <- new.env(parent = app_ctx)

  assign("input", input, envir = logic_env)
  assign("output", output, envir = logic_env)
  assign("session", session, envir = logic_env)

  sys.source(app_server_logic_file(filename), envir = logic_env, keep.source = TRUE)

  logic_names <- setdiff(ls(logic_env, all.names = TRUE), c("input", "output", "session"))
  for (nm in logic_names) {
    assign(nm, base::get(nm, envir = logic_env, inherits = FALSE), envir = app_ctx)
  }

  invisible(app_ctx)
}

app_session_temp_dir <- function(name) {
  path <- file.path(tempdir(), name)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}
