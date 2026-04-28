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

app_temp_root_name <- function() {
  "cytokineprofile-shiny"
}

app_temp_root <- function() {
  path <- file.path(tempdir(), app_temp_root_name())
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

app_temp_sessions_root <- function() {
  path <- file.path(app_temp_root(), "sessions")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

app_safe_temp_id <- function(x, default = "session") {
  if (is.null(x) || !length(x) || is.na(x[[1]])) {
    x <- ""
  }
  x <- as.character(x[[1]])
  x <- gsub("[^A-Za-z0-9_-]", "-", x)
  x <- gsub("-{2,}", "-", x)
  x <- gsub("^-|-$", "", x)

  if (!nzchar(x)) {
    return(default)
  }

  x
}

app_touch_file <- function(path, time = Sys.time()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(path)) {
    file.create(path, showWarnings = FALSE)
  }
  suppressWarnings(Sys.setFileTime(path, time))
  invisible(path)
}

app_delete_path <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    return(invisible(FALSE))
  }

  if (!file.exists(path) && !dir.exists(path)) {
    return(invisible(FALSE))
  }

  unlink(path, recursive = TRUE, force = TRUE)
  invisible(!file.exists(path) && !dir.exists(path))
}

app_cleanup_stale_session_dirs <- function(max_age_hours = 24) {
  sessions_root <- app_temp_sessions_root()
  session_dirs <- list.dirs(
    sessions_root,
    full.names = TRUE,
    recursive = FALSE
  )

  if (!length(session_dirs)) {
    return(invisible(character()))
  }

  cutoff <- Sys.time() - (as.numeric(max_age_hours) * 60 * 60)
  removed <- character(0)

  for (session_dir in session_dirs) {
    heartbeat_path <- file.path(session_dir, ".heartbeat")
    reference_path <- if (file.exists(heartbeat_path)) {
      heartbeat_path
    } else {
      session_dir
    }

    info <- file.info(reference_path)
    mtime <- info$mtime[[1]]
    if (is.na(mtime) || mtime >= cutoff) {
      next
    }

    if (isTRUE(app_delete_path(session_dir))) {
      removed <- c(removed, session_dir)
    }
  }

  invisible(removed)
}

app_session_temp_root <- function(session) {
  token <- tryCatch(session$token, error = function(...) "")
  if (!nzchar(token)) {
    token <- paste0("pid-", Sys.getpid())
  }

  path <- file.path(
    app_temp_sessions_root(),
    paste0("session-", app_safe_temp_id(token))
  )
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

app_named_session_temp_dir <- function(session_root, name) {
  path <- file.path(session_root, name)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

app_session_storage <- function(session, stale_hours = 24) {
  app_cleanup_stale_session_dirs(max_age_hours = stale_hours)

  session_root <- app_session_temp_root(session)
  heartbeat_path <- file.path(session_root, ".heartbeat")
  app_touch_file(heartbeat_path)

  list(
    temp_root = app_temp_root(),
    sessions_root = app_temp_sessions_root(),
    session_root = session_root,
    heartbeat_path = heartbeat_path,
    upload_dir = app_named_session_temp_dir(session_root, "uploads"),
    builtins_dir = app_named_session_temp_dir(session_root, "builtins")
  )
}

app_staged_slot_key <- function(slot_id) {
  app_safe_temp_id(slot_id, default = "slot")
}

app_staged_meta_key <- function(slot_id, field) {
  paste0(app_staged_slot_key(slot_id), "__", app_safe_temp_id(field, "meta"))
}

app_staged_path <- function(registry_env, slot_id) {
  key <- app_staged_slot_key(slot_id)
  if (!exists(key, envir = registry_env, inherits = FALSE)) {
    return(NULL)
  }

  base::get(key, envir = registry_env, inherits = FALSE)
}

app_staged_source_id <- function(registry_env, slot_id) {
  key <- app_staged_meta_key(slot_id, "source")
  if (!exists(key, envir = registry_env, inherits = FALSE)) {
    return(NULL)
  }

  base::get(key, envir = registry_env, inherits = FALSE)
}

app_clear_staged_path <- function(registry_env, slot_id) {
  key <- app_staged_slot_key(slot_id)
  meta_key <- app_staged_meta_key(slot_id, "source")
  existing <- app_staged_path(registry_env, slot_id)

  if (exists(key, envir = registry_env, inherits = FALSE)) {
    rm(list = key, envir = registry_env)
  }
  if (exists(meta_key, envir = registry_env, inherits = FALSE)) {
    rm(list = meta_key, envir = registry_env)
  }

  if (!is.null(existing) && nzchar(existing)) {
    app_delete_path(existing)
  }

  invisible(NULL)
}

app_stage_uploaded_file <- function(
  source_path,
  target_dir,
  slot_id,
  registry_env,
  original_name = NULL,
  source_id = NULL
) {
  if (is.null(source_id)) {
    source_name <- if (is.null(original_name)) "" else as.character(original_name)
    source_id <- paste(source_path, source_name, sep = "::")
  }

  existing_path <- app_staged_path(registry_env, slot_id)
  existing_source_id <- app_staged_source_id(registry_env, slot_id)
  if (
    !is.null(existing_path) &&
      nzchar(existing_path) &&
      file.exists(existing_path) &&
      identical(existing_source_id, source_id)
  ) {
    return(existing_path)
  }

  fileext <- ""
  if (!is.null(original_name) && nzchar(original_name)) {
    ext <- tools::file_ext(original_name)
    if (nzchar(ext)) {
      fileext <- paste0(".", tolower(ext))
    }
  }

  dest <- tempfile(
    pattern = paste0(app_staged_slot_key(slot_id), "-"),
    tmpdir = target_dir,
    fileext = fileext
  )

  copied <- isTRUE(file.copy(source_path, dest, overwrite = TRUE))
  if (!copied) {
    app_delete_path(dest)
    stop("Failed to stage uploaded file.", call. = FALSE)
  }

  app_clear_staged_path(registry_env, slot_id)
  assign(app_staged_slot_key(slot_id), dest, envir = registry_env)
  assign(app_staged_meta_key(slot_id, "source"), source_id, envir = registry_env)
  dest
}

app_stage_rds_object <- function(
  object,
  target_dir,
  slot_id,
  registry_env,
  source_id = NULL
) {
  if (is.null(source_id)) {
    source_id <- slot_id
  }

  existing_path <- app_staged_path(registry_env, slot_id)
  existing_source_id <- app_staged_source_id(registry_env, slot_id)
  if (
    !is.null(existing_path) &&
      nzchar(existing_path) &&
      file.exists(existing_path) &&
      identical(existing_source_id, source_id)
  ) {
    return(existing_path)
  }

  dest <- tempfile(
    pattern = paste0(app_staged_slot_key(slot_id), "-"),
    tmpdir = target_dir,
    fileext = ".rds"
  )

  saveRDS(object, dest)
  app_clear_staged_path(registry_env, slot_id)
  assign(app_staged_slot_key(slot_id), dest, envir = registry_env)
  assign(app_staged_meta_key(slot_id, "source"), source_id, envir = registry_env)
  dest
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
  options(shiny.sanitize.errors = TRUE)

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
