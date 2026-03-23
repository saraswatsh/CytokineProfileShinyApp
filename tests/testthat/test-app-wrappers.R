app_test_normalize_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

app_test_write_file <- function(path, lines = "") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
  path
}

app_test_render_html <- function(tag) {
  htmltools::renderTags(tag)$html
}

test_that("app runtime path helpers respect installed and source fallbacks", {
  source_root <- tempfile("app-source-")
  dir.create(source_root)
  app_test_write_file(file.path(source_root, "config.yml"), "default:\n  mode: source")
  dir.create(file.path(source_root, "WWW"), recursive = TRUE, showWarnings = FALSE)
  app_test_write_file(
    file.path(source_root, "server-logic", "logic.R"),
    "logic_flag <- TRUE"
  )
  source_root <- app_test_normalize_path(source_root)

  old_options <- options(cytokineprofile.app_source_root = source_root)
  on.exit(options(old_options), add = TRUE)

  expect_true(app_using_source_root())
  expect_equal(app_source_root(), source_root)

  testthat::local_mocked_bindings(
    app_installed_file = function(...) "",
    .package = "CytokineProfileShinyApp"
  )

  expect_equal(app_config_path(), file.path(source_root, "config.yml"))
  expect_equal(app_www_dir(), file.path(source_root, "www"))
  expect_equal(
    app_server_logic_file("logic.R"),
    file.path(source_root, "server-logic", "logic.R")
  )
})

test_that("app runtime helpers cover installed-file branches and missing-source errors", {
  old_options <- options(cytokineprofile.app_source_root = "")
  on.exit(options(old_options), add = TRUE)

  expect_false(app_using_source_root())
  expect_equal(app_source_root(), app_test_normalize_path(getwd()))

  testthat::local_mocked_bindings(
    app_package_name = function() "DefinitelyMissingPackage",
    .package = "CytokineProfileShinyApp"
  )
  expect_equal(app_installed_file("config.yml"), "")
})

test_that("app config, asset, and logic helpers use installed files when present", {
  captured_config_path <- NULL

  testthat::local_mocked_bindings(
    app_installed_file = function(...) file.path("C:/installed/app", ...),
    .package = "CytokineProfileShinyApp"
  )
  testthat::local_mocked_bindings(
    get = function(file, ...) {
      captured_config_path <<- file
      list(mode = "installed")
    },
    .package = "config"
  )

  expect_equal(app_config_path(), "C:/installed/app/config.yml")
  expect_equal(app_config(), list(mode = "installed"))
  expect_equal(captured_config_path, "C:/installed/app/config.yml")
  expect_equal(app_www_dir(), "C:/installed/app/www")
  expect_equal(
    app_server_logic_file("theme_toggle.R"),
    "C:/installed/app/server-logic/theme_toggle.R"
  )
  expect_match(app_asset_href("logo.png"), "^app-www/logo\\.png$")
})

test_that("app runtime helpers error cleanly when source assets are missing", {
  source_root <- tempfile("app-source-missing-")
  dir.create(source_root)

  old_options <- options(cytokineprofile.app_source_root = source_root)
  on.exit(options(old_options), add = TRUE)

  testthat::local_mocked_bindings(
    app_installed_file = function(...) "",
    .package = "CytokineProfileShinyApp"
  )

  expect_error(app_www_dir(), "Could not find the app asset directory.")
  expect_error(
    app_server_logic_file("missing_logic.R"),
    "Could not find server logic file: missing_logic.R"
  )
})

test_that("app description helpers prefer package metadata then source files", {
  source_root <- tempfile("app-description-")
  dir.create(source_root)
  desc_path <- file.path(source_root, "DESCRIPTION")

  old_options <- options(cytokineprofile.app_source_root = source_root)
  on.exit(options(old_options), add = TRUE)

  testthat::local_mocked_bindings(
    app_namespace_loaded = function() TRUE,
    .package = "CytokineProfileShinyApp"
  )
  testthat::local_mocked_bindings(
    packageDescription = function(...) list(Version = "9.9.9", Title = "Pkg Title"),
    .package = "utils"
  )

  expect_equal(app_description_field("Title", default = "fallback"), "Pkg Title")
  expect_equal(app_version_string(), "9.9.9")

  app_test_write_file(
    desc_path,
    c(
      "Package: CytokineProfileShinyApp",
      "Version: 1.2.3",
      "Title: Source Title",
      "BlankField: "
    )
  )
  testthat::local_mocked_bindings(
    packageDescription = function(...) stop("metadata unavailable"),
    .package = "utils"
  )

  expect_equal(
    unname(app_description_field("Title", default = "fallback")),
    "Source Title"
  )
  expect_equal(app_description_field("Missing", default = "fallback"), "fallback")
  expect_equal(app_description_field("BlankField", default = "fallback"), "fallback")

  unlink(desc_path)
  expect_equal(app_description_field("Version", default = "fallback"), "fallback")
})

test_that("app built-in datasets fall back to source data files when needed", {
  source_root <- tempfile("app-data-")
  dir.create(file.path(source_root, "data"), recursive = TRUE, showWarnings = FALSE)

  ExampleData1 <- data.frame(Group = c("A", "B"), IL.10 = c(1, 2))
  save(ExampleData1, file = file.path(source_root, "data", "ExampleData1.rda"))

  old_options <- options(cytokineprofile.app_source_root = source_root)
  on.exit(options(old_options), add = TRUE)

  testthat::local_mocked_bindings(
    app_namespace_loaded = function() FALSE,
    app_package_name = function() "DefinitelyMissingPackage",
    .package = "CytokineProfileShinyApp"
  )

  expect_equal(app_builtin_dataset("ExampleData1"), ExampleData1)
  expect_error(
    app_builtin_dataset("ExampleData2"),
    "Could not locate built-in dataset file for ExampleData2"
  )
})

test_that("app resource helpers add missing resource paths and skip duplicates", {
  source_root <- tempfile("app-www-")
  dir.create(file.path(source_root, "www"), recursive = TRUE, showWarnings = FALSE)
  source_root <- app_test_normalize_path(source_root)

  old_options <- options(cytokineprofile.app_source_root = source_root)
  on.exit(options(old_options), add = TRUE)

  captured <- list()

  testthat::local_mocked_bindings(
    app_installed_file = function(...) "",
    .package = "CytokineProfileShinyApp"
  )
  testthat::local_mocked_bindings(
    resourcePaths = function() character(),
    addResourcePath = function(prefix, directoryPath) {
      captured[[length(captured) + 1L]] <<- list(
        prefix = prefix,
        directoryPath = directoryPath
      )
      invisible(NULL)
    },
    .package = "shiny"
  )

  expect_invisible(app_register_resources())
  expect_length(captured, 1L)
  expect_equal(captured[[1]]$prefix, app_resource_prefix())
  expect_equal(
    captured[[1]]$directoryPath,
    app_test_normalize_path(file.path(source_root, "www"))
  )

  testthat::local_mocked_bindings(
    resourcePaths = function() c("app-www" = "C:/existing"),
    addResourcePath = function(...) stop("should not be called"),
    .package = "shiny"
  )
  expect_invisible(app_register_resources())
})

test_that("app logic parent environment switches across source and namespace modes", {
  testthat::local_mocked_bindings(
    app_using_source_root = function() TRUE,
    .package = "CytokineProfileShinyApp"
  )
  expect_identical(app_logic_parent_env(), globalenv())
})

test_that("app logic parent environment uses the package namespace when available", {
  testthat::local_mocked_bindings(
    app_using_source_root = function() FALSE,
    app_namespace_loaded = function() TRUE,
    .package = "CytokineProfileShinyApp"
  )

  expect_identical(app_logic_parent_env(), asNamespace(app_package_name()))
})

test_that("app logic parent environment falls back to the global environment", {
  testthat::local_mocked_bindings(
    app_using_source_root = function() FALSE,
    app_namespace_loaded = function() FALSE,
    .package = "CytokineProfileShinyApp"
  )

  expect_identical(app_logic_parent_env(), globalenv())
})

test_that("app_logic_exec injects runtime objects and writes results back to app_ctx", {
  script_path <- app_test_write_file(
    file.path(tempfile("logic-"), "logic.R"),
    c(
      "stopifnot(existing_value == 5L)",
      "stopifnot(identical(input$token, 'in'))",
      "stopifnot(identical(output$token, 'out'))",
      "stopifnot(identical(session$token, 'session'))",
      "existing_value <- existing_value + 1L",
      "generated_value <- paste(input$token, output$token, session$token, sep = '-')"
    )
  )

  app_ctx <- new.env(parent = emptyenv())
  app_ctx$existing_value <- 5L

  testthat::local_mocked_bindings(
    app_server_logic_file = function(filename) {
      expect_equal(filename, "logic.R")
      script_path
    },
    .package = "CytokineProfileShinyApp"
  )

  result <- app_logic_exec(
    "logic.R",
    input = list(token = "in"),
    output = list(token = "out"),
    session = list(token = "session"),
    app_ctx = app_ctx
  )

  expect_identical(result, app_ctx)
  expect_equal(app_ctx$existing_value, 6L)
  expect_equal(app_ctx$generated_value, "in-out-session")
  expect_false(exists("input", envir = app_ctx, inherits = FALSE))
  expect_false(exists("output", envir = app_ctx, inherits = FALSE))
  expect_false(exists("session", envir = app_ctx, inherits = FALSE))
})

test_that("app server init wrappers delegate to app_logic_exec with expected files", {
  wrapper_map <- c(
    init_theme_server = "theme_toggle.R",
    init_wizard_step_control_server = "wizard_step_control.R",
    init_persistent_state_server = "persistent_state.R",
    init_data_handling_server = "data_handling.R",
    init_data_filtering_server = "data_filtering.R",
    init_options_server = "options_ui.R",
    init_navigation_server = "navigation.R",
    init_update_inputs_server = "update_inputs.R",
    init_analysis_results_server = "analysis_results.R",
    init_save_key_inputs_server = "save_key_inputs.R"
  )
  captured <- list()

  testthat::local_mocked_bindings(
    app_logic_exec = function(filename, input, output, session, app_ctx) {
      captured[[length(captured) + 1L]] <<- list(
        filename = filename,
        input = input,
        output = output,
        session = session,
        app_ctx = app_ctx
      )
      invisible(app_ctx)
    },
    .package = "CytokineProfileShinyApp"
  )

  input <- list(id = "input")
  output <- list(id = "output")
  session <- list(id = "session")
  app_ctx <- new.env(parent = emptyenv())

  for (wrapper_name in names(wrapper_map)) {
    getFromNamespace(wrapper_name, "CytokineProfileShinyApp")(
      input,
      output,
      session,
      app_ctx
    )
  }

  expect_equal(vapply(captured, `[[`, character(1), "filename"), unname(wrapper_map))
  expect_true(all(vapply(captured, function(x) identical(x$input, input), logical(1))))
  expect_true(all(vapply(captured, function(x) identical(x$output, output), logical(1))))
  expect_true(all(vapply(captured, function(x) identical(x$session, session), logical(1))))
  expect_true(all(vapply(captured, function(x) identical(x$app_ctx, app_ctx), logical(1))))
})

test_that("app_server initializes session state and invokes wrapper stages in order", {
  calls <- list()
  capture_stage <- function(stage_name) {
    force(stage_name)
    function(input, output, session, app_ctx) {
      calls[[length(calls) + 1L]] <<- list(
        stage = stage_name,
        input = input,
        output = output,
        session = session,
        app_ctx = app_ctx
      )
      invisible(app_ctx)
    }
  }

  testthat::local_mocked_bindings(
    app_session_temp_dir = function(name) paste0("C:/tmp/", name),
    app_builtin_dataset_names = function() c("ExampleData1", "ExampleData2"),
    init_theme_server = capture_stage("theme"),
    init_wizard_step_control_server = capture_stage("wizard"),
    init_persistent_state_server = capture_stage("persistent"),
    init_data_handling_server = capture_stage("data_handling"),
    init_data_filtering_server = capture_stage("data_filtering"),
    init_options_server = capture_stage("options"),
    init_navigation_server = capture_stage("navigation"),
    init_update_inputs_server = capture_stage("update_inputs"),
    init_analysis_results_server = capture_stage("analysis_results"),
    init_save_key_inputs_server = capture_stage("save_key_inputs"),
    .package = "CytokineProfileShinyApp"
  )
  testthat::local_mocked_bindings(
    observe_helpers = function() invisible(NULL),
    .package = "shinyhelper"
  )

  input <- list(id = "input")
  output <- list(id = "output")
  session <- list(userData = new.env(parent = emptyenv()))

  result <- app_server(input, output, session)
  expect_equal(
    vapply(calls, `[[`, character(1), "stage"),
    c(
      "theme",
      "wizard",
      "persistent",
      "data_handling",
      "data_filtering",
      "options",
      "navigation",
      "update_inputs",
      "analysis_results",
      "save_key_inputs"
    )
  )

  first_ctx <- calls[[1]]$app_ctx
  expect_identical(result, first_ctx)
  expect_equal(first_ctx$upload_dir, "C:/tmp/uploads")
  expect_equal(first_ctx$builtins_dir, "C:/tmp/builtins")
  expect_equal(first_ctx$builtInList, c("ExampleData1", "ExampleData2"))
  expect_true(all(vapply(calls, function(x) identical(x$app_ctx, first_ctx), logical(1))))
  expect_true(exists("stored_theme", envir = session$userData, inherits = FALSE))
  expect_null(session$userData$stored_theme)
})

test_that("announcement_banner and app_ui include the expected structure", {
  expect_null(announcement_banner(list()))

  banner_html <- app_test_render_html(
    announcement_banner(list(announcement = "<strong>Important</strong> update"))
  )
  expect_match(banner_html, "alert alert-info", fixed = TRUE)
  expect_match(banner_html, "<strong>Important</strong> update", fixed = TRUE)

  registered_resources <- FALSE
  testthat::local_mocked_bindings(
    app_register_resources = function() {
      registered_resources <<- TRUE
      invisible("app-www")
    },
    app_config = function() list(announcement = "<strong>Important</strong> update"),
    app_asset_href = function(filename) paste0("assets/", filename),
    app_version_string = function() "1.2.3",
    .package = "CytokineProfileShinyApp"
  )

  ui_html <- app_test_render_html(app_ui())

  expect_true(registered_resources)
  expect_match(ui_html, "<strong>Important</strong> update", fixed = TRUE)
  expect_match(ui_html, "assets/logo.png", fixed = TRUE)
  expect_match(ui_html, "v1.2.3", fixed = TRUE)
  expect_match(ui_html, "theme_choice", fixed = TRUE)
  expect_match(ui_html, "GitHub Repository", fixed = TRUE)
  expect_match(ui_html, "https://shinyinfo.cytokineprofile.org", fixed = TRUE)
  expect_match(ui_html, "stepHeader", fixed = TRUE)
  expect_match(ui_html, "progressBar", fixed = TRUE)
  expect_match(ui_html, "page_content", fixed = TRUE)
})
