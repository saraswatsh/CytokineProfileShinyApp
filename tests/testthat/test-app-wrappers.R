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
  dir.create(file.path(source_root, "inst", "app", "www"), recursive = TRUE)
  app_test_write_file(
    file.path(source_root, "inst", "app", "config.yml"),
    "default:\n  mode: source"
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

  expect_equal(
    app_config_path(),
    file.path(source_root, "inst", "app", "config.yml")
  )
  expect_equal(app_www_dir(), file.path(source_root, "inst", "app", "www"))
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

test_that("app config and asset helpers use installed files when present", {
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

  expect_error(app_config_path(), "Could not find the app config file.")
  expect_error(app_www_dir(), "Could not find the app asset directory.")
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
  dir.create(file.path(source_root, "inst", "app", "www"), recursive = TRUE)
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
    app_test_normalize_path(file.path(source_root, "inst", "app", "www"))
  )

  testthat::local_mocked_bindings(
    resourcePaths = function() c("app-www" = "C:/existing"),
    addResourcePath = function(...) stop("should not be called"),
    .package = "shiny"
  )
  expect_invisible(app_register_resources())
})

test_that("stage helpers persist stage state and support deferred cross-stage lookups", {
  stage_reader <- function(app_ctx) {
    input <- list(token = "in")
    output <- list(token = "out")
    session <- list(token = "session")
    stage_env <- app_stage_init(app_ctx)

    existing_value <- existing_value + 1L
    generated_value <- paste(input$token, output$token, session$token, sep = "-")
    deferred_reader <- function() filteredData()

    invisible(app_stage_commit(app_ctx, stage_env))
  }

  stage_writer <- function(app_ctx) {
    input <- list()
    output <- list()
    session <- list()
    stage_env <- app_stage_init(app_ctx)

    filteredData <- function() 123L

    invisible(app_stage_commit(app_ctx, stage_env))
  }

  app_ctx <- new.env(parent = environment(app_server))
  app_ctx$existing_value <- 5L

  stage_reader(app_ctx)
  stage_writer(app_ctx)

  expect_equal(app_ctx$existing_value, 6L)
  expect_equal(app_ctx$generated_value, "in-out-session")
  expect_equal(app_ctx$deferred_reader(), 123L)
  expect_false(exists("input", envir = app_ctx, inherits = FALSE))
  expect_false(exists("output", envir = app_ctx, inherits = FALSE))
  expect_false(exists("session", envir = app_ctx, inherits = FALSE))
})

test_that("stage commit preserves borrowed callable bindings", {
  borrow_stage <- function(app_ctx) {
    input <- list()
    output <- list()
    session <- list()
    stage_env <- app_stage_init(app_ctx)

    currentStep <- app_ctx$currentStep
    borrowed_value <- currentStep()

    invisible(app_stage_commit(app_ctx, stage_env))
  }

  app_ctx <- new.env(parent = environment(app_server))
  app_ctx$currentStep <- local({
    value <- 2L

    function(new_value) {
      if (missing(new_value)) {
        return(value)
      }

      value <<- new_value
      invisible(value)
    }
  })

  borrow_stage(app_ctx)

  expect_equal(app_ctx$borrowed_value, 2L)
  expect_true(is.function(app_ctx$currentStep))
  expect_equal(app_ctx$currentStep(), 2L)
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
  expect_identical(parent.env(first_ctx), environment(app_server))
  expect_equal(first_ctx$upload_dir, "C:/tmp/uploads")
  expect_equal(first_ctx$builtins_dir, "C:/tmp/builtins")
  expect_equal(first_ctx$builtInList, c("ExampleData1", "ExampleData2"))
  expect_true(all(vapply(calls, function(x) identical(x$app_ctx, first_ctx), logical(1))))
  expect_true(exists("stored_theme", envir = session$userData, inherits = FALSE))
  expect_null(session$userData$stored_theme)
})

test_that("app_server boots without recursive startup errors", {
  testthat::local_mocked_bindings(
    observe_helpers = function() invisible(NULL),
    .package = "shinyhelper"
  )

  expect_no_error(
    shiny::testServer(app_server, {
      shiny:::flushReact()
      expect_true(TRUE)
    })
  )
})

test_that("source launcher in inst/app boots from a source checkout", {
  launch_env <- new.env(parent = globalenv())
  launcher_dir <- testthat::test_path("..", "..", "inst", "app")
  if (!dir.exists(launcher_dir) || !file.exists(file.path(launcher_dir, "app.R"))) {
    testthat::skip(
      "Source-checkout launcher layout is not available in the installed test tree."
    )
  }
  launcher_dir <- normalizePath(launcher_dir, winslash = "/", mustWork = TRUE)
  old_wd <- setwd(launcher_dir)
  old_options <- options(cytokineprofile.app_source_root = "")
  on.exit(options(old_options), add = TRUE)
  on.exit(setwd(old_wd), add = TRUE)

  expect_no_error(sys.source("app.R", envir = launch_env, keep.source = TRUE))
  expect_true(exists("ui", envir = launch_env, inherits = FALSE))
  expect_true(exists("server", envir = launch_env, inherits = FALSE))
  expect_true(inherits(launch_env$ui, "shiny.tag.list"))
  expect_true(is.function(launch_env$server))
  expect_equal(
    getOption("cytokineprofile.app_source_root"),
    app_test_normalize_path(file.path(launcher_dir, "..", ".."))
  )
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
