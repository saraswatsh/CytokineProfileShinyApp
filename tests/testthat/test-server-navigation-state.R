test_that("navigation controls route between top-level pages and wizard steps", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    click_test_input(session, "nav_tutorials")
    expect_equal(app_ctx$currentPage(), "tutorials")

    click_test_input(session, "nav_home")
    expect_equal(app_ctx$currentPage(), "home")

    click_test_input(session, "nav_start_home")
    expect_equal(app_ctx$currentPage(), "step1")

    click_test_input(session, "nav_filter")
    expect_equal(app_ctx$currentPage(), "step2")
    expect_equal(app_ctx$currentStep(), 2)

    click_test_input(session, "nav_options")
    expect_equal(app_ctx$currentPage(), "step3")
    expect_equal(app_ctx$currentStep(), 3)

    click_test_input(session, "nav_args")
    expect_equal(app_ctx$currentPage(), "step4")
    expect_equal(app_ctx$currentStep(), 4)

    click_test_input(session, "nav_results")
    expect_equal(app_ctx$currentPage(), "step5")
    expect_equal(app_ctx$currentStep(), 5)

    click_test_input(session, "nav_news")
    expect_equal(app_ctx$currentPage(), "news")

    click_test_input(session, "nav_contact")
    expect_equal(app_ctx$currentPage(), "contact")
  })
})

test_that("wizard back and next buttons update workflow state", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    prepare_app_server_step2(session)

    click_test_input(session, "back2")
    expect_equal(app_ctx$currentPage(), "step1")
    expect_equal(app_ctx$currentStep(), 1)

    prepare_app_server_step2(session)
    click_test_input(session, "next2")
    expect_equal(app_ctx$currentPage(), "step3")
    expect_equal(app_ctx$currentStep(), 3)
    expect_true(all(c("Group", "Time") %in% app_ctx$userState$selected_columns))

    click_test_input(session, "back3")
    expect_equal(app_ctx$currentPage(), "step2")
    expect_equal(app_ctx$currentStep(), 2)

    app_ctx$currentPage("step4")
    app_ctx$currentStep(4)
    test_server_flush(session)

    click_test_input(session, "back4")
    expect_equal(app_ctx$currentPage(), "step3")
    expect_equal(app_ctx$currentStep(), 3)

    prepare_app_server_step4(session, app_ctx, "Boxplots")
    click_test_input(session, "next4")
    expect_equal(app_ctx$currentPage(), "step5")
    expect_equal(app_ctx$currentStep(), 5)

    click_test_input(session, "back5")
    expect_equal(app_ctx$currentPage(), "step4")
    expect_equal(app_ctx$currentStep(), 4)
  })
})

test_that("fresh-start flow shows a modal and reloads the session on confirm", {
  local_mocked_browser_side_effects()

  modal_html <- NULL
  removed_modal <- FALSE
  reload_count <- 0L

  testthat::local_mocked_bindings(
    showModal = function(ui, session = shiny::getDefaultReactiveDomain()) {
      modal_html <<- htmltools::renderTags(ui)$html
      invisible(NULL)
    },
    removeModal = function(session = shiny::getDefaultReactiveDomain()) {
      removed_modal <<- TRUE
      invisible(NULL)
    },
    .package = "shiny"
  )

  shiny::testServer(app_server, {
    session$reload <- function() {
      reload_count <<- reload_count + 1L
      invisible(NULL)
    }

    click_test_input(session, "fresh_start")

    expect_match(modal_html, "Start fresh\\?")
    expect_match(modal_html, "confirm_fresh_start", fixed = TRUE)
    expect_match(
      modal_html,
      "This will clear uploaded/built-in data and all saved options.",
      fixed = TRUE
    )

    click_test_input(session, "confirm_fresh_start")

    expect_true(removed_modal)
    expect_equal(reload_count, 1L)
  })
})

test_that("new_reuse clears analysis-specific state while preserving Step 2 workflow state", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    prepare_app_server_step3(session, app_ctx = app_ctx, scale_choice = "log2")

    app_ctx$userState$use_builtin <- TRUE
    app_ctx$userState$built_in_choice <- "ExampleData1"
    app_ctx$userState$selected_columns <- c("Group", "Time", "IL.10")
    app_ctx$userState$bp_group_by <- "Group"
    app_ctx$userState$bp_bin_size <- 8
    app_ctx$userState$plsr_keepX <- 5
    app_ctx$userState$plsr_keepX_manual <- TRUE
    app_ctx$userState$rf_ntree <- 100
    app_ctx$userState$volc_top_labels <- 12
    app_ctx$userState$corr_by_group <- TRUE
    app_ctx$userState$bp_font_settings <- list(use_custom = TRUE, plot_title = 20)

    click_test_input(session, "new_reuse")

    expect_equal(app_ctx$currentPage(), "step2")
    expect_equal(app_ctx$currentStep(), 2)

    expect_true(isTRUE(app_ctx$userState$use_builtin))
    expect_equal(app_ctx$userState$built_in_choice, "ExampleData1")
    expect_equal(app_ctx$userState$selected_columns, c("Group", "Time", "IL.10"))
    expect_equal(app_ctx$userState$step2_scale, "log2")

    expect_null(app_ctx$userState$bp_group_by)
    expect_null(app_ctx$userState$bp_bin_size)
    expect_null(app_ctx$userState$plsr_keepX)
    expect_false(isTRUE(app_ctx$userState$plsr_keepX_manual))
    expect_null(app_ctx$userState$rf_ntree)
    expect_null(app_ctx$userState$volc_top_labels)
    expect_false(isTRUE(app_ctx$userState$corr_by_group))
    expect_null(app_ctx$userState$bp_font_settings)
  })
})
