mod_theme_server <- getFromNamespace(
  "mod_theme_server",
  "CytokineProfileShinyApp"
)
mod_wizard_step_control_server <- getFromNamespace(
  "mod_wizard_step_control_server",
  "CytokineProfileShinyApp"
)
mod_persistent_state_server <- getFromNamespace(
  "mod_persistent_state_server",
  "CytokineProfileShinyApp"
)

test_that("theme module tracks system and manual theme choices", {
  local_mocked_browser_side_effects()
  app_ctx <- new_test_app_ctx()

  shiny::testServer(
    wrap_server_with_app_ctx(
      mod_theme_server,
      app_ctx
    ),
    {
      expect_equal(app_ctx$system_theme(), "flatly")
      expect_equal(app_ctx$theme_choice_rv(), "auto")

      set_test_input(session, "system_theme", "minty")
      expect_equal(app_ctx$system_theme(), "minty")

      set_test_input(session, "theme_choice", "darkly")
      expect_equal(app_ctx$theme_choice_rv(), "darkly")
    }
  )
})

test_that("wizard step module initializes the workflow at step 1", {
  app_ctx <- new_test_app_ctx()

  shiny::testServer(
    wrap_server_with_app_ctx(
      mod_wizard_step_control_server,
      app_ctx
    ),
    {
      expect_equal(app_ctx$currentStep(), 1)
    }
  )
})

test_that("persistent state module seeds the shared app context", {
  app_ctx <- new_test_app_ctx()

  shiny::testServer(
    wrap_server_with_app_ctx(
      mod_persistent_state_server,
      app_ctx
    ),
    {
      expect_true(inherits(app_ctx$userState, "reactivevalues"))
      expect_true(inherits(app_ctx$bioplex, "reactivevalues"))
      expect_null(app_ctx$selected_function())
      expect_equal(app_ctx$userState$step2_scale, "none")
      expect_false(isTRUE(app_ctx$userState$use_builtin))
      expect_false(app_ctx$dev_notice_shown())
    }
  )
})
