library(shinytest2)

test_that("{shinytest2} recording: Menu Routing", {
  skip_on_cran()
  skip_if_appdriver_disabled()
  app_dir <- app_test_dir()
  app <- AppDriver$new(
    app_dir,
    variant = platform_variant(),
    name = "menus",
    load_timeout = 30000,
    timeout = 30000
  )

  route_and_capture <- function(menu_id) {
    app$click(menu_id, timeout_ = 30000)
    app_expect_stable_screenshot(app, timeout = 30000)
    app$click("back4", timeout_ = 30000)
    app_wait_for_dom(app, paste0("#", menu_id), timeout = 30000)
  }

  app$click("nav_start_home")
  app$wait_for_idle(4000)

  app$set_inputs(use_builtin = TRUE)
  app_wait_for_input_binding(app, "built_in_choice")

  app$set_inputs(built_in_choice = "ExampleData1", wait_ = FALSE)
  app_wait_for_js_true(
    app,
    paste0(
      "(function() {",
      "var checked = document.querySelector(",
      app_test_js_string("input[name='built_in_choice']:checked"),
      ");",
      "return !!checked && checked.value === 'ExampleData1';",
      "})();"
    )
  )

  app$click("next1", timeout_ = 30000)
  app_wait_for_input_binding(app, "selected_categorical_cols")
  app_wait_for_input_binding(app, "selected_numerical_cols")

  app$click("next2", timeout_ = 30000)
  app_wait_for_dom(app, "#menu_univariate_2lvl")
  app_expect_stable_screenshot(app, timeout = 30000)

  route_and_capture("menu_univariate_2lvl")
  route_and_capture("menu_univariate_multi")
  route_and_capture("menu_two_way_anova")
  route_and_capture("menu_ancova")
  route_and_capture("menu_boxplots")
  route_and_capture("menu_violin")
  route_and_capture("menu_correlation")
  route_and_capture("menu_skewkurt")
  route_and_capture("menu_errorbp")
  route_and_capture("menu_dualflash")
  route_and_capture("menu_heatmap")
  route_and_capture("menu_volcano")
  route_and_capture("menu_PCA")
  route_and_capture("menu_PLSR")
  route_and_capture("menu_splsda")
  route_and_capture("menu_mint_splsda")
  route_and_capture("menu_rf")
  route_and_capture("menu_xgb")

  app$wait_for_idle(timeout = 30000)
  app$stop()
})