app_server <- getFromNamespace(
  "app_server",
  "CytokineProfileShinyApp"
)


test_that("statistical analysis workflow routes a representative univariate path to step 5", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    run_app_server_analysis(
      session,
      app_ctx,
      "menu_univariate_2lvl",
      inputs = list(
        uv2_method = "auto",
        uv2_p_adjust_method = ""
      )
    )

    expect_equal(
      app_ctx$selected_function(),
      "Univariate Tests (T-test, Wilcoxon)"
    )
    expect_equal(app_ctx$currentStep(), 5)
    expect_equal(app_ctx$currentPage(), "step5")
    expect_equal(app_ctx$userState$uv2_method, "auto")
    expect_null(app_ctx$errorMessage())
  })
})

test_that("exploratory workflow routes a representative boxplot path to step 5", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    run_app_server_analysis(
      session,
      app_ctx,
      "menu_boxplots",
      inputs = list(
        bp_group_by = "Group",
        bp_bin_size = 10
      )
    )

    expect_equal(app_ctx$selected_function(), "Boxplots")
    expect_equal(app_ctx$currentStep(), 5)
    expect_equal(app_ctx$currentPage(), "step5")
    expect_equal(app_ctx$userState$bp_group_by, "Group")
    expect_equal(app_ctx$userState$bp_bin_size, 10)
    expect_null(app_ctx$errorMessage())
  })
})

test_that("multivariate workflow routes a representative PCA path to step 5", {
  testthat::skip_if_not_installed("mixOmics")
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    run_app_server_analysis(
      session,
      app_ctx,
      "menu_PCA",
      inputs = list(
        pca_group_col = "Group",
        pca_group_col2 = "",
        pca_comp_num = 2,
        pca_ellipse = FALSE,
        pca_style = "2D",
        pca_pch = c(16, 17, 18),
        pca_colors = c("#1b9e77", "#d95f02", "#7570b3")
      )
    )

    expect_equal(
      app_ctx$selected_function(),
      "Principal Component Analysis (PCA)"
    )
    expect_equal(app_ctx$currentStep(), 5)
    expect_equal(app_ctx$currentPage(), "step5")
    expect_equal(app_ctx$userState$pca_group_col, "Group")
    expect_equal(app_ctx$userState$pca_comp_num, 2)
    expect_null(app_ctx$errorMessage())
  })
})

test_that("machine-learning workflow routes a representative random forest path to step 5", {
  testthat::skip_if_not_installed("randomForest")
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    run_app_server_analysis(
      session,
      app_ctx,
      "menu_rf",
      inputs = list(
        rf_group_col = "Group",
        rf_ntree = 10,
        rf_mtry = 2,
        rf_train_fraction = 0.7,
        rf_plot_roc = FALSE,
        rf_run_rfcv = FALSE,
        rf_k_folds = 3,
        rf_step = 1
      )
    )

    expect_equal(app_ctx$selected_function(), "Random Forest")
    expect_equal(app_ctx$currentStep(), 5)
    expect_equal(app_ctx$currentPage(), "step5")
    expect_equal(app_ctx$userState$rf_group_col, "Group")
    expect_equal(app_ctx$userState$rf_ntree, 10)
    expect_null(app_ctx$errorMessage())
  })
})
