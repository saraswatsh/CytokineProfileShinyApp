app_server <- getFromNamespace(
  "app_server",
  "CytokineProfileShinyApp"
)
app_user_safe_error <- getFromNamespace(
  "app_user_safe_error",
  "CytokineProfileShinyApp"
)

friendly_analysis_failure_message <- paste(
  "We could not complete this analysis.",
  "Check the selected columns and options, then run it again."
)

expect_analysis_failure_state <- function(
  app_ctx,
  message,
  technical = NULL
) {
  expect_equal(app_ctx$currentStep(), 4)
  expect_equal(app_ctx$currentPage(), "step4")
  expect_equal(app_ctx$errorMessage(), message)
  if (!is.null(technical)) {
    expect_match(app_ctx$technicalErrorMessage(), technical, fixed = TRUE)
  }
}

run_analysis_failure_case <- function(
  session,
  app_ctx,
  menu_id,
  inputs,
  selected_numerical_cols = NULL
) {
  prepare_app_server_step2(session)
  if (!is.null(selected_numerical_cols)) {
    set_test_input(session, "selected_numerical_cols", selected_numerical_cols)
  }
  click_test_input(session, "next2")
  click_test_input(session, menu_id)
  for (nm in names(inputs)) {
    set_test_input(session, nm, inputs[[nm]])
  }
  click_test_input(session, "next4")
  invisible(wait_for_analysis_settlement(session, app_ctx))
}


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

test_that("analysis stage uses the current filteredData binding on step 4", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    captured_data <- NULL

    with_temp_pdf_device({
      testthat::local_mocked_bindings(
        cyt_bp = function(data, ...) {
          captured_data <<- data
          record_test_plot(draw_mock_base_plot("Boxplots"))
        },
        .package = "CytokineProfileShinyApp"
      )

      run_app_server_analysis(
        session,
        app_ctx,
        "menu_boxplots",
        inputs = list(
          bp_group_by = "Group",
          bp_bin_size = 10
        ),
        apply_filters = TRUE
      )

      expect_s3_class(captured_data, "data.frame")
      expected_data <- app_ctx$filteredData()
      expected_data$..cyto_id.. <- NULL
      expect_equal(captured_data, expected_data)
    })
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

test_that("sPLS-DA success advances to step 5 only after analysis completes", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    with_temp_pdf_device({
      testthat::local_mocked_bindings(
        cyt_splsda = function(...) {
          warning(
            paste(
              "sPLS-DA dropped unusable predictors before fitting for Group:",
              "MarkerSparse [observed values=2; reasons=fewer than 3 observed values (n=2)].",
              "Consider Step 2 'Treat missing values' if you want to retain sparse predictors."
            ),
            call. = FALSE
          )

          list(
            overall_indiv_plot = record_test_plot(draw_mock_base_plot("sPLS-DA")),
            overall_3D = NULL,
            overall_3D_interactive = NULL,
            overall_ROC = NULL,
            overall_CV = NULL,
            loadings = list(record_test_plot(draw_mock_base_plot("Loadings"))),
            vip_scores = list(
              ggplot2::ggplot(
                data.frame(x = 1, y = 1),
                ggplot2::aes(x, y)
              ) +
                ggplot2::geom_blank()
            ),
            vip_indiv_plot = NULL,
            vip_loadings = NULL,
            vip_3D = NULL,
            vip_3D_interactive = NULL,
            vip_ROC = NULL,
            vip_CV = NULL,
            conf_matrix = NULL
          )
        },
        .package = "CytokineProfileShinyApp"
      )

      run_app_server_analysis(
        session,
        app_ctx,
        "menu_splsda",
        inputs = list(
          splsda_group_col = "Group",
          splsda_group_col2 = "Group",
          splsda_var_num = 5,
          splsda_comp_num = 2
        )
      )

      expect_equal(
        app_ctx$selected_function(),
        "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
      )
      expect_equal(app_ctx$currentStep(), 5)
      expect_equal(app_ctx$currentPage(), "step5")
      expect_null(app_ctx$errorMessage())
      expect_true(any(grepl(
        "dropped unusable predictors",
        app_ctx$warningMessage(),
        fixed = TRUE
      )))
    })
  })
})

test_that("sPLS-DA failure stays on step 4 and records the error", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    testthat::local_mocked_bindings(
      cyt_splsda = function(...) {
        stop("Injected sPLS-DA failure", call. = FALSE)
      },
      .package = "CytokineProfileShinyApp"
    )

    run_app_server_analysis(
      session,
      app_ctx,
      "menu_splsda",
      inputs = list(
        splsda_group_col = "Group",
        splsda_group_col2 = "Group",
        splsda_var_num = 5,
        splsda_comp_num = 2
      )
    )

    expect_equal(
      app_ctx$selected_function(),
      "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
    )
    expect_analysis_failure_state(
      app_ctx,
      friendly_analysis_failure_message,
      technical = "Injected sPLS-DA failure"
    )
    expect_false(grepl(
      "Injected sPLS-DA failure",
      app_ctx$errorMessage(),
      fixed = TRUE
    ))
  })
})

test_that("analysis preflight guards missing group columns for multiple workflows", {
  local_mocked_browser_side_effects()

  cases <- list(
    list(
      menu_id = "menu_errorbp",
      inputs = list(eb_group_col = ""),
      message = "Choose a grouping column before running the Error-Bar Plot."
    ),
    list(
      menu_id = "menu_PCA",
      inputs = list(
        pca_group_col = "",
        pca_group_col2 = "",
        pca_comp_num = 2,
        pca_ellipse = FALSE,
        pca_style = "2D",
        pca_pch = c(16, 17)
      ),
      message = "Choose a grouping column with at least two groups before running PCA."
    ),
    list(
      menu_id = "menu_rf",
      inputs = list(
        rf_group_col = "",
        rf_ntree = 10,
        rf_mtry = 2,
        rf_train_fraction = 0.7,
        rf_plot_roc = FALSE,
        rf_run_rfcv = FALSE,
        rf_k_folds = 3,
        rf_step = 1
      ),
      message = "Choose a grouping column with at least two groups before running Random Forest."
    ),
    list(
      menu_id = "menu_xgb",
      inputs = list(
        xgb_group_col = "",
        xgb_train_fraction = 0.7,
        xgb_nrounds = 10,
        xgb_max_depth = 3,
        xgb_eta = 0.1,
        xgb_nfold = 3,
        xgb_cv = FALSE,
        xgb_eval_metric = "mlogloss",
        xgb_top_n_features = 10,
        xgb_plot_roc = FALSE
      ),
      message = "Choose a grouping column with at least two groups before running XGBoost."
    )
  )

  for (case in cases) {
    shiny::testServer(app_server, {
      run_analysis_failure_case(
        session,
        app_ctx,
        case$menu_id,
        case$inputs
      )

      expect_analysis_failure_state(app_ctx, case$message)
    })
  }
})

test_that("analysis preflight guards invalid response and correlation inputs", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    run_analysis_failure_case(
      session,
      app_ctx,
      "menu_PLSR",
      inputs = list(
        plsr_group_col = "Group",
        plsr_response_col = "Group",
        plsr_predictor_cols = example1_test_numerical_cols()[1:2],
        plsr_comp_num = 2,
        plsr_sparse = FALSE,
        plsr_keepX = 2,
        plsr_cv_opt = "None",
        plsr_fold_num = 3,
        plsr_ellipse = FALSE
      )
    )

    expect_analysis_failure_state(
      app_ctx,
      "Choose a numeric response column before running PLSR."
    )
  })

  shiny::testServer(app_server, {
    run_analysis_failure_case(
      session,
      app_ctx,
      "menu_correlation",
      inputs = list(
        corr_target = "",
        corr_group_col = "",
        corr_by_group = FALSE
      )
    )

    expect_analysis_failure_state(
      app_ctx,
      "Choose a numeric target column before running the Correlation Plot."
    )
  })

  shiny::testServer(app_server, {
    run_analysis_failure_case(
      session,
      app_ctx,
      "menu_correlation",
      inputs = list(
        corr_target = example1_test_numerical_cols()[1],
        corr_group_col = "",
        corr_by_group = TRUE
      )
    )

    expect_analysis_failure_state(
      app_ctx,
      "Choose a grouping column before running grouped Correlation Plots."
    )
  })
})

test_that("analysis preflight guards missing or identical comparison groups", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    run_analysis_failure_case(
      session,
      app_ctx,
      "menu_volcano",
      inputs = list(
        volc_group_col = "Group",
        volc_cond1 = "PreT2D",
        volc_cond2 = "PreT2D",
        volc_fold_change_thresh = 1,
        volc_p_value_thresh = 0.05,
        volc_top_labels = 10
      )
    )

    expect_analysis_failure_state(
      app_ctx,
      "Choose two different comparison groups for the Volcano Plot."
    )
  })

  shiny::testServer(app_server, {
    run_analysis_failure_case(
      session,
      app_ctx,
      "menu_dualflash",
      inputs = list(
        df_group_var = "Group",
        df_cond1 = "",
        df_cond2 = "T2D",
        df_ssmd_thresh = 0.5,
        df_log2fc_thresh = 1,
        df_top_labels = 10
      )
    )

    expect_analysis_failure_state(
      app_ctx,
      "Choose the first comparison group before running the Dual-Flashlight Plot."
    )
  })
})

test_that("analysis preflight guards insufficient predictors for sparse workflows", {
  local_mocked_browser_side_effects()
  one_numeric <- example1_test_numerical_cols()[1]

  shiny::testServer(app_server, {
    run_analysis_failure_case(
      session,
      app_ctx,
      "menu_splsda",
      inputs = list(
        splsda_group_col = "Group",
        splsda_group_col2 = "",
        splsda_var_num = 1,
        splsda_comp_num = 2
      ),
      selected_numerical_cols = one_numeric
    )

    expect_analysis_failure_state(
      app_ctx,
      "Choose data with at least two numeric predictor columns before running sPLS-DA."
    )
  })

  shiny::testServer(app_server, {
    run_analysis_failure_case(
      session,
      app_ctx,
      "menu_mint_splsda",
      inputs = list(
        mint_splsda_group_col = "Group",
        mint_splsda_group_col2 = "",
        mint_splsda_batch_col = "Time",
        mint_splsda_var_num = 1,
        mint_splsda_comp_num = 2,
        mint_splsda_cim = FALSE,
        mint_splsda_ellipse = FALSE,
        mint_splsda_bg = FALSE,
        mint_splsda_roc = FALSE
      ),
      selected_numerical_cols = one_numeric
    )

    expect_analysis_failure_state(
      app_ctx,
      "Choose data with at least two numeric predictor columns before running MINT sPLS-DA."
    )
  })
})

test_that("valid analysis inputs still pass preflight and advance to step 5", {
  local_mocked_browser_side_effects()

  shiny::testServer(app_server, {
    testthat::local_mocked_bindings(
      cyt_volc = function(...) {
        list(
          plot = ggplot2::ggplot(
            data.frame(x = 1, y = 1),
            ggplot2::aes(x, y)
          ) + ggplot2::geom_blank(),
          stats = data.frame(feature = "IL6", p_value = 0.05)
        )
      },
      .package = "CytokineProfileShinyApp"
    )

    run_app_server_analysis(
      session,
      app_ctx,
      "menu_volcano",
      inputs = list(
        volc_group_col = "Group",
        volc_cond1 = "PreT2D",
        volc_cond2 = "T2D",
        volc_fold_change_thresh = 1,
        volc_p_value_thresh = 0.05,
        volc_top_labels = 10
      )
    )

    expect_equal(app_ctx$selected_function(), "Volcano Plot")
    expect_equal(app_ctx$currentStep(), 5)
    expect_equal(app_ctx$currentPage(), "step5")
    expect_null(app_ctx$errorMessage())
  })
})

test_that("export safe errors preserve the user-facing message", {
  err <- tryCatch(
    {
      stop(app_user_safe_error(
        "We could not prepare the export. Try again or choose a different format."
      ))
    },
    error = identity
  )

  expect_s3_class(err, "app_user_error")
  expect_match(
    conditionMessage(err),
    "We could not prepare the export. Try again or choose a different format.",
    fixed = TRUE
  )
})
