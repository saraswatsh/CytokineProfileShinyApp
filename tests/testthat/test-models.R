test_that("cyt_pca returns nested treatment results with recorded plots", {
  with_temp_pdf_device({
    pca_result <- suppress_known_plot_warnings(
      cyt_pca(
        ex1_pca,
        group_col = "Group",
        group_col2 = "Treatment",
        pca_colors = c("black", "red2"),
        scale = "log2",
        comp_num = 2,
        pch_values = c(16, 4),
        ellipse = FALSE,
        output_file = NULL
      )
    )

    expect_true(is.list(pca_result))
    expect_equal(sort(names(pca_result)), sort(unique(ex1_pca$Treatment)))

    first_result <- pca_result[[1]]
    expect_true(inherits(first_result$overall_indiv_plot, "recordedplot"))
    expect_true(inherits(first_result$overall_scree_plot, "recordedplot"))
    expect_length(first_result$loadings, 2)
    expect_true(all(vapply(first_result$loadings, inherits, logical(1), "recordedplot")))
  })
})

test_that("cyt_splsda returns nested treatment results with VIP summaries", {
  with_temp_pdf_device({
    splsda_result <- suppress_known_plot_warnings(
      cyt_splsda(
        ex1_binary_group_treatment,
        group_col = "Group",
        group_col2 = "Treatment",
        var_num = 5,
        comp_num = 2,
        scale = "log2",
        roc = FALSE,
        conf_mat = FALSE,
        cv_opt = NULL,
        splsda_colors = c("black", "purple"),
        pch_values = c(16, 4)
      )
    )

    expect_true(is.list(splsda_result))
    expect_equal(
      sort(names(splsda_result)),
      sort(unique(ex1_binary_group_treatment$Treatment))
    )

    first_result <- splsda_result[[1]]
    expect_true(inherits(first_result$overall_indiv_plot, "recordedplot"))
    expect_length(first_result$loadings, 2)
    expect_length(first_result$vip_scores, 2)
    expect_true(inherits(first_non_null(first_result$vip_scores), "ggplot"))
  })
})

test_that("cyt_mint_splsda returns global and partial plots from ExampleData5", {
  with_temp_pdf_device({
    mint_result <- suppress_known_plot_warnings(
      cyt_mint_splsda(
        ex5_mint,
        group_col = "Group",
        batch_col = "Batch",
        colors = c("black", "purple"),
        ellipse = TRUE,
        var_num = 5,
        comp_num = 2,
        scale = "log2",
        cim = FALSE,
        roc = FALSE
      )
    )

    expect_true(is.list(mint_result))
    expect_true(inherits(mint_result$global_indiv_plot, "recordedplot"))
    expect_true(inherits(mint_result$partial_indiv_plot, "recordedplot"))
    expect_true(inherits(mint_result$correlation_circle_plot, "recordedplot"))
    expect_gt(length(mint_result$partial_loadings_plots), 0)
  })
})

test_that("cyt_plsr supports dense and sparse regression configurations", {
  with_temp_pdf_device({
    plsr_df <- ex1_binary_group[, c("Group", "IL.10", "IL.17F", "GM.CSF", "IFN.G", "IL.13"), drop = FALSE]

    dense_result <- cyt_plsr(
      plsr_df,
      response_col = "IL.10",
      predictor_cols = c("IL.17F", "GM.CSF", "IFN.G", "IL.13"),
      group_col = "Group",
      comp_num = 2,
      sparse = FALSE,
      cv_opt = NULL,
      scale = "log2"
    )

    sparse_result <- cyt_plsr(
      plsr_df,
      response_col = "IL.10",
      predictor_cols = c("IL.17F", "GM.CSF", "IFN.G", "IL.13"),
      group_col = "Group",
      comp_num = 2,
      sparse = TRUE,
      var_num = 2,
      cv_opt = NULL,
      scale = "log2"
    )

    for (result in list(dense_result, sparse_result)) {
      expect_true(inherits(result$scores_plot, "recordedplot"))
      expect_true(inherits(result$pred_vs_obs, "recordedplot"))
      expect_true(inherits(result$residuals_plot, "recordedplot"))
      expect_length(result$loadings, 2)
      expect_length(result$vip_scores, 2)
      expect_true(inherits(first_non_null(result$vip_scores), "ggplot"))
    }
  })
})

test_that("cyt_rf returns summary text, importance data, and ROC plot", {
  with_temp_pdf_device({
    model_features <- names(ex1_binary_group)[vapply(ex1_binary_group, is.numeric, logical(1))][2:8]
    rf_df <- ex1_binary_group[, c("Group", model_features), drop = FALSE]

    rf_result <- cyt_rf(
      rf_df,
      group_col = "Group",
      ntree = 25,
      mtry = 2,
      run_rfcv = FALSE,
      plot_roc = TRUE,
      verbose = FALSE,
      cv = FALSE,
      seed = 123
    )

    expect_true(grepl("RANDOM FOREST RESULTS", rf_result$summary_text, fixed = TRUE))
    expect_true(inherits(rf_result$vip_plot, "ggplot"))
    expect_true(inherits(rf_result$roc_plot, "ggplot"))
    expect_gt(nrow(rf_result$importance_data), 0)
  })
})

test_that("cyt_xgb returns summary text, importance data, and ROC plot", {
  with_temp_pdf_device({
    model_features <- names(ex1_binary_group)[vapply(ex1_binary_group, is.numeric, logical(1))][2:8]
    xgb_df <- ex1_binary_group[, c("Group", model_features), drop = FALSE]

    xgb_result <- cyt_xgb(
      xgb_df,
      group_col = "Group",
      nrounds = 15,
      max_depth = 3,
      learning_rate = 0.1,
      cv = FALSE,
      plot_roc = TRUE,
      print_results = FALSE,
      seed = 123
    )

    expect_true(grepl("XGBOOST RESULTS", xgb_result$summary_text, fixed = TRUE))
    expect_true(inherits(xgb_result$importance_plot, "ggplot"))
    expect_true(inherits(xgb_result$roc_plot, "ggplot"))
    expect_gt(nrow(xgb_result$importance), 0)
  })
})
