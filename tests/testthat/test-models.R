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

test_that("cyt_pca supports the overall non-split path", {
  with_temp_pdf_device({
    pca_result <- suppress_known_plot_warnings(
      cyt_pca(
        ex1_pca,
        group_col = "Group",
        group_col2 = "Group",
        pca_colors = c("black", "red2"),
        scale = "log2",
        comp_num = 2,
        pch_values = c(16, 4),
        output_file = NULL
      )
    )

    expect_true(is.list(pca_result))
    expect_true(all(c(
      "overall_indiv_plot",
      "overall_scree_plot",
      "loadings",
      "biplot",
      "correlation_circle"
    ) %in% names(pca_result)))
    expect_length(pca_result$loadings, 2)
  })
})

test_that("cyt_pca supports the 3D branch", {
  with_temp_pdf_device({
    pca_result <- suppress_known_plot_warnings(
      cyt_pca(
        ex1_pca,
        group_col = "Group",
        group_col2 = "Group",
        pca_colors = c("black", "red2"),
        scale = "log2",
        comp_num = 3,
        pch_values = c(16, 4),
        style = "3D",
        output_file = NULL
      )
    )

    expect_true(inherits(pca_result$overall_3D, "recordedplot"))
  })
})

test_that("cyt_pca supports zscore scaling through apply_scale", {
  with_temp_pdf_device({
    pca_result <- suppress_known_plot_warnings(
      cyt_pca(
        ex1_pca,
        group_col = "Group",
        group_col2 = "Group",
        pca_colors = c("black", "red2"),
        scale = "zscore",
        comp_num = 2,
        pch_values = c(16, 4),
        output_file = NULL
      )
    )

    expect_true(is.list(pca_result))
    expect_true(inherits(pca_result$overall_indiv_plot, "recordedplot"))
    expect_true(inherits(pca_result$overall_scree_plot, "recordedplot"))
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

test_that("cyt_splsda supports overall ROC and LOOCV branches", {
  with_temp_pdf_device({
    splsda_result <- suppress_known_plot_warnings(
      cyt_splsda(
        ex1_binary_group_treatment,
        group_col = "Group",
        group_col2 = "Group",
        var_num = 5,
        comp_num = 2,
        scale = "log2",
        roc = TRUE,
        conf_mat = FALSE,
        cv_opt = "loocv",
        splsda_colors = c("black", "purple"),
        pch_values = c(16, 4)
      )
    )

    expect_true(is.list(splsda_result))
    expect_true(inherits(splsda_result$overall_ROC, "recordedplot"))
    expect_true(inherits(splsda_result$overall_CV, "ggplot"))
  })
})

test_that("cyt_splsda supports the 3D branch", {
  with_temp_pdf_device({
    splsda_result <- suppress_known_plot_warnings(
      cyt_splsda(
        ex1_binary_group_treatment,
        group_col = "Group",
        group_col2 = "Group",
        var_num = 5,
        comp_num = 3,
        scale = "log2",
        roc = FALSE,
        conf_mat = FALSE,
        cv_opt = NULL,
        style = "3d",
        splsda_colors = c("black", "purple"),
        pch_values = c(16, 4)
      )
    )

    expect_true(inherits(splsda_result$overall_3D, "recordedplot"))
  })
})

test_that("cyt_splsda supports shared apply_scale options without UI changes", {
  with_temp_pdf_device({
    scale_df <- ex1_binary_group_treatment[, c(
      "Group",
      "Treatment",
      "IL.10",
      "IL.17F",
      "GM.CSF",
      "IFN.G",
      "IL.13"
    ), drop = FALSE]
    predictor_cols <- names(scale_df)[vapply(scale_df, is.numeric, logical(1))]
    scale_df[, predictor_cols] <- scale_df[, predictor_cols, drop = FALSE] + 1

    none_result <- suppress_known_plot_warnings(
      cyt_splsda(
        scale_df,
        group_col = "Group",
        group_col2 = "Group",
        var_num = 3,
        comp_num = 2,
        scale = "none",
        roc = FALSE,
        conf_mat = FALSE,
        cv_opt = NULL,
        splsda_colors = c("black", "purple"),
        pch_values = c(16, 4)
      )
    )
    null_result <- suppress_known_plot_warnings(
      cyt_splsda(
        scale_df,
        group_col = "Group",
        group_col2 = "Group",
        var_num = 3,
        comp_num = 2,
        scale = NULL,
        roc = FALSE,
        conf_mat = FALSE,
        cv_opt = NULL,
        splsda_colors = c("black", "purple"),
        pch_values = c(16, 4)
      )
    )
    log10_result <- suppress_known_plot_warnings(
      cyt_splsda(
        scale_df,
        group_col = "Group",
        group_col2 = "Group",
        var_num = 3,
        comp_num = 2,
        scale = "log10",
        roc = FALSE,
        conf_mat = FALSE,
        cv_opt = NULL,
        splsda_colors = c("black", "purple"),
        pch_values = c(16, 4)
      )
    )
    zscore_result <- suppress_known_plot_warnings(
      cyt_splsda(
        scale_df,
        group_col = "Group",
        group_col2 = "Group",
        var_num = 3,
        comp_num = 2,
        scale = "zscore",
        roc = FALSE,
        conf_mat = FALSE,
        cv_opt = NULL,
        splsda_colors = c("black", "purple"),
        pch_values = c(16, 4)
      )
    )
    custom_result <- suppress_known_plot_warnings(
      cyt_splsda(
        scale_df,
        group_col = "Group",
        group_col2 = "Group",
        var_num = 3,
        comp_num = 2,
        scale = "custom",
        custom_fn = function(x) x + 1,
        roc = FALSE,
        conf_mat = FALSE,
        cv_opt = NULL,
        splsda_colors = c("black", "purple"),
        pch_values = c(16, 4)
      )
    )

    for (result in list(log10_result, zscore_result, custom_result)) {
      expect_true(is.list(result))
      expect_true(inherits(result$overall_indiv_plot, "recordedplot"))
      expect_length(result$loadings, 2)
      expect_length(result$vip_scores, 2)
      expect_true(inherits(first_non_null(result$vip_scores), "ggplot"))
    }

    expect_equal(
      first_non_null(none_result$vip_scores)$data,
      first_non_null(null_result$vip_scores)$data
    )
  })
})

test_that("cyt_splsda errors on non-positive predictor values for log scales", {
  bad_df <- ex1_binary_group_treatment[, c(
    "Group",
    "Treatment",
    "IL.10",
    "IL.17F",
    "GM.CSF",
    "IFN.G",
    "IL.13"
  ), drop = FALSE]
  bad_df$GM.CSF[1] <- 0
  bad_df$IFN.G[2] <- -1

  expect_error(
    cyt_splsda(
      bad_df,
      group_col = "Group",
      group_col2 = "Group",
      var_num = 3,
      comp_num = 2,
      scale = "log2",
      roc = FALSE,
      conf_mat = FALSE,
      cv_opt = NULL,
      splsda_colors = c("black", "purple"),
      pch_values = c(16, 4)
    ),
    "requires all non-missing selected values to be finite and greater than 0"
  )
  expect_error(
    cyt_splsda(
      bad_df,
      group_col = "Group",
      group_col2 = "Group",
      var_num = 3,
      comp_num = 2,
      scale = "log10",
      roc = FALSE,
      conf_mat = FALSE,
      cv_opt = NULL,
      splsda_colors = c("black", "purple"),
      pch_values = c(16, 4)
    ),
    "requires all non-missing selected values to be finite and greater than 0"
  )
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

test_that("cyt_mint_splsda supports grouped split results by Treatment", {
  with_temp_pdf_device({
    mint_result <- suppress_known_plot_warnings(
      cyt_mint_splsda(
        ex5_mint_split,
        group_col = "Group",
        batch_col = "Batch",
        group_col2 = "Treatment",
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
    expect_true(all(unique(ex5_mint_split$Treatment) %in% names(mint_result)))
    expect_true(all(vapply(mint_result, is.list, logical(1))))
  })
})

test_that("cyt_mint_splsda supports CIM output", {
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
        cim = TRUE,
        roc = FALSE
      )
    )

    expect_true(inherits(mint_result$cim_obj, "recordedplot"))
  })
})

test_that("cyt_mint_splsda supports zscore scaling through apply_scale", {
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
        scale = "zscore",
        cim = FALSE,
        roc = FALSE
      )
    )

    expect_true(is.list(mint_result))
    expect_true(inherits(mint_result$global_indiv_plot, "recordedplot"))
    expect_true(inherits(mint_result$partial_indiv_plot, "recordedplot"))
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

test_that("cyt_plsr supports shared apply_scale options on selected predictors", {
  with_temp_pdf_device({
    plsr_df <- ex1_binary_group[, c(
      "Group",
      "IL.10",
      "IL.17F",
      "GM.CSF",
      "IFN.G",
      "IL.13"
    ), drop = FALSE]
    predictor_cols <- c("IL.17F", "GM.CSF", "IFN.G", "IL.13")
    plsr_df[, predictor_cols] <- plsr_df[, predictor_cols, drop = FALSE] + 1

    none_result <- cyt_plsr(
      plsr_df,
      response_col = "IL.10",
      predictor_cols = predictor_cols,
      group_col = "Group",
      comp_num = 2,
      sparse = FALSE,
      cv_opt = NULL,
      scale = "none"
    )
    null_result <- cyt_plsr(
      plsr_df,
      response_col = "IL.10",
      predictor_cols = predictor_cols,
      group_col = "Group",
      comp_num = 2,
      sparse = FALSE,
      cv_opt = NULL,
      scale = NULL
    )
    log10_result <- cyt_plsr(
      plsr_df,
      response_col = "IL.10",
      predictor_cols = predictor_cols,
      group_col = "Group",
      comp_num = 2,
      sparse = FALSE,
      cv_opt = NULL,
      scale = "log10"
    )
    zscore_result <- cyt_plsr(
      plsr_df,
      response_col = "IL.10",
      predictor_cols = predictor_cols,
      group_col = "Group",
      comp_num = 2,
      sparse = FALSE,
      cv_opt = NULL,
      scale = "zscore"
    )
    custom_result <- cyt_plsr(
      plsr_df,
      response_col = "IL.10",
      predictor_cols = predictor_cols,
      group_col = "Group",
      comp_num = 2,
      sparse = FALSE,
      cv_opt = NULL,
      scale = "custom",
      custom_fn = function(x) x + 1
    )

    for (result in list(log10_result, zscore_result, custom_result)) {
      expect_true(inherits(result$scores_plot, "recordedplot"))
      expect_true(inherits(result$pred_vs_obs, "recordedplot"))
      expect_true(inherits(result$residuals_plot, "recordedplot"))
      expect_length(result$loadings, 2)
      expect_length(result$vip_scores, 2)
      expect_true(inherits(first_non_null(result$vip_scores), "ggplot"))
    }

    expect_equal(
      first_non_null(none_result$vip_scores)$data,
      first_non_null(null_result$vip_scores)$data
    )
  })
})

test_that("cyt_plsr errors on non-positive predictor values for log scales", {
  plsr_df <- ex1_binary_group[, c(
    "Group",
    "IL.10",
    "IL.17F",
    "GM.CSF",
    "IFN.G",
    "IL.13"
  ), drop = FALSE]
  predictor_cols <- c("IL.17F", "GM.CSF", "IFN.G", "IL.13")
  plsr_df$GM.CSF[1] <- 0
  plsr_df$IFN.G[2] <- -1

  expect_error(
    cyt_plsr(
      plsr_df,
      response_col = "IL.10",
      predictor_cols = predictor_cols,
      group_col = "Group",
      comp_num = 2,
      sparse = FALSE,
      cv_opt = NULL,
      scale = "log2"
    ),
    "requires all non-missing selected values to be finite and greater than 0"
  )
  expect_error(
    cyt_plsr(
      plsr_df,
      response_col = "IL.10",
      predictor_cols = predictor_cols,
      group_col = "Group",
      comp_num = 2,
      sparse = FALSE,
      cv_opt = NULL,
      scale = "log10"
    ),
    "requires all non-missing selected values to be finite and greater than 0"
  )
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

test_that("cyt_rf supports multi-class RFCV, caret CV, and PDF output", {
  with_temp_pdf_device({
    model_features <- names(ex1_full)[vapply(ex1_full, is.numeric, logical(1))][2:8]
    rf_df <- ex1_full[, c("Group", model_features), drop = FALSE]

    rf_result <- cyt_rf(
      rf_df,
      group_col = "Group",
      ntree = 25,
      mtry = 2,
      plot_roc = FALSE,
      run_rfcv = TRUE,
      verbose = FALSE,
      cv = TRUE,
      cv_folds = 3,
      k_folds = 3,
      seed = 123
    )

    output_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(output_file), add = TRUE)
    pdf_result <- capture.output(
      cyt_rf(
        rf_df,
        group_col = "Group",
        ntree = 25,
        mtry = 2,
        plot_roc = FALSE,
        run_rfcv = FALSE,
        verbose = FALSE,
        cv = FALSE,
        seed = 123,
        output_file = output_file
      )
    )

    expect_false(is.null(rf_result$rfcv_data))
    expect_false(is.null(rf_result$cv_results))
    expect_null(rf_result$roc_plot)
    expect_true(file.exists(output_file))
    expect_true(any(grepl("RANDOM FOREST RESULTS", pdf_result, fixed = TRUE)))
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

test_that("cyt_xgb supports multi-class CV and PDF output branches", {
  with_temp_pdf_device({
    model_features <- names(ex1_full)[vapply(ex1_full, is.numeric, logical(1))][2:8]
    xgb_df <- ex1_full[, c("Group", model_features), drop = FALSE]

    xgb_result <- cyt_xgb(
      xgb_df,
      group_col = "Group",
      nrounds = 15,
      max_depth = 3,
      learning_rate = 0.1,
      cv = TRUE,
      nfold = 3,
      plot_roc = FALSE,
      print_results = FALSE,
      seed = 123
    )

    output_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(output_file), add = TRUE)
    pdf_result <- capture.output(
      cyt_xgb(
        xgb_df,
        group_col = "Group",
        nrounds = 15,
        max_depth = 3,
        learning_rate = 0.1,
        cv = FALSE,
        plot_roc = FALSE,
        print_results = FALSE,
        seed = 123,
        output_file = output_file
      )
    )

    expect_false(is.null(xgb_result$cv_results))
    expect_null(xgb_result$roc_plot)
    expect_equal(length(xgb_result$class_mapping), 3)
    expect_true(file.exists(output_file))
    expect_true(any(grepl("XGBOOST RESULTS", pdf_result, fixed = TRUE)))
  })
})
