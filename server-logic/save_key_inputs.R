  ## ---------------------------
  ## Save key inputs using shiny::reactiveValues
  ## ---------------------------
  shiny::observeEvent(
    input$selected_columns,
    {
      if (
        !is.null(input$selected_columns) && length(input$selected_columns) > 0
      ) {
        userState$selected_columns <- input$selected_columns
      }
    },
    ignoreNULL = TRUE
  )
  shiny::observeEvent(selected_function(), {
    userState$selected_function <- selected_function()
  })
  # For sheet name
  shiny::observeEvent(input$sheet_name, {
    userState$sheet_name <- input$sheet_name
  })
  # For Boxplots
  shiny::observeEvent(input$bp_bin_size, {
    userState$bp_bin_size <- input$bp_bin_size
  })
  shiny::observeEvent(input$bp_mf_row, {
    userState$bp_mf_row <- input$bp_mf_row
  })
  shiny::observeEvent(input$bp_y_lim, {
    userState$bp_y_lim <- input$bp_y_lim
  })
  # For Enhanced Boxplots
  shiny::observeEvent(input$bp2_mf_row, {
    userState$bp2_mf_row <- input$bp2_mf_row
  })
  shiny::observeEvent(input$bp2_y_lim, {
    userState$bp2_y_lim <- input$bp2_y_lim
  })
  # For Error-Bar Plot
  shiny::observeEvent(input$eb_group_col, {
    userState$eb_group_col <- input$eb_group_col
  })
  shiny::observeEvent(input$eb_p_lab, {
    userState$eb_p_lab <- input$eb_p_lab
  })
  shiny::observeEvent(input$eb_es_lab, {
    userState$eb_es_lab <- input$eb_es_lab
  })
  shiny::observeEvent(input$eb_class_symbol, {
    userState$eb_class_symbol <- input$eb_class_symbol
  })
  shiny::observeEvent(input$eb_x_lab, {
    userState$eb_x_lab <- input$eb_x_lab
  })
  shiny::observeEvent(input$eb_y_lab, {
    userState$eb_y_lab <- input$eb_y_lab
  })
  shiny::observeEvent(input$eb_title, {
    userState$eb_title <- input$eb_title
  })
  # For Dual-Flashlight Plot
  shiny::observeEvent(input$df_group_var, {
    userState$df_group_var <- input$df_group_var
  })
  shiny::observeEvent(input$df_cond1, {
    userState$df_cond1 <- input$df_cond1
  })
  shiny::observeEvent(input$df_cond2, {
    userState$df_cond2 <- input$df_cond2
  })
  shiny::observeEvent(input$df_ssmd_thresh, {
    userState$df_ssmd_thresh <- input$df_ssmd_thresh
  })
  shiny::observeEvent(input$df_log2fc_thresh, {
    userState$df_log2fc_thresh <- input$df_log2fc_thresh
  })
  shiny::observeEvent(input$df_top_labels, {
    userState$df_top_labels <- input$df_top_labels
  })
  # For Heatmap
  shiny::observeEvent(input$hm_annotation, {
    userState$hm_annotation <- input$hm_annotation
  })
  shiny::observeEvent(input$hm_scale, {
    userState$hm_scale <- input$hm_scale
  })
  shiny::observeEvent(input$hm_ann_side, {
    userState$hm_ann_side <- input$hm_ann_side
  })
  # For PCA
  shiny::observeEvent(input$pca_group_col, {
    userState$pca_group_col <- input$pca_group_col
  })
  shiny::observeEvent(input$pca_group_col2, {
    userState$pca_group_col2 <- input$pca_group_col2
  })
  shiny::observeEvent(input$pca_comp_num, {
    userState$pca_comp_num <- input$pca_comp_num
  })
  shiny::observeEvent(input$pca_ellipse, {
    userState$pca_ellipse <- input$pca_ellipse
  })
  shiny::observeEvent(input$pca_style, {
    userState$pca_style <- input$pca_style
  })
  shiny::observeEvent(input$pca_pch, {
    userState$pca_pch <- input$pca_pch
  })
  shiny::observeEvent(input$pca_colors, {
    userState$pca_colors <- input$pca_colors
  })
  # For Random Forest
  shiny::observeEvent(input$rf_group_col, {
    userState$rf_group_col <- input$rf_group_col
  })
  shiny::observeEvent(input$rf_ntree, {
    userState$rf_ntree <- input$rf_ntree
  })
  shiny::observeEvent(input$rf_mtry, {
    userState$rf_mtry <- input$rf_mtry
  })
  shiny::observeEvent(input$rf_train_fraction, {
    userState$rf_train_fraction <- input$rf_train_fraction
  })
  shiny::observeEvent(input$rf_plot_roc, {
    userState$rf_plot_roc <- input$rf_plot_roc
  })
  shiny::observeEvent(input$rf_run_rfcv, {
    userState$rf_run_rfcv <- input$rf_run_rfcv
  })
  shiny::observeEvent(input$rf_k_folds, {
    userState$rf_k_folds <- input$rf_k_folds
  })
  shiny::observeEvent(input$rf_step, {
    userState$rf_step <- input$rf_step
  })
  # For Skewness/Kurtosis
  shiny::observeEvent(input$skku_group_cols, {
    userState$skku_group_cols <- input$skku_group_cols
  })
  shiny::observeEvent(input$skku_print_raw, {
    userState$skku_print_raw <- input$skku_print_raw
  })
  shiny::observeEvent(input$skku_print_log, {
    userState$skku_print_log <- input$skku_print_log
  })

  # For PLSR
  shiny::observeEvent(input$plsr_response_var, {
    userState$plsr_response_var <- input$plsr_response_var
  })
  shiny::observeEvent(input$plsr_comp_num, {
    userState$plsr_comp_num <- input$plsr_comp_num
  })
  shiny::observeEvent(input$plsr_sparse, {
    userState$plsr_sparse <- input$plsr_sparse
  })
  shiny::observeEvent(input$plsr_keepX, {
    df <- filteredData()
    req(df)
    computed_default <- sum(sapply(df, is.numeric))
    # Mark as manually changed if the input does not equal the computed default.
    userState$plsr_keepX_manual <- (input$plsr_keepX != computed_default)
    # Also, update the stored value so it’s available if the user has modified it.
    userState$plsr_keepX <- input$plsr_keepX
  })
  shiny::observeEvent(input$plsr_comp_num, {
    userState$plsr_comp_num <- input$plsr_comp_num
  })
  shiny::observeEvent(input$plsr_cv_opt, {
    userState$plsr_cv_opt <- input$plsr_cv_opt
  })
  shiny::observeEvent(input$plsr_fold_num, {
    userState$plsr_fold_num <- input$plsr_fold_num
  })
  shiny::observeEvent(input$plsr_ellipse, {
    userState$plsr_ellipse <- input$plsr_ellipse
  })
  shiny::observeEvent(input$plsr_colors, {
    userState$plsr_colors <- input$plsr_colors
  })
  # For sPLS-DA
  shiny::observeEvent(input$splsda_group_col, {
    userState$splsda_group_col <- input$splsda_group_col
  })
  shiny::observeEvent(input$splsda_group_col2, {
    userState$splsda_group_col2 <- input$splsda_group_col2
  })
  shiny::observeEvent(input$splsda_use_batch_corr, {
    userState$splsda_use_batch_corr <- input$splsda_use_batch_corr
  })
  shiny::observeEvent(input$splsda_batch_col, {
    userState$splsda_batch_col <- input$splsda_batch_col
  })
  shiny::observeEvent(input$use_splsda_multilevel, {
    userState$splsda_multilevel <- input$use_splsda_multilevel
  })
  shiny::observeEvent(input$splsda_multilevel, {
    userState$splsda_multilevel <- input$splsda_multilevel
  })
  shiny::observeEvent(input$splsda_var_num, {
    df <- filteredData()
    req(df)
    computed_default <- sum(sapply(df, is.numeric))
    # Mark as manually changed if the input does not equal the computed default.
    userState$splsda_var_num_manual <- (input$splsda_var_num !=
      computed_default)
    # Also, update the stored value so it’s available if the user has modified it.
    userState$splsda_var_num <- input$splsda_var_num
  })

  shiny::observeEvent(input$splsda_cv_opt, {
    userState$splsda_cv_opt <- input$splsda_cv_opt
  })
  shiny::observeEvent(input$splsda_fold_num, {
    userState$splsda_fold_num <- input$splsda_fold_num
  })
  shiny::observeEvent(input$splsda_comp_num, {
    userState$splsda_comp_num <- input$splsda_comp_num
  })
  shiny::observeEvent(input$splsda_ind_names_mode, {
    userState$splsda_ind_names_mode <- input$splsda_ind_names_mode
  })
  shiny::observeEvent(input$splsda_ind_names_col, {
    userState$splsda_ind_names_col <- input$splsda_ind_names_col
  })
  shiny::observeEvent(input$splsda_pch, {
    userState$splsda_pch <- input$splsda_pch
  })
  shiny::observeEvent(input$splsda_style, {
    userState$splsda_style <- input$splsda_style
  })
  shiny::observeEvent(input$splsda_show3d_interactive, {
    showModal(modalDialog(
      plotlyOutput("splsda_interactive_plot", width = "100%", height = "600px"),
      easyClose = TRUE,
      size = "l"
    ))
  })
  shiny::observeEvent(input$splsda_show3d_interactive_vip, {
    showModal(modalDialog(
      plotlyOutput(
        "splsda_interactive_plot_vip",
        width = "100%",
        height = "600px"
      ),
      easyClose = TRUE,
      size = "l"
    ))
  })
  shiny::observeEvent(input$splsda_roc, {
    userState$splsda_roc <- input$splsda_roc
  })
  shiny::observeEvent(input$splsda_ellipse, {
    userState$splsda_ellipse <- input$splsda_ellipse
  })
  shiny::observeEvent(input$splsda_bg, {
    userState$splsda_bg <- input$splsda_bg
  })
  shiny::observeEvent(input$splsda_conf_mat, {
    userState$splsda_conf_mat <- input$splsda_conf_mat
  })
  shiny::observeEvent(input$splsda_colors, {
    userState$splsda_colors <- input$splsda_colors
  })
  # For MINT sPLS-DA
  shiny::observeEvent(input$mint_splsda_group_col, {
    userState$mint_splsda_group_col <- input$mint_splsda_group_col
  })
  shiny::observeEvent(input$mint_splsda_group_col2, {
    userState$mint_splsda_group_col2 <- input$mint_splsda_group_col2
  })
  shiny::observeEvent(input$mint_splsda_batch_col, {
    userState$mint_splsda_batch_col <- input$mint_splsda_batch_col
  })
  shiny::observeEvent(input$mint_splsda_var_num, {
    df <- filteredData()
    req(df)
    computed_default <- sum(sapply(df, is.numeric))
    # Mark as manually changed if the input does not equal the computed default.
    userState$mint_splsda_var_num_manual <- (input$mint_splsda_var_num !=
      computed_default)
    # Also, update the stored value so it’s available if the user has modified it.
    userState$mint_splsda_var_num <- input$mint_splsda_var_num
  })
  shiny::observeEvent(input$mint_splsda_comp_num, {
    userState$mint_splsda_comp_num <- input$mint_splsda_comp_num
  })
  shiny::observeEvent(input$mint_splsda_cim, {
    userState$mint_splsda_cim <- input$mint_splsda_cim
  })
  shiny::observeEvent(input$mint_splsda_ellipse, {
    userState$mint_splsda_ellipse <- input$mint_splsda_ellipse
  })
  shiny::observeEvent(input$mint_splsda_bg, {
    userState$mint_splsda_bg <- input$mint_splsda_bg
  })
  shiny::observeEvent(input$mint_splsda_roc, {
    userState$mint_splsda_roc <- input$mint_splsda_roc
  })
  shiny::observeEvent(input$mint_splsda_colors, {
    userState$mint_splsda_colors <- input$mint_splsda_colors
  })
  # For Volcano Plot
  shiny::observeEvent(input$volc_group_col, {
    userState$volc_group_col <- input$volc_group_col
  })
  shiny::observeEvent(input$volc_cond1, {
    userState$volc_cond1 <- input$volc_cond1
  })
  shiny::observeEvent(input$volc_cond2, {
    userState$volc_cond2 <- input$volc_cond2
  })
  shiny::observeEvent(input$volc_fold_change_thresh, {
    userState$volc_fold_change_thresh <- input$volc_fold_change_thresh
  })
  shiny::observeEvent(input$volc_p_value_thresh, {
    userState$volc_p_value_thresh <- input$volc_p_value_thresh
  })
  shiny::observeEvent(input$volc_top_labels, {
    userState$volc_top_labels <- input$volc_top_labels
  })
  # For XGBoost
  shiny::observeEvent(input$xgb_group_col, {
    userState$xgb_group_col <- input$xgb_group_col
  })
  shiny::observeEvent(input$xgb_train_fraction, {
    userState$xgb_train_fraction <- input$xgb_train_fraction
  })
  shiny::observeEvent(input$xgb_nrounds, {
    userState$xgb_nrounds <- input$xgb_nrounds
  })
  shiny::observeEvent(input$xgb_max_depth, {
    userState$xgb_max_depth <- input$xgb_max_depth
  })
  shiny::observeEvent(input$xgb_eta, {
    userState$xgb_eta <- input$xgb_eta
  })
  shiny::observeEvent(input$xgb_nfold, {
    userState$xgb_nfold <- input$xgb_nfold
  })
  shiny::observeEvent(input$xgb_cv, {
    userState$xgb_cv <- input$xgb_cv
  })
  shiny::observeEvent(input$xgb_eval_metric, {
    userState$xgb_eval_metric <- input$xgb_eval_metric
  })
  shiny::observeEvent(input$xgb_top_n_features, {
    userState$xgb_top_n_features <- input$xgb_top_n_features
  })
  shiny::observeEvent(input$xgb_plot_roc, {
    userState$xgb_plot_roc <- input$xgb_plot_roc
  })

  # For Correlation Analysis
  shiny::observeEvent(input$corr_target, {
    userState$corr_target <- input$corr_target
  })
  shiny::observeEvent(input$corr_group_col, {
    userState$corr_group_col <- input$corr_group_col
  })
  shiny::observeEvent(input$corr_by_group, {
    userState$corr_by_group <- input$corr_by_group
  })

  # remember which categories the user checked
  shiny::observeEvent(
    input$analysis_categories,
    {
      userState$analysis_categories <- input$analysis_categories
    },
    ignoreNULL = FALSE
  )
  # remember which function‐picker they chose
  shiny::observeEvent(input$stat_function, {
    userState$stat_function <- input$stat_function
  })
  shiny::observeEvent(input$exploratory_function, {
    userState$exploratory_function <- input$exploratory_function
  })
  shiny::observeEvent(input$multivariate_function, {
    userState$multivariate_function <- input$multivariate_function
  })
  shiny::observeEvent(input$ml_function, {
    userState$ml_function <- input$ml_function
  })
  shiny::observeEvent(
    input$built_in_choice,
    {
      userState$built_in_choice <- input$built_in_choice
    },
    ignoreNULL = FALSE
  )

  shiny::observeEvent(input$menu_ANOVA, {
    selected_function("ANOVA")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_t_test, {
    selected_function("Two-Sample T-Test")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_boxplots, {
    selected_function("Boxplots")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_enhanced_boxplots, {
    selected_function("Enhanced Boxplots")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_errorbp, {
    selected_function("Error-Bar Plot")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_dualflash, {
    selected_function("Dual-Flashlight Plot")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_volcano, {
    selected_function("Volcano Plot")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_heatmap, {
    selected_function("Heatmap")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_correlation, {
    selected_function("Correlation Plots")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_PCA, {
    selected_function("Principal Component Analysis (PCA)")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_PLSR, {
    selected_function("Partial Least Squares Regression (PLSR)")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_splsda, {
    selected_function(
      "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
    )
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_mint_splsda, {
    selected_function(
      "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
    )
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_skewkurt, {
    selected_function("Skewness/Kurtosis")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_rf, {
    selected_function("Random Forest")
    currentPage("step4")
    currentStep(4)
  })
  shiny::observeEvent(input$menu_xgb, {
    selected_function("Extreme Gradient Boosting (XGBoost)")
    currentPage("step4")
    currentStep(4)
  })
