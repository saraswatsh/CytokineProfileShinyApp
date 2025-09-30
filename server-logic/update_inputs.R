  ## ---------------------------
  ## Updating inputs by userState
  ## ---------------------------

  shiny::observeEvent(currentStep(), {
    req(currentStep(), !is.null(selected_function()))
    if (currentStep() == 1) {
      # restore the Use built‑in” toggle
      updateCheckboxInput(session, "use_builtin", value = userState$use_builtin)
      if (
        isTRUE(userState$use_builtin) && !is.null(userState$built_in_choice)
      ) {
        updateSelectInput(
          session,
          "built_in_choice",
          selected = userState$built_in_choice
        )
      }
    }
    if (currentStep() == 2) {
      # recompute the same choices used in step2UI()
      df <- data_after_filters()
      all_cols <- names(df)
      is_numeric_col <- vapply(df, is.numeric, logical(1))
      categorical_cols <- all_cols[!is_numeric_col]
      numerical_cols <- all_cols[is_numeric_col]

      # restore what they had selected
      if (!is.null(userState$selected_categorical_cols)) {
        updateCheckboxGroupInput(
          session,
          "selected_categorical_cols",
          choices = categorical_cols,
          selected = intersect(
            userState$selected_categorical_cols,
            categorical_cols
          )
        )
      }
      if (!is.null(userState$selected_numerical_cols)) {
        updateCheckboxGroupInput(
          session,
          "selected_numerical_cols",
          choices = numerical_cols,
          selected = intersect(
            userState$selected_numerical_cols,
            numerical_cols
          )
        )
      }
      # also restore the log2 box
      updateCheckboxInput(
        session,
        "step2_log2",
        value = userState$step2_log2
      )
    }
    if (currentStep() == 3 && !is.null(userState$selected_function)) {
      if (!is.null(userState$analysis_categories)) {
        updateRadioButtons(
          session,
          "analysis_categories",
          selected = userState$analysis_categories
        )
      }
      # re‐select the dropdowns
      if (!is.null(userState$stat_function)) {
        updateSelectInput(
          session,
          "stat_function",
          selected = userState$stat_function
        )
      }
      if (!is.null(userState$exploratory_function)) {
        updateSelectInput(
          session,
          "exploratory_function",
          selected = userState$exploratory_function
        )
      }
      if (!is.null(userState$multivariate_function)) {
        updateSelectInput(
          session,
          "multivariate_function",
          selected = userState$multivariate_function
        )
      }
      if (!is.null(userState$ml_function)) {
        updateSelectInput(
          session,
          "ml_function",
          selected = userState$ml_function
        )
      }
      # Boxplots
      if (userState$selected_function == "Boxplots") {
        updateSelectInput(session, "bp_mf_row", selected = userState$bp_mf_row)
        updateTextInput(session, "bp_y_lim", value = userState$bp_y_lim)
        updateNumericInput(
          session,
          "bp_bin_size",
          value = userState$bp_bin_size
        )
      }

      # Enhanced Boxplots
      if (userState$selected_function == "Enhanced Boxplots") {
        updateTextInput(session, "bp2_mf_row", value = userState$bp2_mf_row)
        updateTextInput(session, "bp2_y_lim", value = userState$bp2_y_lim)
      }
      if (userState$selected_function == "Error-Bar Plot") {
        updateSelectInput(
          session,
          "eb_group_col",
          selected = userState$eb_group_col
        )
        updateCheckboxInput(session, "eb_p_lab", value = userState$eb_p_lab)
        updateCheckboxInput(session, "eb_es_lab", value = userState$eb_es_lab)
        updateCheckboxInput(
          session,
          "eb_class_symbol",
          value = userState$eb_class_symbol
        )
        updateTextInput(session, "eb_x_lab", value = userState$eb_x_lab)
        updateTextInput(session, "eb_y_lab", value = userState$eb_y_lab)
        updateTextInput(session, "eb_title", value = userState$eb_title)
      }
      # Dual-Flashlight Plot
      if (userState$selected_function == "Dual-Flashlight Plot") {
        updateSelectInput(
          session,
          "df_group_var",
          selected = userState$df_group_var
        )
        updateSelectInput(
          session,
          "df_cond1",
          selected = userState$df_cond1
        )
        updateSelectInput(
          session,
          "df_cond2",
          selected = userState$df_cond2
        )
        updateNumericInput(
          session,
          "df_ssmd_thresh",
          value = userState$df_ssmd_thresh
        )
        updateNumericInput(
          session,
          "df_log2fc_thresh",
          value = userState$df_log2fc_thresh
        )
        updateNumericInput(
          session,
          "df_top_labels",
          value = userState$df_top_labels
        )
      }

      # Heatmap
      if (userState$selected_function == "Heatmap") {
        updateSelectInput(
          session,
          "hm_annotation",
          selected = userState$hm_annotation
        )
        updateSelectInput(session, "hm_scale", selected = userState$hm_scale)
        updateSelectInput(
          session,
          "hm_ann_side",
          selected = userState$hm_ann_side
        )
      }

      # Principal Component Analysis (PCA)
      if (userState$selected_function == "Principal Component Analysis (PCA)") {
        updateSelectInput(
          session,
          "pca_group_col",
          selected = userState$pca_group_col
        )
        updateSelectInput(
          session,
          "pca_group_col2",
          selected = userState$pca_group_col2
        )
        updateNumericInput(
          session,
          "pca_comp_num",
          value = userState$pca_comp_num
        )
        updateCheckboxInput(
          session,
          "pca_ellipse",
          value = userState$pca_ellipse
        )
        updateSelectInput(session, "pca_style", selected = userState$pca_style)
        updateSelectizeInput(session, "pca_pch", selected = userState$pca_pch)
        updateSelectizeInput(
          session,
          "pca_colors",
          selected = userState$pca_colors
        )
      }
      # Partial Least Squares Regression (PLSR)
      if (
        userState$selected_function == "Partial Least Squares Regression (PLSR)"
      ) {
        updateSelectInput(
          session,
          "plsr_group_col",
          selected = userState$plsr_group_col
        )
        updateSelectInput(
          session,
          "plsr_response_col",
          selected = userState$plsr_response_col
        )
        updateNumericInput(
          session,
          "plsr_comp_num",
          value = userState$plsr_comp_num
        )
        updateCheckboxInput(
          session,
          "plsr_sparse",
          value = userState$plsr_sparse
        )
        updateNumericInput(
          session,
          "plsr_keepX",
          value = userState$plsr_keepX
        )
        updateCheckboxInput(
          session,
          "plsr_cv_opt",
          value = userState$plsr_cv_opt
        )
        updateNumericInput(
          session,
          "plsr_fold_num",
          value = userState$plsr_fold_num
        )
        updateCheckboxInput(
          session,
          "plsr_ellipse",
          value = userState$plsr_ellipse
        )
        updateSelectizeInput(
          session,
          "plsr_colors",
          selected = userState$plsr_colors
        )
      }

      # Correlation Plots
      if (userState$selected_function == "Correlation Plots") {
        updateSelectInput(
          session,
          "corr_group_col",
          selected = userState$corr_group_col
        )
        updateSelectInput(
          session,
          "corr_target",
          selected = userState$corr_target
        )
        updateCheckboxInput(
          session,
          "corr_by_group",
          value = userState$corr_by_group
        )
      }

      # Random Forest
      if (userState$selected_function == "Random Forest") {
        updateSelectInput(
          session,
          "rf_group_col",
          selected = userState$rf_group_col
        )
        updateNumericInput(session, "rf_ntree", value = userState$rf_ntree)
        updateNumericInput(session, "rf_mtry", value = userState$rf_mtry)
        updateNumericInput(
          session,
          "rf_train_fraction",
          value = userState$rf_train_fraction
        )
        updateCheckboxInput(
          session,
          "rf_plot_roc",
          value = userState$rf_plot_roc
        )
        updateCheckboxInput(
          session,
          "rf_run_rfcv",
          value = userState$rf_run_rfcv
        )
        updateNumericInput(session, "rf_k_folds", value = userState$rf_k_folds)
        updateNumericInput(session, "rf_step", value = userState$rf_step)
      }

      # Skewness/Kurtosis
      if (userState$selected_function == "Skewness/Kurtosis") {
        updateSelectInput(
          session,
          "skku_group_cols",
          selected = userState$skku_group_cols
        )
        updateCheckboxInput(
          session,
          "skku_print_raw",
          value = userState$skku_print_raw
        )
        updateCheckboxInput(
          session,
          "skku_print_log",
          value = userState$skku_print_log
        )
      }

      # Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)
      if (
        userState$selected_function ==
          "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
      ) {
        df_now <- isolate(data_after_filters())
        if (isTruthy(df_now)) {
          cand <- names(df_now)[!vapply(df_now, is.numeric, logical(1))]
          cand <- setdiff(cand, "..cyto_id..")
        } else {
          cand <- character(0)
        }
        updateSelectInput(
          session,
          "splsda_group_col",
          selected = userState$splsda_group_col
        )
        updateSelectInput(
          session,
          "splsda_group_col2",
          selected = userState$splsda_group_col2
        )
        updateCheckboxInput(
          session,
          "splsda_use_batch_corr",
          value = userState$splsda_use_batch_corr
        )
        updateSelectInput(
          session,
          "splsda_batch_col",
          selected = userState$splsda_batch_col
        )
        updateCheckboxInput(
          session,
          "splsda_use_multilevel",
          value = userState$splsda_use_multilevel
        )
        updateSelectInput(
          session,
          "splsda_multilevel",
          selected = userState$splsda_multilevel
        )
        updateNumericInput(
          session,
          "splsda_var_num",
          value = userState$splsda_var_num
        )
        updateCheckboxInput(
          session,
          "splsda_cv_opt",
          value = userState$splsda_cv_opt
        )
        updateNumericInput(
          session,
          "splsda_fold_num",
          value = userState$splsda_fold_num
        )
        updateNumericInput(
          session,
          "splsda_comp_num",
          value = userState$splsda_comp_num
        )
        updateRadioButtons(
          session,
          "splsda_ind_names_mode",
          selected = userState$splsda_ind_names_mode %||% "off"
        )
        updateSelectInput(
          session,
          "splsda_ind_names_col",
          choices = cand,
          selected = userState$splsda_ind_names_col %||%
            if (length(cand)) cand[1] else NULL
        )
        updateSelectizeInput(
          session,
          "splsda_pch",
          selected = userState$splsda_pch
        )
        updateSelectInput(
          session,
          "splsda_style",
          selected = userState$splsda_style
        )
        updateCheckboxInput(session, "splsda_roc", value = userState$splsda_roc)
        updateCheckboxInput(
          session,
          "splsda_ellipse",
          value = userState$splsda_ellipse
        )
        updateCheckboxInput(session, "splsda_bg", value = userState$splsda_bg)
        updateCheckboxInput(
          session,
          "splsda_conf_mat",
          value = userState$splsda_conf_mat
        )
        updateSelectizeInput(
          session,
          "splsda_colors",
          selected = userState$splsda_colors
        )
      }
      # Volcano Plot
      if (userState$selected_function == "Volcano Plot") {
        updateSelectInput(
          session,
          "volc_group_col",
          selected = userState$volc_group_col
        )
        updateSelectInput(
          session,
          "volc_cond1",
          selected = userState$volc_cond1
        )
        updateSelectInput(
          session,
          "volc_cond2",
          selected = userState$volc_cond2
        )
        updateNumericInput(
          session,
          "volc_fold_change_thresh",
          value = userState$volc_fold_change_thresh
        )
        updateNumericInput(
          session,
          "volc_p_value_thresh",
          value = userState$volc_p_value_thresh
        )
        updateNumericInput(
          session,
          "volc_top_labels",
          value = userState$volc_top_labels
        )
      }

      # Extreme Gradient Boosting (XGBoost)
      if (
        userState$selected_function == "Extreme Gradient Boosting (XGBoost)"
      ) {
        updateSelectInput(
          session,
          "xgb_group_col",
          selected = userState$xgb_group_col
        )
        updateNumericInput(
          session,
          "xgb_train_fraction",
          value = userState$xgb_train_fraction
        )
        updateNumericInput(
          session,
          "xgb_nrounds",
          value = userState$xgb_nrounds
        )
        updateNumericInput(
          session,
          "xgb_max_depth",
          value = userState$xgb_max_depth
        )
        updateNumericInput(session, "xgb_eta", value = userState$xgb_eta)
        updateSelectInput(
          session,
          "xgb_eval_metric",
          selected = userState$xgb_eval_metric
        )
        updateNumericInput(
          session,
          "xgb_top_n_features",
          value = userState$xgb_top_n_features
        )
        updateCheckboxInput(
          session,
          "xgb_plot_roc",
          value = userState$xgb_plot_roc
        )
        updateCheckboxInput(session, "xgb_cv", value = userState$xgb_cv)
        updateNumericInput(session, "xgb_nfold", value = userState$xgb_nfold)
      }
    }
  })

  analysis_inputs <- shiny::reactive({
    list(
      # The function name
      func_name = selected_function(),
      # Data
      df = filteredData(),
      args = list(
        # Boxplots
        bp_bin_size = input$bp_bin_size,
        bp_mf_row = input$bp_mf_row,
        bp_y_lim = input$bp_y_lim,

        # Enhanced Boxplots
        bp2_mf_row = input$bp2_mf_row,
        bp2_y_lim = input$bp2_y_lim,

        # Error-Bar Plot
        eb_group_col = input$eb_group_col,
        eb_p_lab = input$eb_p_lab,
        eb_es_lab = input$eb_es_lab,
        eb_class_symbol = input$eb_class_symbol,
        eb_x_lab = input$eb_x_lab,
        eb_y_lab = input$eb_y_lab,
        eb_title = input$eb_title,

        # Dual-Flashlight Plot
        df_group_var = input$df_group_var,
        df_cond1 = input$df_cond1,
        df_cond2 = input$df_cond2,
        df_ssmd_thresh = input$df_ssmd_thresh,
        df_log2fc_thresh = input$df_log2fc_thresh,
        df_top_labels = input$df_top_labels,

        # Heatmap
        hm_annotation = input$hm_annotation,
        hm_scale = input$hm_scale,
        hm_ann_side = input$hm_ann_side,

        # Correlation Plots
        corr_group_col = input$corr_group_col,
        corr_target = input$corr_target,
        corr_by_group = input$corr_by_group,

        # PCA
        pca_group_col = input$pca_group_col,
        pca_group_col2 = input$pca_group_col2,
        pca_comp_num = input$pca_comp_num,
        pca_ellipse = input$pca_ellipse,
        pca_style = input$pca_style,
        pca_pch = input$pca_pch,
        pca_colors = input$pca_colors,

        # PLSR
        plsr_group_col = input$plsr_group_col,
        plsr_response_col = input$plsr_response_col,
        plsr_comp_num = input$plsr_comp_num,
        plsr_sparse = input$plsr_sparse,
        plsr_keepX = input$plsr_keepX,
        plsr_cv_opt = input$plsr_cv_opt,
        plsr_fold_num = input$plsr_fold_num,
        plsr_ellipse = input$plsr_ellipse,
        plsr_colors = input$plsr_colors,

        # Random Forest
        rf_group_col = input$rf_group_col,
        rf_ntree = input$rf_ntree,
        rf_mtry = input$rf_mtry,
        rf_train_fraction = input$rf_train_fraction,
        rf_plot_roc = input$rf_plot_roc,
        rf_run_rfcv = input$rf_run_rfcv,
        rf_k_folds = input$rf_k_folds,
        rf_step = input$rf_step,

        # Skewness/Kurtosis
        skku_group_cols = input$skku_group_cols,
        skku_print_raw = input$skku_print_raw,
        skku_print_log = input$skku_print_log,

        # sPLS-DA
        splsda_group_col = input$splsda_group_col,
        splsda_group_col2 = input$splsda_group_col2,
        splsda_use_batch_corr = input$splsda_use_batch_corr,
        splsda_batch_col = input$splsda_batch_col,
        splsda_use_multilevel = input$splsda_use_multilevel,
        splsda_multilevel = input$splsda_multilevel,
        splsda_var_num = input$splsda_var_num,
        splsda_cv_opt = input$splsda_cv_opt,
        splsda_fold_num = input$splsda_fold_num,
        splsda_comp_num = input$splsda_comp_num,
        splsda_pch = input$splsda_pch,
        splsda_ind_names_mode = input$splsda_ind_names_mode,
        splsda_ind_names_col = input$splsda_ind_names_col,
        splsda_style = input$splsda_style,
        splsda_roc = input$splsda_roc,
        splsda_ellipse = input$splsda_ellipse,
        splsda_bg = input$splsda_bg,
        splsda_conf_mat = input$splsda_conf_mat,
        splsda_colors = input$splsda_colors,

        # MINT sPLS-DA
        mint_splsda_group_col = input$mint_splsda_group_col,
        mint_splsda_group_col2 = input$mint_splsda_group_col2,
        mint_splsda_batch_col = input$mint_splsda_batch_col,
        mint_splsda_var_num = input$mint_splsda_var_num,
        mint_splsda_comp_num = input$mint_splsda_comp_num,
        mint_splsda_cim = input$mint_splsda_cim,
        mint_splsda_ellipse = input$mint_splsda_ellipse,
        mint_splsda_bg = input$mint_splsda_bg,
        mint_splsda_roc = input$mint_splsda_roc,
        mint_splsda_colors = input$mint_splsda_colors,

        # Volcano Plot
        volc_group_col = input$volc_group_col,
        volc_cond1 = input$volc_cond1,
        volc_cond2 = input$volc_cond2,
        volc_fold_change_thresh = input$volc_fold_change_thresh,
        volc_p_value_thresh = input$volc_p_value_thresh,
        volc_top_labels = input$volc_top_labels,

        # XGBoost
        xgb_group_col = input$xgb_group_col,
        xgb_train_fraction = input$xgb_train_fraction,
        xgb_nrounds = input$xgb_nrounds,
        xgb_max_depth = input$xgb_max_depth,
        xgb_eta = input$xgb_eta,
        xgb_nfold = input$xgb_nfold,
        xgb_cv = input$xgb_cv,
        xgb_eval_metric = input$xgb_eval_metric,
        xgb_top_n_features = input$xgb_top_n_features,
        xgb_plot_roc = input$xgb_plot_roc
      )
    )
  })

