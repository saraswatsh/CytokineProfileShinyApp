## ---------------------------
## Updating inputs by userState
## ---------------------------

shiny::observeEvent(currentStep(), {
  shiny::req(currentStep())
  if (currentStep() == 1) {
    # restore the Use built‑in” toggle
    shiny::updateCheckboxInput(
      session,
      "use_builtin",
      value = userState$use_builtin
    )
    if (isTRUE(userState$use_builtin) && !is.null(userState$built_in_choice)) {
      shiny::updateSelectInput(
        session,
        "built_in_choice",
        selected = userState$built_in_choice
      )
    }
  }
  if (currentStep() == 2) {
    # recompute the same choices used in step2UI()
    df <- userData()
    shiny::req(df)
    col_info <- step2_classify_columns(df)
    all_cols <- col_info$all
    categorical_cols <- col_info$categorical
    numerical_cols <- col_info$numerical

    cat_selected <- step2_restore_bucket_selection(
      userState$selected_columns,
      categorical_cols
    )
    num_selected <- step2_restore_bucket_selection(
      userState$selected_columns,
      numerical_cols
    )
    shiny::updateCheckboxGroupInput(
      session,
      "selected_categorical_cols",
      choices = categorical_cols,
      selected = cat_selected
    )
    shiny::updateCheckboxGroupInput(
      session,
      "selected_numerical_cols",
      choices = numerical_cols,
      selected = num_selected
    )
    # restore the preprocessing selector
    shiny::updateSelectInput(
      session,
      "step2_scale",
      selected = userState$step2_scale %||% "none"
    )
    shiny::updateSelectizeInput(
      session,
      "factor_cols",
      choices = all_cols,
      selected = intersect(userState$step2_factor_cols %||% character(0), all_cols)
    )
    shiny::updateSelectizeInput(
      session,
      "numeric_override_cols",
      choices = all_cols,
      selected = intersect(
        userState$step2_numeric_override_cols %||% character(0),
        all_cols
      )
    )
    shiny::updateCheckboxInput(
      session,
      "factor_order_enable",
      value = isTRUE(userState$step2_factor_order_enable)
    )
    factor_order_choices <- intersect(
      userState$step2_factor_cols %||% character(0),
      all_cols
    )
    factor_order_selected <- intersect(
      userState$step2_factor_order_col %||% character(0),
      factor_order_choices
    )
    if (!length(factor_order_selected) && length(factor_order_choices)) {
      factor_order_selected <- factor_order_choices[1]
    }
    shiny::updateSelectInput(
      session,
      "factor_order_col",
      choices = factor_order_choices,
      selected = factor_order_selected
    )
    shiny::updateTextInput(
      session,
      "factor_levels_csv",
      value = userState$step2_factor_levels_csv %||% ""
    )
  }
  if (currentStep() == 3) {
    if (!is.null(userState$analysis_categories)) {
      shiny::updateRadioButtons(
        session,
        "analysis_categories",
        selected = userState$analysis_categories
      )
    }
    # re‐select the dropdowns
    if (!is.null(userState$stat_function)) {
      shiny::updateSelectInput(
        session,
        "stat_function",
        selected = userState$stat_function
      )
    }
    if (!is.null(userState$exploratory_function)) {
      shiny::updateSelectInput(
        session,
        "exploratory_function",
        selected = userState$exploratory_function
      )
    }
    if (!is.null(userState$multivariate_function)) {
      shiny::updateSelectInput(
        session,
        "multivariate_function",
        selected = userState$multivariate_function
      )
    }
    if (!is.null(userState$ml_function)) {
      shiny::updateSelectInput(
        session,
        "ml_function",
        selected = userState$ml_function
      )
    }
  }
  if (currentStep() == 4 && !is.null(userState$selected_function)) {
    # Boxplots
    if (userState$selected_function == "Boxplots") {
      shiny::updateTextInput(session, "bp_y_lim", value = userState$bp_y_lim)
      shiny::updateNumericInput(
        session,
        "bp_bin_size",
        value = userState$bp_bin_size
      )
      shiny::updateSelectizeInput(
        session,
        "bp_group_by",
        selected = userState$bp_group_by
      )
    }

    if (userState$selected_function == "Violin Plots") {
      shiny::updateSelectizeInput(
        session,
        "vio_group_by",
        selected = userState$vio_group_by
      )
      shiny::updateNumericInput(
        session,
        "vio_bin_size",
        value = userState$vio_bin_size
      )
      shiny::updateTextInput(session, "vio_y_lim", value = userState$vio_y_lim)
      shiny::updateCheckboxInput(
        session,
        "vio_boxplot_overlay",
        value = userState$vio_boxplot_overlay
      )
    }
    if (userState$selected_function == "Univariate Tests (T-test, Wilcoxon)") {
      shiny::updateSelectInput(
        session,
        "uv2_method",
        selected = userState$uv2_method
      )
      shiny::updateSelectInput(
        session,
        "uv2_p_adjust_method",
        selected = userState$uv2_p_adjust_method %||% "BH"
      )
    }
    if (
      userState$selected_function ==
        "Multi-level Univariate Tests (Anova, Kruskal-Wallis)"
    ) {
      shiny::updateSelectInput(
        session,
        "uvm_method",
        selected = userState$uvm_method
      )
      shiny::updateSelectInput(
        session,
        "uvm_p_adjust_method",
        selected = userState$uvm_p_adjust_method %||% "BH"
      )
    }
    if (userState$selected_function == "Two-way ANOVA") {
      shiny::updateSelectInput(
        session,
        "twa_primary_cat_var",
        selected = userState$twa_primary_cat_var
      )
      shiny::updateSelectInput(
        session,
        "twa_secondary_cat_var",
        selected = userState$twa_secondary_cat_var
      )
      shiny::updateCheckboxInput(
        session,
        "twa_include_primary_secondary_interaction",
        value = userState$twa_include_primary_secondary_interaction
      )
    }
    if (userState$selected_function == "ANCOVA") {
      shiny::updateSelectInput(
        session,
        "anc_primary_cat_var",
        selected = userState$anc_primary_cat_var
      )
      shiny::updateSelectInput(
        session,
        "anc_secondary_cat_var",
        selected = userState$anc_secondary_cat_var
      )
      shiny::updateSelectInput(
        session,
        "anc_covariate_col",
        selected = userState$anc_covariate_col
      )
      shiny::updateCheckboxInput(
        session,
        "anc_include_primary_secondary_interaction",
        value = userState$anc_include_primary_secondary_interaction
      )
      shiny::updateCheckboxInput(
        session,
        "anc_include_primary_covariate_interaction",
        value = userState$anc_include_primary_covariate_interaction
      )
    }
    if (userState$selected_function == "Error-Bar Plot") {
      shiny::updateSelectInput(
        session,
        "eb_group_col",
        selected = userState$eb_group_col
      )
      shiny::updateCheckboxInput(
        session,
        "eb_p_lab",
        value = userState$eb_p_lab
      )
      shiny::updateCheckboxInput(
        session,
        "eb_es_lab",
        value = userState$eb_es_lab
      )
      shiny::updateCheckboxInput(
        session,
        "eb_class_symbol",
        value = userState$eb_class_symbol
      )
      shiny::updateTextInput(session, "eb_x_lab", value = userState$eb_x_lab)
      shiny::updateTextInput(session, "eb_y_lab", value = userState$eb_y_lab)
      shiny::updateTextInput(session, "eb_title", value = userState$eb_title)
      shiny::updateSelectInput(session, "eb_stat", selected = userState$eb_stat)
      shiny::updateSelectInput(
        session,
        "eb_error",
        selected = userState$eb_error
      )
      shiny::updateSelectInput(
        session,
        "eb_method",
        selected = userState$eb_method
      )
      shiny::updateSelectInput(
        session,
        "eb_p_adjust_method",
        selected = userState$eb_p_adjust_method %||% "BH"
      )
      shiny::updateNumericInput(
        session,
        "eb_n_col",
        value = userState$eb_n_col %||% 3
      )
      eb_fill_palette_selected <- userState$eb_fill_palette %||% "gray"
      if (identical(eb_fill_palette_selected, "grey")) {
        eb_fill_palette_selected <- "gray"
      }
      shiny::updateSelectInput(
        session,
        "eb_fill_palette",
        selected = eb_fill_palette_selected
      )
    }
    # Dual-Flashlight Plot
    if (userState$selected_function == "Dual-Flashlight Plot") {
      shiny::updateSelectInput(
        session,
        "df_group_var",
        selected = userState$df_group_var
      )
      shiny::updateSelectInput(
        session,
        "df_cond1",
        selected = userState$df_cond1
      )
      shiny::updateSelectInput(
        session,
        "df_cond2",
        selected = userState$df_cond2
      )
      shiny::updateNumericInput(
        session,
        "df_ssmd_thresh",
        value = userState$df_ssmd_thresh
      )
      shiny::updateNumericInput(
        session,
        "df_log2fc_thresh",
        value = userState$df_log2fc_thresh
      )
      shiny::updateNumericInput(
        session,
        "df_top_labels",
        value = userState$df_top_labels
      )
    }

    # Heatmap
    if (userState$selected_function == "Heatmap") {
      shiny::updateSelectInput(
        session,
        "hm_annotation",
        selected = userState$hm_annotation
      )
      shiny::updateSelectInput(
        session,
        "hm_ann_side",
        selected = userState$hm_ann_side
      )
    }

    # Principal Component Analysis (PCA)
    if (userState$selected_function == "Principal Component Analysis (PCA)") {
      shiny::updateSelectInput(
        session,
        "pca_group_col",
        selected = userState$pca_group_col
      )
      shiny::updateSelectInput(
        session,
        "pca_group_col2",
        selected = userState$pca_group_col2
      )
      shiny::updateNumericInput(
        session,
        "pca_comp_num",
        value = userState$pca_comp_num
      )
      shiny::updateCheckboxInput(
        session,
        "pca_ellipse",
        value = userState$pca_ellipse
      )
      shiny::updateSelectInput(
        session,
        "pca_style",
        selected = userState$pca_style
      )
      shiny::updateSelectizeInput(
        session,
        "pca_pch",
        selected = userState$pca_pch
      )
      shiny::updateSelectizeInput(
        session,
        "pca_colors",
        selected = userState$pca_colors
      )
    }
    # Partial Least Squares Regression (PLSR)
    if (
      userState$selected_function == "Partial Least Squares Regression (PLSR)"
    ) {
      shiny::updateSelectInput(
        session,
        "plsr_group_col",
        selected = userState$plsr_group_col
      )
      shiny::updateSelectInput(
        session,
        "plsr_response_col",
        selected = userState$plsr_response_col
      )
      shiny::updateSelectizeInput(
        session,
        "plsr_predictor_cols",
        selected = userState$plsr_predictor_cols
      )
      shiny::updateNumericInput(
        session,
        "plsr_comp_num",
        value = userState$plsr_comp_num
      )
      shiny::updateCheckboxInput(
        session,
        "plsr_sparse",
        value = userState$plsr_sparse
      )
      shiny::updateNumericInput(
        session,
        "plsr_keepX",
        value = userState$plsr_keepX
      )
      shiny::updateRadioButtons(
        session,
        "plsr_cv_opt",
        selected = userState$plsr_cv_opt
      )
      shiny::updateNumericInput(
        session,
        "plsr_fold_num",
        value = userState$plsr_fold_num
      )
      shiny::updateCheckboxInput(
        session,
        "plsr_ellipse",
        value = userState$plsr_ellipse
      )
      shiny::updateSelectizeInput(
        session,
        "plsr_colors",
        selected = userState$plsr_colors
      )
    }

    # Correlation Plots
    if (userState$selected_function == "Correlation Plots") {
      shiny::updateSelectInput(
        session,
        "corr_group_col",
        selected = userState$corr_group_col
      )
      shiny::updateSelectInput(
        session,
        "corr_target",
        selected = userState$corr_target
      )
      shiny::updateCheckboxInput(
        session,
        "corr_by_group",
        value = userState$corr_by_group
      )
    }

    # Random Forest
    if (userState$selected_function == "Random Forest") {
      shiny::updateSelectInput(
        session,
        "rf_group_col",
        selected = userState$rf_group_col
      )
      shiny::updateNumericInput(session, "rf_ntree", value = userState$rf_ntree)
      shiny::updateNumericInput(session, "rf_mtry", value = userState$rf_mtry)
      shiny::updateNumericInput(
        session,
        "rf_train_fraction",
        value = userState$rf_train_fraction
      )
      shiny::updateCheckboxInput(
        session,
        "rf_plot_roc",
        value = userState$rf_plot_roc
      )
      shiny::updateCheckboxInput(
        session,
        "rf_run_rfcv",
        value = userState$rf_run_rfcv
      )
      shiny::updateNumericInput(
        session,
        "rf_k_folds",
        value = userState$rf_k_folds
      )
      shiny::updateNumericInput(session, "rf_step", value = userState$rf_step)
    }

    # Skewness/Kurtosis
    if (userState$selected_function == "Skewness/Kurtosis") {
      shiny::updateSelectInput(
        session,
        "skku_group_cols",
        selected = userState$skku_group_cols
      )
      shiny::updateCheckboxInput(
        session,
        "skku_print_raw",
        value = userState$skku_print_raw
      )
      shiny::updateCheckboxInput(
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
      df_now <- shiny::isolate(data_after_filters())
      if (shiny::isTruthy(df_now)) {
        cand <- names(df_now)[!vapply(df_now, is.numeric, logical(1))]
        cand <- setdiff(cand, "..cyto_id..")
      } else {
        cand <- character(0)
      }
      shiny::updateSelectInput(
        session,
        "splsda_group_col",
        selected = userState$splsda_group_col
      )
      shiny::updateSelectInput(
        session,
        "splsda_group_col2",
        selected = userState$splsda_group_col2
      )
      shiny::updateCheckboxInput(
        session,
        "splsda_use_batch_corr",
        value = userState$splsda_use_batch_corr
      )
      shiny::updateSelectInput(
        session,
        "splsda_batch_col",
        selected = userState$splsda_batch_col
      )
      shiny::updateCheckboxInput(
        session,
        "splsda_use_multilevel",
        value = userState$splsda_use_multilevel
      )
      shiny::updateSelectInput(
        session,
        "splsda_multilevel",
        selected = userState$splsda_multilevel
      )
      shiny::updateNumericInput(
        session,
        "splsda_var_num",
        value = userState$splsda_var_num
      )
      shiny::updateRadioButtons(
        session,
        "splsda_cv_opt",
        selected = userState$splsda_cv_opt
      )
      shiny::updateNumericInput(
        session,
        "splsda_fold_num",
        value = userState$splsda_fold_num
      )
      shiny::updateNumericInput(
        session,
        "splsda_comp_num",
        value = userState$splsda_comp_num
      )
      shiny::updateRadioButtons(
        session,
        "splsda_ind_names_mode",
        selected = userState$splsda_ind_names_mode %||% "off"
      )
      shiny::updateSelectInput(
        session,
        "splsda_ind_names_col",
        choices = cand,
        selected = userState$splsda_ind_names_col %||%
          if (length(cand)) cand[1] else NULL
      )
      shiny::updateSelectizeInput(
        session,
        "splsda_pch",
        selected = userState$splsda_pch
      )
      shiny::updateSelectInput(
        session,
        "splsda_style",
        selected = userState$splsda_style
      )
      shiny::updateCheckboxInput(
        session,
        "splsda_roc",
        value = userState$splsda_roc
      )
      shiny::updateCheckboxInput(
        session,
        "splsda_ellipse",
        value = userState$splsda_ellipse
      )
      shiny::updateCheckboxInput(
        session,
        "splsda_bg",
        value = userState$splsda_bg
      )
      shiny::updateCheckboxInput(
        session,
        "splsda_conf_mat",
        value = userState$splsda_conf_mat
      )
      shiny::updateSelectizeInput(
        session,
        "splsda_colors",
        selected = userState$splsda_colors
      )
    }
    # Volcano Plot
    if (userState$selected_function == "Volcano Plot") {
      shiny::updateSelectInput(
        session,
        "volc_group_col",
        selected = userState$volc_group_col
      )
      shiny::updateSelectInput(
        session,
        "volc_cond1",
        selected = userState$volc_cond1
      )
      shiny::updateSelectInput(
        session,
        "volc_cond2",
        selected = userState$volc_cond2
      )
      shiny::updateNumericInput(
        session,
        "volc_fold_change_thresh",
        value = userState$volc_fold_change_thresh
      )
      shiny::updateNumericInput(
        session,
        "volc_p_value_thresh",
        value = userState$volc_p_value_thresh
      )
      shiny::updateNumericInput(
        session,
        "volc_top_labels",
        value = userState$volc_top_labels
      )
    }

    # Extreme Gradient Boosting (XGBoost)
    if (userState$selected_function == "Extreme Gradient Boosting (XGBoost)") {
      shiny::updateSelectInput(
        session,
        "xgb_group_col",
        selected = userState$xgb_group_col
      )
      shiny::updateNumericInput(
        session,
        "xgb_train_fraction",
        value = userState$xgb_train_fraction
      )
      shiny::updateNumericInput(
        session,
        "xgb_nrounds",
        value = userState$xgb_nrounds
      )
      shiny::updateNumericInput(
        session,
        "xgb_max_depth",
        value = userState$xgb_max_depth
      )
      shiny::updateNumericInput(session, "xgb_eta", value = userState$xgb_eta)
      shiny::updateSelectInput(
        session,
        "xgb_eval_metric",
        selected = userState$xgb_eval_metric
      )
      shiny::updateNumericInput(
        session,
        "xgb_top_n_features",
        value = userState$xgb_top_n_features
      )
      shiny::updateCheckboxInput(
        session,
        "xgb_plot_roc",
        value = userState$xgb_plot_roc
      )
      shiny::updateCheckboxInput(session, "xgb_cv", value = userState$xgb_cv)
      shiny::updateNumericInput(
        session,
        "xgb_nfold",
        value = userState$xgb_nfold
      )
    }

    font_spec <- get_analysis_font_spec(userState$selected_function)
    if (!is.null(font_spec)) {
      restore_font_settings_inputs(
        session = session,
        prefix = font_spec$prefix,
        supported_fields = font_spec$supported_fields,
        state = userState[[font_spec$state_key]],
        default_font_settings = font_spec$default_font_settings
      )
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
      bp_group_by = input$bp_group_by,
      bp_y_lim = input$bp_y_lim,

      # Violin Plots
      vio_group_by = input$vio_group_by,
      vio_bin_size = input$vio_bin_size,
      vio_y_lim = input$vio_y_lim,
      vio_boxplot_overlay = input$vio_boxplot_overlay,

      # Univariate Tests
      uv2_method = input$uv2_method,
      uv2_p_adjust_method = input$uv2_p_adjust_method,
      uvm_method = input$uvm_method,
      uvm_p_adjust_method = input$uvm_p_adjust_method,
      twa_primary_cat_var = input$twa_primary_cat_var,
      twa_secondary_cat_var = input$twa_secondary_cat_var,
      twa_include_primary_secondary_interaction =
        input$twa_include_primary_secondary_interaction,
      anc_primary_cat_var = input$anc_primary_cat_var,
      anc_secondary_cat_var = input$anc_secondary_cat_var,
      anc_covariate_col = input$anc_covariate_col,
      anc_include_primary_secondary_interaction =
        input$anc_include_primary_secondary_interaction,
      anc_include_primary_covariate_interaction =
        input$anc_include_primary_covariate_interaction,

      # Error-Bar Plot
      eb_group_col = input$eb_group_col,
      eb_p_lab = input$eb_p_lab,
      eb_es_lab = input$eb_es_lab,
      eb_class_symbol = input$eb_class_symbol,
      eb_x_lab = input$eb_x_lab,
      eb_y_lab = input$eb_y_lab,
      eb_title = input$eb_title,
      eb_stat = input$eb_stat,
      eb_error = input$eb_error,
      eb_method = input$eb_method,
      eb_p_adjust_method = input$eb_p_adjust_method,
      eb_n_col = input$eb_n_col,
      eb_fill_palette = input$eb_fill_palette,
      # Dual-Flashlight Plot
      df_group_var = input$df_group_var,
      df_cond1 = input$df_cond1,
      df_cond2 = input$df_cond2,
      df_ssmd_thresh = input$df_ssmd_thresh,
      df_log2fc_thresh = input$df_log2fc_thresh,
      df_top_labels = input$df_top_labels,

      # Heatmap
      hm_annotation = input$hm_annotation,
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
      plsr_predictor_cols = input$plsr_predictor_cols,
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
