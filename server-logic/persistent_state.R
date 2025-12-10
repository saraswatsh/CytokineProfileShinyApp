## ---------------------------
## Persistent State: Create a shiny::reactiveValues object
## ---------------------------
selected_function <- shiny::reactiveVal(NULL)
deleted_row_ids <- shiny::reactiveVal(character())
imputed_data <- shiny::reactiveVal(NULL)

userState <- shiny::reactiveValues(
  # General state
  selected_columns = NULL,
  # Built=in Data built‑in tracking:
  use_builtin = FALSE,
  built_in_choice = NULL,

  # Bioplex sheet tracking
  sheet_name = NULL,
  # Imputation
  impute_meta = NULL,

  # Step 2 log2 transformation checkbox
  step2_log2 = FALSE,

  # Boxplots options
  bp_bin_size = NULL,
  bp_mf_row = NULL,
  bp_y_lim = NULL,

  # Enhanced Boxplots options
  bp2_mf_row = NULL,
  bp2_y_lim = NULL,

  # Error-Bar Plot
  eb_group_col = NULL,
  eb_p_lab = NULL,
  eb_es_lab = NULL,
  eb_class_symbol = NULL,
  eb_x_lab = NULL,
  eb_y_lab = NULL,
  eb_title = NULL,

  # Dual-Flashlight Plot options
  df_group_var = NULL,
  df_cond1 = NULL,
  df_cond2 = NULL,
  df_ssmd_thresh = NULL,
  df_log2fc_thresh = NULL,
  df_top_labels = NULL,

  # Heatmap options
  hm_annotation = NULL,
  hm_scale = NULL,
  hm_ann_side = NULL,

  # PCA options
  pca_group_col = NULL,
  pca_group_col2 = NULL,
  pca_comp_num = NULL,
  pca_ellipse = NULL,
  pca_style = NULL,
  pca_pch = NULL,
  pca_colors = NULL,

  # Random Forest options
  rf_group_col = NULL,
  rf_ntree = NULL,
  rf_mtry = NULL,
  rf_train_fraction = NULL,
  rf_plot_roc = NULL,
  rf_run_rfcv = NULL,
  rf_k_folds = NULL,
  rf_step = NULL,

  # PLSR options
  plsr_group_col = NULL,
  plsr_response_col = NULL,
  plsr_comp_num = NULL,
  plsr_keepX = NULL,
  plsr_keepX_manual = FALSE,
  plsr_sparse = FALSE,
  plsr_cv_opt = NULL,
  plsr_fold_num = NULL,
  plsr_ellipse = NULL,
  plsr_colors = NULL,

  # Skewness/Kurtosis options
  skku_group_cols = NULL,
  skku_print_raw = NULL,
  skku_print_log = NULL,

  # sPLS-DA options
  splsda_group_col = NULL,
  splsda_group_col2 = NULL,
  spsda_batch_col = NULL,
  splsda_var_num = NULL,
  splsda_var_num_manual = FALSE,
  splsda_cv_opt = NULL,
  splsda_fold_num = NULL,
  splsda_comp_num = NULL,
  splsda_pch = NULL,
  splsda_ind_names_mode = NULL,
  splsda_ind_names_col = NULL,
  splsda_style = NULL,
  splsda_roc = NULL,
  splsda_ellipse = NULL,
  splsda_bg = NULL,
  splsda_conf_mat = NULL,
  splsda_colors = NULL,
  splsda_multilevel = NULL,
  splsda_use_batch_corr = FALSE,
  splsda_batch_col = NULL,
  splsda_fontsize = NULL,

  # MINT sPLS-DA options
  mint_splsda_group_col = NULL,
  mint_splsda_group_col2 = NULL,
  mint_splsda_batch_col = NULL,
  mint_splsda_var_num = NULL,
  mint_splsda_var_num_manual = FALSE,
  mint_splsda_comp_num = NULL,
  mint_splsda_cim = NULL,
  mint_splsda_ellipse = NULL,
  mint_splsda_bg = NULL,
  mint_splsda_roc = NULL,
  mint_splsda_colors = NULL,

  # Volcano Plot options
  volc_group_col = NULL,
  volc_cond1 = NULL,
  volc_cond2 = NULL,
  volc_fold_change_thresh = NULL,
  volc_p_value_thresh = NULL,
  volc_top_labels = NULL,

  # XGBoost options
  xgb_group_col = NULL,
  xgb_train_fraction = NULL,
  xgb_nrounds = NULL,
  xgb_max_depth = NULL,
  xgb_eta = NULL,
  xgb_nfold = NULL,
  xgb_cv = NULL,
  xgb_eval_metric = NULL,
  xgb_top_n_features = NULL,
  xgb_plot_roc = NULL,

  # Correlation options
  corr_target = NULL,
  corr_group_col = NULL,
  corr_by_group = FALSE
)
# --- Bio-Plex import state ---
bioplex <- shiny::reactiveValues(
  active = FALSE, # when TRUE, userData() will use the confirmed Bio-Plex data
  df = NULL, # working table (while editing)
  final = NULL, # <— persisted dataset after “Save & Use”
  editor_mode = "sheets", # <— "sheets" or "persisted"
  deleted_idx = integer(),
  deleted_by_sheet = list(),
  user_columns = character(0)
)

# Reactive values to store the selection from our new custom buttons
selected_stat_func <- shiny::reactiveVal("ANOVA")
selected_exploratory_func <- shiny::reactiveVal("Boxplots")
selected_multivariate_func <- shiny::reactiveVal(
  "Principal Component Analysis (PCA)"
)
selected_ml_func <- shiny::reactiveVal("Random Forest")
