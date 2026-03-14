## ---------------------------
## Analysis and Results
## ---------------------------
errorMessage <- shiny::reactiveVal(NULL)
warningMessage <- shiny::reactiveVal(character())

parse_numeric_input <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(NULL)
  }

  as.numeric(trimws(strsplit(x, ",")[[1]]))
}

null_if_blank <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(NULL)
  }

  x
}

analysis_font_settings <- function(input, func_name, user_state = userState) {
  spec <- get_analysis_font_spec(func_name)
  if (is.null(spec)) {
    return(NULL)
  }

  state <- font_settings_state_from_inputs(
    input = input,
    prefix = spec$prefix,
    supported_fields = spec$supported_fields,
    default_font_settings = spec$default_font_settings
  )
  if (is.null(state)) {
    state <- user_state[[spec$state_key]]
  }

  font_settings_state_to_backend(
    state,
    default_font_settings = spec$default_font_settings
  )
}

copyable_text_dependencies <- function() {
  shiny::tagList(
    shiny::singleton(
      shiny::tags$style(
        shiny::HTML(
          "
          .copyable-text {
            position: relative;
            margin: 0;
          }
          .copyable-text__button {
            position: absolute;
            top: 0.5rem;
            right: 0.5rem;
            opacity: 0;
            transition: opacity 0.15s ease-in-out;
            z-index: 1;
          }
          .copyable-text:hover .copyable-text__button,
          .copyable-text:focus-within .copyable-text__button {
            opacity: 1;
          }
          .copyable-text__content {
            margin: 0;
            padding: 0.75rem 1rem;
            white-space: pre-wrap;
            word-break: break-word;
          }
          "
        )
      )
    ),
    shiny::singleton(
      shiny::tags$script(
        shiny::HTML(
          "
          window.cytCopyText = function(button) {
            var container = button.closest('.copyable-text');
            var content = container ? container.querySelector('.copyable-text__content') : null;
            if (!content) {
              return;
            }

            var text = content.innerText;
            var fallbackCopy = function(value) {
              var textarea = document.createElement('textarea');
              textarea.value = value;
              textarea.setAttribute('readonly', '');
              textarea.style.position = 'absolute';
              textarea.style.left = '-9999px';
              document.body.appendChild(textarea);
              textarea.select();
              document.execCommand('copy');
              document.body.removeChild(textarea);
            };
            var showCopied = function() {
              var defaultLabel = button.getAttribute('data-default-label') || 'Copy';
              button.textContent = 'Copied';
              window.setTimeout(function() {
                button.textContent = defaultLabel;
              }, 1500);
            };

            if (navigator.clipboard && window.isSecureContext) {
              navigator.clipboard.writeText(text).then(showCopied).catch(function() {
                fallbackCopy(text);
                showCopied();
              });
            } else {
              fallbackCopy(text);
              showCopied();
            }
          };
          "
        )
      )
    )
  )
}

copyable_text_block <- function(text, button_label = "Copy") {
  if (is.null(text) || !length(text)) {
    return(NULL)
  }

  text <- paste(as.character(text), collapse = "\n")

  shiny::tags$div(
    class = "copyable-text",
    shiny::tags$button(
      type = "button",
      class = "btn btn-sm btn-outline-secondary copyable-text__button",
      onclick = "window.cytCopyText(this)",
      `data-default-label` = button_label,
      button_label
    ),
    shiny::tags$pre(class = "copyable-text__content", text)
  )
}

collect_exportable_plots <- function(x, prefix = "plot") {
  if (is.null(x)) {
    return(list())
  }

  if (inherits(x, "pheatmap")) {
    draw_fn <- function() {
      grid::grid.newpage()
      grid::grid.draw(x$gtable)
    }
    return(stats::setNames(list(draw_fn), prefix))
  }

  if (
    inherits(x, c("gg", "ggplot", "grob", "gtable", "recordedplot")) ||
      is.function(x)
  ) {
    return(stats::setNames(list(x), prefix))
  }

  if (!is.list(x) || is.data.frame(x) || is.matrix(x)) {
    return(list())
  }

  out <- list()
  child_names <- names(x)
  if (is.null(child_names)) {
    child_names <- as.character(seq_along(x))
  }

  for (i in seq_along(x)) {
    child_name <- child_names[[i]]
    if (is.null(child_name) || is.na(child_name) || !nzchar(child_name)) {
      child_name <- as.character(i)
    }

    out <- c(
      out,
      collect_exportable_plots(
        x[[i]],
        prefix = paste(prefix, child_name, sep = "_")
      )
    )
  }

  out
}

analysisResult <- shiny::eventReactive(input$next4, {
  errorMessage(NULL)
  warningMessage(character())

  shiny::req(filteredData())
  prog <- shiny::Progress$new()
  on.exit(prog$close())

  tryCatch(
    {
      withCallingHandlers(
        {
          df <- filteredData()
          df$..cyto_id.. <- NULL

          # Simplified: always generate a file path for functions that can output a PDF

          func_to_run <- selected_function()
          shiny::req(func_to_run)

          results <- switch(
            func_to_run,

            # -- Statistical Tests --
            "Univariate Tests (T-test, Wilcoxon)" = cyt_univariate(
              data = df,
              method = input$uv2_method %||% "auto",
              p_adjust_method = if (nzchar(input$uv2_p_adjust_method %||% "")) {
                input$uv2_p_adjust_method
              } else {
                "none"
              },
              ,
              progress = prog,
              scale = NULL,
              format_output = TRUE
            ),

            "Multi-level Univariate Tests (Anova, Kruskal-Wallis)" = cyt_univariate_multi(
              data = df,
              method = input$uvm_method %||% "anova",
              p_adjust_method = if (nzchar(input$uvm_p_adjust_method %||% "")) {
                input$uvm_p_adjust_method
              } else {
                "none"
              },
              progress = prog,
              format_output = TRUE
            ),
            "Two-way ANOVA" = cyt_univariate_multi(
              data = df,
              method = "anova",
              design = "two_way",
              primary_cat_var = input$twa_primary_cat_var,
              secondary_cat_var = input$twa_secondary_cat_var,
              include_primary_secondary_interaction = isTRUE(
                input$twa_include_primary_secondary_interaction
              ),
              progress = prog,
              format_output = TRUE
            ),
            "ANCOVA" = cyt_univariate_multi(
              data = df,
              method = "anova",
              design = "ancova",
              primary_cat_var = input$anc_primary_cat_var,
              secondary_cat_var = null_if_blank(input$anc_secondary_cat_var),
              covariate_col = input$anc_covariate_col,
              include_primary_secondary_interaction = isTRUE(
                input$anc_include_primary_secondary_interaction
              ),
              include_primary_covariate_interaction = isTRUE(
                input$anc_include_primary_covariate_interaction
              ),
              progress = prog,
              format_output = TRUE
            ),

            # -- Exploratory Visualization --
            "Boxplots" = cyt_bp(
              data = df,
              progress = prog,
              group_by = if (length(input$bp_group_by)) {
                input$bp_group_by
              } else {
                NULL
              },
              bin_size = input$bp_bin_size,
              y_lim = parse_numeric_input(input$bp_y_lim),
              scale = "none",
              font_settings = analysis_font_settings(input, "Boxplots")
            ),

            "Violin Plots" = cyt_violin(
              data = df,
              progress = prog,
              group_by = if (length(input$vio_group_by)) {
                input$vio_group_by
              } else {
                NULL
              },
              bin_size = input$vio_bin_size,
              y_lim = parse_numeric_input(input$vio_y_lim),
              scale = "none",
              boxplot_overlay = isTRUE(input$vio_boxplot_overlay),
              font_settings = analysis_font_settings(input, "Violin Plots")
            ),

            "Error-Bar Plot" = cyt_errbp(
              data = df,
              progress = prog,
              group_col = input$eb_group_col,
              p_lab = input$eb_p_lab,
              es_lab = input$eb_es_lab,
              class_symbol = input$eb_class_symbol,
              x_lab = input$eb_x_lab,
              y_lab = input$eb_y_lab,
              title = input$eb_title,
              stat = input$eb_stat %||% "mean",
              error = input$eb_error %||% "se",
              scale = "none",
              method = input$eb_method %||% "auto",
              p_adjust_method = null_if_blank(input$eb_p_adjust_method),
              n_col = input$eb_n_col %||% 3,
              fill_palette = {
                eb_fill_palette_choice <- input$eb_fill_palette %||% "gray"
                if (identical(eb_fill_palette_choice, "grey")) {
                  eb_fill_palette_choice <- "gray"
                }
                switch(
                  eb_fill_palette_choice,
                  "gray" = NULL,
                  "tableau" = c(
                    "#4E79A7",
                    "#F28E2B",
                    "#E15759",
                    "#76B7B2",
                    "#59A14F",
                    "#EDC948"
                  ),
                  "colorblind" = c(
                    "#0072B2",
                    "#E69F00",
                    "#009E73",
                    "#CC79A7",
                    "#56B4E9",
                    "#D55E00"
                  ),
                  "pastel" = c(
                    "#AEC6CF",
                    "#FFD1DC",
                    "#B5EAD7",
                    "#FFDAC1",
                    "#C7CEEA",
                    "#E2F0CB"
                  )
                )
              },
              font_settings = analysis_font_settings(input, "Error-Bar Plot")
            ),

            "Dual-Flashlight Plot" = cyt_dualflashplot(
              data = df,
              group_var = input$df_group_var,
              group1 = input$df_cond1,
              group2 = input$df_cond2,

              progress = prog,
              ssmd_thresh = input$df_ssmd_thresh,
              log2fc_thresh = input$df_log2fc_thresh,
              top_labels = input$df_top_labels,
              font_settings = analysis_font_settings(
                input,
                "Dual-Flashlight Plot"
              )
            ),
            "Heatmap" = {
              num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
              invalid_heatmap <- summarize_invalid_numeric_columns(
                df,
                columns = num_cols
              )
              if (nrow(invalid_heatmap) > 0) {
                stop(
                  paste(
                    "Heatmap uses the Step 2-transformed matrix directly and calls cyt_heatmap(..., scale = NULL), so this is not a second log transformation.",
                    "Invalid values were found in the working numeric matrix before clustering:",
                    format_invalid_numeric_summary(invalid_heatmap),
                    "Use Treat missing values or choose a transformation compatible with your data."
                  ),
                  call. = FALSE
                )
              }

              ann_arg <- if (
                !is.null(input$hm_annotation) && nzchar(input$hm_annotation)
              ) {
                input$hm_annotation
              } else {
                NULL
              }
              side_arg <- input$hm_ann_side %||% "auto"

              # build (silent) pheatmap object; nothing is drawn yet
              ph <- cyt_heatmap(
                data = df,
                scale = NULL,
                annotation_col = ann_arg,
                annotation_side = side_arg,
                font_settings = analysis_font_settings(input, "Heatmap")
              )
              ph # <- return the pheatmap object
            },

            "Skewness/Kurtosis" = cyt_skku(
              data = df,

              progress = prog,
              group_cols = input$skku_group_cols,
              print_res_raw = input$skku_print_raw,
              print_res_log = input$skku_print_log,
              font_settings = analysis_font_settings(input, "Skewness/Kurtosis")
            ),

            "Volcano Plot" = cyt_volc(
              data = df,

              progress = prog,
              group_col = input$volc_group_col,
              cond1 = input$volc_cond1,
              cond2 = input$volc_cond2,
              fold_change_thresh = input$volc_fold_change_thresh,
              p_value_thresh = input$volc_p_value_thresh,
              top_labels = input$volc_top_labels,
              font_settings = analysis_font_settings(input, "Volcano Plot")
            ),

            # -- Multivariate Analysis --
            "Principal Component Analysis (PCA)" = {
              pch_vals <- as.numeric(input$pca_pch)
              grp <- df[[input$pca_group_col]]
              uniq <- unique(na.omit(grp))
              n_levels <- length(uniq)

              if (!length(pch_vals)) {
                pch_vals <- c(16, 17) # fallback if nothing selected
              }

              if (length(pch_vals) < n_levels) {
                needed <- n_levels - length(pch_vals)
                pool <- setdiff(0:25, pch_vals) # standard numeric pch set

                extra <- if (length(pool) >= needed) {
                  sample(pool, needed, replace = FALSE)
                } else {
                  c(pool, sample(0:25, needed - length(pool), replace = TRUE))
                }

                pch_vals <- c(pch_vals, extra)
              }

              cols <- if (length(input$pca_colors)) {
                input$pca_colors
              } else {
                rainbow(length(uniq))
              }

              cyt_pca(
                data = df,
                progress = prog,
                group_col = input$pca_group_col,
                group_col2 = if (nzchar(input$pca_group_col2)) {
                  input$pca_group_col2
                } else {
                  NULL
                },
                comp_num = input$pca_comp_num,
                scale = NULL,
                ellipse = input$pca_ellipse,
                style = if (input$pca_style == "3D") "3d" else NULL,
                pch_values = pch_vals,
                pca_colors = cols,
                font_settings = analysis_font_settings(
                  input,
                  "Principal Component Analysis (PCA)"
                )
              )
            },

            "Partial Least Squares Regression (PLSR)" = {
              df <- filteredData()
              grp <- if (
                !is.null(input$plsr_group_col) && nzchar(input$plsr_group_col)
              ) {
                input$plsr_group_col
              } else {
                NULL
              }

              # Resolve predictor cols — NULL means "use all"
              pred_cols <- if (length(input$plsr_predictor_cols)) {
                input$plsr_predictor_cols
              } else {
                NULL
              }

              res <- cyt_plsr(
                data = df,
                group_col = grp,
                response_col = input$plsr_response_col,
                predictor_cols = pred_cols, # <-- ADD
                comp_num = input$plsr_comp_num,
                sparse = input$plsr_sparse,
                var_num = input$plsr_keepX,
                scale = NULL,
                ellipse = isTRUE(input$plsr_ellipse),
                cv_opt = if (input$plsr_cv_opt == "None") {
                  NULL
                } else {
                  input$plsr_cv_opt
                },
                fold_num = input$plsr_fold_num,
                pls_colors = input$plsr_colors,
                font_settings = analysis_font_settings(
                  input,
                  "Partial Least Squares Regression (PLSR)"
                ),
                progress = prog
              )
            },

            "Correlation Plots" = {
              tgt <- input$corr_target
              grp <- input$corr_group_col
              bygrp <- isTRUE(input$corr_by_group) && nzchar(grp)

              # overall: both methods at once
              both <- cyt_corr(
                data = df,
                target = tgt,
                methods = c("spearman", "pearson"),
                group_var = if (bygrp) input$corr_group_col else NULL,
                compare_groups = FALSE,
                plot = TRUE,
                font_settings = analysis_font_settings(input, "Correlation Plots"),
                progress = prog
              )

              list(
                spearman = list(
                  table = both$spearman$table,
                  heat_mat = both$spearman$heat_mat,
                  plot = both$spearman$plot,
                  group_plots = both$spearman$group_plots
                ),
                pearson = list(
                  table = both$pearson$table,
                  heat_mat = both$pearson$heat_mat,
                  plot = both$pearson$plot,
                  group_plots = both$pearson$group_plots
                ),
                corr_target = tgt
              )
            },

            "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = {
              pch_vals <- as.numeric(input$splsda_pch)
              pch_vals <- pch_vals[!is.na(pch_vals)]
              grp <- df[[input$splsda_group_col]]
              uniq <- unique(na.omit(grp))
              n_levels <- length(uniq)

              if (!length(pch_vals)) {
                pch_vals <- c(16, 17) # sensible fallback if nothing selected
              }

              if (length(pch_vals) < n_levels) {
                needed <- n_levels - length(pch_vals)
                pool <- setdiff(0:25, pch_vals) # standard numeric pch pool, avoid duplicates

                extra <- if (length(pool) >= needed) {
                  sample(pool, needed, replace = FALSE)
                } else {
                  c(pool, sample(0:25, needed - length(pool), replace = TRUE))
                }
                pch_vals <- c(pch_vals, extra)
              }

              cols <- if (length(input$splsda_colors)) {
                input$splsda_colors
              } else {
                rainbow(length(uniq))
              }
              multilevel <- if (isTRUE(input$splsda_use_multilevel)) {
                input$splsda_multilevel
              } else {
                NULL
              }
              batch_col <- if (isTRUE(input$splsda_use_batch_corr)) {
                input$splsda_batch_col
              } else {
                NULL
              }
              ind_names_val <- switch(
                input$splsda_ind_names_mode %||% "off",
                "off" = FALSE,
                "rownames" = TRUE,
                "column" = {
                  shiny::req(input$splsda_ind_names_col)
                  as.character(df[[input$splsda_ind_names_col]])
                },
                FALSE
              )
              cyt_splsda(
                data = df,
                progress = prog,
                group_col = input$splsda_group_col,
                group_col2 = input$splsda_group_col2,
                batch_col = batch_col,
                multilevel = multilevel,
                var_num = input$splsda_var_num,
                cv_opt = if (input$splsda_cv_opt == "None") {
                  NULL
                } else {
                  tolower(input$splsda_cv_opt)
                },
                fold_num = input$splsda_fold_num,
                scale = NULL,
                comp_num = input$splsda_comp_num,
                style = if (input$splsda_style == "3D") "3d" else NULL,
                pch_values = pch_vals,
                splsda_colors = cols,
                roc = input$splsda_roc,
                ellipse = input$splsda_ellipse,
                ind_names = ind_names_val,
                bg = input$splsda_bg,
                conf_mat = input$splsda_conf_mat,
                font_settings = analysis_font_settings(
                  input,
                  "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
                )
              )
            },

            "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)" = {
              grp <- df[[input$mint_splsda_group_col]]
              uniq <- unique(grp)
              cols <- if (length(input$mint_splsda_colors)) {
                input$mint_splsda_colors
              } else {
                rainbow(length(uniq))
              }

              cyt_mint_splsda(
                data = df,

                progress = prog,
                group_col = input$mint_splsda_group_col,
                group_col2 = input$mint_splsda_group_col2,
                batch_col = input$mint_splsda_batch_col,
                var_num = input$mint_splsda_var_num,
                comp_num = input$mint_splsda_comp_num,
                cim = input$mint_splsda_cim,
                scale = NULL,
                colors = cols,
                roc = input$mint_splsda_roc,
                ellipse = input$mint_splsda_ellipse,
                bg = input$mint_splsda_bg,
                font_settings = analysis_font_settings(
                  input,
                  "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
                )
              )
            },

            # -- Machine Learning --
            "Random Forest" = cyt_rf(
              data = df,
              progress = prog,
              group_col = input$rf_group_col,
              ntree = input$rf_ntree,
              mtry = input$rf_mtry,
              train_fraction = input$rf_train_fraction,
              plot_roc = isTRUE(input$rf_plot_roc),
              run_rfcv = isTRUE(input$rf_run_rfcv),
              k_folds = input$rf_k_folds,
              step = input$rf_step,
              scale = "none",
              seed = 123,
              font_settings = analysis_font_settings(input, "Random Forest")
            ),

            "Extreme Gradient Boosting (XGBoost)" = cyt_xgb(
              data = df,
              progress = prog,
              group_col = input$xgb_group_col,
              train_fraction = input$xgb_train_fraction,
              nrounds = input$xgb_nrounds,
              max_depth = input$xgb_max_depth,
              learning_rate = input$xgb_eta,
              nfold = input$xgb_nfold,
              cv = isTRUE(input$xgb_cv),
              eval_metric = input$xgb_eval_metric,
              top_n_features = input$xgb_top_n_features,
              plot_roc = isTRUE(input$xgb_plot_roc),
              scale = "none",
              seed = 123,
              font_settings = analysis_font_settings(
                input,
                "Extreme Gradient Boosting (XGBoost)"
              )
            )
          ) # end switch

          results
        },
        warning = function(w) {
          warningMessage(unique(c(
            warningMessage(),
            conditionMessage(w)
          )))
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      errorMessage(conditionMessage(e))
      NULL
    }
  ) # end tryCatch
})

reactivePlots <- shiny::reactive({
  shiny::req(analysisResult())
  analysisResult()
})

exportablePlots <- shiny::reactive({
  shiny::req(analysisResult(), selected_function())

  func_name <- selected_function()
  res <- analysisResult()

  switch(
    func_name,
    "Univariate Tests (T-test, Wilcoxon)" = list(),
    "Multi-level Univariate Tests (Anova, Kruskal-Wallis)" = list(),
    "Two-way ANOVA" = list(),
    "ANCOVA" = list(),
    "Heatmap" = collect_exportable_plots(res, "heatmap"),
    "Random Forest" = collect_exportable_plots(
      list(
        vip_plot = res$vip_plot,
        roc_plot = res$roc_plot,
        rfcv_plot = res$rfcv_plot
      ),
      "rf"
    ),
    "Extreme Gradient Boosting (XGBoost)" = collect_exportable_plots(
      list(
        importance_plot = res$importance_plot,
        roc_plot = res$roc_plot
      ),
      "xgb"
    ),
    "Skewness/Kurtosis" = collect_exportable_plots(
      list(
        skew_plot = res$p_skew,
        kurt_plot = res$p_kurt
      ),
      "skku"
    ),
    collect_exportable_plots(
      res,
      gsub("[^A-Za-z0-9]+", "_", tolower(func_name))
    )
  )
})

exportableTables <- shiny::reactive({
  shiny::req(analysisResult(), selected_function())

  func_name <- selected_function()
  res <- analysisResult()

  switch(
    func_name,
    "Univariate Tests (T-test, Wilcoxon)" = {
      if (is.data.frame(res)) {
        list(results = res)
      } else {
        list()
      }
    },
    "Multi-level Univariate Tests (Anova, Kruskal-Wallis)" = {
      if (!is.list(res) || is.null(res$results)) {
        return(list())
      }

      tables <- list(
        global = res$results,
        pairwise = res$pairwise %||% data.frame()
      )
      if (!is.null(res$assumptions)) {
        tables$assumptions <- res$assumptions
      }
      tables
    },
    "Two-way ANOVA" = {
      if (!is.list(res) || is.null(res$results)) {
        return(list())
      }

      tables <- list(
        global = res$results,
        pairwise = res$pairwise %||% data.frame()
      )
      if (!is.null(res$assumptions)) {
        tables$assumptions <- res$assumptions
      }
      tables
    },
    "ANCOVA" = {
      if (!is.list(res) || is.null(res$results)) {
        return(list())
      }

      tables <- list(
        global = res$results,
        pairwise = res$pairwise %||% data.frame()
      )
      if (!is.null(res$assumptions)) {
        tables$assumptions <- res$assumptions
      }
      tables
    },
    list()
  )
})

output$errorText <- shiny::renderUI({
  shiny::req(errorMessage())
  shiny::div(
    style = "color:red; padding:5px; border:1px solid red;",
    shiny::strong("Error:"),
    shiny::tags$pre(errorMessage())
  )
})

output$warningText <- shiny::renderUI({
  warnings <- warningMessage()
  shiny::req(length(warnings) > 0L)
  shiny::div(
    style = "color:orange; padding:5px; border:1px solid orange;",
    shiny::strong("Warning:"),
    shiny::tags$pre(paste(unique(warnings), collapse = "\n"))
  )
})

output$result_display <- shiny::renderUI({
  err <- errorMessage()
  if (shiny::isTruthy(err)) {
    return(shiny::tagList(
      shiny::uiOutput("errorText"),
      shiny::actionButton(
        "back6",
        "Change Inputs",
        icon = shiny::icon("arrow-left"),
        class = "btn-secondary"
      )
    ))
  }
  shiny::req(analysisResult())
  res <- analysisResult()
  func_name <- selected_function()
  shiny::req(func_name)

  # Main UI container
  shiny::tagList(
    copyable_text_dependencies(),
    shiny::uiOutput("warningText"), # Display warnings if any
    shiny::hr(),

    # Interactive Mode: Use a switch to render the correct UI
    switch(
      func_name,

      # --- sPLS-DA UI ---
      "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = {
        if ("overall_indiv_plot" %in% names(res)) {
          # Single analysis UI
          shiny::tagList(
            shiny::h4("sPLS-DA Results"),
            shiny::tabsetPanel(
              id = "splsda_tabs",
              type = "tabs",
              shiny::tabPanel(
                "sPLS-DA Plot",
                shinycssloaders::withSpinner(
                  shiny::plotOutput(
                    "splsda_overallIndivPlot",
                    height = "500px"
                  ),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Loadings",
                shinycssloaders::withSpinner(
                  shiny::uiOutput("splsda_loadingsUI"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "VIP Scores",
                shinycssloaders::withSpinner(
                  shiny::uiOutput("splsda_vipScoresUI"),
                  type = 8
                )
              ),
              if (!is.null(res$vip_indiv_plot)) {
                shiny::tabPanel(
                  "VIP Model Plot",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("splsda_vipIndivPlot", height = "500px"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$vip_loadings)) {
                shiny::tabPanel(
                  "VIP Loadings",
                  shinycssloaders::withSpinner(
                    shiny::uiOutput("splsda_vipLoadingsUI"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$overall_3D)) {
                shiny::tabPanel(
                  "3D Plot",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("splsda_overall3DPlot", height = "500px"),
                    type = 8
                  ),
                  # break line
                  shiny::br(),
                  shiny::actionButton(
                    "splsda_show3d_interactive",
                    "Interactive 3D",
                    icon = shiny::icon("fas fa-cube")
                  ),
                  # break line
                  shiny::br(),
                )
              },
              if (!is.null(res$vip_3D)) {
                shiny::tabPanel(
                  "3D Plot (VIP>1)",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("splsda_vip3DPlot", height = "500px"),
                    type = 8
                  ),
                  # break line
                  shiny::br(),
                  shiny::actionButton(
                    "splsda_show3d_interactive_vip",
                    "Interactive 3D (VIP)",
                    icon = shiny::icon("fas fa-cube")
                  ),
                  # break line
                  shiny::br(),
                )
              },

              if (!is.null(res$overall_ROC)) {
                shiny::tabPanel(
                  "ROC",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput(
                      "splsda_overallRocPlot",
                      height = "400px"
                    ),
                    type = 8
                  )
                )
              },

              if (!is.null(res$overall_CV)) {
                shiny::tabPanel(
                  "Cross-Validation",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("splsda_overallCvPlot", height = "400px"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$conf_matrix)) {
                shiny::tabPanel(
                  "Confusion Matrix",
                  shinycssloaders::withSpinner(
                    shiny::uiOutput("splsda_confMatrix"),
                    type = 8
                  )
                )
              }
            )
          )
        } else {
          # Multi-group analysis UI (tabs for each group)
          do.call(
            tabsetPanel,
            c(
              list(id = "splsda_multigroup_tabs"),
              lapply(names(res), function(trt) {
                shiny::tabPanel(
                  title = trt,
                  shiny::tabsetPanel(
                    type = "tabs",
                    shiny::tabPanel(
                      "sPLS-DA Plot",
                      shinycssloaders::withSpinner(
                        shiny::plotOutput(
                          paste0("splsda_overallIndivPlot_", trt),
                          height = "500px"
                        ),
                        type = 8
                      )
                    ),
                    shiny::tabPanel(
                      "Loadings",
                      shinycssloaders::withSpinner(
                        shiny::uiOutput(paste0("splsda_loadingsUI_", trt)),
                        type = 8
                      )
                    ),
                    shiny::tabPanel(
                      "VIP Scores",
                      shinycssloaders::withSpinner(
                        shiny::uiOutput(paste0("splsda_vipScoresUI_", trt)),
                        type = 8
                      )
                    ),
                    if (!is.null(res[[trt]]$vip_indiv_plot)) {
                      shiny::tabPanel(
                        "VIP Model Plot",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0("splsda_vipIndivPlot_", trt),
                            height = "500px"
                          ),
                          type = 8
                        )
                      )
                    },
                    if (!is.null(res[[trt]]$vip_loadings)) {
                      shiny::tabPanel(
                        "VIP Loadings",
                        shinycssloaders::withSpinner(
                          shiny::uiOutput(paste0("splsda_vipLoadingsUI_", trt)),
                          type = 8
                        )
                      )
                    },
                    if (!is.null(res[[trt]]$overall_3D)) {
                      shiny::tabPanel(
                        "3D Plot",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0("splsda_overall3DPlot_", trt),
                            height = "500px"
                          ),
                          type = 8
                        ),
                        # break line
                        shiny::br(),
                        shiny::actionButton(
                          paste0("splsda_show3d_interactive_", trt),
                          "Interactive 3D",
                          icon = shiny::icon("fas fa-cube")
                        ),
                        # break line
                        shiny::br(),
                      )
                    },

                    # VIP 3D tab per-trt (no div wrapper)
                    if (!is.null(res[[trt]]$vip_3D)) {
                      shiny::tabPanel(
                        "3D Plot (VIP>1)",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0("splsda_vip3DPlot_", trt),
                            height = "500px"
                          ),
                          type = 8
                        ),
                        # break line
                        shiny::br(),
                        shiny::actionButton(
                          paste0("splsda_show3d_interactive_vip_", trt),
                          "Interactive 3D (VIP)",
                          icon = shiny::icon("fas fa-cube")
                        ),
                        # break line
                        shiny::br(),
                      )
                    },
                    if (!is.null(res[[trt]]$overall_ROC)) {
                      shiny::tabPanel(
                        "ROC",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0(
                              "splsda_overallRocPlot_",
                              trt
                            ),
                            height = "400px",
                          ),
                          type = 8
                        )
                      )
                    },

                    if (!is.null(res[[trt]]$overall_CV)) {
                      shiny::tabPanel(
                        "Cross-Validation",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0(
                              "splsda_overallCvPlot_",
                              trt
                            ),
                            height = "400px"
                          ),
                          type = 8
                        )
                      )
                    },
                    if (!is.null(res[[trt]]$conf_matrix)) {
                      shiny::tabPanel(
                        "Confusion Matrix",
                        shinycssloaders::withSpinner(
                          shiny::uiOutput(
                            paste0("splsda_confMatrix_", trt)
                          ),
                          type = 8
                        )
                      )
                    }
                  )
                )
              })
            )
          )
        }
      },
      # --- MINT sPLS-DA UI ---
      "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)" = {
        # Check if the result is for a single analysis or multiple (nested list)
        is_nested <- is.list(res) &&
          !is.null(names(res)) &&
          is.list(res[[1]]) &&
          "global_indiv_plot" %in% names(res[[1]])

        if (!is_nested) {
          # UI for a single analysis (original behavior)
          shiny::tagList(
            shiny::h4("MINT sPLS-DA Results"),
            shiny::tabsetPanel(
              id = "mint_splsda_tabs",
              shiny::tabPanel(
                "Global Sample Plot",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("mint_splsda_global_plot"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Partial Sample Plots",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("mint_splsda_partial_plot"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Variable Loadings",
                shinycssloaders::withSpinner(
                  shiny::uiOutput("mint_splsda_loadings_ui"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Correlation Circle",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("mint_splsda_corr_circle_plot"),
                  type = 8
                )
              ),
              if (!is.null(res$cim_obj)) {
                shiny::tabPanel(
                  "Heatmap (CIM)",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput(
                      "mint_splsda_cim_plot",
                      height = "600px"
                    ),
                    type = 8
                  )
                )
              },
              if (!is.null(res$roc_plot)) {
                shiny::tabPanel(
                  "ROC Curve",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("mint_splsda_roc_plot"),
                    type = 8
                  )
                )
              }
            )
          )
        } else {
          # UI for multi-group analysis (nested tabs)
          do.call(
            tabsetPanel,
            c(
              list(id = "mint_splsda_multigroup_tabs"),
              lapply(names(res), function(trt) {
                shiny::tabPanel(
                  title = trt,
                  shiny::tabsetPanel(
                    type = "tabs",
                    shiny::tabPanel(
                      "Global Plot",
                      shinycssloaders::withSpinner(
                        shiny::plotOutput(paste0(
                          "mint_splsda_global_",
                          trt
                        )),
                        type = 8
                      )
                    ),
                    shiny::tabPanel(
                      "Partial Plots",
                      shinycssloaders::withSpinner(
                        shiny::plotOutput(paste0(
                          "mint_splsda_partial_",
                          trt
                        )),
                        type = 8
                      )
                    ),
                    shiny::tabPanel(
                      "Variable Loadings",
                      shinycssloaders::withSpinner(
                        shiny::uiOutput(paste0(
                          "mint_splsda_loadings_",
                          trt
                        )),
                        type = 8
                      )
                    ),
                    shiny::tabPanel(
                      "Correlation",
                      shinycssloaders::withSpinner(
                        shiny::plotOutput(paste0(
                          "mint_splsda_corr_",
                          trt
                        )),
                        type = 8
                      )
                    ),
                    if (!is.null(res[[trt]]$cim_obj)) {
                      shiny::tabPanel(
                        "CIM",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0("mint_splsda_cim_", trt),
                            height = "600px"
                          ),
                          type = 8
                        )
                      )
                    },
                    if (!is.null(res[[trt]]$roc_plot)) {
                      shiny::tabPanel(
                        "ROC",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(paste0(
                            "mint_splsda_roc_",
                            trt
                          )),
                          type = 8
                        )
                      )
                    }
                  )
                )
              })
            )
          )
        }
      },

      # --- PCA UI ---
      "Principal Component Analysis (PCA)" = {
        if ("overall_indiv_plot" %in% names(res)) {
          shiny::tagList(
            shiny::h4("PCA Results"),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                "PCA Plot",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("pca_indivPlot", height = "400px"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Scree Plot",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("pca_screePlot", height = "400px"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Loadings Plots",
                shinycssloaders::withSpinner(
                  shiny::uiOutput("pca_loadingsUI"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Biplot",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("pca_biplot", height = "400px"),
                  type = 8
                )
              ),
              shiny::tabPanel(
                "Correlation Circle",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("pca_corrCircle", height = "400px"),
                  type = 8
                )
              ),
              if (!is.null(res$overall_3D)) {
                shiny::tabPanel(
                  "3D Plot",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("pca_3DPlot", height = "400px"),
                    type = 8
                  )
                )
              }
            )
          )
        } else {
          do.call(
            tabsetPanel,
            c(
              list(id = "pca_multigroup_tabs"),
              lapply(names(res), function(trt) {
                shiny::tabPanel(
                  title = trt,
                  shiny::tabsetPanel(
                    type = "tabs",
                    shiny::tabPanel(
                      "PCA Plot",
                      shinycssloaders::withSpinner(
                        shiny::plotOutput(
                          paste0("pca_indivPlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    ),
                    shiny::tabPanel(
                      "Scree Plot",
                      shinycssloaders::withSpinner(
                        shiny::plotOutput(
                          paste0("pca_screePlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    ),
                    shiny::tabPanel(
                      "Loadings Plots",
                      shinycssloaders::withSpinner(
                        shiny::uiOutput(paste0("pca_loadingsUI_", trt)),
                        type = 8
                      )
                    ),
                    shiny::tabPanel(
                      "Biplot",
                      shinycssloaders::withSpinner(
                        shiny::plotOutput(
                          paste0("pca_biplot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    ),
                    shiny::tabPanel(
                      "Correlation Circle",
                      shinycssloaders::withSpinner(
                        shiny::plotOutput(
                          paste0("pca_corrCircle_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    ),
                    if (!is.null(res[[trt]]$overall_3D)) {
                      shiny::tabPanel(
                        "3D Plot",
                        shinycssloaders::withSpinner(
                          shiny::plotOutput(
                            paste0("pca_3DPlot_", trt),
                            height = "400px"
                          ),
                          type = 8
                        )
                      )
                    }
                  )
                )
              })
            )
          )
        }
      },
      # --- PLSR UI ---
      "Partial Least Squares Regression (PLSR)" = {
        shiny::tagList(
          shiny::h4("PLSR Results"),
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              "Scores Plot",
              shinycssloaders::withSpinner(
                shiny::plotOutput("plsr_indivPlot", height = "400px"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "Predicted vs Observed",
              shinycssloaders::withSpinner(
                shiny::plotOutput("plsr_predPlot", height = "400px"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "Residuals vs Fitted",
              shinycssloaders::withSpinner(
                shiny::plotOutput("plsr_residPlot", height = "400px"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "Loadings Plots",
              shinycssloaders::withSpinner(
                shiny::uiOutput("plsr_loadingsUI"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "VIP Scores",
              shinycssloaders::withSpinner(
                shiny::uiOutput("plsr_vipUI"),
                type = 8
              )
            ),
            if (!is.null(res$cv_plot)) {
              shiny::tabPanel(
                "Cross-Validation",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("plsr_cvPlot", height = "400px"),
                  type = 8
                )
              )
            },
            if (!is.null(res$vip_scores_indiv)) {
              shiny::tabPanel(
                "VIP > 1: Scores",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("plsr_vipIndivPlot", height = "400px"),
                  type = 8
                )
              )
            },
            if (!is.null(res$vip_cv_plot)) {
              shiny::tabPanel(
                "VIP > 1: Cross-Validation",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("plsr_vipCVPlot", height = "400px"),
                  type = 8
                )
              )
            }
          )
        )
      },

      # --- Correlation UI ---
      "Correlation Plots" = {
        shiny::tagList(
          shiny::h4("Correlation Results"),
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              "Spearman",
              shiny::tabsetPanel(
                type = "tabs",
                selected = "Spearman Heatmap",
                shiny::tabPanel(
                  "Spearman Heatmap",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput(
                      "corr_heatmap_spearman",
                      height = "600px"
                    ),
                    type = 8
                  )
                ),
                shiny::tabPanel(
                  "Spearman Table",
                  shinycssloaders::withSpinner(
                    DT::dataTableOutput("corr_tbl_spearman"),
                    type = 8
                  )
                ),
                if (
                  !is.null(res$spearman$group_plots) &&
                    length(res$spearman$group_plots)
                ) {
                  shiny::tabPanel(
                    "Spearman Per-Group Heatmaps",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput("corr_group_heatmap_ui_spearman"),
                      type = 8
                    )
                  )
                }
              )
            ),
            shiny::tabPanel(
              "Pearson",
              shiny::tabsetPanel(
                type = "tabs",
                selected = "Pearson Heatmap", # <- default
                shiny::tabPanel(
                  "Pearson Heatmap",
                  shinycssloaders::withSpinner(
                    shiny::plotOutput("corr_heatmap_pearson", height = "600px"),
                    type = 8
                  )
                ),
                shiny::tabPanel(
                  "Pearson Table",
                  shinycssloaders::withSpinner(
                    DT::dataTableOutput("corr_tbl_pearson"),
                    type = 8
                  )
                ),
                if (
                  !is.null(res$pearson$group_plots) &&
                    length(res$pearson$group_plots)
                ) {
                  shiny::tabPanel(
                    "Pearson Per-Group Heatmaps",
                    shinycssloaders::withSpinner(
                      shiny::uiOutput("corr_group_heatmap_ui_pearson"),
                      type = 8
                    )
                  )
                }
              )
            )
          )
        )
      },
      # --- Random Forest UI ---
      "Random Forest" = {
        shiny::tagList(
          shiny::h4("Random Forest Results"),
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel("Summary", shiny::uiOutput("rf_summary")),
            shiny::tabPanel(
              "Variable Importance",
              shinycssloaders::withSpinner(
                shiny::plotOutput("rf_vipPlot", height = "400px"),
                type = 8
              )
            ),
            if (!is.null(res$roc_plot)) {
              shiny::tabPanel(
                "ROC Curve",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("rf_rocPlot", height = "400px"),
                  type = 8
                )
              )
            },
            if (!is.null(res$rfcv_plot)) {
              shiny::tabPanel(
                "Cross-Validation",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("rf_rfcvPlot", height = "400px"),
                  type = 8
                )
              )
            }
          )
        )
      },

      # --- XGBoost UI ---
      "Extreme Gradient Boosting (XGBoost)" = {
        shiny::tagList(
          shiny::h4("XGBoost Results"),
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              "Summary",
              shiny::uiOutput("xgb_summary")
            ),
            shiny::tabPanel(
              "Variable Importance",
              shinycssloaders::withSpinner(
                shiny::plotOutput("xgb_vipPlot", height = "400px"),
                type = 8
              )
            ),
            if (!is.null(res$roc_plot)) {
              shiny::tabPanel(
                "ROC Curve",
                shinycssloaders::withSpinner(
                  shiny::plotOutput("xgb_rocPlot", height = "400px"),
                  type = 8
                )
              )
            }
          )
        )
      },

      # --- Volcano Plot UI ---
      "Volcano Plot" = {
        shiny::tagList(
          shiny::h4("Volcano Plot Results"),
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              "Plot",
              shinycssloaders::withSpinner(
                shiny::plotOutput("volcPlotOutput", height = "400px"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "Statistics Table",
              shinycssloaders::withSpinner(
                DT::dataTableOutput("volcStats"),
                type = 8
              )
            )
          ),
          shiny::div(
            style = "margin-top: .75rem;",
            shiny::p(
              shiny::tags$a(
                "Learn how to interpret volcano plot results",
                href = "https://shinyinfo.cytokineprofile.org/articles/Understanding-Volcano-Plot.html",
                target = "_blank",
                style = "text-decoration: underline;"
              )
            )
          )
        )
      },

      # --- Heatmap UI ---
      "Heatmap" = {
        shiny::tagList(
          shiny::h4("Heatmap Results"),
          shinycssloaders::withSpinner(
            shiny::imageOutput("heatmapImage", height = "auto", width = "100%"),
            type = 8
          )
        )
      },

      # --- Dual-Flashlight Plot UI ---
      "Dual-Flashlight Plot" = {
        shiny::tagList(
          shiny::h4("Dual-Flashlight Plot Results"),
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              "Plot",
              shinycssloaders::withSpinner(
                shiny::plotOutput("dualflashPlotOutput", height = "400px"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "Statistics Table",
              shinycssloaders::withSpinner(
                DT::dataTableOutput("dualflashStats"),
                type = 8
              )
            )
          ),
          shiny::div(
            style = "margin-top: .75rem;",
            shiny::p(
              shiny::tags$a(
                "Learn how to interpret dualflashlight plot results",
                href = "https://shinyinfo.cytokineprofile.org/articles/Understanding-Dual-Flashlight-Plot.html",
                target = "_blank",
                style = "text-decoration: underline;"
              )
            )
          )
        )
      },
      "Skewness/Kurtosis" = {
        shiny::tagList(
          shiny::h4("Skewness and Kurtosis Results"),
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              "Skewness Plot",
              shinycssloaders::withSpinner(
                shiny::plotOutput("skku_skewPlot", height = "400px"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "Kurtosis Plot",
              shinycssloaders::withSpinner(
                shiny::plotOutput("skku_kurtPlot", height = "400px"),
                type = 8
              )
            ),
            if (input$skku_print_raw) {
              shiny::tabPanel(
                "Raw Data",
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("skku_raw_results"),
                  type = 8
                )
              )
            },
            if (input$skku_print_log) {
              shiny::tabPanel(
                "Log-Transformed Data",
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("skku_log_results"),
                  type = 8
                )
              )
            }
          )
        )
      },
      "Error-Bar Plot" = {
        shiny::tagList(
          shiny::h4("Error-Bar Plot Results"),
          shinycssloaders::withSpinner(
            shiny::plotOutput("errorBarPlotOutput", height = "auto"),
            type = 8
          )
        )
      },
      "Univariate Tests (T-test, Wilcoxon)" = {
        shiny::tagList(
          shiny::h4("Univariate Test Results"),
          shinycssloaders::withSpinner(
            DT::dataTableOutput("univariateResults"),
            type = 8
          )
        )
      },
      "Multi-level Univariate Tests (Anova, Kruskal-Wallis)" = {
        method_used <- input$uvm_method %||% "anova"
        shiny::tagList(
          shiny::h4("Multi-level Univariate Test Results"),
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Global Tests",
              shiny::br(),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("univariateResults"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "Pairwise Comparisons",
              shiny::br(),
              if (method_used == "kruskal") {
                shiny::helpText(
                  shiny::icon("circle-info"),
                  "Pairwise Wilcoxon tests are shown only when the global Kruskal-Wallis test is significant."
                )
              },
              shinycssloaders::withSpinner(
                DT::dataTableOutput("univariatePairwiseResults"),
                type = 8
              )
            ),
            if (method_used == "anova") {
              shiny::tabPanel(
                "Assumption Checks",
                shiny::br(),
                shiny::helpText(
                  shiny::icon("circle-info"),
                  "Residual normality is assessed via Shapiro-Wilk on model residuals.",
                  "Homogeneity of variance is assessed via Bartlett's test.",
                  "A p-value > 0.05 indicates the assumption is met.",
                  "If either assumption is violated, consider switching the method to Kruskal-Wallis,",
                  "which is non-parametric and does not require normality or equal variances."
                ),
                shiny::br(),
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("anovaAssumptionTable"),
                  type = 8
                )
              )
            }
          )
        )
      },
      "Two-way ANOVA" = {
        shiny::tagList(
          shiny::h4("Two-way ANOVA Results"),
          shiny::helpText(
            shiny::icon("circle-info"),
            "Type II and Type III sums of squares are generally preferred for unbalanced designs.",
            "Type I is included for completeness and comparison.",
            "If a modeled interaction is significant, main-effect pairwise comparisons are marginal means and should be interpreted cautiously."
          ),
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Model Terms",
              shiny::br(),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("univariateResults"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "Pairwise Comparisons",
              shiny::br(),
              shiny::helpText(
                shiny::icon("circle-info"),
                "Pairwise comparisons are reported for factor main effects only.",
                "Interaction-specific simple effects are not shown in this version."
              ),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("univariatePairwiseResults"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "Assumption Checks",
              shiny::br(),
              shiny::helpText(
                shiny::icon("circle-info"),
                "Residual normality is assessed via Shapiro-Wilk on model residuals.",
                "Homogeneity of variance is assessed via Levene's test across the effective factor cells.",
                "Warnings also report low cell counts and significant interaction cautions."
              ),
              shiny::br(),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("anovaAssumptionTable"),
                type = 8
              )
            )
          )
        )
      },
      "ANCOVA" = {
        shiny::tagList(
          shiny::h4("ANCOVA Results"),
          shiny::helpText(
            shiny::icon("circle-info"),
            "Type II and Type III sums of squares are generally preferred for unbalanced designs.",
            "Type I is included for completeness and comparison.",
            "If a modeled interaction is significant, main-effect pairwise comparisons are marginal means and should be interpreted cautiously."
          ),
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Model Terms",
              shiny::br(),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("univariateResults"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "Pairwise Comparisons",
              shiny::br(),
              shiny::helpText(
                shiny::icon("circle-info"),
                "Pairwise comparisons are based on adjusted marginal means at the covariate mean.",
                "Interaction-specific simple effects are not shown in this version."
              ),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("univariatePairwiseResults"),
                type = 8
              )
            ),
            shiny::tabPanel(
              "Assumption Checks",
              shiny::br(),
              shiny::helpText(
                shiny::icon("circle-info"),
                "Residual normality is assessed via Shapiro-Wilk on model residuals.",
                "Homogeneity of variance is assessed via Levene's test across the effective factor cells.",
                "If primary:covariate is not modeled directly, the table also reports a slope-homogeneity check."
              ),
              shiny::br(),
              shinycssloaders::withSpinner(
                DT::dataTableOutput("anovaAssumptionTable"),
                type = 8
              )
            )
          )
        )
      },
      # --- Default UI for other functions (plots or tables) ---
      # This will catch simpler functions that return one plot or one table
      {
        if (
          is.list(res) &&
            length(res) > 0 &&
            all(sapply(res, function(x) inherits(x, "ggplot")))
        ) {
          # Handle functions that return a list of ggplot objects (like Boxplots)
          do.call(
            shiny::tagList,
            lapply(seq_along(res), function(i) {
              shinycssloaders::withSpinner(
                shiny::plotOutput(paste0("dynamicPlot_", i), height = "400px"),
                type = 8
              )
            })
          )
        }
      }
    )
  )
}) |>
  shiny::bindCache(
    shiny::reactiveValuesToList(userState, all.names = TRUE),
    userData(),
    cache = "session"
  )

output$textResults <- shiny::renderPrint({
  res <- analysisResult()
  # Add a check here:
  if (
    !(is.list(res) &&
      length(res) > 0 &&
      all(sapply(res, function(x) inherits(x, "ggplot"))))
  ) {
    # Only print if the result is NOT a list of ggplot objects
    print(res)
  } else {
    # Optionally print a message or nothing if it's a plot list
    cat("Plot results displayed below.")
  }
}) |>
  shiny::bindCache(
    shiny::reactiveValuesToList(userState, all.names = TRUE),
    userData(),
    cache = "session"
  )

shiny::observe({
  res <- analysisResult()
  if (
    is.list(res) &&
      length(res) > 0 &&
      all(sapply(res, function(x) inherits(x, "ggplot")))
  ) {
    for (i in seq_along(res)) {
      local({
        my_i <- i
        output[[paste0("dynamicPlot_", my_i)]] <- shiny::renderPlot({
          res[[my_i]]
        })
      })
    }
  }
})

shiny::observeEvent(analysisResult(), {
  # Get results and function name
  res <- analysisResult()
  func_name <- selected_function()
  shiny::req(res, func_name)

  # --- sPLS-DA Rendering Logic ---
  if (
    func_name ==
      "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
  ) {
    if ("overall_indiv_plot" %in% names(res)) {
      # --- Single Analysis Rendering ---
      output$splsda_overallIndivPlot <- shiny::renderPlot({
        grDevices::replayPlot(res$overall_indiv_plot)
      })
      output$splsda_overall3DPlot <- shiny::renderPlot({
        if (!is.null(res$overall_3D)) grDevices::replayPlot(res$overall_3D)
      })
      output$splsda_vip3DPlot <- shiny::renderPlot({
        if (!is.null(res$vip_3D)) grDevices::replayPlot(res$vip_3D)
      })
      output$splsda_interactive_plot <- plotly::renderPlotly({
        res$overall_3D_interactive
      })
      output$splsda_interactive_plot_vip <- plotly::renderPlotly({
        res$vip_3D_interactive
      })
      output$splsda_overallRocPlot <- shiny::renderPlot({
        if (!is.null(res$overall_ROC)) grDevices::replayPlot(res$overall_ROC)
      })
      output$splsda_overallCvPlot <- shiny::renderPlot({
        if (!is.null(res$overall_CV)) print(res$overall_CV)
      })
      output$splsda_vipIndivPlot <- shiny::renderPlot({
        if (!is.null(res$vip_indiv_plot)) {
          grDevices::replayPlot(res$vip_indiv_plot)
        }
      })
      output$splsda_confMatrix <- shiny::renderUI({
        if (!is.null(res$conf_matrix)) {
          copyable_text_block(res$conf_matrix)
        }
      })

      output$splsda_loadingsUI <- shiny::renderUI({
        shiny::req(res$loadings)
        lapply(seq_along(res$loadings), function(i) {
          shiny::plotOutput(paste0("splsda_loading_plot_", i), height = "400px")
        })
      })
      lapply(seq_along(res$loadings), function(i) {
        output[[paste0("splsda_loading_plot_", i)]] <- shiny::renderPlot({
          grDevices::replayPlot(res$loadings[[i]])
        })
      })

      output$splsda_vipScoresUI <- shiny::renderUI({
        shiny::req(res$vip_scores)
        lapply(seq_along(res$vip_scores), function(i) {
          shiny::plotOutput(paste0("splsda_vip_plot_", i), height = "400px")
        })
      })
      lapply(seq_along(res$vip_scores), function(i) {
        output[[paste0("splsda_vip_plot_", i)]] <- shiny::renderPlot({
          print(res$vip_scores[[i]])
        })
      })

      output$splsda_vipLoadingsUI <- shiny::renderUI({
        shiny::req(res$vip_loadings)
        lapply(seq_along(res$vip_loadings), function(i) {
          shiny::plotOutput(
            paste0("splsda_vip_loading_plot_", i),
            height = "400px"
          )
        })
      })
      lapply(seq_along(res$vip_loadings), function(i) {
        output[[paste0("splsda_vip_loading_plot_", i)]] <- shiny::renderPlot({
          grDevices::replayPlot(res$vip_loadings[[i]])
        })
      })
    } else {
      # --- Multi-Group Analysis Rendering (FULLY IMPLEMENTED) ---
      for (trt in names(res)) {
        local({
          current_trt <- trt
          sub_res <- res[[current_trt]]

          output[[paste0(
            "splsda_overallIndivPlot_",
            current_trt
          )]] <- shiny::renderPlot({
            grDevices::replayPlot(sub_res$overall_indiv_plot)
          })
          output[[paste0(
            "splsda_vipIndivPlot_",
            current_trt
          )]] <- shiny::renderPlot(
            {
              if (!is.null(sub_res$vip_indiv_plot)) {
                grDevices::replayPlot(sub_res$vip_indiv_plot)
              }
            }
          )

          output[[paste0(
            "splsda_loadingsUI_",
            current_trt
          )]] <- shiny::renderUI({
            shiny::req(sub_res$loadings)
            lapply(seq_along(sub_res$loadings), function(i) {
              shiny::plotOutput(
                paste0("splsda_loading_plot_", current_trt, "_", i),
                height = "400px"
              )
            })
          })
          lapply(seq_along(sub_res$loadings), function(i) {
            output[[paste0(
              "splsda_loading_plot_",
              current_trt,
              "_",
              i
            )]] <- shiny::renderPlot({
              grDevices::replayPlot(sub_res$loadings[[i]])
            })
          })

          output[[paste0(
            "splsda_vipScoresUI_",
            current_trt
          )]] <- shiny::renderUI({
            shiny::req(sub_res$vip_scores)
            lapply(seq_along(sub_res$vip_scores), function(i) {
              shiny::plotOutput(
                paste0("splsda_vip_plot_", current_trt, "_", i),
                height = "400px"
              )
            })
          })
          lapply(seq_along(sub_res$vip_scores), function(i) {
            output[[paste0(
              "splsda_vip_plot_",
              current_trt,
              "_",
              i
            )]] <- shiny::renderPlot({
              print(sub_res$vip_scores[[i]])
            })
          })

          output[[paste0(
            "splsda_vipLoadingsUI_",
            current_trt
          )]] <- shiny::renderUI({
            shiny::req(sub_res$vip_loadings)
            lapply(seq_along(sub_res$vip_loadings), function(i) {
              shiny::plotOutput(
                paste0("splsda_vip_loading_plot_", current_trt, "_", i),
                height = "400px"
              )
            })
          })
          lapply(seq_along(sub_res$vip_loadings), function(i) {
            output[[paste0(
              "splsda_vip_loading_plot_",
              current_trt,
              "_",
              i
            )]] <- shiny::renderPlot({
              grDevices::replayPlot(sub_res$vip_loadings[[i]])
            })
          })
          output[[paste0(
            "splsda_overall3DPlot_",
            current_trt
          )]] <- shiny::renderPlot({
            if (!is.null(sub_res$overall_3D)) {
              grDevices::replayPlot(sub_res$overall_3D)
            }
          })
          output[[paste0(
            "splsda_vip3DPlot_",
            current_trt
          )]] <- shiny::renderPlot({
            if (!is.null(sub_res$vip_3D)) {
              grDevices::replayPlot(sub_res$vip_3D)
            }
          })
          # ----- Interactive OVERALL 3D per-trt: open modal -----
          shiny::observeEvent(
            input[[paste0("splsda_show3d_interactive_", current_trt)]],
            {
              shiny::showModal(shiny::modalDialog(
                plotly::plotlyOutput(
                  paste0("splsda_interactive_plot_", current_trt),
                  width = "100%",
                  height = "600px"
                ),
                easyClose = TRUE,
                size = "l"
              ))
            }
          )

          # ----- Interactive OVERALL 3D per-trt: render Plotly -----
          output[[paste0(
            "splsda_interactive_plot_",
            current_trt
          )]] <- plotly::renderPlotly({
            shiny::req(sub_res$overall_3D_interactive)
            sub_res$overall_3D_interactive
          })

          # ----- Interactive VIP 3D per-trt: open modal -----
          shiny::observeEvent(
            input[[paste0("splsda_show3d_interactive_vip_", current_trt)]],
            {
              shiny::showModal(shiny::modalDialog(
                plotly::plotlyOutput(
                  paste0("splsda_interactive_plot_vip_", current_trt),
                  width = "100%",
                  height = "75vh"
                ),
                easyClose = TRUE,
                size = "l"
              ))
            }
          )

          # ----- Interactive VIP 3D per-trt: render Plotly -----
          output[[paste0(
            "splsda_interactive_plot_vip_",
            current_trt
          )]] <- plotly::renderPlotly({
            shiny::req(sub_res$vip_3D_interactive)
            sub_res$vip_3D_interactive
          })
          output[[paste0(
            "splsda_overallRocPlot_",
            current_trt
          )]] <- shiny::renderPlot({
            if (!is.null(sub_res$overall_ROC)) {
              grDevices::replayPlot(sub_res$overall_ROC)
            }
          })
          output[[paste0(
            "splsda_overallCvPlot_",
            current_trt
          )]] <- shiny::renderPlot({
            if (!is.null(sub_res$overall_CV)) {
              print(sub_res$overall_CV)
            }
          })
          output[[paste0(
            "splsda_confMatrix_",
            current_trt
          )]] <- shiny::renderUI({
            if (!is.null(sub_res$conf_matrix)) {
              copyable_text_block(sub_res$conf_matrix)
            }
          })
        })
      }
    }
  }
  # --- MINT sPLS-DA Rendering Logic ---
  if (
    func_name ==
      "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
  ) {
    shiny::req(res)
    is_nested <- is.list(res) &&
      !is.null(names(res)) &&
      is.list(res[[1]]) &&
      "global_indiv_plot" %in% names(res[[1]])

    if (!is_nested) {
      # --- Single Analysis Rendering ---
      output$mint_splsda_global_plot <- shiny::renderPlot({
        grDevices::replayPlot(res$global_indiv_plot)
      })
      output$mint_splsda_partial_plot <- shiny::renderPlot({
        grDevices::replayPlot(res$partial_indiv_plot)
      })
      if (!is.null(res$partial_loadings_plots)) {
        output$mint_splsda_loadings_ui <- shiny::renderUI({
          shiny::req(res$partial_loadings_plots)
          shiny::tagList(lapply(
            seq_along(res$partial_loadings_plots),
            function(i) {
              shiny::plotOutput(
                paste0("mint_splsda_loading_plot_", i),
                height = "400px"
              )
            }
          ))
        })

        # Use a for loop to correctly scope the index 'i'
        for (i in seq_along(res$partial_loadings_plots)) {
          local({
            # Use local() to ensure 'i' is captured correctly
            local_i <- i
            output[[paste0(
              "mint_splsda_loading_plot_",
              local_i
            )]] <- shiny::renderPlot({
              grDevices::replayPlot(res$partial_loadings_plots[[local_i]])
            })
          })
        }
      }
      output$mint_splsda_corr_circle_plot <- shiny::renderPlot({
        grDevices::replayPlot(res$correlation_circle_plot)
      })
      output$mint_splsda_cim_plot <- shiny::renderPlot(
        {
          if (!is.null(res$cim_obj) && inherits(res$cim_obj, "recordedplot")) {
            grDevices::replayPlot(res$cim_obj)
          }
        },
        height = 600
      )
      output$mint_splsda_roc_plot <- shiny::renderPlot({
        if (!is.null(res$roc_plot)) grDevices::replayPlot(res$roc_plot)
      })
    } else {
      # --- Multi-Group Analysis Rendering ---
      for (trt in names(res)) {
        local({
          current_trt <- trt
          sub_res <- res[[current_trt]]
          if (!is.null(sub_res)) {
            output[[paste0(
              "mint_splsda_global_",
              current_trt
            )]] <- shiny::renderPlot({
              grDevices::replayPlot(sub_res$global_indiv_plot)
            })
            output[[paste0(
              "mint_splsda_partial_",
              current_trt
            )]] <- shiny::renderPlot({
              grDevices::replayPlot(sub_res$partial_indiv_plot)
            })
            output[[paste0(
              "mint_splsda_loadings_",
              current_trt
            )]] <- shiny::renderUI({
              shiny::req(sub_res$partial_loadings_plots)
              lapply(seq_along(sub_res$partial_loadings_plots), function(i) {
                shiny::plotOutput(
                  paste0("mint_splsda_loading_", current_trt, "_", i),
                  height = "400px"
                )
              })
            })

            lapply(seq_along(sub_res$partial_loadings_plots), function(i) {
              output[[paste0(
                "mint_splsda_loading_",
                current_trt,
                "_",
                i
              )]] <- shiny::renderPlot({
                grDevices::replayPlot(sub_res$partial_loadings_plots[[i]])
              })
            })
            output[[paste0(
              "mint_splsda_corr_",
              current_trt
            )]] <- shiny::renderPlot({
              grDevices::replayPlot(sub_res$correlation_circle_plot)
            })
            output[[paste0(
              "mint_splsda_cim_",
              current_trt
            )]] <- shiny::renderPlot(
              {
                if (
                  !is.null(sub_res$cim_obj) &&
                    inherits(sub_res$cim_obj, "recordedplot")
                ) {
                  grDevices::replayPlot(sub_res$cim_obj)
                }
              },
              height = 600
            )
            output[[paste0(
              "mint_splsda_roc_",
              current_trt
            )]] <- shiny::renderPlot({
              if (!is.null(sub_res$roc_plot)) {
                grDevices::replayPlot(sub_res$roc_plot)
              }
            })
          }
        })
      }
    }
  }
  if (func_name == "Principal Component Analysis (PCA)" && is.list(res)) {
    if ("overall_indiv_plot" %in% names(res)) {
      output$pca_indivPlot <- shiny::renderPlot({
        grDevices::replayPlot(res$overall_indiv_plot)
      })
      output$pca_3DPlot <- shiny::renderPlot({
        if (!is.null(res$overall_3D)) grDevices::replayPlot(res$overall_3D)
      })
      output$pca_screePlot <- shiny::renderPlot({
        if (!is.null(res$overall_scree_plot)) {
          grDevices::replayPlot(res$overall_scree_plot)
        }
      })
      output$pca_biplot <- shiny::renderPlot({
        if (!is.null(res$biplot)) grDevices::replayPlot(res$biplot)
      })
      output$pca_corrCircle <- shiny::renderPlot({
        if (!is.null(res$correlation_circle)) {
          grDevices::replayPlot(res$correlation_circle)
        }
      })

      output$pca_loadingsUI <- shiny::renderUI({
        if (!is.null(res$loadings)) {
          shiny::tagList(lapply(seq_along(res$loadings), function(i) {
            shiny::plotOutput(paste0("pca_loadings_", i), height = "300px")
          }))
        }
      })

      if (!is.null(res$loadings)) {
        for (i in seq_along(res$loadings)) {
          local({
            local_i <- i
            output[[paste0("pca_loadings_", local_i)]] <- shiny::renderPlot({
              grDevices::replayPlot(res$loadings[[local_i]])
            })
          })
        }
      }
    } else {
      for (lvl in names(res)) {
        local({
          currentGroup <- lvl
          subres <- res[[currentGroup]]
          output[[paste0("pca_indivPlot_", currentGroup)]] <- shiny::renderPlot(
            {
              if (!is.null(subres$overall_indiv_plot)) {
                grDevices::replayPlot(subres$overall_indiv_plot)
              }
            }
          )
          output[[paste0("pca_3DPlot_", currentGroup)]] <- shiny::renderPlot({
            if (!is.null(subres$overall_3D)) {
              grDevices::replayPlot(subres$overall_3D)
            }
          })
          output[[paste0("pca_screePlot_", currentGroup)]] <- shiny::renderPlot(
            {
              if (!is.null(subres$overall_scree_plot)) {
                grDevices::replayPlot(subres$overall_scree_plot)
              }
            }
          )
          output[[paste0("pca_biplot_", currentGroup)]] <- shiny::renderPlot({
            if (!is.null(subres$biplot)) grDevices::replayPlot(subres$biplot)
          })
          output[[paste0(
            "pca_corrCircle_",
            currentGroup
          )]] <- shiny::renderPlot({
            if (!is.null(subres$correlation_circle)) {
              grDevices::replayPlot(subres$correlation_circle)
            }
          })

          output[[paste0(
            "pca_loadingsUI_",
            currentGroup
          )]] <- shiny::renderUI({
            if (!is.null(subres$loadings)) {
              shiny::tagList(lapply(seq_along(subres$loadings), function(i) {
                safeGroup <- gsub("[^A-Za-z0-9_]+", "_", currentGroup)
                shiny::plotOutput(
                  paste0("pca_loadings_", safeGroup, "_", i),
                  height = "300px"
                )
              }))
            }
          })
          if (!is.null(subres$loadings)) {
            for (i in seq_along(subres$loadings)) {
              local({
                local_i <- i
                safeGroup <- gsub("[^A-Za-z0-9_]+", "_", currentGroup)
                output[[paste0(
                  "pca_loadings_",
                  safeGroup,
                  "_",
                  local_i
                )]] <- shiny::renderPlot({
                  grDevices::replayPlot(subres$loadings[[local_i]])
                })
              })
            }
          }
        })
      }
    }
  }

  # --- PLSR Rendering Logic ---
  if (func_name == "Partial Least Squares Regression (PLSR)") {
    # Scores (recordedplot)
    output$plsr_indivPlot <- shiny::renderPlot({
      shiny::req(res$scores_plot)
      grDevices::replayPlot(res$scores_plot)
    })

    # Predicted vs Observed (recordedplot)
    output$plsr_predPlot <- shiny::renderPlot({
      shiny::req(res$pred_vs_obs)
      grDevices::replayPlot(res$pred_vs_obs)
    })

    # Residuals vs Fitted (recordedplot)
    output$plsr_residPlot <- shiny::renderPlot({
      shiny::req(res$residuals_plot)
      grDevices::replayPlot(res$residuals_plot)
    })

    # CV plot (recordedplot) + metrics text
    output$plsr_cvPlot <- shiny::renderPlot({
      shiny::req(res$cv_plot)
      grDevices::replayPlot(res$cv_plot)
    })

    # Per-component loadings (each element is a recordedplot)
    output$plsr_loadingsUI <- shiny::renderUI({
      shiny::req(res$loadings)
      shiny::tagList(lapply(seq_along(res$loadings), function(i) {
        shiny::plotOutput(paste0("plsr_loading_plot_", i), height = "350px")
      }))
    })
    if (!is.null(res$loadings)) {
      for (i in seq_along(res$loadings)) {
        local({
          ii <- i
          output[[paste0("plsr_loading_plot_", ii)]] <- shiny::renderPlot({
            grDevices::replayPlot(res$loadings[[ii]])
          })
        })
      }
    }

    # VIP barplots (ggplot objects — do NOT use replayPlot)
    output$plsr_vipUI <- shiny::renderUI({
      shiny::req(res$vip_scores)
      shiny::tagList(lapply(seq_along(res$vip_scores), function(i) {
        shiny::plotOutput(paste0("plsr_vip_plot_", i), height = "350px")
      }))
    })
    if (!is.null(res$vip_scores)) {
      for (i in seq_along(res$vip_scores)) {
        local({
          ii <- i
          output[[paste0("plsr_vip_plot_", ii)]] <- shiny::renderPlot({
            # ggplot object; just return it
            res$vip_scores[[ii]]
          })
        })
      }
    }

    # VIP>1 preview plots (recordedplot, conditional)
    output$plsr_vipIndivPlot <- shiny::renderPlot({
      shiny::req(res$vip_scores_indiv)
      grDevices::replayPlot(res$vip_scores_indiv)
    })
    output$plsr_vipCVPlot <- shiny::renderPlot({
      shiny::req(res$vip_cv_plot)
      grDevices::replayPlot(res$vip_cv_plot)
    })
  }

  # --- Correlation Rendering Logic ---
  # Tables
  output$corr_tbl_spearman <- DT::renderDataTable({
    shiny::req(res$spearman$table)
    DT::datatable(
      res$spearman$table,
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$corr_tbl_pearson <- DT::renderDataTable({
    shiny::req(res$pearson$table)
    DT::datatable(
      res$pearson$table,
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # Heatmaps (overall)
  output$corr_heatmap_spearman <- shiny::renderPlot({
    shiny::req(res$spearman$plot)
    print(res$spearman$plot)
  })

  output$corr_heatmap_pearson <- shiny::renderPlot({
    shiny::req(res$pearson$plot)
    print(res$pearson$plot)
  })

  # Per-group heatmaps (if any) — Spearman
  output$corr_group_heatmap_ui_spearman <- shiny::renderUI({
    shiny::req(res$spearman$group_plots)
    tabs <- lapply(names(res$spearman$group_plots), function(lv) {
      shiny::tabPanel(
        title = lv,
        shiny::plotOutput(
          paste0("corr_heatmap_grp_spear_", gsub("\\W+", "_", lv)),
          height = "600px"
        )
      )
    })
    do.call(shiny::tabsetPanel, c(list(type = "tabs"), tabs))
  })

  if (!is.null(res$spearman$group_plots)) {
    for (lv in names(res$spearman$group_plots)) {
      local({
        lvl <- lv
        output[[paste0(
          "corr_heatmap_grp_spear_",
          gsub("\\W+", "_", lvl)
        )]] <- shiny::renderPlot({
          print(res$spearman$group_plots[[lvl]])
        })
      })
    }
  }

  # Per-group heatmaps (if any) — Pearson
  output$corr_group_heatmap_ui_pearson <- shiny::renderUI({
    shiny::req(res$pearson$group_plots)
    tabs <- lapply(names(res$pearson$group_plots), function(lv) {
      shiny::tabPanel(
        title = lv,
        shiny::plotOutput(
          paste0("corr_heatmap_grp_pear_", gsub("\\W+", "_", lv)),
          height = "600px"
        )
      )
    })
    do.call(shiny::tabsetPanel, c(list(type = "tabs"), tabs))
  })

  if (!is.null(res$pearson$group_plots)) {
    for (lv in names(res$pearson$group_plots)) {
      local({
        lvl <- lv
        output[[paste0(
          "corr_heatmap_grp_pear_",
          gsub("\\W+", "_", lvl)
        )]] <- shiny::renderPlot({
          print(res$pearson$group_plots[[lvl]])
        })
      })
    }
  }

  # --- Random Forest Rendering Logic ---
  if (func_name == "Random Forest") {
    output$rf_summary <- shiny::renderUI({
      copyable_text_block(res$summary_text)
    })
    output$rf_vipPlot <- shiny::renderPlot({
      print(res$vip_plot)
    })
    output$rf_rocPlot <- shiny::renderPlot({
      if (!is.null(res$roc_plot)) print(res$roc_plot)
    })
    output$rf_rfcvPlot <- shiny::renderPlot({
      if (!is.null(res$rfcv_plot)) print(res$rfcv_plot)
    })
  }

  # --- XGBoost Rendering Logic ---
  if (func_name == "Extreme Gradient Boosting (XGBoost)") {
    output$xgb_summary <- shiny::renderUI({
      copyable_text_block(res$summary_text)
    })
    output$xgb_vipPlot <- shiny::renderPlot({
      print(res$importance_plot)
    })
    output$xgb_rocPlot <- shiny::renderPlot({
      if (!is.null(res$roc_plot)) print(res$roc_plot)
    })
  }

  # --- Volcano Plot Rendering Logic ---
  if (func_name == "Volcano Plot") {
    output$volcPlotOutput <- shiny::renderPlot({
      print(res$plot)
    })
    output$volcStats <- DT::renderDataTable(
      {
        res$stats
      },
      options = list(pageLength = 10, scrollX = TRUE)
    )
  }

  # --- Heatmap Rendering Logic ---
  if (func_name == "Heatmap") {
    output$heatmapImage <- shiny::renderImage(
      {
        shiny::req(res)

        # res is the pheatmap object
        ph <- res

        # size for preview; UI will scale it down responsively
        panel_px <- session$clientData$output_heatmapImage_width %||% 1000
        out_png <- file.path(
          tempdir(),
          paste0("cyt_heatmap_", Sys.Date(), ".png")
        )

        grDevices::png(
          out_png,
          width = panel_px,
          height = round(panel_px * 0.62),
          res = 96
        )
        grid::grid.newpage()
        grid::grid.draw(ph$gtable) # draw the heatmap object
        grDevices::dev.off()

        list(src = out_png, contentType = "image/png", alt = "Heatmap")
      },
      deleteFile = FALSE
    )
  }

  # --- Dual-Flashlight Plot Rendering ---
  if (func_name == "Dual-Flashlight Plot") {
    output$dualflashPlotOutput <- shiny::renderPlot({
      print(res$plot)
    })
    output$dualflashStats <- DT::renderDataTable(
      {
        res$stats
      },
      options = list(pageLength = 10, scrollX = TRUE)
    )
  }
  # --- Skewness/Kurtosis Rendering ---
  if (func_name == "Skewness/Kurtosis") {
    output$skku_skewPlot <- shiny::renderPlot({
      print(res$p_skew)
    })
    output$skku_kurtPlot <- shiny::renderPlot({
      print(res$p_kurt)
    })
    output$skku_raw_results <- DT::renderDataTable(
      {
        res$raw_results
      },
      options = list(pageLength = 10, scrollX = TRUE)
    )
    output$skku_log_results <- DT::renderDataTable(
      {
        res$log_results
      },
      options = list(pageLength = 10, scrollX = TRUE)
    )
  }

  # --- Generic Plot List Rendering Logic (for Boxplots, etc.) ---
  if (
    is.list(res) &&
      length(res) > 0 &&
      all(sapply(res, function(x) inherits(x, "ggplot")))
  ) {
    lapply(seq_along(res), function(i) {
      local({
        my_i <- i
        output[[paste0("dynamicPlot_", my_i)]] <- shiny::renderPlot({
          res[[my_i]]
        })
      })
    })
  }
})
# --- Univariate results table ---
output$univariateResults <- DT::renderDataTable(
  {
    res <- analysisResult()
    shiny::req(
      selected_function() %in%
        c(
          "Univariate Tests (T-test, Wilcoxon)",
          "Multi-level Univariate Tests (Anova, Kruskal-Wallis)",
          "Two-way ANOVA",
          "ANCOVA"
        )
    )
    # Multi-level tests return a named list; t-test/Wilcoxon returns a plain data frame
    if (is.list(res) && !is.data.frame(res) && !is.null(res$results)) {
      res$results
    } else {
      res
    }
  },
  options = list(pageLength = 10, scrollX = TRUE),
  rownames = FALSE
)
output$univariatePairwiseResults <- DT::renderDataTable(
  {
    res <- analysisResult()
    shiny::req(
      selected_function() %in% c(
        "Multi-level Univariate Tests (Anova, Kruskal-Wallis)",
        "Two-way ANOVA",
        "ANCOVA"
      ),
      is.list(res),
      !is.null(res$pairwise)
    )
    res$pairwise
  },
  options = list(pageLength = 10, scrollX = TRUE),
  rownames = FALSE
)
output$anovaAssumptionTable <- DT::renderDataTable(
  {
    res <- analysisResult()
    shiny::req(
      selected_function() %in% c(
        "Multi-level Univariate Tests (Anova, Kruskal-Wallis)",
        "Two-way ANOVA",
        "ANCOVA"
      ),
      is.list(res),
      !is.null(res$assumptions)
    )
    res$assumptions
  },
  options = list(pageLength = 10, scrollX = TRUE),
  rownames = FALSE
)
output$errorBarPlotOutput <- shiny::renderPlot(
  {
    shiny::req(analysisResult())
    res <- analysisResult()
    print(res)
  },
  height = function() {
    res <- analysisResult()
    if (is.null(res) || !inherits(res, "ggplot")) {
      return(400L)
    }
    # Extract number of facets from the built plot data if possible
    n_facets <- tryCatch(
      {
        pb <- ggplot2::ggplot_build(res)
        length(unique(pb$data[[1]]$PANEL))
      },
      error = function(e) 9L
    )
    n_cols <- input$eb_n_col %||% 3L
    n_rows <- ceiling(n_facets / n_cols)
    max(400L, n_rows * 180L)
  }
)

# Render the download UI conditionally
output$download_ui <- shiny::renderUI({
  plots <- exportablePlots()
  tables <- exportableTables()

  if (length(plots) > 0) {
    shiny::tagList(
      shiny::textInput(
        "download_filename",
        "Enter filename for download:",
        value = "Results"
      ),
      shiny::selectInput(
        "download_format",
        "Export format",
        choices = c(
          "PDF" = "pdf",
          "PNG" = "png",
          "JPEG" = "jpeg",
          "TIFF" = "tiff",
          "SVG" = "svg"
        ),
        selected = "pdf"
      ),
      shiny::downloadButton("download_output", "Download Results")
    )
  } else if (length(tables) > 0) {
    is_multi_univariate <-
      selected_function() %in% c(
        "Multi-level Univariate Tests (Anova, Kruskal-Wallis)",
        "Two-way ANOVA",
        "ANCOVA"
      )
    shiny::tagList(
      shiny::textInput(
        "download_filename",
        "Enter filename for download:",
        value = if (is_multi_univariate) {
          "Results"
        } else {
          "Results"
        }
      ),
      shiny::downloadButton(
        "download_output",
        if (is_multi_univariate) {
          "Download Results (ZIP)"
        } else {
          "Download Results (CSV)"
        }
      )
    )
  }
})

# Download handler
output$download_output <- shiny::downloadHandler(
  filename = function() {
    tables <- exportableTables()
    if (length(tables) > 0 && length(exportablePlots()) == 0) {
      ext <- if (length(tables) > 1L) "zip" else "csv"
      return(paste0(
        input$download_filename %||% "Results",
        ".",
        ext
      ))
    }

    fmt <- tolower(input$download_format %||% "pdf")
    plot_count <- length(exportablePlots())
    ext <- if (fmt == "pdf" || plot_count <= 1L) fmt else "zip"
    paste0(input$download_filename %||% "Results", ".", ext)
  },
  content = function(file) {
    tables <- exportableTables()
    if (length(tables) > 0 && length(exportablePlots()) == 0) {
      if (length(tables) == 1L) {
        utils::write.csv(tables[[1]], file, row.names = FALSE)
        return(invisible(NULL))
      }

      base_file <- tools::file_path_sans_ext(file)
      generated_files <- vapply(
        names(tables),
        function(table_name) {
          generated_file <- paste0(base_file, "_", table_name, ".csv")
          utils::write.csv(
            tables[[table_name]],
            generated_file,
            row.names = FALSE
          )
          basename(generated_file)
        },
        character(1)
      )

      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(dirname(file))
      utils::zip(zipfile = file, files = generated_files)
      return(invisible(NULL))
    }

    plots <- exportablePlots()
    shiny::req(length(plots) > 0)

    fmt <- tolower(input$download_format %||% "pdf")
    base_file <- tools::file_path_sans_ext(file)

    if (fmt == "pdf") {
      cyt_export(
        plots = plots,
        filename = base_file,
        format = fmt
      )
      return(invisible(NULL))
    }

    cyt_export(
      plots = plots,
      filename = base_file,
      format = fmt
    )

    if (length(plots) == 1L) {
      generated_file <- paste0(base_file, "_001.", fmt)
      if (!file.rename(generated_file, file)) {
        file.copy(generated_file, file, overwrite = TRUE)
      }
      return(invisible(NULL))
    }

    generated_files <- list.files(
      path = dirname(file),
      pattern = paste0("^", basename(base_file), "_[0-9]{3}\\.", fmt, "$"),
      full.names = TRUE
    )
    if (!length(generated_files)) {
      stop("No export files were generated.")
    }

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(dirname(file))
    utils::zip(zipfile = file, files = basename(generated_files))
  }
)
