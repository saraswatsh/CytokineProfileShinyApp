## ---------------------------
## Analysis and Results
## ---------------------------
errorMessage <- shiny::reactiveVal(NULL)
warningMessage <- shiny::reactiveVal(NULL)

analysisResult <- shiny::eventReactive(input$next4, {
  errorMessage(NULL)
  warningMessage(NULL)

  req(filteredData())
  prog <- Progress$new()
  on.exit(prog$close())

  tryCatch(
    {
      withCallingHandlers(
        {
          df <- filteredData()
          df$..cyto_id.. <- NULL

          # Simplified: always generate a file path for functions that can output a PDF

          func_to_run <- selected_function()
          req(func_to_run)

          results <- switch(
            func_to_run,

            # -- Statistical Tests --
            "ANOVA" = cyt_anova(
              data = df,
              progress = prog,
              format_output = TRUE
            ),

            "Two-Sample T-Test" = cyt_ttest(
              data = df,
              progress = prog,
              scale = NULL,
              format_output = TRUE
            ),

            # -- Exploratory Visualization --
            "Boxplots" = cyt_bp(
              data = df,

              progress = prog,
              bin_size = input$bp_bin_size,
              mf_row = if (nzchar(input$bp_mf_row)) {
                as.numeric(strsplit(input$bp_mf_row, ",")[[1]])
              },
              y_lim = if (nzchar(input$bp_y_lim)) {
                as.numeric(strsplit(input$bp_y_lim, ",")[[1]])
              },
              scale = NULL
            ),

            "Enhanced Boxplots" = cyt_bp2(
              data = df,

              progress = prog,
              mf_row = if (nzchar(input$bp2_mf_row)) {
                as.numeric(strsplit(input$bp2_mf_row, ",")[[1]])
              },
              y_lim = if (nzchar(input$bp2_y_lim)) {
                as.numeric(strsplit(input$bp2_y_lim, ",")[[1]])
              },
              scale = NULL
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
              log2 = FALSE
            ),

            "Dual-Flashlight Plot" = cyt_dualflashplot(
              data = df,
              group_var = input$df_group_var,
              group1 = input$df_cond1,
              group2 = input$df_cond2,

              progress = prog,
              ssmd_thresh = input$df_ssmd_thresh,
              log2fc_thresh = input$df_log2fc_thresh,
              top_labels = input$df_top_labels
            ),
            "Heatmap" = {
              scale_arg <- if (
                is.null(input$hm_scale) || input$hm_scale == "none"
              ) {
                "none"
              } else {
                input$hm_scale
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
                scale = if (identical(scale_arg, "none")) NULL else scale_arg,
                annotation_col = ann_arg,
                annotation_side = side_arg
              )
              ph # <- return the pheatmap object
            },

            "Skewness/Kurtosis" = cyt_skku(
              data = df,

              progress = prog,
              group_cols = input$skku_group_cols,
              print_res_raw = input$skku_print_raw,
              print_res_log = input$skku_print_log
            ),

            "Volcano Plot" = cyt_volc(
              data = df,

              progress = prog,
              group_col = input$volc_group_col,
              cond1 = input$volc_cond1,
              cond2 = input$volc_cond2,
              fold_change_thresh = input$volc_fold_change_thresh,
              p_value_thresh = input$volc_p_value_thresh,
              top_labels = input$volc_top_labels
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
                pca_colors = cols
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

              res <- cyt_plsr(
                data = df,
                group_col = grp,
                response_col = input$plsr_response_col,
                comp_num = input$plsr_comp_num,
                sparse = input$plsr_sparse,
                var_num = input$plsr_keepX,
                scale = NULL,
                ellipse = isTRUE(input$plsr_ellipse),
                cv_opt = if (input$plsr_cv_opt == "None") {
                  NULL
                } else {
                  tolower(input$plsr_cv_opt)
                },
                fold_num = input$plsr_fold_num,
                pls_colors = input$plsr_colors,
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
                  req(input$splsda_ind_names_col)
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
                font_scale = input$splsda_fontsize
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
                bg = input$mint_splsda_bg
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
              step = input$rf_step
            ),

            "Extreme Gradient Boosting (XGBoost)" = cyt_xgb(
              data = df,

              progress = prog,
              group_col = input$xgb_group_col,
              train_fraction = input$xgb_train_fraction,
              nrounds = input$xgb_nrounds,
              max_depth = input$xgb_max_depth,
              eta = input$xgb_eta,
              nfold = input$xgb_nfold,
              cv = isTRUE(input$xgb_cv),
              eval_metric = input$xgb_eval_metric,
              top_n_features = input$xgb_top_n_features,
              plot_roc = isTRUE(input$xgb_plot_roc)
            )
          ) # end switch

          results
        },
        warning = function(w) {
          warningMessage(conditionMessage(w))
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
  req(analysisResult())
  analysisResult()
})

output$errorText <- shiny::renderUI({
  req(errorMessage())
  div(
    style = "color:red; padding:5px; border:1px solid red;",
    strong("Error:"),
    tags$pre(errorMessage())
  )
})

output$warningText <- shiny::renderUI({
  req(warningMessage())
  div(
    style = "color:orange; padding:5px; border:1px solid orange;",
    strong("Warning:"),
    tags$pre(warningMessage())
  )
})

output$result_display <- shiny::renderUI({
  err <- errorMessage()
  if (isTruthy(err)) {
    return(tagList(
      uiOutput("errorText"),
      actionButton(
        "back6",
        "Change Inputs",
        icon = icon("arrow-left"),
        class = "btn-secondary"
      )
    ))
  }
  req(analysisResult())
  res <- analysisResult()
  func_name <- selected_function()
  req(func_name)

  # Main UI container
  tagList(
    uiOutput("warningText"), # Display warnings if any
    hr(),

    # Interactive Mode: Use a switch to render the correct UI
    switch(
      func_name,

      # --- sPLS-DA UI ---
      "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = {
        if ("overall_indiv_plot" %in% names(res)) {
          # Single analysis UI
          tagList(
            h4("sPLS-DA Results"),
            tabsetPanel(
              id = "splsda_tabs",
              type = "tabs",
              tabPanel(
                "sPLS-DA Plot",
                shinycssloaders::withSpinner(
                  plotOutput("splsda_overallIndivPlot", height = "500px"),
                  type = 8
                )
              ),
              tabPanel(
                "Loadings",
                shinycssloaders::withSpinner(
                  uiOutput("splsda_loadingsUI"),
                  type = 8
                )
              ),
              tabPanel(
                "VIP Scores",
                shinycssloaders::withSpinner(
                  uiOutput("splsda_vipScoresUI"),
                  type = 8
                )
              ),
              if (!is.null(res$vip_indiv_plot)) {
                tabPanel(
                  "VIP Model Plot",
                  shinycssloaders::withSpinner(
                    plotOutput("splsda_vipIndivPlot", height = "500px"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$vip_loadings)) {
                tabPanel(
                  "VIP Loadings",
                  shinycssloaders::withSpinner(
                    uiOutput("splsda_vipLoadingsUI"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$overall_3D)) {
                tabPanel(
                  "3D Plot",
                  shinycssloaders::withSpinner(
                    plotOutput("splsda_overall3DPlot", height = "500px"),
                    type = 8
                  ),
                  # break line
                  br(),
                  actionButton(
                    "splsda_show3d_interactive",
                    "Interactive 3D",
                    icon = icon("fas fa-cube")
                  ),
                  # break line
                  br(),
                )
              },
              if (!is.null(res$vip_3D)) {
                tabPanel(
                  "3D Plot (VIP>1)",
                  shinycssloaders::withSpinner(
                    plotOutput("splsda_vip3DPlot", height = "500px"),
                    type = 8
                  ),
                  # break line
                  br(),
                  actionButton(
                    "splsda_show3d_interactive_vip",
                    "Interactive 3D (VIP)",
                    icon = icon("fas fa-cube")
                  ),
                  # break line
                  br(),
                )
              },

              if (!is.null(res$overall_ROC)) {
                tabPanel(
                  "ROC",
                  shinycssloaders::withSpinner(
                    plotOutput("splsda_overallRocPlot", height = "400px"),
                    type = 8
                  )
                )
              },

              if (!is.null(res$overall_CV)) {
                tabPanel(
                  "Cross-Validation",
                  shinycssloaders::withSpinner(
                    plotOutput("splsda_overallCvPlot", height = "400px"),
                    type = 8
                  )
                )
              },
              if (!is.null(res$conf_matrix)) {
                tabPanel(
                  "Confusion Matrix",
                  shinycssloaders::withSpinner(
                    verbatimTextOutput("splsda_confMatrix"),
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
                tabPanel(
                  title = trt,
                  tabsetPanel(
                    type = "tabs",
                    tabPanel(
                      "sPLS-DA Plot",
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("splsda_overallIndivPlot_", trt),
                          height = "500px"
                        ),
                        type = 8
                      )
                    ),
                    tabPanel(
                      "Loadings",
                      shinycssloaders::withSpinner(
                        uiOutput(paste0("splsda_loadingsUI_", trt)),
                        type = 8
                      )
                    ),
                    tabPanel(
                      "VIP Scores",
                      shinycssloaders::withSpinner(
                        uiOutput(paste0("splsda_vipScoresUI_", trt)),
                        type = 8
                      )
                    ),
                    if (!is.null(res[[trt]]$vip_indiv_plot)) {
                      tabPanel(
                        "VIP Model Plot",
                        shinycssloaders::withSpinner(
                          plotOutput(
                            paste0("splsda_vipIndivPlot_", trt),
                            height = "500px"
                          ),
                          type = 8
                        )
                      )
                    },
                    if (!is.null(res[[trt]]$vip_loadings)) {
                      tabPanel(
                        "VIP Loadings",
                        shinycssloaders::withSpinner(
                          uiOutput(paste0("splsda_vipLoadingsUI_", trt)),
                          type = 8
                        )
                      )
                    },
                    if (!is.null(res[[trt]]$overall_3D)) {
                      tabPanel(
                        "3D Plot",
                        shinycssloaders::withSpinner(
                          plotOutput(
                            paste0("splsda_overall3DPlot_", trt),
                            height = "500px"
                          ),
                          type = 8
                        ),
                        # break line
                        br(),
                        actionButton(
                          paste0("splsda_show3d_interactive_", trt),
                          "Interactive 3D",
                          icon = icon("fas fa-cube")
                        ),
                        # break line
                        br(),
                      )
                    },

                    # VIP 3D tab per-trt (no div wrapper)
                    if (!is.null(res[[trt]]$vip_3D)) {
                      tabPanel(
                        "3D Plot (VIP>1)",
                        shinycssloaders::withSpinner(
                          plotOutput(
                            paste0("splsda_vip3DPlot_", trt),
                            height = "500px"
                          ),
                          type = 8
                        ),
                        # break line
                        br(),
                        actionButton(
                          paste0("splsda_show3d_interactive_vip_", trt),
                          "Interactive 3D (VIP)",
                          icon = icon("fas fa-cube")
                        ),
                        # break line
                        br(),
                      )
                    },
                    if (!is.null(res[[trt]]$overall_ROC)) {
                      tabPanel(
                        "ROC",
                        shinycssloaders::withSpinner(
                          plotOutput(
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
                      tabPanel(
                        "Cross-Validation",
                        shinycssloaders::withSpinner(
                          plotOutput(
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
                      tabPanel(
                        "Confusion Matrix",
                        shinycssloaders::withSpinner(
                          verbatimTextOutput(
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
          tagList(
            h4("MINT sPLS-DA Results"),
            tabsetPanel(
              id = "mint_splsda_tabs",
              tabPanel(
                "Global Sample Plot",
                withSpinner(plotOutput("mint_splsda_global_plot"), type = 8)
              ),
              tabPanel(
                "Partial Sample Plots",
                withSpinner(plotOutput("mint_splsda_partial_plot"), type = 8)
              ),
              tabPanel(
                "Variable Loadings",
                withSpinner(uiOutput("mint_splsda_loadings_ui"), type = 8)
              ),
              tabPanel(
                "Correlation Circle",
                withSpinner(
                  plotOutput("mint_splsda_corr_circle_plot"),
                  type = 8
                )
              ),
              if (!is.null(res$cim_obj)) {
                tabPanel(
                  "Heatmap (CIM)",
                  withSpinner(
                    plotOutput(
                      "mint_splsda_cim_plot",
                      height = "600px"
                    ),
                    type = 8
                  )
                )
              },
              if (!is.null(res$roc_plot)) {
                tabPanel(
                  "ROC Curve",
                  withSpinner(plotOutput("mint_splsda_roc_plot"), type = 8)
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
                tabPanel(
                  title = trt,
                  tabsetPanel(
                    type = "tabs",
                    tabPanel(
                      "Global Plot",
                      withSpinner(
                        plotOutput(paste0(
                          "mint_splsda_global_",
                          trt
                        )),
                        type = 8
                      )
                    ),
                    tabPanel(
                      "Partial Plots",
                      withSpinner(
                        plotOutput(paste0(
                          "mint_splsda_partial_",
                          trt
                        )),
                        type = 8
                      )
                    ),
                    tabPanel(
                      "Variable Loadings",
                      withSpinner(
                        uiOutput(paste0(
                          "mint_splsda_loadings_",
                          trt
                        )),
                        type = 8
                      )
                    ),
                    tabPanel(
                      "Correlation",
                      withSpinner(
                        plotOutput(paste0(
                          "mint_splsda_corr_",
                          trt
                        )),
                        type = 8
                      )
                    ),
                    if (!is.null(res[[trt]]$cim_obj)) {
                      tabPanel(
                        "CIM",
                        withSpinner(
                          plotOutput(
                            paste0("mint_splsda_cim_", trt),
                            height = "600px"
                          ),
                          type = 8
                        )
                      )
                    },
                    if (!is.null(res[[trt]]$roc_plot)) {
                      tabPanel(
                        "ROC",
                        withSpinner(
                          plotOutput(paste0(
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
          tagList(
            h4("PCA Results"),
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "PCA Plot",
                shinycssloaders::withSpinner(
                  plotOutput("pca_indivPlot", height = "400px"),
                  type = 8
                )
              ),
              tabPanel(
                "Scree Plot",
                shinycssloaders::withSpinner(
                  plotOutput("pca_screePlot", height = "400px"),
                  type = 8
                )
              ),
              tabPanel(
                "Loadings Plots",
                shinycssloaders::withSpinner(
                  uiOutput("pca_loadingsUI"),
                  type = 8
                )
              ),
              tabPanel(
                "Biplot",
                shinycssloaders::withSpinner(
                  plotOutput("pca_biplot", height = "400px"),
                  type = 8
                )
              ),
              tabPanel(
                "Correlation Circle",
                shinycssloaders::withSpinner(
                  plotOutput("pca_corrCircle", height = "400px"),
                  type = 8
                )
              ),
              if (!is.null(res$overall_3D)) {
                tabPanel(
                  "3D Plot",
                  shinycssloaders::withSpinner(
                    plotOutput("pca_3DPlot", height = "400px"),
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
                tabPanel(
                  title = trt,
                  tabsetPanel(
                    type = "tabs",
                    tabPanel(
                      "PCA Plot",
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("pca_indivPlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    ),
                    tabPanel(
                      "Scree Plot",
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("pca_screePlot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    ),
                    tabPanel(
                      "Loadings Plots",
                      shinycssloaders::withSpinner(
                        uiOutput(paste0("pca_loadingsUI_", trt)),
                        type = 8
                      )
                    ),
                    tabPanel(
                      "Biplot",
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("pca_biplot_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    ),
                    tabPanel(
                      "Correlation Circle",
                      shinycssloaders::withSpinner(
                        plotOutput(
                          paste0("pca_corrCircle_", trt),
                          height = "400px"
                        ),
                        type = 8
                      )
                    ),
                    if (!is.null(res[[trt]]$overall_3D)) {
                      tabPanel(
                        "3D Plot",
                        shinycssloaders::withSpinner(
                          plotOutput(
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
        tagList(
          h4("PLSR Results"),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Scores Plot",
              shinycssloaders::withSpinner(
                plotOutput("plsr_indivPlot", height = "400px"),
                type = 8
              )
            ),
            tabPanel(
              "Predicted vs Observed",
              shinycssloaders::withSpinner(
                plotOutput("plsr_predPlot", height = "400px"),
                type = 8
              )
            ),
            tabPanel(
              "Residuals vs Fitted",
              shinycssloaders::withSpinner(
                plotOutput("plsr_residPlot", height = "400px"),
                type = 8
              )
            ),
            tabPanel(
              "Loadings Plots",
              shinycssloaders::withSpinner(
                uiOutput("plsr_loadingsUI"),
                type = 8
              )
            ),
            tabPanel(
              "VIP Scores",
              shinycssloaders::withSpinner(
                uiOutput("plsr_vipUI"),
                type = 8
              )
            ),
            if (!is.null(res$cv_plot)) {
              tabPanel(
                "Cross-Validation",
                shinycssloaders::withSpinner(
                  plotOutput("plsr_cvPlot", height = "400px"),
                  type = 8
                )
              )
            },
            if (!is.null(res$vip_scores_indiv)) {
              tabPanel(
                "VIP > 1: Scores",
                shinycssloaders::withSpinner(
                  plotOutput("plsr_vipIndivPlot", height = "400px"),
                  type = 8
                )
              )
            },
            if (!is.null(res$vip_cv_plot)) {
              tabPanel(
                "VIP > 1: Cross-Validation",
                shinycssloaders::withSpinner(
                  plotOutput("plsr_vipCVPlot", height = "400px"),
                  type = 8
                )
              )
            }
          )
        )
      },

      # --- Correlation UI ---
      "Correlation Plots" = {
        tagList(
          h4("Correlation Results"),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Spearman",
              tabsetPanel(
                type = "tabs",
                selected = "Spearman Heatmap",
                tabPanel(
                  "Spearman Heatmap",
                  shinycssloaders::withSpinner(
                    plotOutput("corr_heatmap_spearman", height = "600px"),
                    type = 8
                  )
                ),
                tabPanel(
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
                  tabPanel(
                    "Spearman Per-Group Heatmaps",
                    shinycssloaders::withSpinner(
                      uiOutput("corr_group_heatmap_ui_spearman"),
                      type = 8
                    )
                  )
                }
              )
            ),
            tabPanel(
              "Pearson",
              tabsetPanel(
                type = "tabs",
                selected = "Pearson Heatmap", # <- default
                tabPanel(
                  "Pearson Heatmap",
                  shinycssloaders::withSpinner(
                    plotOutput("corr_heatmap_pearson", height = "600px"),
                    type = 8
                  )
                ),
                tabPanel(
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
                  tabPanel(
                    "Pearson Per-Group Heatmaps",
                    shinycssloaders::withSpinner(
                      uiOutput("corr_group_heatmap_ui_pearson"),
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
        tagList(
          h4("Random Forest Results"),
          tabsetPanel(
            type = "tabs",
            tabPanel("Summary", verbatimTextOutput("rf_summary")),
            tabPanel(
              "Variable Importance",
              shinycssloaders::withSpinner(
                plotOutput("rf_vipPlot", height = "400px"),
                type = 8
              )
            ),
            if (!is.null(res$roc_plot)) {
              tabPanel(
                "ROC Curve",
                shinycssloaders::withSpinner(
                  plotOutput("rf_rocPlot", height = "400px"),
                  type = 8
                )
              )
            },
            if (!is.null(res$rfcv_plot)) {
              tabPanel(
                "Cross-Validation",
                shinycssloaders::withSpinner(
                  plotOutput("rf_rfcvPlot", height = "400px"),
                  type = 8
                )
              )
            }
          )
        )
      },

      # --- XGBoost UI ---
      "Extreme Gradient Boosting (XGBoost)" = {
        tagList(
          h4("XGBoost Results"),
          tabsetPanel(
            type = "tabs",
            tabPanel("Summary", verbatimTextOutput("xgb_summary")),
            tabPanel(
              "Variable Importance",
              shinycssloaders::withSpinner(
                plotOutput("xgb_vipPlot", height = "400px"),
                type = 8
              )
            ),
            if (!is.null(res$roc_plot)) {
              tabPanel(
                "ROC Curve",
                shinycssloaders::withSpinner(
                  plotOutput("xgb_rocPlot", height = "400px"),
                  type = 8
                )
              )
            }
          )
        )
      },

      # --- Volcano Plot UI ---
      "Volcano Plot" = {
        tagList(
          h4("Volcano Plot Results"),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(
                plotOutput("volcPlotOutput", height = "400px"),
                type = 8
              )
            ),
            tabPanel(
              "Statistics Table",
              shinycssloaders::withSpinner(
                DT::dataTableOutput("volcStats"),
                type = 8
              )
            )
          ),
          div(
            style = "margin-top: .75rem;",
            p(
              tags$a(
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
        tagList(
          h4("Heatmap Results"),
          shinycssloaders::withSpinner(
            imageOutput("heatmapImage", height = "auto", width = "100%"),
            type = 8
          )
        )
      },

      # --- Dual-Flashlight Plot UI ---
      "Dual-Flashlight Plot" = {
        tagList(
          h4("Dual-Flashlight Plot Results"),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Plot",
              shinycssloaders::withSpinner(
                plotOutput("dualflashPlotOutput", height = "400px"),
                type = 8
              )
            ),
            tabPanel(
              "Statistics Table",
              shinycssloaders::withSpinner(
                DT::dataTableOutput("dualflashStats"),
                type = 8
              )
            )
          ),
          div(
            style = "margin-top: .75rem;",
            p(
              tags$a(
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
        tagList(
          h4("Skewness and Kurtosis Results"),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Skewness Plot",
              shinycssloaders::withSpinner(
                plotOutput("skku_skewPlot", height = "400px"),
                type = 8
              )
            ),
            tabPanel(
              "Kurtosis Plot",
              shinycssloaders::withSpinner(
                plotOutput("skku_kurtPlot", height = "400px"),
                type = 8
              )
            ),
            if (input$skku_print_raw) {
              tabPanel(
                "Raw Data",
                shinycssloaders::withSpinner(
                  DT::dataTableOutput("skku_raw_results"),
                  type = 8
                )
              )
            },
            if (input$skku_print_log) {
              tabPanel(
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
        tagList(
          h4("Error-Bar Plot Results"),
          shinycssloaders::withSpinner(
            plotOutput("errorBarPlotOutput", height = "400px"),
            type = 8
          )
        )
      },
      "ANOVA" = {
        tagList(
          h4("ANOVA Results"),
          shinycssloaders::withSpinner(
            DT::dataTableOutput("anovaResults"),
            type = 8
          )
        )
      },
      "Two-Sample T-Test" = {
        tagList(
          h4("Two-Sample T-Test Results"),
          shinycssloaders::withSpinner(
            DT::dataTableOutput("ttestResults"),
            type = 8
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
            tagList,
            lapply(seq_along(res), function(i) {
              shinycssloaders::withSpinner(
                plotOutput(paste0("dynamicPlot_", i), height = "400px"),
                type = 8
              )
            })
          )
        }
      }
    )
  )
}) %>%
  shiny::bindCache(
    reactiveValuesToList(userState, all.names = TRUE),
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
}) %>%
  shiny::bindCache(
    reactiveValuesToList(userState, all.names = TRUE),
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
        output[[paste0("dynamicPlot_", my_i)]] <- renderPlot({
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
  req(res, func_name)

  # --- sPLS-DA Rendering Logic ---
  if (
    func_name ==
      "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
  ) {
    if ("overall_indiv_plot" %in% names(res)) {
      # --- Single Analysis Rendering ---
      output$splsda_overallIndivPlot <- renderPlot({
        replayPlot(res$overall_indiv_plot)
      })
      output$splsda_overall3DPlot <- renderPlot({
        if (!is.null(res$overall_3D)) replayPlot(res$overall_3D)
      })
      output$splsda_vip3DPlot <- renderPlot({
        if (!is.null(res$vip_3D)) replayPlot(res$vip_3D)
      })
      output$splsda_interactive_plot <- renderPlotly({
        res$overall_3D_interactive
      })
      output$splsda_interactive_plot_vip <- renderPlotly({
        res$vip_3D_interactive
      })
      output$splsda_overallRocPlot <- renderPlot({
        if (!is.null(res$overall_ROC)) replayPlot(res$overall_ROC)
      })
      output$splsda_overallCvPlot <- renderPlot({
        if (!is.null(res$overall_CV)) print(res$overall_CV)
      })
      output$splsda_vipIndivPlot <- renderPlot({
        if (!is.null(res$vip_indiv_plot)) replayPlot(res$vip_indiv_plot)
      })
      output$splsda_confMatrix <- renderPrint({
        if (!is.null(res$conf_matrix)) {
          cat(paste(res$conf_matrix, collapse = "\n"))
        }
      })

      output$splsda_loadingsUI <- renderUI({
        req(res$loadings)
        lapply(seq_along(res$loadings), function(i) {
          plotOutput(paste0("splsda_loading_plot_", i), height = "400px")
        })
      })
      lapply(seq_along(res$loadings), function(i) {
        output[[paste0("splsda_loading_plot_", i)]] <- renderPlot({
          replayPlot(res$loadings[[i]])
        })
      })

      output$splsda_vipScoresUI <- renderUI({
        req(res$vip_scores)
        lapply(seq_along(res$vip_scores), function(i) {
          plotOutput(paste0("splsda_vip_plot_", i), height = "400px")
        })
      })
      lapply(seq_along(res$vip_scores), function(i) {
        output[[paste0("splsda_vip_plot_", i)]] <- renderPlot({
          print(res$vip_scores[[i]])
        })
      })

      output$splsda_vipLoadingsUI <- renderUI({
        req(res$vip_loadings)
        lapply(seq_along(res$vip_loadings), function(i) {
          plotOutput(paste0("splsda_vip_loading_plot_", i), height = "400px")
        })
      })
      lapply(seq_along(res$vip_loadings), function(i) {
        output[[paste0("splsda_vip_loading_plot_", i)]] <- renderPlot({
          replayPlot(res$vip_loadings[[i]])
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
          )]] <- renderPlot({
            replayPlot(sub_res$overall_indiv_plot)
          })
          output[[paste0("splsda_vipIndivPlot_", current_trt)]] <- renderPlot(
            {
              if (!is.null(sub_res$vip_indiv_plot)) {
                replayPlot(sub_res$vip_indiv_plot)
              }
            }
          )

          output[[paste0("splsda_loadingsUI_", current_trt)]] <- renderUI({
            req(sub_res$loadings)
            lapply(seq_along(sub_res$loadings), function(i) {
              plotOutput(
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
            )]] <- renderPlot({
              replayPlot(sub_res$loadings[[i]])
            })
          })

          output[[paste0("splsda_vipScoresUI_", current_trt)]] <- renderUI({
            req(sub_res$vip_scores)
            lapply(seq_along(sub_res$vip_scores), function(i) {
              plotOutput(
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
            )]] <- renderPlot({
              print(sub_res$vip_scores[[i]])
            })
          })

          output[[paste0("splsda_vipLoadingsUI_", current_trt)]] <- renderUI({
            req(sub_res$vip_loadings)
            lapply(seq_along(sub_res$vip_loadings), function(i) {
              plotOutput(
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
            )]] <- renderPlot({
              replayPlot(sub_res$vip_loadings[[i]])
            })
          })
          output[[paste0(
            "splsda_overall3DPlot_",
            current_trt
          )]] <- renderPlot({
            if (!is.null(sub_res$overall_3D)) {
              replayPlot(sub_res$overall_3D)
            }
          })
          output[[paste0("splsda_vip3DPlot_", current_trt)]] <- renderPlot({
            if (!is.null(sub_res$vip_3D)) {
              replayPlot(sub_res$vip_3D)
            }
          })
          # ----- Interactive OVERALL 3D per-trt: open modal -----
          observeEvent(
            input[[paste0("splsda_show3d_interactive_", current_trt)]],
            {
              showModal(modalDialog(
                plotlyOutput(
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
          )]] <- renderPlotly({
            req(sub_res$overall_3D_interactive)
            sub_res$overall_3D_interactive
          })

          # ----- Interactive VIP 3D per-trt: open modal -----
          observeEvent(
            input[[paste0("splsda_show3d_interactive_vip_", current_trt)]],
            {
              showModal(modalDialog(
                plotlyOutput(
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
          )]] <- renderPlotly({
            req(sub_res$vip_3D_interactive)
            sub_res$vip_3D_interactive
          })
          output[[paste0(
            "splsda_overallRocPlot_",
            current_trt
          )]] <- renderPlot({
            if (!is.null(sub_res$overall_ROC)) {
              replayPlot(sub_res$overall_ROC)
            }
          })
          output[[paste0(
            "splsda_overallCvPlot_",
            current_trt
          )]] <- renderPlot({
            if (!is.null(sub_res$overall_CV)) {
              print(sub_res$overall_CV)
            }
          })
          output[[paste0("splsda_confMatrix_", current_trt)]] <- renderPrint({
            if (!is.null(sub_res$conf_matrix)) {
              cat(paste(sub_res$conf_matrix, collapse = "\n"))
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
    req(res)
    is_nested <- is.list(res) &&
      !is.null(names(res)) &&
      is.list(res[[1]]) &&
      "global_indiv_plot" %in% names(res[[1]])

    if (!is_nested) {
      # --- Single Analysis Rendering ---
      output$mint_splsda_global_plot <- renderPlot({
        replayPlot(res$global_indiv_plot)
      })
      output$mint_splsda_partial_plot <- renderPlot({
        replayPlot(res$partial_indiv_plot)
      })
      if (!is.null(res$partial_loadings_plots)) {
        output$mint_splsda_loadings_ui <- renderUI({
          req(res$partial_loadings_plots)
          tagList(lapply(seq_along(res$partial_loadings_plots), function(i) {
            plotOutput(
              paste0("mint_splsda_loading_plot_", i),
              height = "400px"
            )
          }))
        })

        # Use a for loop to correctly scope the index 'i'
        for (i in seq_along(res$partial_loadings_plots)) {
          local({
            # Use local() to ensure 'i' is captured correctly
            local_i <- i
            output[[paste0(
              "mint_splsda_loading_plot_",
              local_i
            )]] <- renderPlot({
              replayPlot(res$partial_loadings_plots[[local_i]])
            })
          })
        }
      }
      output$mint_splsda_corr_circle_plot <- renderPlot({
        replayPlot(res$correlation_circle_plot)
      })
      output$mint_splsda_cim_plot <- renderPlot(
        {
          if (!is.null(res$cim_obj) && inherits(res$cim_obj, "recordedplot")) {
            replayPlot(res$cim_obj)
          }
        },
        height = 600
      )
      output$mint_splsda_roc_plot <- renderPlot({
        if (!is.null(res$roc_plot)) replayPlot(res$roc_plot)
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
            )]] <- renderPlot({
              replayPlot(sub_res$global_indiv_plot)
            })
            output[[paste0(
              "mint_splsda_partial_",
              current_trt
            )]] <- renderPlot({
              replayPlot(sub_res$partial_indiv_plot)
            })
            output[[paste0(
              "mint_splsda_loadings_",
              current_trt
            )]] <- renderUI({
              req(sub_res$partial_loadings_plots)
              lapply(seq_along(sub_res$partial_loadings_plots), function(i) {
                plotOutput(
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
              )]] <- renderPlot({
                replayPlot(sub_res$partial_loadings_plots[[i]])
              })
            })
            output[[paste0("mint_splsda_corr_", current_trt)]] <- renderPlot({
              replayPlot(sub_res$correlation_circle_plot)
            })
            output[[paste0("mint_splsda_cim_", current_trt)]] <- renderPlot(
              {
                if (
                  !is.null(sub_res$cim_obj) &&
                    inherits(sub_res$cim_obj, "recordedplot")
                ) {
                  replayPlot(sub_res$cim_obj)
                }
              },
              height = 600
            )
            output[[paste0("mint_splsda_roc_", current_trt)]] <- renderPlot({
              if (!is.null(sub_res$roc_plot)) replayPlot(sub_res$roc_plot)
            })
          }
        })
      }
    }
  }
  if (func_name == "Principal Component Analysis (PCA)" && is.list(res)) {
    if ("overall_indiv_plot" %in% names(res)) {
      output$pca_indivPlot <- renderPlot({
        replayPlot(res$overall_indiv_plot)
      })
      output$pca_3DPlot <- renderPlot({
        if (!is.null(res$overall_3D)) replayPlot(res$overall_3D)
      })
      output$pca_screePlot <- renderPlot({
        if (!is.null(res$overall_scree_plot)) {
          replayPlot(res$overall_scree_plot)
        }
      })
      output$pca_biplot <- renderPlot({
        if (!is.null(res$biplot)) replayPlot(res$biplot)
      })
      output$pca_corrCircle <- renderPlot({
        if (!is.null(res$correlation_circle)) {
          replayPlot(res$correlation_circle)
        }
      })

      output$pca_loadingsUI <- shiny::renderUI({
        if (!is.null(res$loadings)) {
          tagList(lapply(seq_along(res$loadings), function(i) {
            plotOutput(paste0("pca_loadings_", i), height = "300px")
          }))
        }
      })

      if (!is.null(res$loadings)) {
        for (i in seq_along(res$loadings)) {
          local({
            local_i <- i
            output[[paste0("pca_loadings_", local_i)]] <- renderPlot({
              replayPlot(res$loadings[[local_i]])
            })
          })
        }
      }
    } else {
      for (lvl in names(res)) {
        local({
          currentGroup <- lvl
          subres <- res[[currentGroup]]
          output[[paste0("pca_indivPlot_", currentGroup)]] <- renderPlot({
            if (!is.null(subres$overall_indiv_plot)) {
              replayPlot(subres$overall_indiv_plot)
            }
          })
          output[[paste0("pca_3DPlot_", currentGroup)]] <- renderPlot({
            if (!is.null(subres$overall_3D)) replayPlot(subres$overall_3D)
          })
          output[[paste0("pca_screePlot_", currentGroup)]] <- renderPlot({
            if (!is.null(subres$overall_scree_plot)) {
              replayPlot(subres$overall_scree_plot)
            }
          })
          output[[paste0("pca_biplot_", currentGroup)]] <- renderPlot({
            if (!is.null(subres$biplot)) replayPlot(subres$biplot)
          })
          output[[paste0("pca_corrCircle_", currentGroup)]] <- renderPlot({
            if (!is.null(subres$correlation_circle)) {
              replayPlot(subres$correlation_circle)
            }
          })

          output[[paste0(
            "pca_loadingsUI_",
            currentGroup
          )]] <- shiny::renderUI({
            if (!is.null(subres$loadings)) {
              tagList(lapply(seq_along(subres$loadings), function(i) {
                safeGroup <- gsub("[^A-Za-z0-9_]+", "_", currentGroup)
                plotOutput(
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
                )]] <- renderPlot({
                  replayPlot(subres$loadings[[local_i]])
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
    output$plsr_indivPlot <- renderPlot({
      req(res$scores_plot)
      replayPlot(res$scores_plot)
    })

    # Predicted vs Observed (recordedplot)
    output$plsr_predPlot <- renderPlot({
      req(res$pred_vs_obs)
      replayPlot(res$pred_vs_obs)
    })

    # Residuals vs Fitted (recordedplot)
    output$plsr_residPlot <- renderPlot({
      req(res$residuals_plot)
      replayPlot(res$residuals_plot)
    })

    # CV plot (recordedplot) + metrics text
    output$plsr_cvPlot <- renderPlot({
      req(res$cv_plot)
      replayPlot(res$cv_plot)
    })

    # Per-component loadings (each element is a recordedplot)
    output$plsr_loadingsUI <- renderUI({
      req(res$loadings)
      tagList(lapply(seq_along(res$loadings), function(i) {
        plotOutput(paste0("plsr_loading_plot_", i), height = "350px")
      }))
    })
    if (!is.null(res$loadings)) {
      for (i in seq_along(res$loadings)) {
        local({
          ii <- i
          output[[paste0("plsr_loading_plot_", ii)]] <- renderPlot({
            replayPlot(res$loadings[[ii]])
          })
        })
      }
    }

    # VIP barplots (ggplot objects — do NOT use replayPlot)
    output$plsr_vipUI <- renderUI({
      req(res$vip_scores)
      tagList(lapply(seq_along(res$vip_scores), function(i) {
        plotOutput(paste0("plsr_vip_plot_", i), height = "350px")
      }))
    })
    if (!is.null(res$vip_scores)) {
      for (i in seq_along(res$vip_scores)) {
        local({
          ii <- i
          output[[paste0("plsr_vip_plot_", ii)]] <- renderPlot({
            # ggplot object; just return it
            res$vip_scores[[ii]]
          })
        })
      }
    }

    # VIP>1 preview plots (recordedplot, conditional)
    output$plsr_vipIndivPlot <- renderPlot({
      req(res$vip_scores_indiv)
      replayPlot(res$vip_scores_indiv)
    })
    output$plsr_vipCVPlot <- renderPlot({
      req(res$vip_cv_plot)
      replayPlot(res$vip_cv_plot)
    })
  }

  # --- Correlation Rendering Logic ---
  # Tables
  output$corr_tbl_spearman <- DT::renderDataTable({
    req(res$spearman$table)
    DT::datatable(
      res$spearman$table,
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$corr_tbl_pearson <- DT::renderDataTable({
    req(res$pearson$table)
    DT::datatable(
      res$pearson$table,
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # Heatmaps (overall)
  output$corr_heatmap_spearman <- renderPlot({
    req(res$spearman$plot)
    print(res$spearman$plot)
  })

  output$corr_heatmap_pearson <- renderPlot({
    req(res$pearson$plot)
    print(res$pearson$plot)
  })

  # Per-group heatmaps (if any) — Spearman
  output$corr_group_heatmap_ui_spearman <- renderUI({
    req(res$spearman$group_plots)
    tabs <- lapply(names(res$spearman$group_plots), function(lv) {
      tabPanel(
        title = lv,
        plotOutput(
          paste0("corr_heatmap_grp_spear_", gsub("\\W+", "_", lv)),
          height = "600px"
        )
      )
    })
    do.call(tabsetPanel, c(list(type = "tabs"), tabs))
  })

  if (!is.null(res$spearman$group_plots)) {
    for (lv in names(res$spearman$group_plots)) {
      local({
        lvl <- lv
        output[[paste0(
          "corr_heatmap_grp_spear_",
          gsub("\\W+", "_", lvl)
        )]] <- renderPlot({
          print(res$spearman$group_plots[[lvl]])
        })
      })
    }
  }

  # Per-group heatmaps (if any) — Pearson
  output$corr_group_heatmap_ui_pearson <- renderUI({
    req(res$pearson$group_plots)
    tabs <- lapply(names(res$pearson$group_plots), function(lv) {
      tabPanel(
        title = lv,
        plotOutput(
          paste0("corr_heatmap_grp_pear_", gsub("\\W+", "_", lv)),
          height = "600px"
        )
      )
    })
    do.call(tabsetPanel, c(list(type = "tabs"), tabs))
  })

  if (!is.null(res$pearson$group_plots)) {
    for (lv in names(res$pearson$group_plots)) {
      local({
        lvl <- lv
        output[[paste0(
          "corr_heatmap_grp_pear_",
          gsub("\\W+", "_", lvl)
        )]] <- renderPlot({
          print(res$pearson$group_plots[[lvl]])
        })
      })
    }
  }

  # --- Random Forest Rendering Logic ---
  if (func_name == "Random Forest") {
    output$rf_summary <- renderPrint({
      cat(res$summary_text)
    })
    output$rf_vipPlot <- renderPlot({
      print(res$vip_plot)
    })
    output$rf_rocPlot <- renderPlot({
      if (!is.null(res$roc_plot)) print(res$roc_plot)
    })
    output$rf_rfcvPlot <- renderPlot({
      if (!is.null(res$rfcv_plot)) print(res$rfcv_plot)
    })
  }

  # --- XGBoost Rendering Logic ---
  if (func_name == "Extreme Gradient Boosting (XGBoost)") {
    output$xgb_summary <- renderPrint({
      cat(res$summary_text)
    })
    output$xgb_vipPlot <- renderPlot({
      print(res$plot)
    })
    output$xgb_rocPlot <- renderPlot({
      if (!is.null(res$roc_plot)) print(res$roc_plot)
    })
  }

  # --- Volcano Plot Rendering Logic ---
  if (func_name == "Volcano Plot") {
    output$volcPlotOutput <- renderPlot({
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
    output$heatmapImage <- renderImage(
      {
        req(res)

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
    output$dualflashPlotOutput <- renderPlot({
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
    output$skku_skewPlot <- renderPlot({
      print(res$p_skew)
    })
    output$skku_kurtPlot <- renderPlot({
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
        output[[paste0("dynamicPlot_", my_i)]] <- renderPlot({
          res[[my_i]]
        })
      })
    })
  }
})

output$textResults <- shiny::renderPrint({
  res <- analysisResult()
  print(res)
})
# --- ANOVA table ---
output$anovaResults <- DT::renderDataTable(
  {
    res <- analysisResult()
    # Ensure this renderer only runs when ANOVA was the selected function
    req(selected_function() == "ANOVA")
    res$out_df
  },
  options = list(pageLength = 10, scrollX = TRUE)
)

# --- Two-Sample T-Test table ---
output$ttestResults <- DT::renderDataTable(
  {
    res <- analysisResult()
    # Ensure this renderer only runs when T-Test was the selected function
    req(selected_function() == "Two-Sample T-Test")
    res$out_df
  },
  options = list(pageLength = 10, scrollX = TRUE)
)

output$errorBarPlotOutput <- shiny::renderPlot({
  req(analysisResult())
  res <- analysisResult()
  print(res)
})

# Render the download UI conditionally
output$download_ui <- shiny::renderUI({
  res <- analysisResult()
  # Show download button only if the result is of a type that generates a downloadable plot
  if (
    !is.null(res) &&
      !selected_function() %in% c("ANOVA", "Two-Sample T-Test") &&
      !inherits(res, "data.frame") &&
      (is.list(res) || is.character(res))
  ) {
    tagList(
      textInput(
        "download_filename",
        "Enter filename for download:",
        value = "Cytokine_Analysis_Results"
      ),
      downloadButton("download_output", "Download Results as PDF")
    )
  }
})

# Download handler
output$download_output <- shiny::downloadHandler(
  filename = function() {
    paste0(input$download_filename, ".pdf")
  },
  content = function(file) {
    pdf(file, width = 10, height = 8)
    plots <- reactivePlots()
    # print each ggplot in order
    for (p in plots) {
      if (inherits(p, "ggplot")) {
        print(p)
      } else if (inherits(p, "pheatmap")) {
        # <— add
        grid::grid.newpage()
        grid::grid.draw(p$gtable)
      } else if (inherits(p, "grob") || inherits(p, "gtable")) {
        # <— add (fallback)
        grid::grid.newpage()
        grid::grid.draw(p)
      } else {
        print(p)
      }
    }
    dev.off()
  }
)
