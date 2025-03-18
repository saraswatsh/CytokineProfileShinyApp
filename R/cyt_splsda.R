#' Analyze data with Sparse Partial Least Squares Discriminant Analysis (sPLS-DA).
#'
#' @param data A matrix or data frame containing the variables. Columns not
#'   specified by \code{group_col} or \code{trt_col} are assumed to be continuous
#'   variables for analysis.
#' @param group_col A string specifying the column name that contains group
#'   information. If \code{trt_col} is not provided, it will be used for both
#'   grouping and treatment.
#' @param trt_col A string specifying the column name for treatments. Default is
#'   \code{NULL}.
#' @param colors A vector of colors for the groups or treatments. If
#'   \code{NULL}, a random palette (using \code{rainbow}) is generated based on
#'   the number of groups.
#' @param pdf_title A string specifying the file name for saving the PDF output.
#'                If set to NULL, the function runs in interactive mode.
#' @param ellipse Logical. Whether to draw a 95\% confidence ellipse on the figures.
#'   Default is \code{FALSE}.
#' @param bg Logical. Whether to draw the prediction background in the figures.
#'   Default is \code{FALSE}.
#' @param conf_mat Logical. Whether to print the confusion matrix for the classifications.
#'   Default is \code{FALSE}.
#' @param var_num Numeric. The number of variables to be used in the PLS-DA model.
#' @param cv_opt Character. Option for cross-validation method: either "loocv" or "Mfold".
#'   Default is \code{NULL}.
#' @param fold_num Numeric. The number of folds to use if \code{cv_opt} is "Mfold". Default is 5.
#' @param scale Character. Option for data transformation; if set to \code{"log2"}, a log2
#'   transformation is applied to the continuous variables. Default is \code{NULL}.
#' @param comp_num Numeric. The number of components to calculate in the sPLS-DA model.
#'   Default is 2.
#' @param pch_values A vector of integers specifying the plotting characters (pch values)
#'   to be used in the plots.
#' @param style Character. If set to \code{"3D"} or \code{"3d"} and \code{comp_num} equals 3,
#'   a 3D plot is generated using the \code{plot3D} package. Default is \code{NULL}.
#' @param roc Logical. Whether to compute and plot the ROC curve for the model.
#'   Default is \code{FALSE}.
#'
#' @return In Download mode (pdf_title not NULL), a PDF file is written and the function
#'         returns NULL invisibly. In Interactive mode (pdf_title = NULL), a named list is
#'         returned with the following elements (in this order):
#'         1. overall_indiv_plot: Main individual classification plot
#'         2. overall_3D: Main 3D plot (if generated)
#'         3. overall_ROC: ROC curve plot for the overall model
#'         4. overall_CV: Cross-validation error plot for the overall model
#'         5. loadings: A list of loadings plots (one per component)
#'         6. vip_scores: A list of VIP score bar plots for each component
#'         7. vip_indiv_plot: Main individual plot for the VIP>1 model
#'         8. vip_3D: 3D plot for the VIP>1 model (if generated)
#'         9. vip_ROC: ROC curve plot for the VIP>1 model
#'         10. vip_CV: Cross-validation error plot for the VIP>1 model
#'         11. conf_matrix: Confusion matrix text output
#'
#' @examples
#' data.df <- cytodata
#' cyt_splsda(data.df[,-c(1,4)], pdf_title = "Example sPLS-DA Analysis.pdf",
#'            colors = c("black", "purple", "red2"), bg = TRUE, scale = "log2",
#'            conf_mat = TRUE, var_num = 25, cv_opt = "loocv", comp_num = 3,
#'            pch_values = c(16, 4, 3), style = "3d",
#'            group_col = "Group", trt_col = "Treatment", roc = TRUE)
#'
#' @export
#' @import mixOmics
#' @import ggplot2
#' @import plot3D
#' @import reshape2
#' @import caret
cyt_splsda <- function(data, group_col = NULL, trt_col = NULL, colors = NULL,
                       pdf_title, ellipse = FALSE, bg = FALSE, conf_mat = FALSE,
                       var_num, cv_opt = NULL, fold_num = 5, scale = NULL,
                       comp_num = 2, pch_values, style = NULL, roc = FALSE,
                       progress = NULL) {
  # ---------------------------
  # Preliminary Processing
  # ---------------------------
  withProgress(message = paste("Starting Preliminary Processing"), value = 0, {
  if (is.null(group_col) && !is.null(trt_col)) {
    incProgress(0, detail = "No group column provided; using treatment column as grouping variable.")
    group_col <- trt_col
  }
  if (is.null(trt_col) && !is.null(group_col)) {
    incProgress(0, detail = "No treatment column provided; using group column as treatment variable.")
    trt_col <- group_col
  }
  if (is.null(group_col) && is.null(trt_col)) {
    stop("At least one factor column must be provided.")
  }
  
  if (!is.null(scale) && scale == "log2") {
    data <- data.frame(
      data[, c(group_col, trt_col), drop = FALSE],
      log2(data[, !(names(data) %in% c(group_col, trt_col)), drop = FALSE])
    )
    incProgress(0, detail = "Results based on log2 transformation:")
  } else {
    incProgress(0, detail ="Results based on no transformation:")
  }
  })
  names(data)[names(data) %in% c(group_col, trt_col)] <-
    tolower(names(data)[names(data) %in% c(group_col, trt_col)])
  group_col <- tolower(group_col)
  trt_col <- tolower(trt_col)
  
  if (is.null(colors)) {
    num_groups <- length(unique(data[[group_col]]))
    colors <- rainbow(num_groups)
  }
  
  ## ---------------------------
  ## PDF Mode Branch
  ## ---------------------------
  if (!is.null(pdf_title)) {
    if (!is.null(progress)) progress$inc(0.05, detail = "Starting PDF mode analysis")
    pdf(file = pdf_title, width = 8.5, height = 8)
    
    # If single-level analysis (group and treatment the same)
    if (group_col == trt_col) {
      overall_analysis <- "Overall Analysis"
      the_data_df <- data[, !(names(data) %in% c(group_col))]
      the_data_df <- the_data_df[, sapply(the_data_df, is.numeric)]
      the_groups <- as.vector(data[[group_col]])
      if (length(unique(the_groups)) < 2) {
        stop("The grouping variable must have at least two levels for sPLS-DA.")
      }
      
      if (!is.null(progress)) progress$inc(0.05, detail = "Fitting sPLS-DA model (overall)")
      model <- mixOmics::splsda(the_data_df, the_groups,
                                scale = TRUE, ncomp = comp_num,
                                keepX = rep(var_num, comp_num))
      if (!is.null(progress)) progress$inc(0.05, detail = "Calculating predictions (overall)")
      splsda_predict <- predict(model, the_data_df, dist = "max.dist")
      prediction1 <- cbind(original = the_groups, splsda_predict$class$max.dist)
      acc1 <- 100 * signif(sum(prediction1[,1] == prediction1[,2]) / length(prediction1[,1]), digits = 2)
      
      if (!is.null(progress)) progress$inc(0.05, detail = "Generating classification plot (overall)")
      bg_obj <- mixOmics::background.predict(model, comp.predicted = 2, dist = "max.dist")
      group_factors <- sort(unique(the_groups))
      plot_args <- list(model,
                        ind.names = NA, legend = TRUE, col = colors,
                        pch = pch_values, pch.levels = group_factors,
                        title = paste(overall_analysis, "With Accuracy:", acc1, "%"),
                        legend.title = group_col)
      if (ellipse) plot_args$ellipse <- TRUE
      if (bg) plot_args$background <- bg_obj
      do.call(mixOmics::plotIndiv, plot_args)
      
      if (!is.null(style) && comp_num == 3 && tolower(style) == "3d") {
        scores <- model$variates$X
        plot3D::scatter3D(scores[,1], scores[,2], scores[,3],
                          pch = pch_values, col = colors,
                          xlab = "Component 1", ylab = "Component 2", zlab = "Component 3",
                          main = paste("3D Plot:", overall_analysis),
                          theta = 20, phi = 30, bty = "g", colkey = FALSE)
      }
      
      if (roc) {
        mixOmics::auroc(model,
                        newdata = the_data_df,
                        outcome.test = the_groups,
                        plot = TRUE, roc.comp = comp_num,
                        title = paste0("ROC Curve:", overall_analysis), print = FALSE)
      }
      
      if (!is.null(cv_opt)) {
        if (cv_opt == "loocv") {
          set.seed(123)
          cv_res <- mixOmics::perf(model, validation = "loo")
          err_rates <- cv_res$error.rate$overall[,"max.dist"]
          error_df <- data.frame(Component = seq_len(nrow(cv_res$error.rate$overall)),
                                 ErrorRate = err_rates)
          cv_plot <- ggplot2::ggplot(error_df, ggplot2::aes(x = Component, y = ErrorRate)) +
            ggplot2::geom_line(color = "blue") +
            ggplot2::geom_point(color = "blue", size = 3) +
            ggplot2::labs(title = paste("LOOCV Error Rate:", overall_analysis),
                          x = "Number of Components", y = "Error Rate") +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          print(cv_plot)
        } else if (cv_opt == "Mfold") {
          set.seed(123)
          cv_res <- mixOmics::perf(model, validation = "Mfold", folds = fold_num, nrepeat = 1000)
          err_rates <- cv_res$error.rate$overall[,"max.dist"]
          error_df <- data.frame(Component = seq_len(nrow(cv_res$error.rate$overall)),
                                 ErrorRate = err_rates)
          cv_plot <- ggplot2::ggplot(error_df, ggplot2::aes(x = Component, y = ErrorRate)) +
            ggplot2::geom_line(color = "blue") +
            ggplot2::geom_point(color = "blue", size = 3) +
            ggplot2::labs(title = paste("Mfold Error Rate:", overall_analysis),
                          x = "Number of Components", y = "Error Rate") +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          print(cv_plot)
        }
      }
      
      for (comp in 1:comp_num) {
        mixOmics::plotLoadings(model,
                               comp = comp, contrib = "max", method = "mean",
                               size.names = 1, size.legend = 1, size.title = 1,
                               legend.color = colors,
                               title = paste("Loadings for Component", comp, ":", overall_analysis),
                               legend = TRUE)
      }
      
      all_vip <- mixOmics::vip(model)
      for (comp in 1:comp_num) {
        vscore <- as.data.frame(all_vip[, comp, drop = FALSE])
        vscore$metabo <- rownames(vscore)
        vscore$score <- vscore[,1]
        bar <- vscore[, c("metabo", "score")]
        bar <- bar[order(bar$score, decreasing = TRUE), ]
        vip_plot <- ggplot2::ggplot(bar, ggplot2::aes(x = reorder(metabo, score), y = score)) +
          ggplot2::geom_bar(stat = "identity", fill = "red2") +
          ggplot2::coord_flip() +
          ggplot2::labs(title = paste("VIP Scores for Component", comp),
                        x = "Variable", y = "VIP Score") +
          ggplot2::theme_minimal()
        print(vip_plot)
      }
      
      if (sum(all_vip[,1] > 1) > 0) {
        vip_filter <- all_vip[,1] > 1
        predictors_vip <- the_data_df[, vip_filter, drop = FALSE]
        vip_model <- mixOmics::splsda(predictors_vip, the_groups,
                                      scale = TRUE, ncomp = comp_num,
                                      keepX = rep(sum(vip_filter), comp_num))
        vip_pred <- predict(vip_model, predictors_vip, dist = "max.dist")
        prediction2 <- cbind(original = the_groups, vip_pred$class$max.dist)
        acc2 <- 100 * signif(sum(prediction2[,1] == prediction2[,2]) / length(prediction2[,1]), digits = 2)
        
        bg2 <- mixOmics::background.predict(vip_model, comp.predicted = 2, dist = "max.dist")
        plot_args2 <- list(vip_model,
                           ind.names = NA, legend = TRUE, col = colors,
                           pch = pch_values, pch.levels = group_factors,
                           title = paste(overall_analysis, "(VIP>1) With Accuracy:", acc2, "%"),
                           legend.title = group_col)
        if (ellipse) plot_args2$ellipse <- TRUE
        if (bg) plot_args2$background <- bg2
        do.call(mixOmics::plotIndiv, plot_args2)
        
        if (!is.null(style) && comp_num == 3 && tolower(style) == "3d") {
          scores2 <- vip_model$variates$X
          plot3D::scatter3D(scores2[,1], scores2[,2], scores2[,3],
                            pch = pch_values, col = colors,
                            xlab = "Component 1", ylab = "Component 2", zlab = "Component 3",
                            main = paste("3D Plot (VIP>1):", overall_analysis),
                            theta = 20, phi = 30, bty = "g", colkey = FALSE)
        }
        
        if (roc) {
          mixOmics::auroc(vip_model,
                          newdata = predictors_vip,
                          outcome.test = the_groups,
                          plot = TRUE, roc.comp = comp_num,
                          title = paste("ROC Curve (VIP>1):", overall_analysis),
                          print = FALSE)
        }
        
        if (!is.null(cv_opt)) {
          if (cv_opt == "loocv") {
            set.seed(123)
            vip_cv_res <- mixOmics::perf(vip_model, validation = "loo")
            vip_err_rates <- vip_cv_res$error.rate$overall[,"max.dist"]
            vip_err_df <- data.frame(Component = seq_len(nrow(vip_cv_res$error.rate$overall)),
                                     ErrorRate = vip_err_rates)
            vip_cv_plot <- ggplot2::ggplot(vip_err_df, ggplot2::aes(x = Component, y = ErrorRate)) +
              ggplot2::geom_line(color = "red") +
              ggplot2::geom_point(color = "red", size = 3) +
              ggplot2::labs(title = paste("LOOCV Error Rate (VIP>1):", overall_analysis),
                            x = "Component", y = "Error Rate") +
              ggplot2::theme_minimal()
            print(vip_cv_plot)
          } else if (cv_opt == "Mfold") {
            set.seed(123)
            vip_cv_res <- mixOmics::perf(vip_model, validation = "Mfold", folds = fold_num, nrepeat = 1000)
            vip_err_rates <- vip_cv_res$error.rate$overall[,"max.dist"]
            vip_err_df <- data.frame(Component = seq_len(nrow(vip_cv_res$error.rate$overall)),
                                     ErrorRate = vip_err_rates)
            vip_cv_plot <- ggplot2::ggplot(vip_err_df, ggplot2::aes(x = Component, y = ErrorRate)) +
              ggplot2::geom_line(color = "red") +
              ggplot2::geom_point(color = "red", size = 3) +
              ggplot2::labs(title = paste("Mfold Error Rate (VIP>1):", overall_analysis),
                            x = "Component", y = "Error Rate") +
              ggplot2::theme_minimal()
            print(vip_cv_plot)
          }
        }
      }
      
      if (conf_mat == TRUE) {
        conf_text_overall <- capture.output({
          cat("Overall Confusion Matrix for PLS-DA Comparison\n")
          prediction1 <- cbind(original = the_groups, model$predict$class$max.dist)
          cm <- caret::confusionMatrix(data = as.factor(prediction1[,2]),
                                       reference = as.factor(prediction1[,1]))
          print(cm$table)
          cat("Accuracy:", signif(cm$overall["Accuracy"], 2), "\n")
          if (nlevels(as.factor(prediction1[,1])) == 2) {
            cat("Sensitivity:", signif(cm$byClass["Sensitivity"], 2), "\n")
            cat("Specificity:", signif(cm$byClass["Specificity"], 2), "\n")
          } else {
            cat("\nPer-Class Sensitivity:\n")
            print(signif(cm$byClass[, "Sensitivity"], 2))
            cat("\nPer-Class Specificity:\n")
            print(signif(cm$byClass[, "Specificity"], 2))
          }
        })
        conf_text_vip <- capture.output({
          cat("Overall Confusion Matrix for PLS-DA Comparison with VIP Score > 1\n")
          prediction2 <- cbind(original = the_groups, vip_pred$class$max.dist)
          cm_vip <- caret::confusionMatrix(data = as.factor(prediction2[,2]),
                                           reference = as.factor(prediction2[,1]))
          print(cm_vip$table)
          cat("Accuracy:", signif(cm_vip$overall["Accuracy"], 2), "\n")
          if (nlevels(as.factor(prediction2[,1])) == 2) {
            cat("Sensitivity:", signif(cm_vip$byClass["Sensitivity"], 2), "\n")
            cat("Specificity:", signif(cm_vip$byClass["Specificity"], 2), "\n")
          } else {
            cat("\nPer-Class Sensitivity:\n")
            print(signif(cm_vip$byClass[, "Sensitivity"], 2))
            cat("\nPer-Class Specificity:\n")
            print(signif(cm_vip$byClass[, "Specificity"], 2))
          }
        })
        conf_text <- c(conf_text_overall, conf_text_vip)
      } else {
        conf_text <- NULL
      }
      
      dev.off()
      return(paste("Output file generated:", normalizePath(pdf_title)))
      
    } else {
      # ---------------------------
      # PDF Multi-Level Branch: when group_col != trt_col
      # ---------------------------
      # Open a PDF device; each treatment will be a separate page.
      if (!is.null(progress)) progress$inc(0.05, detail = "Starting multi-level PDF analysis")
      
      pdf(file = pdf_title, width = 8.5, height = 8)
      treatments <- unique(data[[trt_col]])
      
      for (trt in treatments) {
        current_level <- trt
        overall_analysis <- current_level
        condt <- data[[trt_col]] == current_level
        the_data_df <- data[condt, -which(names(data) %in% c(group_col, trt_col))]
        the_data_df <- the_data_df[, sapply(the_data_df, is.numeric), drop = FALSE]
        the_groups <- as.vector(data[condt, group_col])
        if (length(unique(the_groups)) < 2) {
          stop("The grouping variable must have at least two levels for sPLS-DA.")
        }
        if (!is.null(progress)) progress$inc(0.05, detail = paste("Fitting sPLS-DA model for treatment", current_level))
        model <- mixOmics::splsda(the_data_df, the_groups,
                                  scale = TRUE, ncomp = comp_num,
                                  keepX = rep(var_num, comp_num))
        if (!is.null(progress)) progress$inc(0.05, detail = paste("Calculating predictions for treatment", current_level))
        splsda_predict <- predict(model, the_data_df, dist = "max.dist")
        prediction1 <- cbind(original = the_groups, splsda_predict$class$max.dist)
        acc1 <- 100 * signif(sum(prediction1[,1] == prediction1[,2]) / length(prediction1[,1]), digits = 2)
        
        if (!is.null(progress)) progress$inc(0.05, detail = paste("Generating classification plot for treatment", current_level))
        bg_obj <- mixOmics::background.predict(model, comp.predicted = 2, dist = "max.dist")
        group_factors <- sort(unique(the_groups))
        plot_args <- list(model,
                          ind.names = NA, legend = TRUE, col = colors,
                          pch = pch_values, pch.levels = group_factors,
                          title = paste("Treatment =", overall_analysis, "With Accuracy:", acc1, "%"),
                          legend.title = group_col)
        if (ellipse) plot_args$ellipse <- TRUE
        if (bg) plot_args$background <- bg_obj
        do.call(mixOmics::plotIndiv, plot_args)
        
        if (!is.null(style) && comp_num == 3 && tolower(style) == "3d") {
          scores <- model$variates$X
          plot3D::scatter3D(scores[,1], scores[,2], scores[,3],
                            pch = pch_values, col = colors,
                            xlab = "Component 1", ylab = "Component 2", zlab = "Component 3",
                            main = paste("3D Plot: Treatment =", overall_analysis),
                            theta = 20, phi = 30, bty = "g", colkey = FALSE)
        }
        
        if (roc) {
          mixOmics::auroc(model,
                          newdata = the_data_df,
                          outcome.test = the_groups,
                          plot = TRUE, roc.comp = comp_num,
                          title = paste("ROC Curve: Treatment =", overall_analysis),
                          print = FALSE)
        }
        
        if (!is.null(cv_opt)) {
          if (cv_opt == "loocv") {
            set.seed(123)
            cv_res <- mixOmics::perf(model, validation = "loo")
            err_rates <- cv_res$error.rate$overall[,"max.dist"]
            error_df <- data.frame(Component = seq_len(nrow(cv_res$error.rate$overall)),
                                   ErrorRate = err_rates)
            cv_plot <- ggplot2::ggplot(error_df, ggplot2::aes(x = Component, y = ErrorRate)) +
              ggplot2::geom_line(color = "blue") +
              ggplot2::geom_point(color = "blue", size = 3) +
              ggplot2::labs(title = paste("LOOCV Error Rate: Treatment =", overall_analysis),
                            x = "Number of Components", y = "Error Rate") +
              ggplot2::theme_minimal() +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
            print(cv_plot)
          } else if (cv_opt == "Mfold") {
            set.seed(123)
            cv_res <- mixOmics::perf(model, validation = "Mfold", folds = fold_num, nrepeat = 1000)
            err_rates <- cv_res$error.rate$overall[,"max.dist"]
            error_df <- data.frame(Component = seq_len(nrow(cv_res$error.rate$overall)),
                                   ErrorRate = err_rates)
            cv_plot <- ggplot2::ggplot(error_df, ggplot2::aes(x = Component, y = ErrorRate)) +
              ggplot2::geom_line(color = "blue") +
              ggplot2::geom_point(color = "blue", size = 3) +
              ggplot2::labs(title = paste("Mfold Error Rate: Treatment =", overall_analysis),
                            x = "Number of Components", y = "Error Rate") +
              ggplot2::theme_minimal() +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
            print(cv_plot)
          }
        }
        
        for (comp in 1:comp_num) {
          mixOmics::plotLoadings(model,
                                 comp = comp, contrib = "max", method = "mean",
                                 size.names = 1, size.legend = 1, size.title = 1,
                                 legend.color = colors,
                                 title = paste("Loadings for Component", comp, ":", "Treatment =", overall_analysis),
                                 legend = TRUE)
        }
        
        all_vip <- mixOmics::vip(model)
        for (comp in 1:comp_num) {
          vscore <- as.data.frame(all_vip[, comp, drop = FALSE])
          vscore$metabo <- rownames(vscore)
          vscore$score <- vscore[,1]
          bar <- vscore[, c("metabo", "score")]
          bar <- bar[order(bar$score, decreasing = TRUE), ]
          vip_plot <- ggplot2::ggplot(bar, ggplot2::aes(x = reorder(metabo, score), y = score)) +
            ggplot2::geom_bar(stat = "identity", fill = "red2") +
            ggplot2::coord_flip() +
            ggplot2::labs(title = paste("VIP Scores for Component", comp, ":", "Treatment =", overall_analysis),
                          x = "Variable", y = "VIP Score") +
            ggplot2::theme_minimal()
          print(vip_plot)
        }
        
        if (sum(all_vip[,1] > 1) > 0) {
          vip_filter <- all_vip[,1] > 1
          predictors_vip <- the_data_df[, vip_filter, drop = FALSE]
          vip_model <- mixOmics::splsda(predictors_vip, the_groups,
                                        scale = TRUE, ncomp = comp_num,
                                        keepX = rep(sum(vip_filter), comp_num))
          vip_pred <- predict(vip_model, predictors_vip, dist = "max.dist")
          prediction2 <- cbind(original = the_groups, vip_pred$class$max.dist)
          acc2 <- 100 * signif(sum(prediction2[,1] == prediction2[,2]) / length(prediction2[,1]), digits = 2)
          
          bg2 <- mixOmics::background.predict(vip_model, comp.predicted = 2, dist = "max.dist")
          plot_args2 <- list(vip_model,
                             ind.names = NA, legend = TRUE, col = colors,
                             pch = pch_values, pch.levels = group_factors,
                             title = paste("Treatment =", overall_analysis, "(VIP>1) With Accuracy:", acc2, "%"),
                             legend.title = group_col)
          if (ellipse) plot_args2$ellipse <- TRUE
          if (bg) plot_args2$background <- bg2
          do.call(mixOmics::plotIndiv, plot_args2)
          
          if (!is.null(style) && comp_num == 3 && tolower(style) == "3d") {
            scores2 <- vip_model$variates$X
            plot3D::scatter3D(scores2[,1], scores2[,2], scores2[,3],
                              pch = pch_values, col = colors,
                              xlab = "Component 1", ylab = "Component 2", zlab = "Component 3",
                              main = paste("3D Plot (VIP>1): Treatment =", overall_analysis),
                              theta = 20, phi = 30, bty = "g", colkey = FALSE)
          }
          
          if (roc) {
            mixOmics::auroc(vip_model,
                            newdata = predictors_vip,
                            outcome.test = the_groups,
                            plot = TRUE, roc.comp = comp_num,
                            title = paste("ROC Curve (VIP>1): Treatment =", overall_analysis),
                            print = FALSE)
          }
          
          if (!is.null(cv_opt)) {
            if (cv_opt == "loocv") {
              set.seed(123)
              vip_cv_res <- mixOmics::perf(vip_model, validation = "loo")
              vip_err_rates <- vip_cv_res$error.rate$overall[,"max.dist"]
              vip_err_df <- data.frame(Component = seq_len(nrow(vip_cv_res$error.rate$overall)),
                                       ErrorRate = vip_err_rates)
              vip_cv_plot <- ggplot2::ggplot(vip_err_df, ggplot2::aes(x = Component, y = ErrorRate)) +
                ggplot2::geom_line(color = "red") +
                ggplot2::geom_point(color = "red", size = 3) +
                ggplot2::labs(title = paste("LOOCV Error Rate (VIP>1): Treatment =", overall_analysis),
                              x = "Component", y = "Error Rate") +
                ggplot2::theme_minimal()
              print(vip_cv_plot)
            } else if (cv_opt == "Mfold") {
              set.seed(123)
              vip_cv_res <- mixOmics::perf(vip_model, validation = "Mfold", folds = fold_num, nrepeat = 1000)
              vip_err_rates <- vip_cv_res$error.rate$overall[,"max.dist"]
              vip_err_df <- data.frame(Component = seq_len(nrow(vip_cv_res$error.rate$overall)),
                                       ErrorRate = vip_err_rates)
              vip_cv_plot <- ggplot2::ggplot(vip_err_df, ggplot2::aes(x = Component, y = ErrorRate)) +
                ggplot2::geom_line(color = "red") +
                ggplot2::geom_point(color = "red", size = 3) +
                ggplot2::labs(title = paste("Mfold Error Rate (VIP>1): Treatment =", overall_analysis),
                              x = "Component", y = "Error Rate") +
                ggplot2::theme_minimal()
              print(vip_cv_plot)
            }
          }
        }
        
        if (conf_mat == TRUE) {
          conf_text_overall <- capture.output({
            cat("Overall Confusion Matrix for PLS-DA Comparison\n")
            prediction1 <- cbind(original = the_groups, model$predict$class$max.dist)
            cm <- caret::confusionMatrix(data = as.factor(prediction1[,2]),
                                         reference = as.factor(prediction1[,1]))
            print(cm$table)
            cat("Accuracy:", signif(cm$overall["Accuracy"], 2), "\n")
            if (nlevels(as.factor(prediction1[,1])) == 2) {
              cat("Sensitivity:", signif(cm$byClass["Sensitivity"], 2), "\n")
              cat("Specificity:", signif(cm$byClass["Specificity"], 2), "\n")
            } else {
              cat("\nPer-Class Sensitivity:\n")
              print(signif(cm$byClass[, "Sensitivity"], 2))
              cat("\nPer-Class Specificity:\n")
              print(signif(cm$byClass[, "Specificity"], 2))
            }
          })
          conf_text_vip <- capture.output({
            cat("Overall Confusion Matrix for PLS-DA Comparison with VIP Score > 1\n")
            prediction2 <- cbind(original = the_groups, vip_pred$class$max.dist)
            cm_vip <- caret::confusionMatrix(data = as.factor(prediction2[,2]),
                                             reference = as.factor(prediction2[,1]))
            print(cm_vip$table)
            cat("Accuracy:", signif(cm_vip$overall["Accuracy"], 2), "\n")
            if (nlevels(as.factor(prediction2[,1])) == 2) {
              cat("Sensitivity:", signif(cm_vip$byClass["Sensitivity"], 2), "\n")
              cat("Specificity:", signif(cm_vip$byClass["Specificity"], 2), "\n")
            } else {
              cat("\nPer-Class Sensitivity:\n")
              print(signif(cm_vip$byClass[, "Sensitivity"], 2))
              cat("\nPer-Class Specificity:\n")
              print(signif(cm_vip$byClass[, "Specificity"], 2))
            }
          })
          conf_text <- c(conf_text_overall, conf_text_vip)
        } else {
          conf_text <- NULL
        }
      }
      dev.off()
      return(paste("Output file generated:", normalizePath(pdf_title)))
    }
  }
  # ---------------------------
  # INTERACTIVE MODE
  # ---------------------------
  run_overall_interactive <- function(df_subset, analysis_label) {
    withProgress(message = paste("Running sPLS-DA for", analysis_label), value = 0, {
      incProgress(0.1, detail = "Building overall model...")
      conf_text <- NULL
      predictors <- df_subset[, setdiff(names(df_subset), unique(c(group_col, trt_col))), drop = FALSE]
      predictors <- predictors[, sapply(predictors, is.numeric), drop = FALSE]
      groups <- as.vector(df_subset[[group_col]])
      if (length(unique(groups)) < 2) {
        stop("The grouping variable must have at least two levels for sPLS-DA.")
      }
      
      overall_model <- mixOmics::splsda(predictors, groups,
                                        scale = TRUE, ncomp = comp_num,
                                        keepX = rep(var_num, comp_num))
      incProgress(0.1, detail = "Calculating overall accuracy...")
      overall_pred <- predict(overall_model, predictors, dist = "max.dist")
      prediction1 <- cbind(original = groups, overall_pred$class$max.dist)
      acc1 <- 100 * signif(sum(prediction1[,1] == prediction1[,2]) / length(prediction1[,1]), digits = 2)
      
      incProgress(0.2, detail = "Generating main classification plot...")
      dev.control(displaylist = "enable")
      mixOmics::plotIndiv(overall_model,
                          ind.names = NA, legend = TRUE,
                          col = colors, pch = pch_values,
                          pch.levels = sort(unique(groups)),
                          title = paste("sPLS-DA:", analysis_label, "With Accuracy:", acc1, "%"),
                          ellipse = ellipse,
                          background = if(bg) mixOmics::background.predict(overall_model, comp.predicted = 2, dist = "max.dist") else NULL)
      overall_indiv_plot <- recordPlot()
      
      incProgress(0.05, detail = "Generating 3D plot...")
      overall_3D <- NULL
      if (!is.null(style) && tolower(style) == "3d" && comp_num == 3) {
        dev.control(displaylist = "enable")
        plot3D::scatter3D(overall_model$variates$X[,1], overall_model$variates$X[,2], overall_model$variates$X[,3],
                          pch = pch_values, col = colors,
                          xlab = "Component 1", ylab = "Component 2", zlab = "Component 3",
                          main = paste("3D Plot:", analysis_label),
                          theta = 20, phi = 30, bty = "g", colkey = FALSE)
        overall_3D <- recordPlot()
      }
      
      incProgress(0.05, detail = "Generating ROC curve...")
      overall_ROC <- NULL
      if (roc) {
        dev.control(displaylist = "enable")
        mixOmics::auroc(overall_model,
                        newdata = predictors,
                        outcome.test = groups,
                        plot = TRUE, roc.comp = comp_num,
                        title = paste("ROC Curve:", analysis_label),
                        print = FALSE)
        overall_ROC <- recordPlot()
      }
      
      incProgress(0.05, detail = "Generating CV error plot...")
      overall_CV <- NULL
      if (!is.null(cv_opt)) {
        if (cv_opt == "loocv") {
          set.seed(123)
          cv_res <- mixOmics::perf(overall_model, validation = "loo")
          err_rates <- cv_res$error.rate$overall[,"max.dist"]
          overall_err_df <- data.frame(Component = seq_len(nrow(cv_res$error.rate$overall)),
                                       ErrorRate = err_rates)
          overall_CV <- ggplot2::ggplot(overall_err_df, ggplot2::aes(x = Component, y = ErrorRate)) +
            ggplot2::geom_line(color = "blue") +
            ggplot2::geom_point(color = "blue", size = 3) +
            ggplot2::labs(title = paste("LOOCV Error Rate:", analysis_label),
                          x = "Number of Components", y = "Error Rate") +
            ggplot2::theme_minimal()
        } else if (cv_opt == "Mfold") {
          set.seed(123)
          cv_res <- mixOmics::perf(overall_model, validation = "Mfold", folds = fold_num, nrepeat = 1000)
          err_rates <- cv_res$error.rate$overall[,"max.dist"]
          overall_err_df <- data.frame(Component = seq_len(nrow(cv_res$error.rate$overall)),
                                       ErrorRate = err_rates)
          overall_CV <- ggplot2::ggplot(overall_err_df, ggplot2::aes(x = Component, y = ErrorRate)) +
            ggplot2::geom_line(color = "blue") +
            ggplot2::geom_point(color = "blue", size = 3) +
            ggplot2::labs(title = paste("Mfold Error Rate:", analysis_label),
                          x = "Number of Components", y = "Error Rate") +
            ggplot2::theme_minimal()
        }
      }
      
      incProgress(0.1, detail = "Generating loadings plots...")
      loadings_list <- lapply(1:comp_num, function(comp) {
        dev.control(displaylist = "enable")
        mixOmics::plotLoadings(overall_model,
                               comp = comp, contrib = "max", method = "mean",
                               size.names = 1, size.legend = 1,
                               legend.color = colors,
                               title = paste("Loadings for Component", comp, ":", analysis_label),
                               legend = TRUE)
        recordPlot()
      })
      
      incProgress(0.05, detail = "Generating VIP score plots...")
      all_vip <- mixOmics::vip(overall_model)
      vip_scores <- lapply(1:comp_num, function(comp) {
        vscore <- as.data.frame(all_vip[, comp, drop = FALSE])
        vscore$variable <- rownames(vscore)
        vscore$score <- vscore[,1]
        bar <- vscore[, c("variable", "score")]
        bar <- bar[order(bar$score, decreasing = TRUE), ]
        ggplot2::ggplot(bar, ggplot2::aes(x = reorder(variable, score), y = score)) +
          ggplot2::geom_bar(stat = "identity", fill = "red2") +
          ggplot2::coord_flip() +
          ggplot2::labs(title = paste("VIP Scores for Component", comp),
                        x = "Variable", y = "VIP Score") +
          ggplot2::theme_minimal()
      })
      
      incProgress(0.1, detail = "Generating VIP model...")
      vip_indiv_plot <- NULL
      vip_3D <- NULL
      vip_ROC <- NULL
      vip_CV <- NULL
      if (sum(all_vip[,1] > 1) > 0) {
        vip_filter <- all_vip[,1] > 1
        predictors_vip <- predictors[, vip_filter, drop = FALSE]
        vip_model <- mixOmics::splsda(predictors_vip, groups,
                                      scale = TRUE, ncomp = comp_num,
                                      keepX = rep(sum(vip_filter), comp_num))
        vip_pred <- predict(vip_model, predictors_vip, dist = "max.dist")
        prediction2 <- cbind(original = groups, vip_pred$class$max.dist)
        acc2 <- 100 * signif(sum(prediction2[,1] == prediction2[,2]) / length(prediction2[,1]), digits = 2)
        
        incProgress(0.05, detail = "Generating VIP individual plot...")
        dev.control(displaylist = "enable")
        mixOmics::plotIndiv(vip_model,
                            ind.names = NA, legend = TRUE,
                            col = colors, pch = pch_values,
                            pch.levels = sort(unique(groups)),
                            title = paste("sPLS-DA (VIP>1):", analysis_label, "With Accuracy:", acc2, "%"),
                            ellipse = ellipse,
                            background = if(bg) mixOmics::background.predict(vip_model, comp.predicted = 2, dist = "max.dist") else NULL)
        vip_indiv_plot <- recordPlot()
        
        if (!is.null(style) && comp_num == 3 && tolower(style) == "3d") {
          dev.control(displaylist = "enable")
          vip_scores_mat <- vip_model$variates$X
          plot3D::scatter3D(vip_scores_mat[,1], vip_scores_mat[,2], vip_scores_mat[,3],
                            pch = pch_values, col = colors,
                            xlab = "Component 1", ylab = "Component 2", zlab = "Component 3",
                            main = paste("3D Plot (VIP>1):", analysis_label),
                            theta = 20, phi = 30, bty = "g", colkey = FALSE)
          vip_3D <- recordPlot()
        }
        
        if (roc) {
          dev.control(displaylist = "enable")
          mixOmics::auroc(vip_model,
                          newdata = predictors_vip,
                          outcome.test = groups,
                          plot = TRUE, roc.comp = comp_num,
                          title = paste("ROC Curve (VIP>1):", analysis_label),
                          print = FALSE)
          vip_ROC <- recordPlot()
        }
        
        if (!is.null(cv_opt)) {
          if (cv_opt == "loocv") {
            set.seed(123)
            vip_cv_res <- mixOmics::perf(vip_model, validation = "loo")
            vip_err_rates <- vip_cv_res$error.rate$overall[,"max.dist"]
            vip_err_df <- data.frame(Component = seq_len(nrow(vip_cv_res$error.rate$overall)),
                                     ErrorRate = vip_err_rates)
            vip_CV <- ggplot2::ggplot(vip_err_df, ggplot2::aes(x = Component, y = ErrorRate)) +
              ggplot2::geom_line(color = "red") +
              ggplot2::geom_point(color = "red", size = 3) +
              ggplot2::labs(title = paste("LOOCV Error Rate (VIP>1):", analysis_label),
                            x = "Component", y = "Error Rate") +
              ggplot2::theme_minimal()
            print(vip_CV)
          } else if (cv_opt == "Mfold") {
            set.seed(123)
            vip_cv_res <- mixOmics::perf(vip_model, validation = "Mfold", folds = fold_num, nrepeat = 1000)
            vip_err_rates <- vip_cv_res$error.rate$overall[,"max.dist"]
            vip_err_df <- data.frame(Component = seq_len(nrow(vip_cv_res$error.rate$overall)),
                                     ErrorRate = vip_err_rates)
            vip_CV <- ggplot2::ggplot(vip_err_df, ggplot2::aes(x = Component, y = ErrorRate)) +
              ggplot2::geom_line(color = "red") +
              ggplot2::geom_point(color = "red", size = 3) +
              ggplot2::labs(title = paste("Mfold Error Rate (VIP>1):", analysis_label),
                            x = "Component", y = "Error Rate") +
              ggplot2::theme_minimal()
            print(vip_CV)
          }
        }
      }
      
      if (conf_mat == TRUE) {
        conf_text_overall <- capture.output({
          cat("Overall Confusion Matrix for PLS-DA Comparison\n")
          prediction1 <- cbind(original = groups, overall_pred$class$max.dist)
          cm <- caret::confusionMatrix(data = as.factor(prediction1[,2]),
                                       reference = as.factor(prediction1[,1]))
          print(cm$table)
          cat("Accuracy:", signif(cm$overall["Accuracy"], 2), "\n")
          if (nlevels(as.factor(prediction1[,1])) == 2) {
            cat("Sensitivity:", signif(cm$byClass["Sensitivity"], 2), "\n")
            cat("Specificity:", signif(cm$byClass["Specificity"], 2), "\n")
          } else {
            cat("\nPer-Class Sensitivity:\n")
            print(signif(cm$byClass[, "Sensitivity"], 2))
            cat("\nPer-Class Specificity:\n")
            print(signif(cm$byClass[, "Specificity"], 2))
          }
        })
        conf_text_vip <- capture.output({
          cat("Overall Confusion Matrix for PLS-DA Comparison with VIP Score > 1\n")
          prediction2 <- cbind(original = groups, vip_pred$class$max.dist)
          cm_vip <- caret::confusionMatrix(data = as.factor(prediction2[,2]),
                                           reference = as.factor(prediction2[,1]))
          print(cm_vip$table)
          cat("Accuracy:", signif(cm_vip$overall["Accuracy"], 2), "\n")
          if (nlevels(as.factor(prediction2[,1])) == 2) {
            cat("Sensitivity:", signif(cm_vip$byClass["Sensitivity"], 2), "\n")
            cat("Specificity:", signif(cm_vip$byClass["Specificity"], 2), "\n")
          } else {
            cat("\nPer-Class Sensitivity:\n")
            print(signif(cm_vip$byClass[, "Sensitivity"], 2))
            cat("\nPer-Class Specificity:\n")
            print(signif(cm_vip$byClass[, "Specificity"], 2))
          }
        })
        conf_text <- c(conf_text_overall, conf_text_vip)
      } else {
        conf_text <- NULL
      }
      
      incProgress(0.05, detail = "Finalizing interactive mode...")
      result_list <- list(
        overall_indiv_plot = overall_indiv_plot,
        overall_3D         = overall_3D,
        overall_ROC        = overall_ROC,
        overall_CV         = overall_CV,
        loadings           = loadings_list,
        vip_scores         = vip_scores,
        vip_indiv_plot     = vip_indiv_plot,
        vip_3D             = vip_3D,
        vip_ROC            = vip_ROC,
        vip_CV             = vip_CV,
        conf_matrix        = conf_text
      )
      return(result_list)
    })
  }
  
  ## ---------------------------
  ## Determine Analysis Branch (Interactive)
  ## ---------------------------
  if (group_col == trt_col) {
    return(run_overall_interactive(data, "Overall Analysis"))
  } else {
    treatments <- unique(data[[trt_col]])
    results_by_treatment <- lapply(treatments, function(trt) {
      subset_data <- data[data[[trt_col]] == trt, , drop = FALSE]
      run_overall_interactive(subset_data, paste("Treatment =", trt))
    })
    names(results_by_treatment) <- treatments
    return(results_by_treatment)
  }
}
