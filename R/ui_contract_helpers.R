#' Internal UI contract helpers
#'
#' Shared helper values for UI labels, help text, and conditional expressions
#' that need to stay consistent across the stage-based runtime and future
#' modular extractions.
#'
#' @name ui_contract_helpers
#' @noRd
NULL

ui_two_group_test_choices <- function() {
  c(
    "Auto" = "auto",
    "Welch t-test" = "ttest",
    "Wilcoxon" = "wilcox"
  )
}

ui_two_group_univariate_help_text <- function() {
  paste(
    "Choose how the app compares two groups for each numeric outcome.",
    "Use Auto if you are unsure.",
    "The app will pick between Welch's t-test and the Wilcoxon test based on the data,",
    "while the other options force one method for every outcome."
  )
}

ui_error_bar_test_help_text <- function() {
  paste(
    "Choose which statistical test is used when the plot adds pairwise significance labels.",
    "Use Auto if you are unsure.",
    "The app will choose between Welch's t-test and the Wilcoxon test,",
    "while the other options force one method across all outcomes."
  )
}

ui_imputation_method_input_id <- function() {
  "imputation_method"
}

ui_imputation_method_choices <- function() {
  c(
    "Mean" = "mean",
    "Median" = "median",
    "Mode" = "mode",
    "kNN (sample-wise)" = "knn_sample",
    "kNN (feature-wise)" = "knn_feature"
  )
}

ui_imputation_knn_condition_expr <- function() {
  sprintf(
    "input.%s === 'knn_sample' || input.%s === 'knn_feature'",
    ui_imputation_method_input_id(),
    ui_imputation_method_input_id()
  )
}
ui_analysis_font_helper_specs <- function() {
  entry <- function(title, content) {
    list(title = title, content = content)
  }

  list(
    "Boxplots" = list(
      plot_title = entry(
        "Boxplot Title Text",
        "Changes the main title shown above each boxplot page."
      ),
      x_title = entry(
        "Boxplot X-Axis Title",
        "Changes the label under the horizontal axis, such as the feature or grouping name."
      ),
      y_title = entry(
        "Boxplot Y-Axis Title",
        "Changes the label beside the vertical axis for the measured values."
      ),
      x_text = entry(
        "Boxplot X-Axis Labels",
        "Changes the names shown along the horizontal axis, such as groups or feature labels."
      ),
      y_text = entry(
        "Boxplot Y-Axis Labels",
        "Changes the value labels shown along the vertical axis."
      ),
      legend_title = entry(
        "Boxplot Legend Title",
        "Changes the heading at the top of the boxplot legend when colors or fills are used."
      ),
      legend_text = entry(
        "Boxplot Legend Labels",
        "Changes the group names listed inside the boxplot legend."
      ),
      strip_text = entry(
        "Boxplot Facet Labels",
        "Changes the small panel headers when the boxplots are split into multiple facets."
      )
    ),
    "Violin Plots" = list(
      plot_title = entry(
        "Violin Plot Title Text",
        "Changes the main title shown above each violin-plot page."
      ),
      x_title = entry(
        "Violin X-Axis Title",
        "Changes the label under the horizontal axis, such as the feature or grouping name."
      ),
      y_title = entry(
        "Violin Y-Axis Title",
        "Changes the label beside the vertical axis for the measured values."
      ),
      x_text = entry(
        "Violin X-Axis Labels",
        "Changes the names shown along the horizontal axis, such as groups or feature labels."
      ),
      y_text = entry(
        "Violin Y-Axis Labels",
        "Changes the value labels shown along the vertical axis."
      ),
      legend_title = entry(
        "Violin Legend Title",
        "Changes the heading at the top of the violin-plot legend when colors or fills are used."
      ),
      legend_text = entry(
        "Violin Legend Labels",
        "Changes the group names listed inside the violin-plot legend."
      ),
      strip_text = entry(
        "Violin Facet Labels",
        "Changes the small panel headers when the violin plots are split into multiple facets."
      )
    ),
    "Error-Bar Plot" = list(
      plot_title = entry(
        "Error-Bar Plot Title Text",
        "Changes the main title shown above the error-bar plot."
      ),
      x_title = entry(
        "Error-Bar X-Axis Title",
        "Changes the label under the horizontal axis for the compared groups or features."
      ),
      y_title = entry(
        "Error-Bar Y-Axis Title",
        "Changes the label beside the vertical axis for the plotted summary values."
      ),
      x_text = entry(
        "Error-Bar X-Axis Labels",
        "Changes the names shown along the horizontal axis, such as groups or cytokine labels."
      ),
      y_text = entry(
        "Error-Bar Y-Axis Labels",
        "Changes the value labels shown along the vertical axis."
      ),
      legend_title = entry(
        "Error-Bar Legend Title",
        "Changes the heading at the top of the legend when the plot uses grouped colors or fills."
      ),
      legend_text = entry(
        "Error-Bar Legend Labels",
        "Changes the group names listed inside the legend."
      ),
      strip_text = entry(
        "Error-Bar Facet Labels",
        "Changes the small panel headers when the error-bar plot is split into facets."
      ),
      annotation_text = entry(
        "Error-Bar Annotation Text",
        "Changes the size of significance labels, effect-size labels, and other text drawn above the plotted summaries."
      )
    ),
    "Dual-Flashlight Plot" = list(
      plot_title = entry(
        "Dual-Flashlight Plot Title Text",
        "Changes the main title shown above the dual-flashlight plot."
      ),
      x_title = entry(
        "Dual-Flashlight X-Axis Title",
        "Changes the label under the horizontal axis for the first comparison scale."
      ),
      y_title = entry(
        "Dual-Flashlight Y-Axis Title",
        "Changes the label beside the vertical axis for the second comparison scale."
      ),
      x_text = entry(
        "Dual-Flashlight X-Axis Labels",
        "Changes the labels shown along the horizontal axis."
      ),
      y_text = entry(
        "Dual-Flashlight Y-Axis Labels",
        "Changes the labels shown along the vertical axis."
      ),
      legend_title = entry(
        "Dual-Flashlight Legend Title",
        "Changes the heading at the top of the legend when the plot uses grouped colors or categories."
      ),
      legend_text = entry(
        "Dual-Flashlight Legend Labels",
        "Changes the group names listed inside the legend."
      ),
      strip_text = entry(
        "Dual-Flashlight Facet Labels",
        "Changes the small panel headers when the dual-flashlight plot is split into facets."
      ),
      annotation_text = entry(
        "Dual-Flashlight Annotation Text",
        "Changes the size of highlighted feature names and other annotation text placed on the plot."
      )
    ),
    "Heatmap" = list(
      row_names = entry(
        "Heatmap Row Labels",
        "Changes the feature names shown on the left side of the heatmap."
      ),
      col_names = entry(
        "Heatmap Column Labels",
        "Changes the sample or group names shown across the top of the heatmap."
      )
    ),
    "Correlation Plots" = list(
      plot_title = entry(
        "Correlation Plot Title Text",
        "Changes the main title shown above the correlation heatmap."
      ),
      x_text = entry(
        "Correlation X-Axis Labels",
        "Changes the feature names shown across the top of the correlation heatmap."
      ),
      y_text = entry(
        "Correlation Y-Axis Labels",
        "Changes the feature names shown along the left side of the correlation heatmap."
      ),
      legend_title = entry(
        "Correlation Legend Title",
        "Changes the heading above the color scale legend."
      ),
      legend_text = entry(
        "Correlation Legend Labels",
        "Changes the numeric labels shown on the color scale legend."
      ),
      cell_text = entry(
        "Correlation Cell Labels",
        "Changes the correlation numbers printed inside each heatmap square."
      )
    ),
    "Skewness/Kurtosis" = list(
      plot_title = entry(
        "Skewness and Kurtosis Plot Titles",
        "Changes the titles shown above the skewness and kurtosis plots."
      ),
      x_title = entry(
        "Skewness and Kurtosis X-Axis Title",
        "Changes the label under the horizontal axis for the compared variables or groups."
      ),
      y_title = entry(
        "Skewness and Kurtosis Y-Axis Title",
        "Changes the label beside the vertical axis for the plotted values."
      ),
      x_text = entry(
        "Skewness and Kurtosis X-Axis Labels",
        "Changes the names shown along the horizontal axis, such as groups or variable names."
      ),
      y_text = entry(
        "Skewness and Kurtosis Y-Axis Labels",
        "Changes the value labels shown along the vertical axis."
      ),
      legend_title = entry(
        "Skewness and Kurtosis Legend Title",
        "Changes the heading at the top of the legend when grouped colors or categories are shown."
      ),
      legend_text = entry(
        "Skewness and Kurtosis Legend Labels",
        "Changes the group names listed inside the legend."
      ),
      strip_text = entry(
        "Skewness and Kurtosis Facet Labels",
        "Changes the small panel headers when these plots are split into facets."
      )
    ),
    "Volcano Plot" = list(
      plot_title = entry(
        "Volcano Plot Title Text",
        "Changes the main title shown above the volcano plot."
      ),
      x_title = entry(
        "Volcano X-Axis Title",
        "Changes the label under the horizontal axis for log2 fold change."
      ),
      y_title = entry(
        "Volcano Y-Axis Title",
        "Changes the label beside the vertical axis for statistical significance."
      ),
      x_text = entry(
        "Volcano X-Axis Labels",
        "Changes the numeric labels shown along the fold-change axis."
      ),
      y_text = entry(
        "Volcano Y-Axis Labels",
        "Changes the numeric labels shown along the significance axis."
      ),
      legend_title = entry(
        "Volcano Legend Title",
        "Changes the heading at the top of the volcano-plot legend."
      ),
      legend_text = entry(
        "Volcano Legend Labels",
        "Changes the category names listed inside the volcano-plot legend."
      ),
      strip_text = entry(
        "Volcano Facet Labels",
        "Changes the small panel headers when the volcano plot is split into facets."
      ),
      annotation_text = entry(
        "Volcano Feature Labels",
        "Changes the names of highlighted features shown next to plotted points."
      )
    ),
    "Principal Component Analysis (PCA)" = list(
      plot_title = entry(
        "PCA Plot Titles",
        "Changes the titles shown above PCA score, loading, biplot, and scree plots."
      ),
      x_title = entry(
        "PCA X-Axis Title",
        "Changes the horizontal axis title for the displayed principal component."
      ),
      y_title = entry(
        "PCA Y-Axis Title",
        "Changes the vertical axis title for the displayed principal component."
      ),
      x_text = entry(
        "PCA X-Axis Labels",
        "Changes the tick labels shown along the horizontal component axis."
      ),
      y_text = entry(
        "PCA Y-Axis Labels",
        "Changes the tick labels shown along the vertical component axis."
      ),
      legend_title = entry(
        "PCA Legend Title",
        "Changes the heading at the top of the PCA legend."
      ),
      legend_text = entry(
        "PCA Legend Labels",
        "Changes the group names listed inside the PCA legend."
      ),
      strip_text = entry(
        "PCA Facet Labels",
        "Changes the small panel headers when PCA outputs are split into facets or grouped panels."
      ),
      annotation_text = entry(
        "PCA Annotation Text",
        "Changes the text used for annotated PCA views, such as scree labels or other callouts."
      ),
      variable_names = entry(
        "PCA Variable Labels",
        "Changes the feature names drawn on loading plots, biplots, and correlation-circle style views."
      ),
      point_labels = entry(
        "PCA Sample Labels or Points",
        "Changes the sample labels on PCA individual plots. If sample names are hidden, this same control changes point size instead."
      )
    ),
    "Partial Least Squares Regression (PLSR)" = list(
      plot_title = entry(
        "PLSR Plot Titles",
        "Changes the titles shown above PLSR score, prediction, residual, loading, and VIP plots."
      ),
      x_title = entry(
        "PLSR X-Axis Title",
        "Changes the label under the horizontal axis on PLSR plots."
      ),
      y_title = entry(
        "PLSR Y-Axis Title",
        "Changes the label beside the vertical axis on PLSR plots."
      ),
      x_text = entry(
        "PLSR X-Axis Labels",
        "Changes the labels shown along the horizontal axis on PLSR plots."
      ),
      y_text = entry(
        "PLSR Y-Axis Labels",
        "Changes the labels shown along the vertical axis on PLSR plots."
      ),
      legend_title = entry(
        "PLSR Legend Title",
        "Changes the heading at the top of the PLSR legend."
      ),
      legend_text = entry(
        "PLSR Legend Labels",
        "Changes the group names listed inside the PLSR legend."
      ),
      strip_text = entry(
        "PLSR Facet Labels",
        "Changes the small panel headers when PLSR outputs are split into panels."
      ),
      variable_names = entry(
        "PLSR Variable Labels",
        "Changes the predictor names drawn on loading and VIP plots."
      ),
      point_labels = entry(
        "PLSR Sample Labels or Points",
        "Changes the sample labels on PLSR score plots. If sample names are hidden, this same control changes point size instead."
      )
    ),
    "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)" = list(
      plot_title = entry(
        "sPLS-DA Plot Titles",
        "Changes the titles shown above sPLS-DA score, loading, ROC, and cross-validation plots."
      ),
      x_title = entry(
        "sPLS-DA X-Axis Title",
        "Changes the label under the horizontal axis on sPLS-DA plots."
      ),
      y_title = entry(
        "sPLS-DA Y-Axis Title",
        "Changes the label beside the vertical axis on sPLS-DA plots."
      ),
      x_text = entry(
        "sPLS-DA X-Axis Labels",
        "Changes the labels shown along the horizontal axis on sPLS-DA plots."
      ),
      y_text = entry(
        "sPLS-DA Y-Axis Labels",
        "Changes the labels shown along the vertical axis on sPLS-DA plots."
      ),
      legend_title = entry(
        "sPLS-DA Legend Title",
        "Changes the heading at the top of the sPLS-DA legend."
      ),
      legend_text = entry(
        "sPLS-DA Legend Labels",
        "Changes the group names listed inside the sPLS-DA legend."
      ),
      strip_text = entry(
        "sPLS-DA Facet Labels",
        "Changes the small panel headers when sPLS-DA outputs are split into panels."
      ),
      variable_names = entry(
        "sPLS-DA Variable Labels",
        "Changes the selected feature names drawn on loading and related plots."
      ),
      point_labels = entry(
        "sPLS-DA Sample Labels or Points",
        "Changes the sample labels on sPLS-DA individual plots. If sample names are hidden, this same control changes point size instead."
      )
    ),
    "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)" = list(
      plot_title = entry(
        "MINT sPLS-DA Plot Titles",
        "Changes the titles shown above MINT sPLS-DA score, loading, correlation, CIM, and ROC plots."
      ),
      x_title = entry(
        "MINT sPLS-DA X-Axis Title",
        "Changes the label under the horizontal axis on MINT sPLS-DA plots."
      ),
      y_title = entry(
        "MINT sPLS-DA Y-Axis Title",
        "Changes the label beside the vertical axis on MINT sPLS-DA plots."
      ),
      x_text = entry(
        "MINT sPLS-DA X-Axis Labels",
        "Changes the labels shown along the horizontal axis on MINT sPLS-DA plots."
      ),
      y_text = entry(
        "MINT sPLS-DA Y-Axis Labels",
        "Changes the labels shown along the vertical axis on MINT sPLS-DA plots."
      ),
      legend_title = entry(
        "MINT sPLS-DA Legend Title",
        "Changes the heading at the top of the MINT sPLS-DA legend."
      ),
      legend_text = entry(
        "MINT sPLS-DA Legend Labels",
        "Changes the group names listed inside the MINT sPLS-DA legend."
      ),
      strip_text = entry(
        "MINT sPLS-DA Facet Labels",
        "Changes the small panel headers when MINT sPLS-DA outputs are split into panels."
      ),
      variable_names = entry(
        "MINT sPLS-DA Variable Labels",
        "Changes the selected feature names drawn on loading and related multiblock plots."
      ),
      point_labels = entry(
        "MINT sPLS-DA Sample Labels or Points",
        "Changes the sample labels on MINT sPLS-DA individual plots. If sample names are hidden, this same control changes point size instead."
      )
    ),
    "Random Forest" = list(
      plot_title = entry(
        "Random Forest Plot Titles",
        "Changes the titles shown above random forest importance, ROC, and validation plots."
      ),
      x_title = entry(
        "Random Forest X-Axis Title",
        "Changes the label under the horizontal axis on random forest plots."
      ),
      y_title = entry(
        "Random Forest Y-Axis Title",
        "Changes the label beside the vertical axis on random forest plots."
      ),
      x_text = entry(
        "Random Forest X-Axis Labels",
        "Changes the labels shown along the horizontal axis on random forest plots."
      ),
      y_text = entry(
        "Random Forest Y-Axis Labels",
        "Changes the labels shown along the vertical axis on random forest plots."
      ),
      legend_title = entry(
        "Random Forest Legend Title",
        "Changes the heading at the top of the random forest legend."
      ),
      legend_text = entry(
        "Random Forest Legend Labels",
        "Changes the group names listed inside the random forest legend."
      ),
      strip_text = entry(
        "Random Forest Facet Labels",
        "Changes the small panel headers if a random forest plot is split into facets."
      )
    ),
    "Extreme Gradient Boosting (XGBoost)" = list(
      plot_title = entry(
        "XGBoost Plot Titles",
        "Changes the titles shown above XGBoost importance, ROC, and validation plots."
      ),
      x_title = entry(
        "XGBoost X-Axis Title",
        "Changes the label under the horizontal axis on XGBoost plots."
      ),
      y_title = entry(
        "XGBoost Y-Axis Title",
        "Changes the label beside the vertical axis on XGBoost plots."
      ),
      x_text = entry(
        "XGBoost X-Axis Labels",
        "Changes the labels shown along the horizontal axis on XGBoost plots."
      ),
      y_text = entry(
        "XGBoost Y-Axis Labels",
        "Changes the labels shown along the vertical axis on XGBoost plots."
      ),
      legend_title = entry(
        "XGBoost Legend Title",
        "Changes the heading at the top of the XGBoost legend."
      ),
      legend_text = entry(
        "XGBoost Legend Labels",
        "Changes the group names listed inside the XGBoost legend."
      ),
      strip_text = entry(
        "XGBoost Facet Labels",
        "Changes the small panel headers if an XGBoost plot is split into facets."
      )
    )
  )
}

ui_analysis_font_helper_spec <- function(func_name, field) {
  specs <- ui_analysis_font_helper_specs()
  func_spec <- specs[[func_name]]

  if (is.null(func_spec)) {
    return(NULL)
  }

  func_spec[[field]]
}
