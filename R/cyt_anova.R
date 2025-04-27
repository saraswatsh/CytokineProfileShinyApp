#' ANOVA Analysis on Continuous Variables.
#'
#' This function performs an analysis of variance (ANOVA) for each continuous
#' variable against every categorical predictor in the input data. Character
#' columns are automatically converted to factors; all factor columns are used
#' as predictors while numeric columns are used as continuous outcomes.
#' For each valid predictor (i.e., with more than one level and no more than 10 levels),
#' Tukey's Honest Significant Difference (HSD) test is conducted and the adjusted
#' p-values for pairwise comparisons are extracted.
#'
#' @param data A data frame or matrix containing both categorical and continuous variables.
#'   Character columns will be converted to factors and used as predictors, while numeric columns
#'   will be used as continuous outcomes.
#' @param format_output Logical. If TRUE, returns the results as a tidy data frame instead of a list.
#'   Default is FALSE.
#'
#' @return If \code{format_output} is FALSE (default), a list of adjusted p-values from Tukey's HSD tests
#'   for each combination of continuous outcome and categorical predictor.
#'   If \code{format_output} is TRUE, a data frame with columns "Outcome", "Categorical", "Comparison", and "P_adj".
#'
#' @examples
#' data("ExampleData1")
#' cyt_anova(ExampleData1[, c(1:2, 5:6)], format_output = TRUE)
#'
#' @export

cyt_anova <- function(data, format_output = FALSE, progress = NULL) {
  if (!is.null(progress))
    progress$inc(0.05, detail = "Converting data to data frame")
  x1_df <- as.data.frame(data)

  if (!is.null(progress))
    progress$inc(0.05, detail = "Converting characters to factors")
  cat_vars <- sapply(x1_df, is.character)
  if (any(cat_vars)) {
    x1_df[cat_vars] <- lapply(x1_df[cat_vars], as.factor)
  }

  if (!is.null(progress))
    progress$inc(0.05, detail = "Identifying predictors and outcomes")
  cat_preds <- sapply(x1_df, is.factor)
  cont_vars <- sapply(x1_df, is.numeric)

  tukey_results <- list()
  iter_count <- 0

  for (cat_var in names(x1_df)[cat_preds]) {
    num_levels <- length(levels(x1_df[[cat_var]]))
    if (num_levels == 1 || num_levels > 10) next

    for (outcome in names(x1_df)[cont_vars]) {
      model <- stats::aov(
        stats::as.formula(paste(outcome, "~", cat_var)),
        data = x1_df
      )
      tukey_result <- stats::TukeyHSD(model)
      p_vals <- tukey_result[[cat_var]][, "p adj"]
      result_key <- paste(outcome, cat_var, sep = "_")
      tukey_results[[result_key]] <- p_vals
      iter_count <- iter_count + 1
      if (!is.null(progress))
        progress$inc(0, detail = paste("Processed", iter_count, "comparisons"))
    }
  }

  if (length(tukey_results) == 0) {
    if (!is.null(progress))
      progress$inc(0.1, detail = "No valid comparisons performed")
    return(
      "No valid comparisons were performed. Check that your data has numeric columns and factors with 2-10 levels."
    )
  }

  if (!is.null(progress)) progress$inc(0.1, detail = "Formatting output")
  if (!format_output) {
    return(tukey_results)
  } else {
    out_df <- data.frame(
      Outcome = character(),
      Categorical = character(),
      Comparison = character(),
      P_adj = numeric(),
      stringsAsFactors = FALSE
    )

    for (key in names(tukey_results)) {
      parts <- unlist(strsplit(key, "_"))
      outcome <- parts[1]
      cat_var <- parts[2]
      p_vals <- tukey_results[[key]]
      for (comp in names(p_vals)) {
        out_df <- rbind(
          out_df,
          data.frame(
            Outcome = outcome,
            Categorical = cat_var,
            Comparison = comp,
            P_adj = round(p_vals[comp], 3),
            stringsAsFactors = FALSE
          )
        )
      }
    }
    if (!is.null(progress)) progress$inc(0.05, detail = "Done")
    return(out_df)
  }
}
