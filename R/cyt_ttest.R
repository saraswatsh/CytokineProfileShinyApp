#' Two Sample T-test Comparisons.
#'
#' This function performs pairwise comparisons between two groups for each combination
#' of a categorical predictor (with exactly two levels) and a continuous outcome variable.
#' It first converts any character variables in \code{data} to factors and, if specified,
#' applies a log2 transformation to the continuous variables. Depending on the value of
#' \code{scale}, the function conducts either a two-sample t-test (if \code{scale = "log2"})
#' or a Mann-Whitney U test (if \code{scale} is \code{NULL}). The resulting p-values are printed
#' and returned.
#'
#' @param data A matrix or data frame containing continuous and categorical variables.
#' @param scale A character specifying a transformation for continuous variables.
#'   Options are \code{NULL} (default) and \code{"log2"}. When \code{scale = "log2"},
#'   a log2 transformation is applied and a two-sample t-test is used; when \code{scale} is \code{NULL},
#'   a Mann-Whitney U test is performed.
#' @param format_output Logical. If TRUE, returns the results as a tidy data frame.
#'   Default is FALSE.
#'
#' @return If \code{format_output} is FALSE, returns a list of p-values (named by Outcome and Categorical variable).
#'   If TRUE, returns a data frame with columns "Outcome", "Categorical", "Comparison", and "P_value".
#'
#' @examples
#' \dontrun{
#' data_df <- cytodata[, -c(1, 4)]
#' data_df <- dplyr::filter(data_df, Group != "ND", Treatment != "Unstimulated")
#' # Two-sample t-test (log2 transformation)
#' res <- cyt_ttest(data_df[, c(1,2,5:6)], scale = "log2", format_output = TRUE)
#' head(res)
#'
#' # Mann-Whitney U test (no transformation)
#' res2 <- cyt_ttest(data_df[, c(1,2,5:6)], format_output = TRUE)
#' head(res2)
#' }
#'
#' @export
cyt_ttest <- function(data, scale = NULL, format_output = FALSE, progress = NULL) {
  if (!is.null(progress)) progress$inc(0.05, detail = "Converting data to data frame")
  x1_df <- as.data.frame(data)
  
  if (!is.null(progress)) progress$inc(0.05, detail = "Converting characters to factors")
  cat_vars <- sapply(x1_df, is.character)
  if (any(cat_vars)) {
    x1_df[cat_vars] <- lapply(x1_df[cat_vars], as.factor)
  }
  
  if (!is.null(progress)) progress$inc(0.05, detail = "Identifying categorical and numeric variables")
  cat_preds <- sapply(x1_df, is.factor)
  cont_vars <- sapply(x1_df, is.numeric)
  
  test_results <- list()
  
  if (!is.null(scale) && scale == "log2") {
    if (!is.null(progress)) progress$inc(0.05, detail = "Applying log2 transformation")
    x1_df[cont_vars] <- lapply(x1_df[cont_vars], function(x) {
      x[x <= 0] <- NA
      log2(x)
    })
  }
  
  iter_count <- 0
  if (!is.null(progress)) progress$inc(0.05, detail = "Performing tests")
  for (cat_var in names(x1_df)[cat_preds]) {
    if (length(levels(x1_df[[cat_var]])) != 2) {
      warning(paste("Skipping", cat_var, ": must have exactly two levels."))
      next
    }
    for (outcome in names(x1_df)[cont_vars]) {
      groups <- levels(x1_df[[cat_var]])
      group1 <- x1_df[[outcome]][x1_df[[cat_var]] == groups[1]]
      group2 <- x1_df[[outcome]][x1_df[[cat_var]] == groups[2]]
      
      if (length(group1) < 2 || length(group2) < 2) {
        message(paste("Skipping test for", outcome, "in", cat_var, "due to insufficient data."))
        next
      }
      if (sd(group1, na.rm = TRUE) == 0 || sd(group2, na.rm = TRUE) == 0) {
        message(paste("Skipping test for", outcome, "in", cat_var, "due to low variance."))
        next
      }
      
      comparison_name <- paste(groups[1], "vs", groups[2])
      test_formula <- as.formula(paste(outcome, "~", cat_var))
      
      if (!is.null(scale) && scale == "log2") {
        test_result <- t.test(test_formula, data = x1_df)
        message_text <- paste("T-test p-value for", comparison_name, "on", outcome, ":", signif(test_result$p.value, 4))
      } else {
        test_result <- wilcox.test(test_formula, data = x1_df)
        message_text <- paste("Mann-Whitney U test p-value for", comparison_name, "on", outcome, ":", signif(test_result$p.value, 4))
      }
      
      message(message_text)
      result_key <- paste(outcome, cat_var, sep = "_")
      test_results[[result_key]] <- test_result$p.value
      iter_count <- iter_count + 1
      if (!is.null(progress))
        progress$inc(0.01, detail = paste("Processed", iter_count, "tests"))
    }
  }
  
  if (length(test_results) == 0) {
    if (!is.null(progress)) progress$inc(0.05, detail = "No valid tests performed")
    return("No valid tests were performed.")
  }
  
  if (!is.null(progress)) progress$inc(0.05, detail = "Formatting results")
  if (!format_output) {
    return(test_results)
  } else {
    out_df <- data.frame(Outcome = character(),
                         Categorical = character(),
                         Comparison = character(),
                         P_value = numeric(),
                         stringsAsFactors = FALSE)
    
    for (key in names(test_results)) {
      parts <- unlist(strsplit(key, "_"))
      outcome <- parts[1]
      cat_var <- parts[2]
      groups <- levels(x1_df[[cat_var]])
      comp <- paste(groups[1], "vs", groups[2])
      out_df <- rbind(out_df, data.frame(Outcome = outcome,
                                         Categorical = cat_var,
                                         Comparison = comp,
                                         P_value = test_results[[key]],
                                         stringsAsFactors = FALSE))
    }
    if (!is.null(progress)) progress$inc(0.05, detail = "Done")
    return(out_df)
  }
}

