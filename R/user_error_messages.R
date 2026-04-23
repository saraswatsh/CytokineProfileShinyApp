app_note_technical_error <- function(context, error) {
  msg <- conditionMessage(error)
  if (nzchar(msg)) {
    message(sprintf("%s failed: %s", context, msg))
  }
  invisible(NULL)
}

app_user_safe_error <- function(message) {
  err <- shiny::safeError(message)
  class(err) <- c("app_user_error", class(err))
  err
}

app_analysis_user_message <- function(error) {
  if (inherits(error, "app_user_error")) {
    return(conditionMessage(error))
  }

  app_friendly_condition_message(error, "analysis")
}

app_friendly_condition_message <- function(error, context = "this step") {
  msg <- conditionMessage(error)

  if (grepl("Unsupported file type", msg, ignore.case = TRUE)) {
    return("Upload a CSV, TXT, XLS, or XLSX file.")
  }

  if (grepl("No sheets found|excel_sheets", msg, ignore.case = TRUE)) {
    return("Choose an Excel file with at least one readable sheet.")
  }

  if (grepl("log2|log10", msg, ignore.case = TRUE) &&
      grepl("greater than 0|positive|finite", msg, ignore.case = TRUE)) {
    return(paste(
      "This transformation needs positive finite values in the selected numeric columns.",
      "Fix zero, negative, or infinite values, or choose another preprocessing method."
    ))
  }

  if (grepl("zscore|z-score", msg, ignore.case = TRUE)) {
    return(paste(
      "Z-score preprocessing needs finite numeric values and at least one observed value",
      "in each selected column."
    ))
  }

  if (grepl("kNN \\(feature-wise\\) requires numeric columns", msg)) {
    return("Feature-wise kNN imputation needs at least one numeric column.")
  }

  if (grepl("kNN \\(feature-wise\\) requires at least 2 numeric columns", msg)) {
    return("Feature-wise kNN imputation needs at least two numeric columns.")
  }

  if (grepl("imputation failed|impute", msg, ignore.case = TRUE)) {
    return(paste(
      "Imputation could not be completed with the selected columns.",
      "Try a different method or adjust the columns."
    ))
  }

  if (grepl("No export files were generated|cyt_export|zip", msg, ignore.case = TRUE)) {
    return("We could not prepare the export. Try again or choose a different format.")
  }

  if (identical(context, "analysis")) {
    return(paste(
      "We could not complete this analysis.",
      "Check the selected columns and options, then run it again."
    ))
  }

  paste(
    "Something went wrong while completing this step.",
    "Check your inputs and try again."
  )
}
