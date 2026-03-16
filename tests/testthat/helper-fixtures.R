data("ExampleData1", package = "CytokineProfileShinyApp", envir = environment())
data("ExampleData5", package = "CytokineProfileShinyApp", envir = environment())

ex1_full <- as.data.frame(ExampleData1)
ex1_numeric <- ex1_full[, -c(1:3), drop = FALSE]
ex1_group <- ex1_full[, -c(2:3), drop = FALSE]
ex1_binary_group <- subset(ex1_full, Group != "ND")
ex1_binary_group_treatment <- subset(
  ex1_full,
  Group != "ND" & Treatment != "Unstimulated"
)
ex1_pca <- subset(
  ex1_full[, -c(3, 23), drop = FALSE],
  Group != "ND" & Treatment != "Unstimulated"
)
ex5_mint <- subset(ExampleData5[, -c(2, 4), drop = FALSE], Group != "ND")
ex5_mint_split <- subset(ExampleData5[, -4, drop = FALSE], Group != "ND")

with_temp_pdf_device <- function(code) {
  pdf_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf_file)
  dev_num <- grDevices::dev.cur()
  on.exit(
    {
      if (!is.null(grDevices::dev.list()) && dev_num %in% grDevices::dev.list()) {
        grDevices::dev.off(dev_num)
      }
      if (file.exists(pdf_file)) {
        unlink(pdf_file)
      }
    },
    add = TRUE
  )

  force(code)
}

suppress_known_plot_warnings <- function(code) {
  withCallingHandlers(
    code,
    warning = function(w) {
      warning_message <- conditionMessage(w)
      if (
        grepl("`aes_string\\(\\)` was deprecated", warning_message) ||
          grepl("the standard deviation is zero", warning_message, fixed = TRUE)
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

first_non_null <- function(x) {
  values <- Filter(Negate(is.null), x)
  if (!length(values)) {
    return(NULL)
  }

  values[[1]]
}

make_progress_recorder <- function() {
  log <- list(set = list(), inc = list())

  list(
    set = function(message = NULL, value = NULL, detail = NULL) {
      log$set[[length(log$set) + 1L]] <<- list(
        message = message,
        value = value,
        detail = detail
      )
      invisible(NULL)
    },
    inc = function(amount, detail = NULL) {
      log$inc[[length(log$inc) + 1L]] <<- list(
        amount = amount,
        detail = detail
      )
      invisible(NULL)
    },
    get_log = function() log
  )
}

helper_invalid_numeric_df <- data.frame(
  good = c(1, 2, 3),
  has_na = c(1, NA_real_, 3),
  has_nan = c(1, NaN, 3),
  has_inf = c(1, Inf, 3),
  has_ninf = c(1, -Inf, 3)
)

helper_font_settings <- list(
  base_size = 12,
  plot_title = 14,
  x_title = 13,
  y_title = 13,
  x_text = 11,
  y_text = 10,
  legend_title = 12,
  legend_text = 11,
  strip_text = 12,
  annotation_text = 10,
  row_names = 9,
  col_names = 8,
  cell_text = 7,
  variable_names = 15,
  point_labels = 16
)

anova_test_df <- data.frame(
  Group = factor(rep(c("A", "B", "C"), each = 4)),
  Batch = factor(rep(c("X", "Y"), times = 6)),
  Outcome1 = c(1, 2, 2, 3, 4, 5, 5, 6, 7, 8, 8, 9),
  Outcome2 = c(2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10),
  stringsAsFactors = FALSE
)

anova_invalid_df <- data.frame(
  Group = factor(rep("A", 4)),
  Outcome = c(1, 2, 3, 4)
)

bp2_test_df <- data.frame(
  Group = factor(rep(c("A", "B"), each = 4)),
  Marker1 = c(1, 2, 0, 4, 5, 6, -1, 8),
  Marker2 = c(2, 4, 8, 16, 32, 64, 128, 256)
)

heatmap_constant_df <- data.frame(
  Group = factor(c("A", "B", "A", "B")),
  Marker1 = c(1, 1, 1, 1),
  Marker2 = c(1, 2, 3, 4)
)

heatmap_bad_df <- data.frame(
  Marker1 = c(1, NA_real_, 3),
  Marker2 = c(2, Inf, 4)
)
