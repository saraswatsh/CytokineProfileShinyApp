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
