#' Launch the CytokineProfile Shiny App
#' @export
run_app <- function() {
  app_file <- system.file("app.R", package = "CytokineProfileShinyApp")
  if (!nzchar(app_file)) {
    stop("Could not find app directory.")
  }
  shiny::runApp(dirname(app_file), display.mode = "normal")
}
