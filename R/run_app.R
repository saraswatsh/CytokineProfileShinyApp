#' Launch the CytokineProfile Shiny App
#' @export
run_app <- function() {
  app_dir <- system.file("app.R", package = "CytokineProfileShinyApp")
  if (app_dir == "") {
    stop("Could not find app directory.")
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
