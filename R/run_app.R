# Internal helper for locating the installed app entrypoint.
run_app_path <- function() {
  system.file("app.R", package = "CytokineProfileShinyApp")
}

#' Launch the CytokineProfile Shiny App
#' @export
run_app <- function() {
  app_file <- run_app_path()
  if (!nzchar(app_file)) {
    stop("Could not find app directory.")
  }
  shiny::runApp(dirname(app_file), display.mode = "normal")
}
