# Internal helper for locating the installed app directory.
run_app_path <- function() {
  system.file("app", package = "CytokineProfileShinyApp")
}

#' Launch the CytokineProfile Shiny App
#' @export
run_app <- function() {
  app_dir <- run_app_path()
  if (!nzchar(app_dir)) {
    stop("Could not find app directory.")
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
