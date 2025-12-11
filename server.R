# Define server logic
server <- function(input, output, session) {
  # Creating a temp dir for data uploads
  upload_dir <- file.path(tempdir(), "uploads")
  dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)
  builtins_dir <- file.path(tempdir(), "builtins")
  dir.create(builtins_dir, recursive = TRUE, showWarnings = FALSE)

  # Helpers
  shinyhelper::observe_helpers()
  shinyFeedback::useShinyFeedback()
  load_user_theme <- function() {
    path <- "WWW/user_theme.txt"
    if (!file.exists(path)) {
      return(NULL)
    }
    val <- readLines(path, warn = FALSE)
    if (!length(val)) {
      return(NULL)
    }
    # only allow known values
    if (val[1] %in% c("auto", "flatly", "darkly")) val[1] else NULL
  }
  session$userData$stored_theme <- load_user_theme()
  source("server-logic/theme_toggle.R", local = TRUE)
  source("server-logic/wizard_step_control.R", local = TRUE)
  source("server-logic/persistent_state.R", local = TRUE)
  source("server-logic/data_handling.R", local = TRUE)
  source("server-logic/data_filtering.R", local = TRUE)
  source("server-logic/options_ui.R", local = TRUE)
  source("server-logic/navigation.R", local = TRUE)
  source("server-logic/update_inputs.R", local = TRUE)
  source("server-logic/analysis_results.R", local = TRUE)
  source("server-logic/save_key_inputs.R", local = TRUE)
}
