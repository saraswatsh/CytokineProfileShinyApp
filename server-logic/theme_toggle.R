## ---------------------------
## Theme Toggle
## ---------------------------
# Use session$userData to store the user's theme across navigation,
# and fall back to the system theme when theme_choice == "auto".
# When the app launches, pick from: stored_user_theme, system_theme, or a default.

# Set initial theme after system_theme becomes available
# Helper: system theme with fallback
system_theme <- reactive({
  if (is.null(input$system_theme)) "flatly" else input$system_theme
})

# Initial theme: run once when session starts and system_theme is known
observeEvent(
  system_theme(),
  {
    isolate({
      # 1) stored theme from previous session (if any)
      choice <- session$userData$stored_theme %||% "auto"

      # 2) derive the bootswatch name
      bootswatch <- switch(
        choice,
        "auto" = system_theme(),
        "flatly" = "flatly",
        "darkly" = "darkly",
        system_theme() # fallback, should never hit
      )

      session$setCurrentTheme(
        bslib::bs_theme(
          bootswatch = bootswatch,
          base_font = bslib::font_google("Inter"),
          code_font = bslib::font_google("Roboto Mono")
        )
      )

      # Make sure the selectInput reflects the choice
      updateSelectInput(session, "theme_choice", selected = choice)
    })
  },
  once = TRUE
)

# When the user picks a new theme
observeEvent(input$theme_choice, {
  req(input$theme_choice)

  bootswatch <- switch(
    input$theme_choice,
    "auto" = system_theme(),
    "flatly" = "flatly",
    "darkly" = "darkly",
    system_theme()
  )

  session$setCurrentTheme(
    bslib::bs_theme(
      bootswatch = bootswatch,
      base_font = bslib::font_google("Inter"),
      code_font = bslib::font_google("Roboto Mono")
    )
  )

  # Remember what the user picked
  session$userData$stored_theme <- input$theme_choice

  # Persist to disk so we see it next time
  path <- "WWW/user_theme.txt"
  dir.create("WWW", showWarnings = FALSE)
  writeLines(input$theme_choice, path)
})
