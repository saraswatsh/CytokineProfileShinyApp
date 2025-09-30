  ## ---------------------------
  ## Theme Toggle
  ## ---------------------------
  # Set an initial theme
  session$setCurrentTheme(
    bslib::bs_theme(
      bootswatch = "darkly",
      base_font = font_google("Inter"),
      code_font = font_google("Roboto Mono")
    )
  )

  # Rebuild the theme when the user picks a new theme
  shiny::observeEvent(input$theme_choice, {
    session$setCurrentTheme(
      bslib::bs_theme(
        bootswatch = input$theme_choice,
        base_font = font_google("Inter"),
        code_font = font_google("Roboto Mono")
      )
    )
  })
