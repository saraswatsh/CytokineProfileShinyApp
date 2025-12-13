## ---------------------------
## Theme Toggle (per-session, per-browser via localStorage)
## ---------------------------

base_theme <- bslib::bs_theme(
  base_font = bslib::font_google("Inter"),
  code_font = bslib::font_google("Roboto Mono")
)

system_theme <- reactive({
  if (is.null(input$system_theme)) "flatly" else input$system_theme
})

theme_choice_rv <- reactiveVal("auto")

# IMPORTANT: do NOT ignore init; we want the initial value on refresh
observeEvent(
  input$theme_choice,
  {
    req(input$theme_choice)
    theme_choice_rv(input$theme_choice)

    # persist per-browser
    shinyjs::runjs(sprintf(
      "localStorage.setItem('user_theme', '%s');",
      input$theme_choice
    ))
  },
  ignoreInit = FALSE
)

# Apply theme whenever either theme_choice_rv() or system_theme() changes
observe({
  choice <- theme_choice_rv()
  bootswatch <- if (identical(choice, "auto")) system_theme() else choice

  session$setCurrentTheme(
    bslib::bs_theme_update(base_theme, bootswatch = bootswatch)
  )
})
