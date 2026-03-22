pkg_ns <- asNamespace("CytokineProfileShinyApp")
ui <- base::get("app_ui", envir = pkg_ns)()
server <- base::get("app_server", envir = pkg_ns)

shiny::shinyApp(ui = ui, server = server)
