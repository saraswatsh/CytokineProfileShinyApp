source("global.R")
source("ui.R")

shiny::shinyApp(ui = ui, server = app_server)
