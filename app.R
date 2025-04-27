library(shiny)

source("global.R") # Sourcing built in data set R script and functions
# Source the UI and server code
source("ui.R") # This should create the object "ui"
source("server.R") # This should create the object "server"
shiny::shinyApp(ui = ui, server = server)
