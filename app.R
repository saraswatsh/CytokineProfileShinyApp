source("R/libraries.R") # Sourcing packages
source("global.R") # Sourcing built in data set R script and functions
# Source the UI and server code
source("ui.R")
source("server.R")
shiny::shinyApp(ui = ui, server = server)
