# Loading libraries
library(shiny)
library(shinyjs)
library(bslib)
library(jsonlite)

## Define UI for application (wizard style)
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(),
  tags$style(HTML("
       .dark-theme-fix .selectize-dropdown .option:hover,
       .dark-theme-fix .selectize-dropdown .option.active {
         background-color: #0066cc !important;
         color: #ffffff !important;
       }
  ")),
  # Add a style block to override text and background in selectize controls:
  tags$style(HTML("
    .dark-theme-fix .selectize-control {
    background-color: #2f2f2f !important;
    color: #FFF !important;
    }

    .dark-theme-fix .selectize-input,
    .dark-theme-fix .selectize-control.single .selectize-input,
    .dark-theme-fix .selectize-control.multi .selectize-input > div,
    .dark-theme-fix .selectize-dropdown,
    .dark-theme-fix .selectize-dropdown .option {
      color: #FFF !important; 
      background-color: #2f2f2f !important;
      border-color: #444 !important;
    }

    .dark-theme-fix .selectize-control.multi .selectize-input > div .item {
      color: #FFF !important;
      background-color: #444 !important;
      border: none !important;
    }
  ")),
  titlePanel("CytoProfile Shiny App"),
  
  # Always-visible theme toggle in an absolute panel
  absolutePanel(top = 10, right = 10, width = 200,
                radioButtons("theme_mode", "Choose Theme:",
                             choices = c("Light", "Dark"), 
                             selected = "Dark")
  ),
  
  # Only show the header links on the first page (wizard step 1)
  conditionalPanel(
    condition = "output.currentStep == 1",
    div(
      style = "text-align: center; margin-bottom: 20px;",
      tags$a(href = "https://github.com/saraswatsh/CytoProfileShinyApp", "GitHub Repository", target = "_blank"),
      " | ",
      tags$a(href = "https://saraswatsh.github.io/CytoProfileShinyApp/", "Project Website", target = "_blank")
    )
  ),
  
  # Wizard UI and result display
  uiOutput("wizardUI"),
  uiOutput("result_display"),
  
  br(),
  # Download button (visible when output_mode is Download)
  conditionalPanel(
    condition = "input.output_mode == 'Download'",
    downloadButton("download_output", "Download PDF")
  )
)