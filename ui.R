# Loading libraries
library(shiny)
library(shinyjs)
library(bslib)
library(jsonlite)
library(fontawesome)

## Define UI for application (wizard style)
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(),
  
  tags$head(
    # Load Font Awesome, etc.
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
    ),
    # Then define placeholder CSS for both themes
    tags$style(HTML("
      /* ----------------------------------
         SELECTIZE PLACEHOLDER COLOR
         ---------------------------------- */
      /* Light theme default: black placeholders */
      .selectize-control .selectize-input > input[placeholder] {
        color: #000; 
      }
      
      /* Dark theme override: white placeholders */
      .dark-theme-fix .selectize-control .selectize-input > input[placeholder] {
        color: #fff; 
      }

      /* ----------------------------------
         REGULAR INPUT PLACEHOLDERS
         ---------------------------------- */
      /* Light theme (default) placeholders: black */
      ::-webkit-input-placeholder { color: #000; }
      :-ms-input-placeholder      { color: #000; }
      ::-ms-input-placeholder     { color: #000; }
      ::placeholder               { color: #000; }

      /* Dark theme override: white */
      .dark-theme-fix ::-webkit-input-placeholder { color: #000; }
      .dark-theme-fix :-ms-input-placeholder      { color: #000; }
      .dark-theme-fix ::-ms-input-placeholder     { color: #000; }
      .dark-theme-fix ::placeholder               { color: #000; }
    "))
  ),
  
  # Additional style blocks for other dark-theme overrides
  tags$style(HTML("
    .dark-theme-fix .selectize-dropdown .option:hover,
    .dark-theme-fix .selectize-dropdown .option.active {
      background-color: #0066cc !important;
      color: #ffffff !important;
    }
    ...
  ")),
  
  # Additional styling for selectize controls
  tags$style(HTML("
    .dark-theme-fix .selectize-control {
      background-color: #2f2f2f !important;
      color: #FFF !important;
    }
    ...
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