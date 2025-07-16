# Loading libraries
library(shiny)
library(shinyjs)
library(bslib)
library(jsonlite)
library(fontawesome)
library(shinyWidgets)

# Vector for different themes
available_themes <- c(
  "flatly",
  "darkly"
)
## Define UI for application (wizard style)
ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  theme = bs_theme(),
  tags$head(
    # Load Font Awesome, etc.
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
    )
  ),
  tags$head(
    tags$style(HTML(
      "
      .scrollable-checkbox-group {
        max-height: 400px; /* Adjust height as needed */
        overflow-y: auto;
        border: 1px solid #ddd;
        padding: 10px;
        border-radius: 5px;
      }
    "
    ))
  ),
  tags$head(
    tags$style(HTML(
      "
      .card-header-tabs .nav-link {
        color: var(--bs-body-color) !important;
      }
    "
    ))
  ),
  tags$head(
    tags$style(HTML(
      "
    .card-header-tabs .nav-link {
      color: var(--bs-body-color) !important;
    }
    
    /* ADD THIS CLASS */
    .overflow-visible {
      overflow: visible !important;
    }
    
    .scrollable-checkbox-group {
      max-height: 400px; 
      overflow-y: auto;
      border: 1px solid #ddd;
      padding: 10px;
      border-radius: 5px;
    }
  "
    ))
  ),
  tags$head(
    tags$style(HTML(
      "    
    /* --- General styles --- */
    .overflow-visible { overflow: visible !important; }
    .scrollable-checkbox-group {
      max-height: 400px; 
      overflow-y: auto; 
      border: 1px solid #ddd;
      padding: 10px; 
      border-radius: 5px;
    }

    /* --- Styles for the main navigation tabs --- */
    .nav-tabs .nav-link { color: var(--bs-body-color) !important; }
    .nav-tabs .nav-link.active {
      background-color: #4E5D6C !important;
      color: white !important;
    }
  "
    ))
  ),
  tags$head(
    tags$style(HTML(
      "
     .hero-card {
      display:         flex;
      flex-wrap:       wrap;               /* allow wrapping */
      justify-content: space-between;
      align-items:     center;
      max-width:       960px;              /* limit width */
      margin:          0 auto 1.5rem auto; /* center horizontally */
      padding:         0.5rem;
       background-color: var(--bs-card-bg)   !important;
      color: var(--bs-card-color) !important;
      border: 1px solid var(--bs-card-border-color);
      border-radius:     0.5rem;
    }
    .hero-text {
      flex:       1 1 600px;  /* grow, but at least 600px */
      text-align: left !important;
      margin-right: 1rem;
    }
    .hexagon {
      flex: none;             /* don’t flex/shrink */
      width: 280px;
      height:266px;
      background-size: 80%;
      background-position: center;
      background-repeat: no-repeat;
      clip-path: polygon(25% 0%, 75% 0%, 100% 50%, 75% 100%, 25% 100%, 0% 50%);
      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
    }
    @media (max-width: 768px) {
      .hero-card { flex-direction: column; }
      .hero-text { margin-bottom: 0.5rem; }
    }
    /* push the rest of the app down so it isn’t hidden */
    #main_content {
      margin-top: 1rem;
    }
    /* tighten the space under every step heading */
    .wizard-ui h3 {
      margin-bottom: 0.25rem !important;
    }
    .step-title {
      margin-bottom: -0.1rem !important;
    }
      /* wrapper to size & space the bar */
    .progress-wrapper {
      width: 60ch;               /* ~60 characters wide */
      margin-top: -0.1rem;       /* small gap above bar */
      margin-bottom: 1.5rem;     /* small gap below bar */
    }
    "
    ))
  ),
  # Always-visible theme toggle in an absolute panel
  shiny::absolutePanel(
    top = 10,
    right = 10,
    width = 200,
    selectInput(
      inputId = "theme_choice",
      label = "Theme:",
      choices = setNames(available_themes, toupper(available_themes)),
      selected = "darkly"
    )
  ),

  # Only show the header links on the first page (wizard step 1)
  shiny::conditionalPanel(
    condition = "output.currentStep == 1",

    # The hero-card now elegantly contains the welcome text, links, and logo
    div(
      class = "hero-card",

      # Column for the text and new buttons
      div(
        class = "hero-text",
        h1(
          "Welcome to CytokineProfile",
          style = "font-weight:300; font-size:2.5rem;"
        ),
        tags$p(
          HTML(paste0(
            "CytokineProfile is an R Shiny Application based on the CytoProfile R package available at ",
            "<a href='https://cran.r-project.org/package=CytoProfile'>CRAN</a>. ",
            "This application is designed for advanced cytokine data analysis. ",
            "It provides a comprehensive suite of functions for exploratory, univariate, ",
            "and multivariate analysis as well as machine learning methods tailored to your data."
          ))
        ),

        # --- NEW INTEGRATED LINKS ---
        br(), # Adds a little space
        # --- The new code with high-contrast buttons ---
        tags$a(
          href = "https://github.com/saraswatsh/CytokineProfileShinyApp",
          target = "_blank",
          class = "btn btn-outline-primary", # Changed to -primary for better visibility
          icon("github"),
          " GitHub Repository"
        ),
        tags$a(
          href = "https://shinyinfo.cytokineprofile.org/",
          target = "_blank",
          class = "btn btn-outline-primary", # Changed to -primary for better visibility
          icon("globe"),
          " Project Website"
        )
        # --- END NEW SECTION ---
      ),

      # Column for the hexagon logo (this part remains unchanged)
      div(
        class = "hexagon",
        style = "background-image: url('logo.png');"
      )
    )
  ),
  div(
    shiny::conditionalPanel(
      h1(
        "CytokineProfile",
        style = "font-weight:300; font-size:2.5rem;"
      ),
      class = "sticky-progress",
      style = "
      max-width: 960px;
      margin: 1rem left;
      display: flex;
      align-items: center;
    ",
      condition = "output.currentStep > 1",
      div(
        class = "hexagon",
        style = "
            background-image: url('logo.png');
            flex: none;             /* don’t flex/shrink */
            width: 280px;
            height:266px;
            background-size: 80%;
            background-position: center;
            background-repeat: no-repeat;
            clip-path: polygon(25% 0%, 75% 0%, 100% 50%, 75% 100%, 25% 100%, 0% 50%);
            box-shadow: 0 4px 8px rgba(0,0,0,0.2);     
        "
      )
    )
  ),
  # Wizard UI and result display
  div(id = "main_content", shiny::uiOutput("wizardUI")),

  br(),
  # Download button (visible when output_mode is Download)
  shiny::conditionalPanel(
    condition = "input.output_mode == 'Download'",
    shiny::downloadButton("download_output", "Download PDF")
  )
)
