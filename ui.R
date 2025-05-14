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
  "spacelab",
  "slate",
  "cyborg"
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
      width: 120px;
      height:104px;
      background-size: cover;
      background-position: center;
      clip-path: polygon(
        25% 6.7%,  75% 6.7%, 100% 50%,
        75% 93.3%, 25% 93.3%,   0% 50%
      );
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
      selected = "cyborg"
    )
  ),

  # Only show the header links on the first page (wizard step 1)
  shiny::conditionalPanel(
    condition = "output.currentStep == 1",
    div(
      style = "text-align: center; margin-bottom: 20px;",
      tags$a(
        href = "https://github.com/saraswatsh/CytokineProfileShinyApp",
        "GitHub Repository",
        target = "_blank"
      ),
      " | ",
      tags$a(
        href = "https://saraswatsh.github.io/CytokineProfileShinyApp/",
        "Project Website",
        target = "_blank"
      )
    ),
    div(
      class = "hero-card",
      # 2/3 width text
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
        )
      ),
      # 1/3 width hexagon logo
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
          margin-left: 1rem;
          margin-right: 1rem;
          flex: none;       /* keep it its own size */
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
