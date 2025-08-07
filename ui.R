# Loading libraries
library(shiny)
library(shinyjs)
library(bslib)
library(jsonlite)
library(fontawesome)
library(shinyWidgets)

ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$title("CytokineProfile"),
    # 2) Point to favicon in www/
    tags$link(
      rel = "icon",
      type = "image/png",
      href = "logo.png"
    )
  ),
  # 1) Apply theme (we’ll wire the selector in the header below)
  theme = bs_theme(bootswatch = "darkly"),

  # 2) Global CSS to tighten up spacing (optional)
  tags$head(
    tags$style(HTML(
      "
    /* ── App header padding & bottom border ───────────────────────── */
    .app-header {
      padding: 1rem;
      border-bottom: 1px solid #444;
    }
    .app-header .btn {
      margin-left: 0.5rem;
    }
    .app-header .btn-sm {
      margin: 0 0.5rem;
    }
    /* 1) Make the sidebar well fixed-position, full height, scroll only vertically */
      .sidebar .well {
        position: fixed;
        top: 0;
        bottom: 0;
        width: 100px; /* adjust to whatever fixed width you like */
        padding-top: 20px; /* keep some breathing room at top */
        padding-bottom: 20px; /* keep some breathing room at bottom */
        overflow-y: auto; /* if it’s too tall, let it scroll vertically */
        overflow-x: hidden; /* never scroll sideways */
      }

      /* 2) Push the main panel over so it doesn’t hide under the sidebar */
      .main-panel {
        margin-left: 10px; /* = sidebar width + some gutter */
      }
      
    /* ── Sidebar logo sizing ───────────────────────────────────────── */
    .sidebar-logo {
      width: 95% !important; height: auto !important;
        display: block;
        margin: 0 auto 20px;
    }

    /* —— Sidebar buttons —— */
    .sidebar .btn {
      display: block; /* full width */
      width: 100% !important;
      margin: 12px 0 !important; /* more breathing room top/bottom */
      padding: 12px 8px !important; /* a bit more click area */
      background-color: #343a40 !important; /* dark gray */
      color: #fff !important;
      border: none !important;
      border-radius: 4px !important; opacity: 0.8 !important;
      transition: background-color 0.15s, opacity 0.15s;
    }
    /* only target the submenu buttons under #nav_submenu */
    #nav_submenu .btn {
      width: 85% !important; /* shrink them to 60% of the sidebar */
      margin: 4px auto !important; /* center them and give a tiny vertical gap */
      padding: 6px 8px !important; /* reduce click‑area slightly to feel “nested” */
      font-size: 0.9rem !important; /* a little smaller text */
    }
    /* target submenu  */
    .sidebar .btn.submenu {
      width: 85% !important; margin: 4px auto !important;
      padding: 6px 8px !important;
      font-size: 0.9rem !important; }
    /* Hover */
    .sidebar .btn:hover {
      background-color: #495057 !important; opacity: 1 !important;
    }

    /* When the button is focused or being clicked */
    .sidebar .btn:focus,
    .sidebar .btn:active {
      background-color: #0d6efd !important; opacity: 1 !important;
    }

    /* Permanent active state */
    .sidebar .btn.active {
      background-color: #0a58ca !important; opacity: 1 !important;
      box-shadow: inset 0 0 8px rgba(0,0,0,0.3) !important; }

    /* ── Progress bar wrapper ──────────────────────────────────────── */
    .progress-wrapper {
      width: auto; margin-top: -0.1rem;
      margin-bottom: 1.5rem;
    }

    /* ── Full‑height page, hide Shiny errors offscreen ─────────────── */
    html, body {
      height: 100%; margin: 0;
    }
    .shiny-output-error {
      visibility: hidden; }

    /* ── Centered text helper ──────────────────────────────────────── */
    .main-panel .text-center {
      text-align: center !important; }

    /* ── Light‑theme tabs (only when theme ≠ darkly) ───────────────── */
    body:not([data-bs-theme='darkly']) .nav-tabs .nav-link {
      background-color: #f8f9fa; color: #333;
      border: 1px solid rgba(0,0,0,0.1);
    }
    body:not([data-bs-theme='darkly']) .nav-tabs .nav-link.active {
      background-color: #375a7f !important; color: #fff !important;
      border-color: #375a7f !important;
    }
    body:not([data-bs-theme='darkly']) .nav-pills .nav-link {
      background-color: #f8f9fa; color: #333;
    }
    body:not([data-bs-theme='darkly']) .nav-pills .nav-link.active {
      background-color: #375a7f !important; color: #fff !important;
    }
    scrollable-checkbox-group {
      max-height: 30vh; /* cap the height */
      overflow-y: auto; /* vertical scroll if needed */
      padding:     0.5rem; /* breathing room */
    }

    /* ─────────────────────────────────────────────
       Inline checkboxes (BS3 + BS5)
    ───────────────────────────────────────────── */
    .scrollable-checkbox-group .checkbox-inline,
    .scrollable-checkbox-group .form-check-inline {
      display:        inline-block !important; margin-right:   1.5rem     !important;  /* gutter */
      vertical-align: middle     !important; white-space:    nowrap      !important; /* no wrapping between box+label */
      padding:        0.25rem 0  !important; /* tiny top/bottom padding */
    }
  "
    )),
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    )
  ),

  # 3) THE PERSISTENT HEADER
  div(
    class = "app-header d-flex align-items-center",
    # left: title + description
    div(
      tagList(
        tags$h1("CytokineProfile", style = "margin:0;"),
      )
    ),
    # middle: GitHub & Website links
    div(
      class = "d-flex align-items-right",
      tags$img(
        src = "logo.png",
        height = "60px",
        style = "margin-left:1rem;"
      )
    ),
    # ── Spacer to push the next flex item to the right
    div(class = "flex-grow-1"),
    # ── Right: GitHub / Website + Theme selector
    div(
      class = "d-flex align-items-center",
      tags$a(
        icon("github"),
        "GitHub Repository",
        href = "https://github.com/saraswatsh/CytokineProfileShinyApp",
        target = "_blank",
        class = "btn btn-light btn-sm me-2"
      ),
      tags$a(
        icon("globe"),
        "Project Website",
        href = "https://shinyinfo.cytokineprofile.org",
        target = "_blank",
        class = "btn btn-light btn-sm me-4"
      ),
      div(
        selectInput(
          "theme_choice",
          NULL,
          choices = c("Light" = "flatly", "Dark" = "darkly"),
          selected = "darkly",
          width = "100px"
        ),
        style = "margin-right: 1rem; margin-top: 0.7rem;"
      )
    )
  ),

  # 4) SIDEBAR + MAIN PANEL
  sidebarLayout(
    # –––––– Sidebar ––––––
    sidebarPanel(
      class = "sidebar",
      width = 2,
      # small logo above the nav
      tags$img(src = "logo.png", class = "sidebar-logo"),

      # nav buttons
      actionButton(
        "nav_home",
        "Home",
        icon = icon("home"),
        width = "100%",
        value = "home"
      ),
      actionButton(
        "nav_tutorials",
        "Tutorials",
        icon = icon("book"),
        width = "100%",
        value = "tutorials"
      ),
      actionButton(
        "nav_start",
        "Start Analysis",
        icon = icon("play-circle"),
        width = "100%",
        value = "step1"
      ),
      hidden(
        div(
          id = "nav_submenu",
          style = "margin-left: 10px; margin-top: 5px;",
          actionButton("nav_upload", "1. Upload Data", class = "submenu"),
          actionButton(
            "nav_filter",
            "2. Select Cytokines & Apply Filters",
            class = "submenu"
          ),
          actionButton("nav_options", "3. Analysis Options", class = "submenu"),
          actionButton("nav_args", "4. Analysis Arguments", class = "submenu"),
          actionButton("nav_results", "5. Results", class = "submenu")
        )
      ),
      actionButton(
        "nav_news",
        "News & Updates",
        icon = icon("bullhorn"),
        width = "100%",
        value = "news"
      ),
      actionButton(
        "nav_contact",
        "Contact",
        icon = icon("envelope"),
        width = "100%",
        value = "contact"
      ),
      style = "
    flex: 0 0 250px; /* fixed 250px wide, no grow, no shrink */
    overflow-y: auto; /* if sidebar ever overflows, scroll */
    padding: 1rem;",
    ),

    # –––––– Main content ––––––
    mainPanel(
      class = "main-panel",
      style = "
    flex: 1 1 auto;         /* take all remaining width */
    display: flex;
    flex-direction: column;
    overflow: hidden;       /* prevent it from pushing footer off screen */
    ",
      # 1) the step header (fixed height)
      uiOutput("stepHeader"),

      # 2) the progress bar (fixed height)
      div(style = "padding:0.5rem 1rem;", uiOutput("progressBar")),

      # 3) the *scrollable* content area
      div(
        style = "
      flex: 1;
      overflow-y: auto;
      padding: 1rem;
    ",
        uiOutput("page_content")
      )
    )
  )
)
