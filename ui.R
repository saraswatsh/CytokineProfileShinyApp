cfg_file <- system.file("config.yml", package = "CytokineProfileShinyApp")
if (!nzchar(cfg_file)) {
  cfg_file <- "config.yml"
} # fallback when running from source
cfg <- config::get(file = cfg_file)
announcement_banner <- function() {
  if (!is.null(cfg$announcement)) {
    shiny::tags$div(
      class = "alert alert-info",
      style = "margin: 10px;",
      shiny::HTML(cfg$announcement)
    )
  }
}
ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  announcement_banner(), # shows below if there's an announcement
  shiny::tags$head(
    shiny::tags$title("CytokineProfile"),
    # 2) Point to favicon in www/
    shiny::tags$link(
      rel = "icon",
      type = "image/png",
      href = "logo.png"
    )
  ),
  # 1) Apply theme
  theme = bslib::bs_theme(),

  # 2) Global CSS to tighten up spacing
  shiny::tags$head(
    shiny::tags$style(shiny::HTML(
      "
  // On page load, tell Shiny about a previously saved theme
  $(document).on('shiny:connected', function() {
    var saved = localStorage.getItem('user_theme');
    if (saved) {
      var el = document.getElementById('theme_choice');
      if (el) {
        el.value = saved;
        $(el).trigger('change');  // ensures Shiny input binding sees it
      }
    }
  });
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
    .scrollable-checkbox-group {
      max-height: 30vh; /* cap the height */
      overflow-y: auto; /* vertical scroll if needed */
      padding:     0.5rem; /* breathing room */
    }

    /* ─────────────────────────────────────────────
       Inline checkboxes for scrollable checkboxes
    ───────────────────────────────────────────── */
    .scrollable-checkbox-group .checkbox-inline,
    .scrollable-checkbox-group .form-check-inline {
      display:        inline-block !important; margin-right:   1.5rem     !important;  /* gutter */
      vertical-align: middle     !important; white-space:    nowrap      !important; /* no wrapping between box+label */
      padding:        0.25rem 0  !important; /* tiny top/bottom padding */
    }
    /* Brand row: title + version + logo */
    #brand { display:flex; align-items:center; gap:.55rem; }
    #brand .brand-title { margin:0; line-height:1; }
    
    /* Header divider (slightly stronger in dark) */
    .app-header { border-bottom: 1px solid var(--bs-border-color) !important; }

    /* Shared pill layout */
    #brand .app-version {
      font-size: .78rem;
      line-height: 1;
      padding: .08rem .45rem;
      border-radius: .5rem;
      position: relative;
      top: 1em;              /* subscript nudge; scale with text */
      font-weight: 500;
      letter-spacing: .01em;
    }

    /* Light (flatly): dark text on light pill */
    [data-bs-theme='flatly'] #brand .app-version {
      background: rgba(0,0,0,.04);
      border: 1px solid rgba(0,0,0,.18);
      color: #212529;          /* standard body text in light */
    }

    /* Dark (darkly): light text on darker pill */
    [data-bs-theme='darkly'] #brand .app-version {
      background: rgba(255,255,255,.14);
      border: 1px solid rgba(255,255,255,.28);
      color: #fff;
    }

    #brand .brand-logo {
      height:60px; object-fit:cover; border-radius:.35rem;
    }
    @media (max-width: 576px) {
      #brand .app-version { display:none; } /* hide on very small screens */
    }
     /* Make large modals almost full width; keep a tall body with scrolling */
    #shiny-modal .modal-dialog { max-width: 98vw; width: 98vw; }   /* more width */
    #shiny-modal .modal-body   { max-height: 70vh; overflow-y: auto; }  /* less length (scrolls) */
    /* Highlight full selected column */
  table.dataTable tbody td.col-selected,
  .dataTables_scrollHeadInner th.col-selected,
  .dataTables_scrollFootInner th.col-selected { 
    background-color: rgba(0,123,255,.18) !important;
  }
  tfoot th { cursor: pointer; }
  "
    )),
    shiny::tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    )
  ),
  shiny::tags$script(shiny::HTML(
    "
  Shiny.addCustomMessageHandler('toggle-popover', function(id) {
    var el = document.getElementById(id);
    if (el) el.click();
  });
"
  )),

  # 3) THE PERSISTENT HEADER
  shiny::div(
    class = "app-header d-flex align-items-center",
    # left: title + description
    shiny::div(
      id = "brand",
      shiny::tags$h1(
        "CytokineProfile",
        class = "brand-title",
        style = "margin:0;"
      ),
      shiny::tags$span(class = "app-version", paste0("v0.0.0.9000")),
      shiny::tags$img(
        src = "logo.png",
        class = "brand-logo",
        alt = "CytokineProfile logo"
      )
    ),
    # ── Spacer to push the next flex item to the right
    shiny::div(class = "flex-grow-1"),
    # ── Right: GitHub / Website + Theme selector
    shiny::div(
      class = "d-flex align-items-center",
      shiny::tags$a(
        shiny::icon("github"),
        "GitHub Repository",
        href = "https://github.com/saraswatsh/CytokineProfileShinyApp",
        target = "_blank",
        class = "btn btn-light btn-sm me-2"
      ),
      shiny::tags$a(
        shiny::icon("globe"),
        "Project Website",
        href = "https://shinyinfo.cytokineprofile.org",
        target = "_blank",
        class = "btn btn-light btn-sm me-4"
      ),
      shiny::div(
        shiny::selectInput(
          "theme_choice",
          NULL,
          choices = c("Auto" = "auto", "Light" = "flatly", "Dark" = "darkly"),
          selected = "Light",
          width = "100px"
        ),
        style = "margin-right: 1rem; margin-top: 0.7rem;"
      )
    )
  ),

  # 4) SIDEBAR + MAIN PANEL
  shiny::sidebarLayout(
    # –––––– Sidebar ––––––
    shiny::sidebarPanel(
      class = "sidebar",
      width = 2,
      # small logo above the nav
      shiny::tags$img(src = "logo.png", class = "sidebar-logo"),

      # nav buttons
      shiny::actionButton(
        "nav_home",
        "Home",
        icon = shiny::icon("home"),
        width = "100%",
        value = "home"
      ),
      shiny::actionButton(
        "nav_tutorials",
        "Tutorials",
        icon = shiny::icon("book"),
        width = "100%",
        value = "tutorials"
      ),
      shiny::actionButton(
        "nav_start",
        "Start Analysis",
        icon = shiny::icon("play-circle"),
        width = "100%",
        value = "step1"
      ),
      shinyjs::hidden(
        shiny::div(
          id = "nav_submenu",
          style = "margin-left: 10px; margin-top: 5px;",
          shiny::actionButton(
            "nav_upload",
            "1. Upload Data",
            class = "submenu"
          ),
          shiny::actionButton(
            "nav_filter",
            "2. Select Cytokines & Apply Filters",
            class = "submenu"
          ),
          shiny::actionButton(
            "nav_options",
            "3. Analysis Options",
            class = "submenu"
          ),
          shiny::actionButton(
            "nav_args",
            "4. Analysis Arguments",
            class = "submenu"
          ),
          shiny::actionButton("nav_results", "5. Results", class = "submenu")
        )
      ),
      shiny::actionButton(
        "nav_news",
        "News & Updates",
        icon = shiny::icon("bullhorn"),
        width = "100%",
        value = "news"
      ),
      shiny::actionButton(
        "nav_contact",
        "Contact",
        icon = shiny::icon("envelope"),
        width = "100%",
        value = "contact"
      ),
      style = "
    flex: 0 0 250px; /* fixed 250px wide, no grow, no shrink */
    overflow-y: auto; /* if sidebar ever overflows, scroll */
    padding: 1rem;",
    ),

    # –––––– Main content ––––––
    shiny::mainPanel(
      class = "main-panel",
      style = "
    flex: 1 1 auto;         /* take all remaining width */
    display: flex;
    flex-direction: column;
    overflow: hidden;       /* prevent it from pushing footer off screen */
    ",
      # 1) the step header (fixed height)
      shiny::uiOutput("stepHeader"),

      # 2) the progress bar (fixed height)
      shiny::div(
        style = "padding:0.5rem 1rem;",
        shiny::uiOutput("progressBar")
      ),

      # 3) the *scrollable* content area
      shiny::div(
        style = "
      flex: 1;
      overflow-y: auto;
      padding: 1rem;
    ",
        shiny::uiOutput("page_content")
      )
    )
  )
)
