mod_contact_ui <- function(id = NULL, ns = NULL) {
  app_resolve_ns(id = id, ns = ns)

  shiny::fluidPage(
    shiny::h1("About Us"),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::h2("Shubh Saraswat"),
        shiny::p(shiny::em(
          "Maintainer, Co-Creator, and Author of CytokineProfile"
        )),
        shiny::p("Biomedical Data Scientist"),
        shiny::p("PhD Student in Epidemiology & Biostatistics"),
        shiny::p("University of Kentucky"),
        shiny::tags$a(
          href = "mailto:shubh.saraswat@uky.edu",
          class = "btn btn-primary me-2",
          shiny::icon("envelope"),
          "Email"
        ),
        shiny::tags$a(
          href = "https://www.linkedin.com/in/ssaraswat22",
          class = "btn btn-info me-2",
          shiny::icon("linkedin"),
          "LinkedIn"
        ),
        shiny::tags$a(
          href = "https://github.com/saraswatsh",
          class = "btn btn-dark me-2",
          shiny::icon("github"),
          "GitHub"
        ),
        shiny::tags$a(
          href = "https://orcid.org/0009-0009-2359-1484",
          class = "btn btn-link",
          shiny::icon("orcid"),
          "ORCID"
        )
      ),
      shiny::column(
        width = 6,
        shiny::h2("Xiaohua Douglas Zhang"),
        shiny::p(shiny::em("Co-Creator and Author of CytokineProfile")),
        shiny::p("Professor, Department of Biostatistics"),
        shiny::p("University of Kentucky"),
        shiny::tags$a(
          href = "mailto:xiaohua.zhang@uky.edu",
          class = "btn btn-primary me-2",
          shiny::icon("envelope"),
          "Email"
        ),
        shiny::tags$a(
          href = "https://orcid.org/0000-0002-2486-7931",
          class = "btn btn-link",
          shiny::icon("orcid"),
          "ORCID"
        )
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::h2("Bira Arumndari Nurrahma"),
        shiny::p(shiny::em("Author of CytokineProfile")),
        shiny::p("PhD Student in Nutritional Sciences"),
        shiny::p("University of Kentucky"),
        shiny::tags$a(
          href = "mailto:biraarum@uky.edu",
          class = "btn btn-primary me-2",
          shiny::icon("envelope"),
          "Email"
        )
      )
    ),
    shiny::column(
      width = 12,
      shiny::br(),
      shiny::p(
        "For issues related to the application, submit an issue at the ",
        shiny::tags$a(
          href = "https://github.com/saraswatsh/CytokineProfileShinyApp/issues",
          "GitHub repository."
        ),
        "For additional questions or concerns, contact the maintainer Shubh Saraswat with the provided email above."
      )
    )
  )
}
