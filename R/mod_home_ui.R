mod_home_ui <- function(id = NULL, ns = NULL) {
  ns <- app_resolve_ns(id = id, ns = ns)

  shiny::tagList(
    shiny::h1("Welcome to CytokineProfile", style = "font-weight:300;"),
    shiny::p(shiny::HTML(paste0(
      "CytokineProfile is an R Shiny Application based on the CytoProfile R package available at ",
      "<a href='https://cran.r-project.org/package=CytoProfile'>CRAN</a>. ",
      "This application is designed for advanced cytokine data analysis. ",
      "It provides a comprehensive suite of functions for exploratory, univariate, ",
      "and multivariate analysis as well as machine learning methods tailored to your data."
    ))),
    shiny::tags$h3("Features we offer:", style = "margin-top:2rem;"),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::div(
          class = "card h-100",
          shiny::div(
            class = "card-header bg-primary text-white",
            "Univariate Analysis"
          ),
          shiny::div(
            class = "card-body",
            shiny::tags$ul(
              shiny::tags$li("Univariate Tests (T-test, Wilcoxon)"),
              shiny::tags$li(
                "Multi-level Univariate Tests (Anova, Kruskal-Wallis)"
              ),
              shiny::tags$li("Two-way ANOVA"),
              shiny::tags$li("ANCOVA")
            )
          )
        )
      ),
      shiny::column(
        width = 3,
        shiny::div(
          class = "card h-100",
          shiny::div(
            class = "card-header bg-primary text-white",
            "Exploratory Analysis"
          ),
          shiny::div(
            class = "card-body",
            shiny::tags$ul(
              shiny::tags$li("Boxplots"),
              shiny::tags$li("Violin Plots"),
              shiny::tags$li("Correlation Plots"),
              shiny::tags$li("Error-Bar Plot"),
              shiny::tags$li("Dual-Flashlight Plot"),
              shiny::tags$li("Heatmap"),
              shiny::tags$li("Skewness/Kurtosis Plots"),
              shiny::tags$li("Volcano Plot")
            )
          )
        )
      ),
      shiny::column(
        width = 3,
        shiny::div(
          class = "card h-100",
          shiny::div(
            class = "card-header bg-primary text-white",
            "Multivariate Analysis"
          ),
          shiny::div(
            class = "card-body",
            shiny::tags$ul(
              shiny::tags$li("Principal Component Analysis (PCA)"),
              shiny::tags$li("Partial Least Squares Regression (PLSR)"),
              shiny::tags$li(
                "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)"
              ),
              shiny::tags$li(
                "Multivariate INTegrative Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)"
              )
            )
          )
        )
      ),
      shiny::column(
        width = 3,
        shiny::div(
          class = "card h-100",
          shiny::div(
            class = "card-header bg-primary text-white",
            "Machine Learning Methods"
          ),
          shiny::div(
            class = "card-body",
            shiny::tags$ul(
              shiny::tags$li("Random Forest"),
              shiny::tags$li("Extreme Gradient Boosting (XGBoost)")
            )
          )
        )
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        align = "center",
        shiny::actionButton(
          ns("nav_start_home"),
          "Let's get started!",
          icon = shiny::icon("arrow-right"),
          class = "btn-primary btn-lg"
        )
      )
    )
  )
}
