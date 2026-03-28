mod_step3_analysis_select_ui <- function(id = NULL, ns = NULL) {
  ns <- app_resolve_ns(id = id, ns = ns)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        bslib::card(
          bslib::card_header("Univariate Analysis", class = "bg-info"),
          bslib::card_body(
            shiny::actionButton(
              ns("menu_univariate_2lvl"),
              "Univariate Tests (T-test, Wilcoxon)",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_univariate_multi"),
              "Multi-level Univariate Tests (Anova, Kruskal-Wallis)",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_two_way_anova"),
              "Two-way ANOVA",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_ancova"),
              "ANCOVA",
              class = "menu-card"
            )
          )
        )
      ),
      shiny::column(
        width = 3,
        bslib::card(
          bslib::card_header("Exploratory Analysis", class = "bg-info"),
          bslib::card_body(
            shiny::actionButton(
              ns("menu_boxplots"),
              "Boxplots",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_violin"),
              "Violin Plots",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_correlation"),
              "Correlation Plots",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_skewkurt"),
              "Skewness/Kurtosis",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_errorbp"),
              "Error-Bar Plot",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_dualflash"),
              "Dual-Flashlight Plot",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_heatmap"),
              "Heatmap",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_volcano"),
              "Volcano Plot",
              class = "menu-card"
            )
          )
        )
      ),
      shiny::column(
        width = 3,
        bslib::card(
          bslib::card_header("Multivariate Analysis", class = "bg-info"),
          bslib::card_body(
            shiny::actionButton(
              ns("menu_PCA"),
              "Principal Component Analysis (PCA)",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_PLSR"),
              "Partial Least Squares Regression (PLSR)",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_splsda"),
              "Sparse Partial Least Squares - Discriminant Analysis (sPLS-DA)",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_mint_splsda"),
              "Multivariate INTegration Sparse Partial Least Squares - Discriminant Analysis (MINT sPLS-DA)",
              class = "menu-card"
            )
          )
        )
      ),
      shiny::column(
        width = 3,
        bslib::card(
          bslib::card_header("Machine Learning Methods", class = "bg-info"),
          bslib::card_body(
            shiny::actionButton(
              ns("menu_rf"),
              "Random Forest",
              class = "menu-card"
            ),
            shiny::actionButton(
              ns("menu_xgb"),
              "Extreme Gradient Boosting (XGBoost)",
              class = "menu-card"
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::div(
            style = "margin-top: 1rem; display: flex; justify-content: flex-start;",
            shiny::actionButton(
              ns("back3"),
              "Back",
              icon = shiny::icon("arrow-left"),
              class = "btn-secondary"
            )
          )
        )
      )
    )
  )
}
