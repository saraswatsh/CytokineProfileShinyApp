mod_step1_data_ui <- function(id = NULL, ns = NULL, user_state) {
  ns <- app_resolve_ns(id = id, ns = ns)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 5,
        bslib::card(
          bslib::card_header(shiny::h4(
            shiny::icon("upload"),
            "Step 1: Provide Your Data"
          )),
          bslib::card_body(
            shiny::tags$h5("Option A: Upload a File"),
            shiny::fileInput(
              ns("datafile"),
              label = NULL,
              accept = c(".csv", ".txt", ".xls", ".xlsx")
            ),
            shiny::helpText(
              "Accepted Formats: '.csv', '.txt', '.xls', '.xlsx'"
            ),
            shiny::uiOutput(ns("sheet_selector")),
            shiny::uiOutput(ns("open_editor_btn")),
            shiny::hr(),
            shiny::tags$h5("Option B: Use Built-in Data"),
            shiny::checkboxInput(
              ns("use_builtin"),
              "Use a built-in dataset?",
              value = shiny::isolate(user_state$use_builtin) %||% FALSE
            ),
            shiny::uiOutput(ns("built_in_selector")),
            shiny::uiOutput(ns("step1_bottom_block"))
          ),
          bslib::card_footer(
            shiny::div(
              style = "text-align: right;",
              shiny::actionButton(
                ns("next1"),
                "Next Step",
                icon = shiny::icon("arrow-right"),
                class = "btn-primary btn-lg"
              )
            )
          )
        )
      ),
      shiny::column(
        width = 7,
        shiny::conditionalPanel(
          condition = "output.data_is_loaded == true",
          ns = ns,
          bslib::navset_card_tab(
            id = ns("step1_data_tabs"),
            bslib::nav_panel(
              "Data Preview",
              shiny::conditionalPanel(
                condition = "input.view_data",
                ns = ns,
                shiny::uiOutput(ns("data_summary")),
                shinycssloaders::withSpinner(
                  shiny::uiOutput(ns("preview_ui")),
                  type = 8
                )
              ),
              shiny::conditionalPanel(
                condition = "!input.view_data",
                ns = ns,
                shiny::p(
                  style = "padding: 1rem;",
                  "Check 'View Data Loaded?' to see a preview of your data here."
                )
              )
            ),
            bslib::nav_panel(
              "Summary Statistics",
              shiny::conditionalPanel(
                condition = "input.show_summary",
                ns = ns,
                shinycssloaders::withSpinner(
                  DT::DTOutput(ns("summary_stats_table")),
                  type = 8
                )
              ),
              shiny::conditionalPanel(
                condition = "!input.show_summary",
                ns = ns,
                shiny::p(
                  style = "padding: 1rem;",
                  "Check 'Show summary statistics' to see a summary here."
                )
              )
            )
          )
        )
      )
    )
  )
}
