mod_step2_preprocess_ui <- function(
  id = NULL,
  ns = NULL,
  user_state,
  get_step2_typed_data,
  get_step2_typed_col_info,
  theme_choice = "auto"
) {
  ns <- app_resolve_ns(id = id, ns = ns)
  get_step2_typed_data()
  col_info <- get_step2_typed_col_info()
  numeric_cols <- col_info$numerical
  categorical_cols <- col_info$categorical
  selected_cat <- step2_restore_bucket_selection(
    user_state$selected_columns,
    categorical_cols
  )
  selected_num <- step2_restore_bucket_selection(
    user_state$selected_columns,
    numeric_cols
  )

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 5,
        bslib::card(
          bslib::card_header(
            class = "bg-info",
            "1. Select Categorical Columns"
          ),
          bslib::card_body(
            shiny::div(
              style = "margin-bottom: 10px;",
              shiny::actionButton(
                ns("select_all_cat"),
                "Select All",
                class = "btn-sm"
              ),
              shiny::actionButton(
                ns("deselect_all_cat"),
                "Deselect All",
                class = "btn-sm"
              )
            ),
            shiny::div(
              class = "scrollable-checkbox-group",
              shiny::checkboxGroupInput(
                inputId = ns("selected_categorical_cols"),
                label = NULL,
                choices = categorical_cols,
                selected = selected_cat,
                inline = TRUE
              )
            )
          )
        ),
        bslib::card(
          bslib::card_header(
            class = "bg-info",
            "2. Select Numerical Columns"
          ),
          bslib::card_body(
            shiny::div(
              style = "margin-bottom: 6px;",
              shiny::actionButton(
                ns("select_all_num"),
                "Select All",
                class = "btn-sm"
              ),
              shiny::actionButton(
                ns("deselect_all_num"),
                "Deselect All",
                class = "btn-sm"
              )
            ),
            shiny::div(
              class = "scrollable-checkbox-group",
              shiny::checkboxGroupInput(
                inputId = ns("selected_numerical_cols"),
                label = NULL,
                choices = numeric_cols,
                selected = selected_num,
                inline = TRUE
              )
            )
          )
        ),
        bslib::card(
          class = "mb-3",
          bslib::card_header(
            class = "bg-info",
            "2a. Override Column Types"
          ),
          bslib::card_body(
            shiny::uiOutput(ns("step2_type_override_ui"))
          )
        ),
        bslib::card(
          bslib::card_header(
            class = "bg-info",
            "3. Optional: Data Transformation"
          ),
          bslib::card_body(
            style = "overflow: visible; min-height: 8rem;",
            shiny::selectInput(
              ns("step2_scale"),
              label = shinyhelper::helper(
                type = "inline",
                title = "Data Transformation",
                icon = "fas fa-question-circle",
                shiny_tag = shiny::HTML(
                  "<span style='margin-right: 15px;'>Apply a preprocessing method to selected numerical columns</span>"
                ),
                content = "Apply one preprocessing method to the selected numerical columns before imputation and downstream analysis. Log transforms require all non-missing selected values to be finite and greater than zero.",
                colour = if (theme_choice %in% c("darkly", "cyborg")) {
                  "red"
                } else {
                  "blue"
                }
              ),
              choices = c(
                "None" = "none",
                "log2" = "log2",
                "log10" = "log10",
                "Z-score" = "zscore"
              ),
              selected = shiny::isolate(user_state$step2_scale) %||% "none",
              selectize = FALSE,
              width = "100%"
            )
          )
        ),
        shiny::uiOutput(ns("conditional_filter_ui"))
      ),
      shiny::column(
        width = 7,
        bslib::card(
          style = "height: 40vh; display: flex; flex-direction: column;",
          bslib::card_header(shiny::h4(
            shiny::icon("table"),
            "Filtered Data Explorer"
          )),
          bslib::card_body(
            style = "flex: 1 1 auto; overflow-y: auto; padding: 1rem;",
            shiny::div(
              style = "max-height: 40vh; overflow-y: auto; padding: 1rem;",
              DT::DTOutput(ns("filtered_data_preview"))
            )
          ),
          bslib::card_footer(
            style = paste(
              "display: flex;",
              "justify-content: center;",
              "padding: 0.75rem;",
              "border-top: 1px solid #444;",
              "background: inherit;"
            ),
            shiny::actionButton(
              ns("delete_selected_rows"),
              "Delete Selected",
              icon = shiny::icon("trash"),
              class = "btn-danger me-2"
            ),
            shiny::actionButton(
              ns("expand_filtered"),
              "Enlarge Window",
              icon = shiny::icon("expand"),
              class = "btn-secondary"
            )
          )
        ),
        bslib::card(
          style = "display: flex; flex-direction: column;",
          bslib::card_header(shiny::h4(
            shiny::icon("table"),
            "Deleted Samples"
          )),
          bslib::card_body(
            style = "flex: 1 1 auto; overflow-y: auto; padding: 1rem;",
            shiny::div(
              style = "max-height: 40vh; overflow-y: auto; padding: 1rem;",
              DT::DTOutput(ns("deleted_data_preview"))
            )
          ),
          bslib::card_footer(
            style = paste(
              "display: flex;",
              "justify-content: center;",
              "padding: 0.75rem;",
              "border-top: 1px solid #444;",
              "background: inherit;"
            ),
            shiny::actionButton(
              ns("restore_selected_rows"),
              "Restore Selected",
              icon = shiny::icon("undo"),
              class = "btn-secondary me-2"
            ),
            shiny::actionButton(
              ns("restore_all_rows"),
              "Restore All",
              icon = shiny::icon("undo"),
              class = "btn-secondary me-2"
            ),
            shiny::actionButton(
              ns("expand_deleted"),
              "Enlarge Window",
              icon = shiny::icon("expand"),
              class = "btn-secondary"
            )
          )
        )
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          style = "margin-top:0.5rem;",
          shiny::actionButton(
            ns("back2"),
            "Back",
            icon = shiny::icon("arrow-left")
          ),
          shiny::conditionalPanel(
            "input.step2_scale && input.step2_scale !== 'none'",
            ns = ns,
            shiny::actionButton(
              ns("preview_transform"),
              "Preview Transformation",
              icon = shiny::icon("magnifying-glass"),
              class = "btn-secondary"
            )
          ),
          shiny::actionButton(
            ns("open_impute_modal"),
            "Treat missing values",
            icon = shiny::icon("fas fa-eraser"),
            class = "btn-secondary"
          ),
          shiny::actionButton(
            ns("next2"),
            "Next",
            icon = shiny::icon("arrow-right"),
            class = "btn-primary"
          )
        )
      )
    )
  )
}
