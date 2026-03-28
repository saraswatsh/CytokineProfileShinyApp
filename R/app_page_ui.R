app_page_ui <- function(
  current_page,
  get_current_step,
  get_selected_function,
  user_state,
  get_step2_typed_data,
  get_step2_typed_col_info,
  theme_choice,
  id = NULL,
  ns = NULL
) {
  selected_function <- if (is.function(get_selected_function)) {
    get_selected_function()
  } else {
    get_selected_function
  }

  switch(
    current_page,
    "home" = mod_home_ui(id = id, ns = ns),
    "tutorials" = mod_tutorials_ui(id = id, ns = ns),
    "step1" = mod_step1_data_ui(id = id, ns = ns, user_state = user_state),
    "step2" = mod_step2_preprocess_ui(
      id = id,
      ns = ns,
      user_state = user_state,
      get_step2_typed_data = get_step2_typed_data,
      get_step2_typed_col_info = get_step2_typed_col_info,
      theme_choice = theme_choice %||% "auto"
    ),
    "step3" = mod_step3_analysis_select_ui(id = id, ns = ns),
    "step4" = mod_step4_analysis_args_ui(
      id = id,
      ns = ns,
      selected_function = selected_function
    ),
    "step5" = mod_step5_results_ui(
      id = id,
      ns = ns,
      selected_function = selected_function
    ),
    "news" = mod_news_ui(id = id, ns = ns),
    "contact" = mod_contact_ui(id = id, ns = ns),
    mod_home_ui(id = id, ns = ns)
  )
}
