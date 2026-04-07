mod_tutorials_ui <- function(id = NULL, ns = NULL) {
  app_resolve_ns(id = id, ns = ns)

  shiny::tagList(
    shiny::includeMarkdown("TUTORIALS.md")
  )
}
