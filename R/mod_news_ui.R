mod_news_ui <- function(id = NULL, ns = NULL) {
  app_resolve_ns(id = id, ns = ns)

  shiny::tagList(
    shiny::includeMarkdown("NEWS.md")
  )
}
