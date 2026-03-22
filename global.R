options(
  cytokineprofile.app_source_root = normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
)

if (!exists("%||%", envir = globalenv(), inherits = FALSE)) {
  assign("%||%", getFromNamespace("%||%", "rlang"), envir = globalenv())
}

if (!exists(".data", envir = globalenv(), inherits = FALSE)) {
  assign(".data", getFromNamespace(".data", "rlang"), envir = globalenv())
}

files.sources <- list.files("R", full.names = TRUE, pattern = "\\.R$")
invisible(lapply(files.sources, source, local = globalenv()))
