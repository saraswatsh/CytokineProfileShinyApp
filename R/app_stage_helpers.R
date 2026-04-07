app_stage_init <- function(app_ctx, stage_env = parent.frame()) {
  if (!identical(parent.env(stage_env), app_ctx)) {
    parent.env(stage_env) <- app_ctx
  }
  invisible(stage_env)
}

app_stage_commit <- function(
  app_ctx,
  stage_env = parent.frame(),
  exclude = c("input", "output", "session", "app_ctx", "stage_env")
) {
  logic_names <- setdiff(ls(stage_env, all.names = TRUE), exclude)
  for (nm in logic_names) {
    assign(
      nm,
      base::get(nm, envir = stage_env, inherits = FALSE),
      envir = app_ctx
    )
  }
  invisible(app_ctx)
}
