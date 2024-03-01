withr::local_options(
  list(
    warnPartialMatchDollar = TRUE,
    warnPartialMatchArgs = TRUE,
    warnPartialMatchAttr = TRUE
  ),
  .local_envir = teardown_env()
)