# Runs tests using a variety of backends
test_selenider <- function(x) {
  # Avoid explicit dependency on devtools
  cli::cli_alert_info("Running tests using Selenium and Chrome")
  withr::with_envvar(c(
    "SELENIDER_SESSION" = "selenium",
    "SELENIDER_BROWSER" = "chrome"
  ), rlang::ns_env("devtools")$test())


  cli::cli_alert_info("Running tests using Selenium and Firefox")
  withr::with_envvar(c(
    "SELENIDER_SESSION" = "selenium",
    "SELENIDER_BROWSER" = "firefox"
  ), rlang::ns_env("devtools")$test())

  cli::cli_alert_info("Running tests using Chromote")
  withr::with_envvar(c(
    "SELENIDER_SESSION" = "chromote",
    "SELENIDER_BROWSER" = "chrome"
  ), rlang::ns_env("devtools")$test())
}
