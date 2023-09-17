# Runs tests using a variety of backends:
# Requires RSelenium, chromote, Chrome and Firefox to be installed
test_selenider <- function(x,
                           selenium_chrome = TRUE,
                           selenium_firefox = TRUE,
                           chromote = TRUE,
                           chromote_view = TRUE,
                           manual = TRUE) {
  if (selenium_chrome) {
    # Avoid explicit dependency on devtools
    cli::cli_alert_info("Running tests using Selenium and Chrome")
    withr::with_envvar(c(
      "SELENIDER_SESSION" = "selenium",
      "SELENIDER_BROWSER" = "chrome"
    ), rlang::ns_env("devtools")$test())
  }

  if (selenium_firefox) {
    cli::cli_alert_info("Running tests using Selenium and Firefox")
    withr::with_envvar(c(
      "SELENIDER_SESSION" = "selenium",
      "SELENIDER_BROWSER" = "firefox"
    ), rlang::ns_env("devtools")$test())
  }

  if (chromote) {
    cli::cli_alert_info("Running tests using Chromote")
    withr::with_envvar(c(
      "SELENIDER_SESSION" = "chromote",
      "SELENIDER_BROWSER" = "chrome"
    ), rlang::ns_env("devtools")$test())
  }

  if (chromote_view) {
    cli::cli_alert_info("Running tests using Chromote session, displayed in the browser")
    withr::with_envvar(c(
      "SELENIDER_SESSION" = "chromote",
      "SELENIDER_BROWSER" = "chrome",
      "SELENIDER_VIEW" = "TRUE"
    ), rlang::ns_env("devtools")$test())
  }

  if (manual) {
    cli::cli_alert_info("Running manual tests")
    withr::with_envvar(c(
      "NOT_CRAN" = "true"
    ), testthat::test_dir("tests/manual"))
  }
}
