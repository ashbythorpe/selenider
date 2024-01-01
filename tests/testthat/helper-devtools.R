selenider_test_manual <- function() {
  cli::cli_alert_info("Running manual tests")
  withr::with_envvar(c(
    "NOT_CRAN" = "true"
  ), testthat::test_dir("tests/manual"))
}

selenider_test_selenium_chrome <- function() {
  cli::cli_alert_info("Running tests using Selenium and Chrome")
  withr::with_envvar(c(
    "SELENIDER_SESSION" = "selenium",
    "SELENIDER_BROWSER" = "chrome"
  ), rlang::ns_env("devtools")$test())
}

selenider_test_selenium_firefox <- function() {
  cli::cli_alert_info("Running tests using Selenium and Firefox")
  withr::with_envvar(c(
    "SELENIDER_SESSION" = "selenium",
    "SELENIDER_BROWSER" = "firefox"
  ), rlang::ns_env("devtools")$test())
}

selenider_test_chromote <- function() {
  cli::cli_alert_info("Running tests using Chromote")
  withr::with_envvar(c(
    "SELENIDER_SESSION" = "chromote",
    "SELENIDER_BROWSER" = "chrome"
  ), rlang::ns_env("devtools")$test())
}


selenider_test_chromote_non_headless <- function() {
  cli::cli_alert_info("Running tests using Chromote session, displayed in the browser")
  withr::with_envvar(c(
    "SELENIDER_SESSION" = "chromote",
    "SELENIDER_BROWSER" = "chrome",
    "SELENIDER_HEADLESS" = "FALSE"
  ), rlang::ns_env("devtools")$test())
}

selenider_test_rselenium_chrome <- function(docker = FALSE) {
  cli::cli_alert_info("Running tests using Selenium and Chrome")
  withr::with_envvar(c(
    "SELENIDER_SESSION" = "rselenium",
    "SELENIDER_BROWSER" = "chrome",
    "SELENIDER_DOCKER" = as.character(docker)
  ), rlang::ns_env("devtools")$test())
}

selenider_test_rselenium_firefox <- function(docker = FALSE) {
  cli::cli_alert_info("Running tests using Selenium and Firefox")
  withr::with_envvar(c(
    "SELENIDER_SESSION" = "rselenium",
    "SELENIDER_BROWSER" = "firefox",
    "SELENIDER_DOCKER" = as.character(docker)
  ), rlang::ns_env("devtools")$test())
}
