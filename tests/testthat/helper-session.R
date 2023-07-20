selenider_test_session <- function(x, .env = rlang::caller_env()) {
  session <- Sys.getenv("SELENIDER_SESSION", "selenium")
  browser <- Sys.getenv("SELENIDER_BROWSER", "chrome")

  if (session == "chromote") {
    chromote::set_chrome_args(c(
      # https://peter.sh/experiments/chromium-command-line-switches/#disable-crash-reporter
      "--disable-crash-reporter",
      chromote::default_chrome_args()
    ))

    result <- selenider_session(session, browser = browser, .env = .env)

    withr::defer({
      # Delete the Crashpad folder if it exists
      unlink(file.path(tempdir(), "Crashpad"), recursive = TRUE)
    }, envir = .env)
  } else if (isTRUE(as.logical(Sys.getenv("CI", "FALSE")))) {
    extra_args <- list(
      port = 4444L,
    )

    result <- selenider_session(session, browser = browser, .env = .env, extra_args = extra_args)
  } else {
    result <- selenider_session(session, browser = browser, .env = .env)
  }

  result
}
