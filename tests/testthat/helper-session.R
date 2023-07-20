selenider_test_session <- function(x, .env = rlang::caller_env()) {
  session <- Sys.getenv("SELENIDER_SESSION", "selenium")
  browser <- Sys.getenv("SELENIDER_BROWSER", "chrome")

  if (session == "chromote") {
    chromote::set_chrome_args(c(
      # https://peter.sh/experiments/chromium-command-line-switches/#disable-crash-reporter
      "--disable-crash-reporter",
      chromote::default_chrome_args()
    ))
  }

  result <- selenider_session(session, browser = browser, .env = .env)

  if (session == "chromote") {
    withr::defer({
      # Delete the Crashpad folder if it exists
      unlink(file.path(tempdir(), "Crashpad"), recursive = TRUE)
    }, envir = .env)
  }

  result
}
