selenider_test_session <- function(x, .env = rlang::caller_env()) {
  session <- Sys.getenv("SELENIDER_SESSION", "chromote")
  browser <- Sys.getenv("SELENIDER_BROWSER", "chrome")
  ip <- Sys.getenv("SELENIDER_IP")
  port <- as.integer(Sys.getenv("SELENIDER_PORT", "4567"))

  if (session == "chromote") {
    chromote::set_chrome_args(c(
      # https://peter.sh/experiments/chromium-command-line-switches/#disable-crash-reporter
      "--disable-crash-reporter",
      "--no-sandbox",
      chromote::default_chrome_args()
    ))

    result <- selenider_session(session, browser = browser, .env = .env)

    withr::defer({
      # Delete the Crashpad folder if it exists
      unlink(file.path(tempdir(), "Crashpad"), recursive = TRUE)
    }, envir = .env)
  } else if (ip != "") {
    client <- create_selenium_client(browser, port = port, remoteServerAddr = ip)
    result <- selenider_session(driver = client)
  } else {
    result <- selenider_session(session, browser = browser, .env = .env)
  }

  result
}
