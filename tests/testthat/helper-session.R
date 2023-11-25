selenider_test_session <- function(x, .env = rlang::caller_env()) {
  session <- Sys.getenv("SELENIDER_SESSION", "chromote")
  browser <- Sys.getenv("SELENIDER_BROWSER", "chrome")
  docker <- as.logical(Sys.getenv("SELENIDER_DOCKER", "FALSE"))
  port <- as.integer(Sys.getenv("SELENIDER_PORT", "4444"))

  skip_if_selenider_unavailable(session)

  if (session == "chromote") {
    view <- as.logical(Sys.getenv("SELENIDER_VIEW", "FALSE"))

    chromote::set_chrome_args(c(
      # https://peter.sh/experiments/chromium-command-line-switches/#disable-crash-reporter
      "--disable-crash-reporter",
      "--no-sandbox",
      chromote::default_chrome_args()
    ))

    result <- selenider_session(session, browser = browser, view = view, .env = .env)

    withr::defer(
      {
        # Delete the Crashpad folder if it exists
        unlink(file.path(tempdir(), "Crashpad"), recursive = TRUE)
      },
      envir = .env
    )
  } else if (docker && session == "selenium") {
    client <- create_selenium_client(browser, port = port)

    result <- selenider_session(driver = client, .env = .env)
  } else if (session == "selenium") {
    result <- selenider_session(session, browser = browser, .env = .env)
  } else if (docker && session == "rselenium") {
    client <- create_rselenium_client(browser, port = port)

    result <- selenider_session(driver = list(client = client), .env = .env)
  } else {
    result <- selenider_session(
      "rselenium",
      browser = browser,
      selenium_manager = FALSE,
      .env = .env
    )
  }

  result
}
