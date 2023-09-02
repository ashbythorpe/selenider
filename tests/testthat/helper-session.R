selenider_test_session <- function(x, .env = rlang::caller_env()) {
  session <- Sys.getenv("SELENIDER_SESSION", "chromote")
  browser <- Sys.getenv("SELENIDER_BROWSER", "chrome")
  docker <- as.logical(Sys.getenv("SELENIDER_DOCKER", "FALSE"))
  port <- as.integer(Sys.getenv("SELENIDER_PORT", "4567"))
  ip <- Sys.getenv("SELENIDER_IP")

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
  } else if (docker) {
    if (ip == "") {
      rlang::abort("To test selenider using docker, the `SELENIDER_IP` environment variable must be set.")
    }

    client <- rlang::try_fetch(
      create_selenium_client(browser, port = port, remoteServerAddr = ip),
      error = function(e) {
        cli::cli_abort(c(
          "Creating selenium client failed with port {.val {port}} ip {.val {ip}}."
        ), parent = e)
      }
    result <- selenider_session(driver = client, .env = .env)

    Sys.sleep(10)
    ready <- result$driver$client$getStatus()$ready
    if (!isTRUE(ready)) {
      rlang::abort(as.character(result$driver$client$getStatus()))
    }
  } else {
    result <- selenider_session(session, browser = browser, .env = .env)
  }

  result
}
