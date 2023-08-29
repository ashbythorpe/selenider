# https://github.com/rstudio/shinytest2/blob/main/tests/testthat/setup-disable-crashpad.R

chromote::set_chrome_args(c(
  # https://peter.sh/experiments/chromium-command-line-switches/#disable-crash-reporter
  #> Disable crash reporter for headless. It is enabled by default in official builds
  "--disable-crash-reporter",
  chromote::default_chrome_args()
))

withr::defer(
  {
    # Close the browser
    if (chromote::has_default_chromote_object()) {
      try(chromote::default_chromote_object()$get_browser()$close())
    }

    # Clean up chromote sessions
    gc()
    Sys.sleep(2) # Wait for any supervisors to exit

    # Delete the Crashpad folder if it exists
    unlink(file.path(tempdir(), "Crashpad"), recursive = TRUE)
  },
  envir = testthat::teardown_env()
)
