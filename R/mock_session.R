#' Create a mock session
#'
#' Create a fake session, that can be used for examples and testing.
#'
#' @param browser The name of the browser to run the session in.
#' @param timeout The default time to wait when collecting an element.
#' @param driver A driver object to use instead of creating one manually.
#' @param local Whether to set the session as the local session object, 
#'   using [local_session()].
#' @param quiet Not used.
#' @param .env Passed into [local_session()] function, to define the 
#'   environment in which the session is used. Change this if you want to
#'   create the session inside a function and then use it outside the
#'   function.
#' 
#' @returns A `selenider_session` object.
#'
#' @examples
#' mock_selenider_session()
#'
#' @export
mock_selenider_session <- function(browser = c(
                                     "chrome", "firefox", 
                                     "phantomjs", "internet explorer"
                                   ),
                                   timeout = 4,
                                   driver = NULL,
                                   local = TRUE,
                                   quiet = TRUE,
                                   .env = rlang::caller_env()) {
  browser <- rlang::arg_match0(
    browser[1],
    c(browser, tools::toTitleCase(browser), "PhantomJS")
  )
  
  if (is.null(driver)) {
    driver <- mock_driver(browser)
  }

  session <- new_selenider_session(driver = driver, timeout)
  
  if (local) {
    local_session(session, .local_envir = .env)
  }
  
  session
}
 
mock_driver <- function(browser) {
  client <- list(
    browserName = browser,
    port = 0000L,
    click = function(...) NULL,
    close = function(...) NULL,
    executeScript = function(...) NULL,
    findElement = function(...) mock_element(),
    findElements = function(...) list(mock_element()),
    goBack = function(...) NULL,
    goForward = function(...) NULL,
    mouseMoveToLocation = function(...) NULL,
    navigate = function(...) NULL,
    refresh = function(...) NULL,
    screenshot = function(...) NULL
  )
  
  class(client) <- "mock_client"

  server <- list(
    stop = function(...) NULL
  )
  
  res <- list(
    client = client,
    server = server
  )

  class(res) <- "mock_driver"

  res
}

mock_element <- function() {
  res <- list(
    elementId = "aaaa",
    clickElement = function(...) NULL,
    compareElements = function(...) NULL,
    findChildElement = function(...) mock_element(),
    findChildElements = function(...) list(mock_element()),
    getElementAttribute = function(...) NULL,
    getElementTagName = function(...) NULL,
    getElementText = function(...) "Example text",
    getElementValueOfCssProperty = function(...) NULL,
    isElementDisplayed = function(...) TRUE,
    isElementEnabled = function(...) TRUE,
    isElementSelected = function(...) TRUE,
    sendKeysToElement = function(...) NULL,
    setElementAttribute = function(...) NULL,
    submitElement = function(...) NULL,
    getElementLocation = function(...) NULL,
    getElementSize = function(...) NULL
  )

  class(res) <- "mock_element"

  res
}

