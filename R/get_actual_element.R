#' Get the element associated with a selenider element
#'
#' @description
#' Turn a lazy selenium element or element collection into a backendNodeId
#' (chromote) or a [selenium::WebElement]. Use this to perform certain actions
#' on the element that are not implemented in selenider.
#'
#' `get_actual_element()` turns a `selenider_element` object into a single
#' backendNodeId or [selenium::WebElement] object. The function will wait for
#' the object to exist in the DOM.
#'
#' `get_actual_elements()` turns a `selenider_elements` object into a list
#' of [selenium::WebElement] objects, waiting for any parent objects to
#' exist in the DOM.
#'
#' @param x A `selenider_element` or `selenider_elements` object, produced by
#'   [find_element()] / [find_elements()].
#' @param timeout The timeout to use while asserting that the item exists. If
#'   NULL, the timeout of the `selenider_element` will be used.
#'
#' @returns An integer (backendNodeId), or a [selenium::WebElement] object.
#' `get_actual_elements()` returns a list of such objects.
#'
#' @seealso
#' * [s()], [ss()], [find_element()] and [find_elements()] to select selenider
#'   elements.
#' * [elem_cache()] and [elem_cache()] to cache these values.
#' * The [Chrome Devtools Protocol documentation](https://chromedevtools.github.io/devtools-protocol/tot/)
#'   for the operations that can be performed using a backend node id. Note
#'   that this requires the [chromote::ChromoteSession] object, which can be
#'   retrieved using `<selenider_session>$driver`.
#' * The documentation for [selenium::WebElement()] to see the things you can
#'   do with a webElement.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div>
#' <p>Text</p>
#' <p>More text</p>
#' </div>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' elem <- s("div") |>
#'   get_actual_element()
#'
#' # The ChromoteSession/SeleniumSession can be accessed using session$driver
#' driver <- session$driver
#'
#' if (inherits(driver, "ChromoteSession")) {
#'   driver$DOM$getBoxModel(backendNodeId = elem)
#' } else if (inherits(elem, "WebElement")) {
#'   elem$get_rect()
#' }
#'
#' elems <- ss("p") |>
#'   get_actual_elements()
#'
#' if (inherits(driver, "ChromoteSession")) {
#'   driver$DOM$describeNode(backendNodeId = elems[[1]])
#' } else if (inherits(elems[[1]], "WebElement")) {
#'   elems[[1]]$get_rect()
#' }
#'
#' @export
get_actual_element <- function(x, timeout = NULL) {
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)

  check_active(x)

  timeout <- get_timeout(timeout, x$timeout)

  get_with_timeout(timeout, get_element, x)
}

#' @rdname get_actual_element
#'
#' @export
get_actual_elements <- function(x, timeout = NULL) {
  check_class(x, "selenider_elements")
  check_number_decimal(timeout, allow_null = TRUE)

  check_active(x)

  timeout <- get_timeout(timeout, x$timeout)

  # as.list() in case the result is a lazy list
  as.list(get_with_timeout(timeout, get_element, x))
}

get_element <- function(x) {
  element <- x$element

  if (length(x$steps) == 0) {
    if (is.null(element)) {
      cli::cli_abort("Element does not have any steps", .internal = TRUE)
    }

    return(element)
  }

  for (step in x$steps) {
    element <- apply_step(x$driver, element, step)

    if (is.null(element)) {
      return(NULL)
    }
  }

  element
}
