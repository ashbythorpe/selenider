#' Force an element to be collected and stored
#'
#' @description
#' `selenider_element`/`selenider_elements` objects are generally
#' *lazy*, meaning they only collect the actual element in the DOM
#' when absolutely necessary, and forget it immediately after. This
#' is to avoid situations where the DOM changes after an element
#' has been collected, resulting in errors and unreliable behaviour.
#'
#' `elem_cache()` forces an element or collection of elements to be collected
#' and stored, making it eager rather than lazy. This is useful when you are
#' operating on the same element multiple times, since only collecting the
#' element once will improve performance. However, you must be sure that the
#' element will not change on the page while you are using it.
#'
#' @param x A `selenider_element`/`selenider_elements` object.
#' @param timeout How long to wait for the element(s) to exist while collecting
#'   them.
#'
#' @details
#' These functions do not make selenider elements *permanently* eager. Further
#' sub-elements will not be cached unless specified.
#'
#' For example, consider the following code:
#' ``` r
#' s(".class1") |>
#'   elem_parent() |>
#'   elem_cache() |>
#'   find_element(".class2")
#' ```
#'
#' In this example, the parent of the element with class ".class1" will be
#' cached, but the child element with class ".class2" will not.
#'
#' @returns
#' A modified version of `x`. The result of `elem_cache()` can be used
#' as a normal `selenider_element`/`selenider_elements` object.
#'
#' @seealso
#' * [find_element()] and [find_elements()] to select elements.
#' * [element_list()], [find_each_element()] and [find_all_elements()] if you
#'   want to iterate over an element collection.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div>
#' <p id='specifictext'></p>
#' <button></button>
#' </div>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' # Selecting this button may be slow, since we are using relative XPath
#' # selectors.
#' button <- s("#specifictext") |>
#'   elem_siblings() |>
#'   elem_find(has_name("button"))
#'
#' # But we need to click the button 10 times!
#' # Normally, this would involve fetching the button from the DOM 10 times
#' click_button_10_times <- function(x) {
#'   lapply(1:10, \(unnused) elem_click(x))
#'   invisible(NULL)
#' }
#'
#' # But with elem_cache(), the button will only be fetched once
#' cached_button <- elem_cache(button)
#'
#' click_button_10_times(cached_button)
#'
#' # But the cached button is less reliable if the DOM is changing
#' execute_js_fn("x => { x.outerHTML = '<button></button>'; }", button)
#'
#' try(elem_click(cached_button, timeout = 0.1))
#'
#' # But the non-cached version works
#' elem_click(button)
#'
#' @export
elem_cache <- function(x, timeout = NULL) {
  check_class(x, c("selenider_element", "selenider_elements"))
  check_number_decimal(timeout, allow_null = TRUE)

  check_active(x)

  timeout <- get_timeout(timeout, x$timeout)

  if (inherits(x, "selenider_element")) {
    cache_element(x, timeout)
  } else {
    cache_elements(x, timeout)
  }
}

cache_element <- function(x, timeout = NULL) {
  element <- get_element_for_property(
    x,
    action = "cache {.arg x}",
    timeout = timeout,
  )

  x$element <- element

  x$to_be_found <- 0
  x$selectors[[length(x$selectors)]]$to_be_filtered <- 0

  x
}

cache_elements <- function(x, timeout = NULL) {
  elements <- get_elements_for_property(
    x,
    action = "cache the elements in {.arg x}",
    timeout = timeout
  )

  x$element <- elements

  x$to_be_found <- 0
  x$selectors[[length(x$selectors)]]$to_be_filtered <- 0

  x
}
