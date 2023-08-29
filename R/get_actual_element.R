#' Get the webElement associated with a selenider element
#'
#' @description
#' Turn a lazy selenium element or element collection into a
#' [RSelenium::webElement]. Use this to perform certain actions on the element
#' that are not implemented in selenider (e.g. getElementLocation())
#'
#' `get_actual_webelement()` turns a `selenider_element` object into a
#' [RSelenium::webElement] object. The function will wait for the object to
#' exist in the DOM.
#'
#' `get_actual_webelements()` turns a `selenider_elements` object into a list
#' of [RSelenium::webElement] objects.
#'
#' @param x A `selenider_element` or `selenider_elements` object, produced by
#'   [html_element()] / [html_elements()]
#' @param timeout The timeout to use while asserting that the item exists. If
#'   NULL, the timeout of the `selenider_element` will be used.
#'
#' @returns A [RSelenium::webElement] object, or a list of such objects.
#'
#' @seealso
#' * [s()], [ss()], [html_element()] and [html_elements()] to select selenider
#'   elements
#' * The documentation for [RSelenium::webElement()] to see the things you can
#'   do with them.
#'
#' @examplesIf selenider_available(online = FALSE)
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
#'   get_actual_webelement()
#'
#' # The webDriver/ChromoteSession can be accessed using session$driver
#' driver <- session$driver
#'
#' if (inherits(driver, "ChromoteSession")) {
#'   driver$DOM$getBoxModel(backendNodeId = elem)
#' } else {
#'   elem$getElementLocation()
#' }
#'
#' elems <- ss("p") |>
#'   get_actual_webelements()
#'
#' if (inherits(driver, "ChromoteSession")) {
#'   driver$DOM$describeNode(backendNodeId = elems[[1]])
#' } else {
#'   elems[[1]]$describeElement()
#' }
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
get_actual_webelement <- function(x, timeout = NULL) {
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  get_with_timeout(timeout, get_element, x)
}

#' @rdname get_actual_webelement
#'
#' @export
get_actual_webelements <- function(x, timeout = NULL) {
  check_class(x, "selenider_elements")
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  # as.list in case the result is a lazy list
  as.list(get_with_timeout(timeout, get_elements, x))
}

get_element <- function(x) {
  element <- x$element

  if (is.null(element)) {
    element <- x$driver
  }

  to_be_filtered <- if (x$to_be_found < length(x$selectors)) {
    selector_to_filter <- x$selectors[[length(x$selectors) - x$to_be_found]]
    selector_to_filter$to_be_filtered
  } else {
    0
  }

  if (is_multiple_elements(element) && to_be_filtered != 0) {
    filter <- selector_to_filter$filter
    relevant_filters <- utils::tail(filter, to_be_filtered)

    element <- filter_elements(element, relevant_filters)

    if (is.null(element)) {
      return(NULL)
    }
  }

  if (x$to_be_found == 0) {
    return(element)
  }

  selectors <- utils::tail(x$selectors, x$to_be_found)

  for (selector in selectors) {
    elements <- use_selector(selector, element, driver = x$driver)
    element <- filter_elements(elements, selector$filter)
    if (is.null(element)) {
      return(NULL)
    }
  }

  element
}

get_elements <- function(x) {
  element <- x$element

  if (is.null(element)) {
    element <- x$driver
  }


  to_be_filtered <- if (x$to_be_found < length(x$selectors)) {
    selector_to_filter <- x$selectors[[length(x$selectors) - x$to_be_found]]
    selector_to_filter$to_be_filtered
  } else {
    0
  }

  if (is_multiple_elements(element) && to_be_filtered != 0) {
    filter <- selector_to_filter$filter
    relevant_filters <- utils::tail(filter, to_be_filtered)

    element <- filter_elements(element, relevant_filters, multiple = (x$to_be_found == 0))

    if (is.null(element)) {
      return(NULL)
    }
  }

  if (x$to_be_found == 0) {
    return(element)
  }

  selectors <- utils::tail(x$selectors, x$to_be_found)

  for (selector in utils::head(selectors, -1)) {
    elements <- use_selector(selector, element, driver = x$driver)
    element <- filter_elements(elements, selector$filter)

    if (is.null(element)) {
      return(NULL)
    }
  }

  selector <- selectors[[length(selectors)]]
  elements <- use_selector(selector, element, driver = x$driver)
  filter_elements(elements, selector$filter, multiple = TRUE)
}

filter_elements <- function(elements, filter, multiple = FALSE) {
  if (length(filter) == 0) {
    stopifnot(multiple) # we need a filter to get a single element
    elements
  } else if (length(filter) == 1 && is.numeric(filter[[1]])) {
    if (length(filter[[1]]) == 1 && filter[[1]] > 0) {
      res <- get_item(elements, filter[[1]])

      if (multiple) {
        list(res)
      } else {
        res
      }
    } else {
      stopifnot(multiple)
      elements[filter[[1]]]
    }
  } else if (length(filter) == 1 && is_function(filter[[1]])) {
    stopifnot(multiple)
    lazy_filter(elements, filter[[1]])
  } else if (!multiple) {
    if (is_function(filter[[length(filter)]])) {
      last <- 1
    } else {
      last <- filter[[length(filter)]]
      filter[[length(filter)]] <- NULL
    }

    for (f in filter) {
      if (is_function(f)) {
        elements <- lazy_filter(elements, f)
      } else {
        elements <- elements[f]
      }
    }

    get_item(elements, last)
  } else {
    for (f in filter) {
      if (is_function(f)) {
        elements <- lazy_filter(elements, f)
      } else {
        elements <- elements[f]
      }
    }

    elements
  }
}
