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
#' @examples
#' session <- mock_selenider_session()
#'
#' elem <- s(".class1") |>
#'   get_actual_webelement()
#' 
#' elem$getElementLocation()
#'
#' elems <- ss(".class2") |>
#'   get_actual_webelements()
#'
#' elems[[1]]$getElementSize()
#'
#' @export
get_actual_webelement <- function(x, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  res <- get_with_timeout(timeout, get_element, x)
}

#' @rdname get_actual_webelement
#'
#' @export
get_actual_webelements <- function(x, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  res <- get_with_timeout(timeout, get_elements, x)
}

get_element <- function(x) {
  if (!is.list(x)) stop(as.character(x))
  element <- x$element

  if (is.null(element)) {
    element <- x$driver
  }
  
  if (!inherits(element, c("webElement", "remoteDriver", "mock_element", "mock_client"))) {
    filter <- x$selectors[[length(x$selectors) - x$to_be_found + 1]]$filter
    
    element <- filter_elements(element, filter)
    
    if (is.null(element)) {
      return(NULL)
    }
  }

  if (x$to_be_found == 0) {
    return(element)
  }

  selectors <- utils::tail(x$selectors, x$to_be_found)

  for (selector in selectors) {
    elements <- use_selector(selector, element)
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

  if (x$to_be_found == 0) {
    return(element)
  }
  
  if (!inherits(element, c("webelement", "remotedriver", "mock_element", "mock_client"))) {
    filter <- x$selectors[[length(x$selectors) - x$to_be_found]]$filter

    element <- filter_elements(element, filter)

    if (is.null(element)) {
      return(NULL)
    }
  }

  selectors <- utils::tail(x$selectors, x$to_be_found)

  for (selector in utils::head(selectors, -1)) {
    elements <- use_selector(selector, element)
    element <- filter_elements(elements, selector$filter)

    if (is.null(element)) {
      return(NULL)
    }
  }
  
  selector <- selectors[[length(selectors)]]
  elements <- use_selector(selector, element, multiple = TRUE)
  filter_elements(elements, selector$filter, multiple = TRUE)
}
    
filter_elements <- function(elements, filter, multiple = FALSE) {
  if (is.null(filter)) {
    stopifnot(multiple) # we need a filter to get a single element
    elements
  } else if (length(filter) == 1 && is.numeric(filter[[1]])) {
    if (length(filter[[1]]) == 1) {
      if (length(elements) < filter[[1]]) {
        return(NULL)
      }

      res <- elements[[filter[[1]]]]

      if (multiple) {
        list(res)
      } else {
        res
      }
    } else {
      stopifnot(multiple)
      if (length(elements) < min(filter[[1]])) {
        return(NULL)
      }

      elements[filter[[1]]]
    }
  } else if (is.function(filter[[1]])) {
    res <- NULL
    .f <- filter[[1]]
    
    if (multiple) {
      Filter(.f, elements)
    } else {
      nth_found <- 1
      for (element in elements) {
        if (.f(element)) {
          res <- element
          break
        }
      }
      
      if (is.null(res)) {
        return(NULL)
      }
      
      res
    }
  } else if (!multiple) {
    if (is.function(filter[[length(filter)]])) {
      last <- 1
    } else {
      last <- filter[[length(filter)]]
      filter[[length(filter)]] <- NULL
    }

    for (f in filter) {
      if (is.function(f)) {
        elements <- lazy_filter(elements, f)
      } else {
        elements <- item_slice(elements, f)
      }
    }

    item_extract(elements, last)
  } else {
    for (f in filter) {
      if (is.function(f)) {
        elements <- lazy_filter(elements, f)
      } else {
        elements <- item_slice(elements, f)
      }
    }

    all_items(elements)
  }
}
