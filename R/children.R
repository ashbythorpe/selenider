#' Get the DOM family of an element
#'
#' @description
#' Find all elements with a certain relative position to an HTML element.
#'
#' `html_ancestors()` selects every element which contains the current element
#' (children, grand-children, etc.).
#'
#' `html_parent()` selects the element that contains the current element.
#'
#' `html_siblings()` selects every element which has the same parent as the current
#' element.
#'
#' `html_children()` selects every element which is connected to and directly below
#' the current element.
#'
#' `html_descendants()` selects every element that is contained by the current element.
#' The current element does not have to be a direct parent, but must be some type of
#' ancestor.
#'
#' @param x A `selenider_element` object.
#'
#' @details
#' All functions except `html_children()` and `html_descendants()` use XPath selectors,
#' so may be slow, especially when using `chromote` as a backend.
#'
#' @returns All functions return a `selenider_elements` object, except `html_parent()`,
#' which returns a `selenider_element` object (since an element can only have one parent).
#'
#' @seealso
#' * <http://web.simmons.edu/~grovesd/comm244/notes/week4/document-tree> for a simple
#'   and visual explanation of the document tree.
#' * [html_element()] and [html_elements()] for other ways of selecting elements. These
#'   functions allow you to select ancestors using one or more conditions (e.g. CSS
#'   selectors).
#' * [html_filter()] and [html_find()] for filtering element collections.
#'
#' @examplesIf selenider_available(online = FALSE)
#' html <- "
#' <html>
#' <body>
#'   <div>
#'     <div id='current'>
#'       <p></p>
#'       <div>
#'         <p></p>
#'         <br>
#'       </div>
#'     </div>
#'     <div></div>
#'     <p></p>
#'   </div>
#' </body>
#' </html>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' current <- s("#current")
#'
#' # Get all the names of an element collection
#' html_names <- function(x) {
#'   x |>
#'     as.list() |>
#'     vapply(html_name, FUN.VALUE = character(1))
#' }
#'
#' current |>
#'   html_ancestors() |>
#'   html_expect(has_length(3)) |>
#'   html_names() # html, div, body
#'
#' current |>
#'   html_parent() |>
#'   html_name() # div
#'
#' current |>
#'   html_siblings() |>
#'   html_expect(has_length(2)) |>
#'   html_names() # div, p
#'
#' current |>
#'   html_children() |>
#'   html_expect(has_length(2)) |>
#'   html_names() # p, div
#'
#' current |>
#'   html_descendants() |>
#'   html_expect(has_length(4)) |>
#'   html_names() # p, div, p, br
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
html_ancestors <- function(x) {
  check_class(x, "selenider_element")

  selector <- list(
    xpath = "./ancestor::*",
    filter = list(),
    to_be_filtered = 0
  )

  class(selector) <- c("selenider_ancestor_selector", "selenider_selector")

  x$selectors <- append(x$selectors, list(selector))

  x$to_be_found <- x$to_be_found + 1

  class(x) <- c("selenider_elements", "list")

  x
}

#' @rdname html_ancestors
#'
#' @export
html_parent <- function(x) {
  check_class(x, "selenider_element")

  selector <- list(
    xpath = "./..",
    filter = list(1),
    to_be_filtered = 1
  )

  class(selector) <- c("selenider_parent_selector", "selenider_selector")

  x$selectors <- append(x$selectors, list(selector))

  x$to_be_found <- x$to_be_found + 1

  x
}

#' @rdname html_ancestors
#'
#' @export
html_siblings <- function(x) {
  check_class(x, "selenider_element")

  selector <- list(
    xpath = "./following-sibling::* | ./preceding-sibling::*",
    filter = list(),
    to_be_filtered = 0
  )

  class(selector) <- c("selenider_sibling_selector", "selenider_selector")

  x$selectors <- append(x$selectors, list(selector))

  x$to_be_found <- x$to_be_found + 1

  class(x) <- c("selenider_elements", "list")

  x
}

#' @rdname html_ancestors
#'
#' @export
html_children <- function(x) {
  check_class(x, "selenider_element")

  selector <- list(
    xpath = "./*",
    filter = list(),
    to_be_filtered = 0
  )

  class(selector) <- c("selenider_child_selector", "selenider_selector")

  x$selectors <- append(x$selectors, list(selector))

  x$to_be_found <- x$to_be_found + 1

  class(x) <- c("selenider_elements", "list")

  x
}

#' @rdname html_ancestors
#'
#' @export
html_descendants <- function(x) {
  check_class(x, "selenider_element")

  selector <- list(
    xpath = ".//*",
    filter = list(),
    to_be_filtered = 0
  )

  class(selector) <- c("selenider_descendant_selector", "selenider_selector")

  x$selectors <- append(x$selectors, list(selector))

  x$to_be_found <- x$to_be_found + 1

  class(x) <- c("selenider_elements", "list")

  x
}
