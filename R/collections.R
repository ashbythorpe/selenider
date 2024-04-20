#' Iterate over an element collection
#'
#' Transform a `selenider_elements` object into a list of
#' `selenider_element` objects. The result can then be used in for loops and
#' higher order functions like [lapply()]/[purrr::map()] (whereas a
#' `selenider_element` object cannot).
#'
#' @param x A `selenider_elements` object.
#' @param timeout How long to wait for `x` to exist while computing its length.
#' @param ... Not used.
#'
#' @description
#' This function essentially turns `x` into:
#' `list(x[[1]], x[[2]], ...)`
#' However, to do this, the length of `x` must be computed. This means that
#' while each element inside the list is still lazy, the list itself cannot be
#' considered lazy, since the number of elements in the DOM may change. To
#' avoid problems, it is recommended to use an element list just after it is
#' created, to make sure the list is an accurate representation of the DOM
#' when it is being used.
#'
#' @returns
#' A list of `selenider_element` objects.
#'
#' @seealso
#' * [elem_flatten()] to combine multiple
#'   `selenider_element`/`selenider_elements` objects into a single object.
#' * [find_each_element()] and [find_all_elements()] to select elements
#'   using an element collection while preserving laziness.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div id='div1'>
#'   <p>Text 1</p>
#' </div>
#' <div id='div2'>
#'   <p>Text 2</p>
#' </div>
#' <div id='div3'>
#'   <p>Text 3</p>
#' </div>
#' <div id='div4'>
#'   <p>Text 4</p>
#' </div>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' p_tags <- ss("p")
#'
#' for (elem in as.list(p_tags)) {
#'   print(elem_text(elem))
#' }
#'
#' p_tags |>
#'   as.list() |>
#'   lapply(elem_text)
#'
#' @export
as.list.selenider_elements <- function(x, timeout = NULL, ...) {
  check_class(x, "selenider_elements")
  check_number_whole(timeout, allow_null = TRUE)
  rlang::check_dots_empty()

  check_active(x)

  timeout <- get_timeout(timeout, x$timeout)

  size <- elem_size(x, timeout = timeout)

  lapply(seq_len(size), function(i) x[[i]])
}

#' Iterate over an element collection
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `elem_flatmap()` previously allowed you to apply a function to each
#' element in a collection in a lazy manner. This function is now deprecated,
#' as it did not work in all cases. Use [find_each_element()] and
#' [find_all_elements()] instead for the simple case where you want to
#' select the children of a collection.
#'
#' `element_list()` is a deprecated alias for [as.list.selenider_elements()].
#'
#' @param x A `selenider_elements` object.
#' @param .f A function that takes a `selenider_element` and returns a
#'   `selenider_element` or `selenider_elements` object.
#' @param ... Passed into `.f`.
#' @param timeout How long to wait for `x` to exist while computing its length.
#'
#' @returns
#' `elem_flatmap()` returns a `selenider_elements` object.
#'
#' `element_list()` returns a list of `selenider_element` objects.
#'
#' @export
elem_flatmap <- function(x, .f, ...) {
  lifecycle::deprecate_stop("0.4.0", "elem_flatmap()", I("`find_each_element()` or `find_all_elements()`"))
}

#' @rdname elem_flatmap
#'
#' @export
element_list <- function(x, timeout = NULL) {
  check_class(x, "selenider_elements")

  lifecycle::deprecate_warn("0.4.0", "element_list()", "as.list()")

  as.list(x, timeout = timeout)
}

#' Find HTML children from a collection
#'
#' @description
#' Find HTML child elements from elements in a collection. Provides
#' a convenient way to operate on a collection of elements.
#'
#' `find_each_element()` finds the first child element of each element in
#' the collection.
#'
#' `find_all_elements()` finds every child element of every element in the
#' collection.
#'
#' @param x A `selenider_elements` object.
#' @param css A CSS selector.
#' @param xpath An XPath.
#' @param id The id of the elements you want to select.
#' @param class_name The class name of the elements you want to select.
#' @param name The name attribute of the elements you want to select.
#'
#' @details
#' `find_each_element()` will usually preserve the length of the input, since
#' for each element in the collection, one new element will be found. However,
#' if an element in the collection cannot be found, it will not be included in
#' the resulting collection.
#'
#' `find_each_element(x, ...)` is roughly equivalent to:
#'
#' ```r
#' x |>
#'   as.list() |>
#'   lapply(\(x) find_element(x, ...)) |>
#'   elem_flatten()
#' ```
#'
#' Similarly, `find_all_elements(x, ...)` is roughly equivalent to:
#'
#' ```r
#' x |>
#'   as.list() |>
#'   lapply(\(x) find_elements(x, ...)) |>
#'   elem_flatten()
#' ```
#'
#' @returns
#' A `selenider_elements` object.
#'
#' @seealso
#' * [as.list()] to iterate over an element collection.
#' * [elem_flatten()] to combine multiple
#'   `selenider_element`/`selenider_elements` objects into a single object.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div id='div1'>
#'   <p>Text 1</p>
#'   <button>Button 1</button>
#' </div>
#' <div id='div2'>
#'   <p>Text 2</p>
#' </div>
#' <div id='div3'>
#'   <p>Text 3</p>
#' </div>
#' <div id='div4'>
#'   <p>Text 4</p>
#' </div>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' divs <- ss("div")
#'
#' # Get the <p> tag inside each div.
#' divs |>
#'   find_each_element("p")
#'
#'
#' # Get the <button> tag in the first div as well.
#' divs |>
#'   find_all_elements("*")
#'
#' @export
find_each_element <- function(x,
                              css = NULL,
                              xpath = NULL,
                              id = NULL,
                              class_name = NULL,
                              name = NULL) {
  check_class(x, "selenider_elements")

  selector <- new_flatmap_selector(css, xpath, id, class_name, name)

  x$selectors <- append(x$selectors, list(selector))

  x$to_be_found <- x$to_be_found + 1

  x
}

#' @rdname find_each_element
#'
#' @export
find_all_elements <- function(x,
                              css = NULL,
                              xpath = NULL,
                              id = NULL,
                              class_name = NULL,
                              name = NULL) {
  check_class(x, "selenider_elements")

  selector <- new_flatmap_selector(
    css,
    xpath,
    id,
    class_name,
    name,
    multiple = TRUE
  )

  x$selectors <- append(x$selectors, list(selector))

  x$to_be_found <- x$to_be_found + 1

  x
}

new_flatmap_selector <- function(..., multiple = FALSE) {
  res <- new_selector(..., filter = list(), multiple = FALSE, inner_multiple = multiple)

  class(res) <- c("selenider_flatmap_selector", "selenider_selector")

  res
}
