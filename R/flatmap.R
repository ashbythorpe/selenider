#' Iterate over an element collection
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `as.list()` transforms a `selenider_elements` object into a list of
#' `selenider_element` objects. The result can then be used in for loops and
#' higher order functions like [lapply()]/[purrr::map()] (whereas a `selenider_element`
#' object cannot).a  This function is stable.
#'
#' `element_list()` is the underlying function called by `element_list()`.
#'
#' Use `elem_flatmap()` when you want to select further sub-elements
#' *for each* element of a collection.
#'
#' `elem_flatmap()` allows you to apply a function to each element of
#' a `selenider_elements` object, provided that the function returns a
#' `selenider_element`/`selenider_elements` object itself. The result will
#' then be flattened into a single `selenider_elements` object. The benefit
#' of this over traditional iteration techniques is that the laziness of the
#' elements will be maintained, and nothing will be fetched from the DOM.
#' This function is experimental, and won't work if `.f` uses [elem_flatten()]
#' (or nested `elem_flatmap()`).
#'
#' @param x A `selenider_elements` object.
#' @param timeout How long to wait for `x` to exist while computing its length.
#' @param .f A function to apply to each element of `x`.
#' @param ... Passed into `.f`.
#'
#' @description
#' `elem_flatmap()` works by executing `.f` on a mock element, then recording the
#' results in `x`. This means that no matter the length of `x`, `.f` is only evaluated
#' once, and during the `elem_flatmap()` call. For this reason, `.f` should not invoke
#' any side effects or do anything other than selecting sub-elements.
#'
#' `elem_flatmap()` can essentially be viewed as a map operation (e.g. [lapply()], [purrr::map()])
#' followed by a flattening operation ([elem_flatmap()]). This means that:
#' ```
#' x |>
#'   elem_flatmap(.f)
#' ```
#' is essentially equivalent to:
#' ```
#' x |>
#'   as.list() |>
#'   lapply(.f) |>
#'   elem_flatten()
#' ```
#' However, the second approach is not done lazily.
#'
#' `as.list()`/`element_list()` essentially turns `x` into:
#' `list(x[[1]], x[[2]], ...)`
#' However, to do this, the length of `x` must be computed. This means that while
#' each element inside the list is still lazy, the list itself cannot be considered
#' lazy, since the number of elements in the DOM may change. To avoid problems, it is
#' recommended to use an element list just after it is created, to make sure the
#' list is an accurate representation of the DOM when it is being used.
#'
#' @returns
#' `elem_flatmap()` returns a `selenider_element` object.
#' `as.list()`/`element_list()` returns a list of `selenider_element` objects.
#'
#' @seealso
#' * [elem_flatten()] to combine multiple `selenider_element`/`selenider_elements` objects
#'   into a single object.
#' * [elem_filter()] and [elem_find()] to filter element collections using a condition.
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
#' divs <- ss("div")
#'
#' # Get the <p> tag inside each div.
#' divs |>
#'   elem_flatmap(\(x) x |> find_element("p"))
#'
#' # Or:
#' p_tags <- divs |>
#'   elem_flatmap(find_element, "p")
#'
#' # To get the text in each tag, we can't use elem_flatmap()
#' for (elem in as.list(p_tags)) {
#'   print(elem_text(elem))
#' }
#'
#' # Or:
#' lapply(as.list(p_tags), elem_text)
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
as.list.selenider_elements <- function(x, timeout = NULL, ...) {
  element_list(x)
}

#' @rdname as.list.selenider_elements
#'
#' @export
element_list <- function(x, timeout = NULL) {
  check_class(x, "selenider_elements")

  timeout <- get_timeout(timeout, x$timeout)

  size <- elem_size(x, timeout = timeout)

  lapply(seq_len(size), function(i) x[[i]])
}

#' @rdname as.list.selenider_elements
#'
#' @export
elem_flatmap <- function(x, .f, ...) {
  check_class(x, "selenider_elements")

  mock_element <- list(
    driver = x$driver,
    driver_id = x$driver_id,
    element = NULL,
    timeout = x$timeout,
    selectors = list(),
    to_be_found = 0
  )

  class(mock_element) <- "selenider_element"

  fn_result <- try_fetch(
    with_timeout(0, .f(mock_element, ...)),
    error = function(cnd) {
      stop_flatmap_return_value(cnd, error = TRUE)
    }
  )

  if (!inherits_any(fn_result, c("selenider_element", "selenider_elements"))) {
    stop_flatmap_return_value(fn_result)
  } else if (length(fn_result$selectors) <= 0) {
    # Identity transformation
    return(x)
  }

  selectors <- fn_result$selectors

  selector <- new_flatmap_selector(x, selectors, class(fn_result))

  res <- list(
    driver = x$driver,
    driver_id = x$driver_id,
    element = NULL,
    timeout = x$timeout,
    selectors = list(selector),
    to_be_found = 1
  )

  class(res) <- c("selenider_elements", "list")

  res
}

new_flatmap_selector <- function(x, selectors, class) {
  res <- list(
    element = remove_driver(x),
    selectors = selectors,
    resulting_class = class,
    filter = NULL,
    to_be_filtered = 0
  )

  class(res) <- c("selenider_flatmap_selector", "selenider_selector")

  res
}
