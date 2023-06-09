#' Test conditions on multiple elements
#'
#' `html_expect_all()` and `html_wait_until_all()` are complements to
#' [html_expect()] and [html_wait_until()] that test conditions on
#' multiple elements in an element collection.
#'
#' @param x A `selenider_elements()` object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Function calls or functions
#'   that must return a logical value. If multiple conditions are given, they
#'   must all be `TRUE` for the test to pass. See [html_expect()] for more
#'   details.
#' @param testthat Whether to treat the expectation as a `testthat` test. You 
#'   *do not* need to explicitly provide this most of the time, since by default,
#'   we can use [testthat::is_testing()] to figure out whether `html_expect()` is
#'   being called from within a `testthat` test.
#' @param timeout The number of seconds to wait for a condition to pass. If not
#'   specified, the timeout used for `x` will be used, or the timeout of the
#'   local session if an element is not given.
#'
#' @details
#' If `x` does not contain any elements, `html_expect_all()` and
#' `html_wait_until_all()` will succeed. You may want to first verify that
#' at least one element exists with [has_at_least()].
#'
#' `html_expect_all()` and `html_wait_until_all()` can be thought of as alternatives
#' to the use of `all(vapply(FUN.VALUE = logical(1)))` (or [purrr::every()])
#' within [html_expect()] and [html_wait_until()].
#'
#' For example, the following two expressions are equivalent (where `x` is an
#' element collection).
#' ```
#' html_expect(x, \(element) all(vapply(element, is_present, logical(1))))
#' html_expect_all(x, is_present)
#' ```
#'
#' However, the second example will give a more detailed error message on failure.
#'
#' @returns
#' `html_expect_all()` returns the elements `x`, or `NULL` if an element or
#' collection of elements was not given in `x`.
#'
#' `html_wait_until_all()` returns a boolean flag: TRUE if the test passes, FALSE
#' otherwise.
#'
#' @seealso
#' * [html_expect()] and [html_wait_until()]
#' * [is_present()] and other conditions for predicates for a single HTML element.
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' ss(".class1") |>
#'   html_expect_all(is_present)
#'
#' @export
html_expect_all <- function(x, ..., testthat = NULL, timeout = NULL) {
  dots <- enquos(...)

  result <- eval_all_conditions(x, dots, timeout)

  check_number_decimal(timeout, allow_null = TRUE)
  check_bool(testthat, allow_null = TRUE)
  # `testthat` can only be TRUE if it is installed.
  if (is.null(testthat)) {
    testthat <- is_installed("testthat") && testthat::is_testing()
  } else {
    if (testthat) {
      check_installed("testthat", reason = "for `html_expect(testthat = TRUE)`.")
    }
  }

  timeout <- result$timeout
  calls <- result$calls
  exprs <- result$exprs
  res <- result$res

  if (!isTRUE(res)) {
    n <- res$expr_n
    x_res <- x[[res$element_n]]
    call <- calls[[n]]
    expr <- exprs[[n]]
    x_name <- paste0("x[[", res$element_n, "]]")

    diagnose_condition(
      x_res,
      n = n,
      call = call,
      original_expr = expr,
      result = res$val,
      timeout = timeout,
      original_env = quo_get_env(x),
      x_name = x_name,
      testthat = testthat
    )
  }

  invisible(x)
}

#' @rdname html_expect_all
#'
#' @export
html_wait_until_all <- function(x, ..., timeout = NULL) {
  dots <- enquos(...)

  check_number_decimal(timeout, allow_null = TRUE)
  result <- eval_all_conditions(x, dots, timeout)

  isTRUE(result$res)
}
