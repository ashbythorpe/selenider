#' Test one or more conditions on HTML elements
#' 
#' @description 
#' `html_expect()` waits for a set of conditions to return TRUE. If, after a
#' certain period of time (by default 4 seconds), this does not happen, an
#' informative error is thrown. Otherwise, the original element is returned.
#' 
#' `html_wait_until()` does the same, but returns a logical value (whether or
#' not the test passed), allowing you to handle the failure case explicitly.
#' 
#' @param x A `selenider_element`/`selenider_elements` object, or a condition.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Function calls or functions
#'   that must return a logical value. If multiple conditions are given, they
#'   must all be `TRUE` for the test to pass.
#' 
#' @details 
#' 
#' Conditions can be functions or calls.
#' 
#' Functions allow you to use unary conditions without formatting them as a
#' call (e.g. `exists` rather than `exists()`). It also allows you to make
#' use of R's [anonymous function syntax][base::function] to quickly create
#' custom conditions. `x` will be used as the first argument of this function.
#' 
#' Function calls allow you to use conditions that take multiple arguments (e.g.
#' `has_text()`) without the use of an intermediate function. The call will
#' be modified so that `x` is the argument `x` to the function call. For 
#' example, `has_text("a")` will be modified to become: `has_text("a", x = x)`.
#' This means that for a custom condition to work as a call, its first argument
#' must be named `x`.
#' 
#' @returns 
#' `html_expect()` returns the element(s) `x` when the test passes, or `NULL` if
#' an element or collection of elements was not given in `x`.
#' 
#' `html_wait_for()` returns a boolean flag: TRUE if the test passes, FALSE
#' otherwise.
#' 
#' @seealso 
#' * [html-conditions] for predicates for a single HTML element.
#' * [html-conditions-multiple] for predicates for multiple HTML elements.
#' 
#' @examples
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   html_expect(exists)
#'
#' s(".class1") |>
#'   html_expect(is_visible, is_enabled)
#' 
#' s(".class1") |>
#'   html_expect(is_visible || is_enabled)
#' 
#' s(".class2") |>
#'   html_expect(!exists)
#' # Or is_absent, etc.
#'
#' # html_expect() returns the element, so can be used in chains
#' s(".button1") |>
#'   html_expect(is_visible) |>
#'   click()
#' # Note that click() will do this automatically
#'
#' s(".text1") |>
#'   html_expect(has_exact_text("Hello"))
#'
#' # Or use an anonymous function
#' s(".text1") |>
#'   html_expect(\(elem) identical(html_text(elem), "Hello"))
#'
#' # This is useful for complex conditions:
#' s(".text1") |>
#'   html_expect(
#'     \(elem) elem |>
#'       html_text() |>
#'       grepl("Hello .*", x = _)
#'   )
#' 
#' # If your conditions are not specific to an element, you can omit the `x` argument
#' elem_1 <- s(".class1")
#' elem_2 <- s(".class2")
#'
#' html_expect(exists(elem_1) || exists(elem_2))
#' 
#' # We can now use the conditions on their own to figure out which element exists
#' if (exists(elem_1)) {
#'   click(elem_1)
#' } else {
#'   click(elem_2)
#' }
#'
#' # Use html_wait_for() to handle failures manually
#' elem <- s(".class2")
#' if (html_wait_for(elem, exists)) {
#'   click(elem)
#' } else {
#'   reload()
#' }
#' 
#' @export
html_expect <- function(x, ...) {
  x <- rlang::enquo(x)
  dots <- rlang::enquos(...)
  
  result <- eval_conditions(x, dots)
  timeout <- result$timeout
  error_exprs <- result$error_exprs
  exprs <- result$exprs
  res <- result$res

  if (!res) {
    for (a in seq_along(exprs)) {
      expr <- exprs[[a]]
      error_expr <- error_exprs[[a]]
      
      expr_res <- rlang::eval_tidy(expr)
      if (!expr_res) {
        cli::cli_abort(c(
          "Condition failed after waiting for {timeout} seconds:",
          "{.code {rlang::quo_get_expr(error_expr)}}"
        ))
      }
    }
  }

  if (is.logical(x_res)) {
    NULL
  } else {
    update_element(x_res)
  }
}
