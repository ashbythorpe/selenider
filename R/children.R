#' Get the family elements of an element
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
#' @returns All functions return a `selenider_elements` object, except `html_parent()`,
#' which returns a `selenider_element` object (since an element can only have one parent).
#'
#' @seealso 
#' * <http://web.simmons.edu/~grovesd/comm244/notes/week4/document-tree> for a simple
#'   and visual explanation of the document tree.
#'
#' @export
html_ancestors <- function(x) {
  selector <- list(
    xpath = "./ancestor::*",
    filter = list()
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
  selector <- list(
    xpath = "./..",
    filter = list()
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
  selector <- list(
    xpath = "./following-sibling::* | ./preceding-sibling::*",
    filter = list()
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
  selector <- list(
    xpath = "*",
    filter = list()
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
  selector <- list(
    xpath = ".//*",
    filter = list()
  )

  class(selector) <- c("selenider_descendant_selector", "selenider_selector")

  x$selectors <- append(x$selectors, list(selector))

  x$to_be_found <- x$to_be_found + 1
  
  class(x) <- c("selenider_elements", "list")
  
  x
}
