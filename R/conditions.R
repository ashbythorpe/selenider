exists <- function(x) {
  !is.null(update_element(x))
}

is_present <- exists

is_in_dom <- exists

is_missing <- function(x) !exists(x)

is_absent <- is_missing

is_visible <- function(x) {
  element <- update_element(x)
  
  stopifnot(element$element$isElementDisplayed())

  TRUE
}

is_displayed <- is_visible

is_hidden <- function(x) {
  !is_visible(x)
}

is_enabled <- function(x) {
  element <- update_element(x)

  stopifnot(element$element$isElementEnabled())

  element
}


