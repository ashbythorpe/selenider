has_default_selenium_object <- function() {
  !is.null(default_selenium_object()) &&
    default_selenium_object()$is_alive()
}

default_selenium_object <- function() {
  get_from_env("selenium")
}

set_default_selenium_object <- function(x) {
  set_in_env(selenium = x)
}
