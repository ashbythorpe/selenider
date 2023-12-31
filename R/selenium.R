has_default_selenium_object <- function() {
  !is.null(default_selenium_object()) &&
    default_selenium_object()$is_alive()
}

default_selenium_object <- function() {
  get_from_env("selenium")
}

set_default_selenium_object <- function(x) {
  if (has_default_selenium_object()) {
    default_selenium_object()$kill()
  }

  set_in_env(selenium = x)
}

default_selenium_options <- function() {
  get_from_env("selenium_options")
}

set_default_selenium_options <- function(x) {
  set_in_env(selenium_options = x)
}
