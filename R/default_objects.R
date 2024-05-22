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

# We manage our own default chromote object to make sure we make a new one
# whenever the arguments passed into chrome change.
default_chromote_object <- function() {
  get_from_env("chromote")
}

has_default_chromote_object <- function() {
  !is.null(default_chromote_object()) &&
    default_chromote_object()$get_browser()$get_process()$is_alive()
}

set_default_chromote_object <- function(x) {
  set_in_env(chromote = x)
}

default_chromote_args <- function() {
  get_from_env("chromote_args")
}

set_default_chromote_args <- function(x) {
  set_in_env(chromote_args = x)
}
