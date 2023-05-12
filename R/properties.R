html_name <- function(x, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)
  
  element <- get_element_for_action(
    x,
    action = "get the tag name of {.arg x}",
    conditions = list(),
    timeout = timeout,
    failure_messages = c(),
    conditions_text = c()
  )

  element$getElementTagName()
}
