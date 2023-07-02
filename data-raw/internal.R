## code to prepare `internal` dataset goes here

condition_dependencies <- list(
  none = c(
    "is present", "is in the DOM", "doesn't exist", "is not present",
    "is not in the DOM", "is missing", "is absent",
    "is visible", "is displayed", "is not visible", "is not displayed",
    "is hidden", "is invisible", "is enabled", "is not enabled", "is disabled"
  ),
  name = c("has tag name", "does not have tag name"),
  text = c("has text", "has exact text", "does not have text", "does not have exact text"),
  attribute = c("has attr", "attr contains", "does not have attr", "does not have exact attr"),
  value = c("has value", "does not have value"),
  css = c("has css property", "does not have css property")
)

usethis::use_data(condition_dependencies, internal = TRUE, overwrite = TRUE)
