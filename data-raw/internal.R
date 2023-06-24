## code to prepare `internal` dataset goes here

condition_dependencies <- list(
  none = c(
    "is present", "is in the DOM", "doesn't exist", "is not present",
    "is not in the DOM", "is missing", "is absent",
    "is visible", "is displayed", "is not visible", "is not displayed",
    "is hidden", "is invisible", "is enabled", "is not enabled", "is disabled"
  ),
  text = c("has_text", "has_exact_text")
)

usethis::use_data(condition_dependencies, internal = TRUE, overwrite = TRUE)
