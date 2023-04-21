click <- function(x, type = c("left", "right", "centre")) {
  type <- rlang::match_arg(type)
  
  id <- switch(type, left = 0, middle = 1, right = 2)
  
  # Expect exists

  x$element$click(id)
}
