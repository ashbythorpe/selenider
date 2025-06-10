new_single_selector <- function(css = NULL,
                                xpath = NULL,
                                id = NULL,
                                class_name = NULL,
                                name = NULL) {
  args <- list(
    css = css,
    xpath = xpath,
    id = id,
    class_name = class_name,
    name = name
  )

  args <- args[!vapply(args, is.null, logical(1))]

  class(args) <- c("selenider_selector", "selenider_single_selector")

  args
}

new_single_selector <- function(css = NULL,
                                xpath = NULL,
                                id = NULL,
                                class_name = NULL,
                                name = NULL) {
  args <- list(
    css = css,
    xpath = xpath,
    id = id,
    class_name = class_name,
    name = name
  )

  args <- args[!vapply(args, is.null, logical(1))]

  class(args) <- c("selenider_selector", "selenider_multiple_selector")

  args
}

new_single_inner_selector <- function(css = NULL,
                                      xpath = NULL,
                                      id = NULL,
                                      class_name = NULL,
                                      name = NULL) {
  args <- list(
    css = css,
    xpath = xpath,
    id = id,
    class_name = class_name,
    name = name
  )

  args <- args[!vapply(args, is.null, logical(1))]

  class(args) <- c("selenider_selector", "selenider_single_inner_selector")

  args
}

new_multiple_inner_selector <- function(css = NULL,
                                        xpath = NULL,
                                        id = NULL,
                                        class_name = NULL,
                                        name = NULL) {
  args <- list(
    css = css,
    xpath = xpath,
    id = id,
    class_name = class_name,
    name = name
  )

  args <- args[!vapply(args, is.null, logical(1))]

  class(args) <- c("selenider_selector", "selenider_multiple_inner_selector")

  args
}

new_flatten <- function(elements) {
  res <- list(elements = elements)

  class(res) <- c("selenider_selector", "selenider_flatten")

  res
}

new_index <- function(index) {
  res <- list(index = index)

  class(res) <- c("selenider_filter", "selenider_index")

  res
}

new_subset <- function(index) {
  res <- list(index = index)

  class(res) <- c("selenider_filter", "selenider_subset")

  res
}

new_filter <- function(filter) {
  res <- list(filter = filter)

  class(res) <- c("selenider_filter", "selenider_predicate_filter")

  res
}

new_find <- function(filter) {
  res <- list(filter = filter)

  class(res) <- c("selenider_filter", "selenider_find")

  res
}
