#' @export
format.selenider_selector <- function(x, first = FALSE, multiple = FALSE, ...) {
  if (multiple) {
    format_selector_multiple(x, first)
  } else {
    format_selector(x, first)
  }
}

#' @export
format.selenider_flattened_selector <- function(x, multiple = FALSE, ...) {
  if (multiple) {
    format_flattened_selector_multiple(x)
  } else {
    format_flattened_selector(x)
  }
}

#' @export
format.selenider_ancestor_selector <- function(x, multiple = FALSE, ...) {
  if (multiple) {
    format_relative_selector_multiple(x, " ancestor")
  } else {
    format_relative_selector(x, " ancestor")
  }
}

#' @export
format.selenider_parent_selector <- function(x, multiple = FALSE, ...) {
  if (multiple) {
    format_relative_selector_multiple(x, " parent")
  } else {
    format_relative_selector(x, " parent")
  }
}

#' @export
format.selenider_parent_selector <- function(x, multiple = FALSE, ...) {
  if (multiple) {
    format_relative_selector_multiple(x, " parent")
  } else {
    format_relative_selector(x, " parent")
  }
}

#' @export
format.selenider_sibling_selector <- function(x, multiple = FALSE, ...) {
  if (multiple) {
    format_relative_selector_multiple(x, " sibling")
  } else {
    format_relative_selector(x, " sibling")
  }
}

#' @export
format.selenider_child_selector <- function(x, multiple = FALSE, ...) {
  if (multiple) {
    format_relative_selector_multiple(x, " direct child")
  } else {
    format_relative_selector(x, " direct child")
  }
}

#' @export
format.selenider_descendant_selector <- function(x, multiple = FALSE, ...) {
  if (multiple) {
    format_relative_selector_multiple(x, " descendant")
  } else {
    format_relative_selector(x, " descendant")
  }
}

format_selector <- function(selector, first) {
  child <- if (first) "" else " child"

  filter <- selector$filter

  selector$filter <- NULL
  
  names <- names(selector)

  values <- unlist(selector, use.names = FALSE)
  
  names <- gsub("_", " ", names)

  values <- paste0("{.val ", values, "}")
  
  to_pluralize <- paste(names, values)

  text <- cli::pluralize("{to_pluralize}")

  if (length(filter) == 1) {
    paste0("The ", ordinal(filter[[1]]), child, " element with ", text) 
  } else {
    last <- filter[[length(filter)]]
    stopifnot(is.numeric(last))

    if (length(filter) == 2) {
      body <- as.character(fn_body(filter[[1]]))[-1]
      if (length(body) == 1 && !grepl("\n", body, fixed = TRUE)) {
        return(paste0("The ", ordinal(last), child, " element with ", text, " matching the following condition:\n {.code ", escape_squirlies(body), "}"))
      }
    }

    paste0("The ", ordinal(last), child, " element with ", text, " matching a custom condition")
  }
}

format_selector_multiple <- function(selector, first) {
  child <- if (first) "" else " child"
  
  filter <- selector$filter
  
  selector$filter <- NULL
  
  names <- names(selector)
  
  values <- unlist(selector, use.names = FALSE)
  
  names <- ifelse(
    names == "css",
    "css selector",
    gsub("_", " ", names, fixed = TRUE)
  )
  
  values <- paste0("{.val ", values, "}")
  
  to_pluralize <- paste(names, values)
  
  text <- cli::pluralize("{to_pluralize}")
  
  if (length(filter) == 0) {
    paste0("The", child, " elements with ", text)
  } else if (length(filter) == 1) {
    if (is.numeric(filter[[1]])) {
      format_ordinal(filter[[1]], child, text)
    } else {
      body <- fn_body(filter[[1]])[-1]
      if (length(body) == 1) {
        paste0("The", child, " elements with ", text, " matching the following condition:\n, {.code ", escape_squirlies(body), "}")
      } else {
        paste0("The", child, " elements with ", text, " matching a custom condition")
      }
    }
  } else {
    last <- filter[[length(filter)]]

    if (is.numeric(last)) {
      if (length(filter) == 2) {
        body <- as.character(fn_body(filter[[1]]))[-1]
        if (length(body) == 1 && !grepl("\n", body, fixed = TRUE)) {
          return(format_ordinal(last, child, text, paste0(" matching the following condition:\n {.code ", escape_squirlies(body), "}")))
        }
      }

      format_ordinal(last, child, text,  " matching a custom condition")
    } else {
      paste0("The", child, " elements with ", text, " matching a custom condition")
    }
  }
}

format_ordinal <- function(x, child, text, condition = "") {
  if (all(x) >= 0) {
    element <- if (length(x) == 1) " element" else " elements"
    paste0("The ", subscript_ordinal(x), child, element, " with ", text, condition)
  } else {
    paste0("All", child, " elements with ", text, " except the ", subscript_ordinal(x), condition)
  }
}

subscript_ordinal <- function(x) {
  if (length(x) == 1) {
    ordinal(x)
  } else {
    cli::pluralize("{ordinal_numbers(x)}")
  }
}

format_flattened_selector <- function(selector) {
  filter <- selector$filter

  if (length(filter) == 1) {
    "The first of a combination of elements"
  } else {
    last <- filter[[length(filter)]]
    stopifnot(is.numeric(last))

    if (length(filter) == 2) {
      body <- as.character(fn_body(filter[[1]]))[-1]
      if (length(body) == 1 && !grepl("\n", body, fixed = TRUE)) {
        return(paste0("The ", ordinal(last), " of a combination of elements matching the following condition:\n {.code ", escape_squirlies(body), "}"))
      }
    }

    paste0("The ", ordinal(last), " of a combination of elements matching a custom condition")
  }
}

format_flattened_selector_multiple <- function(selector) {
  filter <- selector$filter

  if (length(filter) == 0) {
    "A combination of elements"
  } else if (length(filter) == 1) {
    if (is.numeric(filter[[1]])) {
      format_ordinal_flattened(filter[[1]])
    } else {
      body <- fn_body(filter[[1]])[-1]
      if (length(body) == 1) {
        paste0("The elements in a combination of elements that match the following condition:\n, {.code ", escape_squirlies(body), "}")
      } else {
        "The elements in a combination of elements that match a custom condition"
      }
    }
  } else {
    last <- filter[[length(filter)]]

    if (is.numeric(last)) {
      if (length(filter) == 2) {
        body <- as.character(fn_body(filter[[1]]))[-1]
        if (length(body) == 1 && !grepl("\n", body, fixed = TRUE)) {
          return(format_ordinal(last, paste0(" matching the following condition:\n {.code ", escape_squirlies(body), "}")))
        }
      }

      format_ordinal(last, " matching a custom condition")
    } else {
      "The elements in a combination of elements that match a custom condition"
    }
  }
}

format_ordinal_flattened <- function(x, condition = "") {
  if (all(x) >= 0) {
    element <- if (length(x) == 1) " element" else " elements"
    paste0("The ", subscript_ordinal(x), " of a combination of elements", condition)
  } else {
    paste0("All", " elements of a combination of elements except the ", subscript_ordinal(x), condition)
  }
}

format_relative_selector <- function(x, descriptor) {
  filter <- x$filter

  if (length(filter) == 1) {
    paste0("The ", ordinal(filter[[1]]), descriptor, " element") 
  } else {
    last <- filter[[length(filter)]]
    stopifnot(is.numeric(last))

    if (length(filter) == 2) {
      body <- as.character(fn_body(filter[[1]]))[-1]
      if (length(body) == 1 && !grepl("\n", body, fixed = TRUE)) {
        return(paste0("The ", ordinal(last), descriptor, " element matching the following condition:\n {.code ", escape_squirlies(body), "}"))
      }
    }

    paste0("The ", ordinal(last), descriptor, " element with matching a custom condition")
  }
}

format_relative_selector_multiple <- function(x, descriptor) {
  filter <- x$filter

  if (length(filter) == 0) {
    paste0("The", descriptor, " elements")
  } else if (length(filter) == 1) {
    if (is.numeric(filter[[1]])) {
      format_ordinal(filter[[1]], descriptor)
    } else {
      body <- fn_body(filter[[1]])[-1]
      if (length(body) == 1) {
        paste0("The", descriptor, " elements matching the following condition:\n, {.code ", escape_squirlies(body), "}")
      } else {
        paste0("The", descriptor, " elements matching a custom condition")
      }
    }
  } else {
    last <- filter[[length(filter)]]

    if (is.numeric(last)) {
      if (length(filter) == 2) {
        body <- as.character(fn_body(filter[[1]]))[-1]
        if (length(body) == 1 && !grepl("\n", body, fixed = TRUE)) {
          return(format_ordinal(last, descriptor, paste0(" matching the following condition:\n {.code ", escape_squirlies(body), "}")))
        }
      }

      format_ordinal(last, descriptor, " matching a custom condition")
    } else {
      paste0("The", descriptor, " elements matching a custom condition")
    }
  }
}

format_ordinal_relative <- function(x, descriptor, condition = "") {
  if (all(x) >= 0) {
    element <- if (length(x) == 1) " element" else " elements"
    paste0("The ", subscript_ordinal(x), descriptor, " elements", condition)
  } else {
    paste0("All", descriptor, " elements except the ", subscript_ordinal(x), condition)
  }
}
