#' @export
format.selenider_selector <- function(x, first = FALSE, multiple = FALSE, element_name = NULL, of = NULL, ...) {
  if (multiple) {
    format_selector_multiple(x, first, element_name = element_name, of = of)
  } else {
    format_selector(x, first, element_name = element_name, of = of)
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
format.selenider_ancestor_selector <- function(x, multiple = FALSE, of = NULL, ...) {
  if (multiple) {
    element_name <- if (!is.null(of)) paste("ancestors of", of, "element") else "ancestors"
    format_selector_multiple(x, element_name = element_name)
  } else {
    element_name <- if (!is.null(of)) paste("ancestor of", of, "element") else "ancestor"
    format_selector(x, element_name = element_name)
  }
}

#' @export
format.selenider_parent_selector <- function(x, of = NULL, ...) {
  format_parent_selector(x, of)
}

#' @export
format.selenider_sibling_selector <- function(x, multiple = FALSE, of = NULL, ...) {
  if (multiple) {
    element_name <- if (!is.null(of)) paste("siblings of", of, "element") else "siblings"
    format_selector_multiple(x, element_name = element_name)
  } else {
    element_name <- if (!is.null(of)) paste("sibling of", of, "element") else "sibling"
    format_selector(x, element_name = element_name)
  }
}

#' @export
format.selenider_child_selector <- function(x, multiple = FALSE, of = NULL, ...) {
  if (multiple) {
    element_name <- if (!is.null(of)) paste("children of", of, "element") else "children"
    format_selector_multiple(x, element_name = element_name)
  } else {
    element_name <- if (!is.null(of)) paste("child of", of, "element") else "child"
    format_selector(x, element_name = element_name)
  }
}

#' @export
format.selenider_descendant_selector <- function(x, multiple = FALSE, of = NULL, ...) {
  if (multiple) {
    element_name <- if (!is.null(of)) paste("descendants of", of, "element") else "descendants"
    format_selector_multiple(x, element_name = element_name)
  } else {
    element_name <- if (!is.null(of)) paste("descendant of", of, "element") else "descendant"
    format_selector(x, element_name = element_name)
  }
}

#' @export
format.selenider_flatmap_selector <- function(x, multiple = FALSE, inside_flatmap = FALSE, ...) {
  if (inside_flatmap) {
    format_flatmap_selector_simple(x, multiple, ...)
  } else {
    format_flatmap_selector(x, multiple)
  }
}

format_selector <- function(selector, first, element_name = NULL, of = NULL, with = NULL) {
  if (is.null(element_name)) {
    element_name <- if (!is.null(of)) {
      paste(" child of", of, "element")
    } else {
      paste0(
        if (first) "" else " child",
        " element"
      )
    }
  }

  filter <- selector$filter

  selector$filter <- NULL
  
  element <- if (is.null(with)) {
    format_query(selector, element_name)
  } else {
    paste(element_name, with)
  }

  if (length(filter) == 1) {
    paste0("The ", ordinal(filter[[1]]), element) 
  } else {
    last <- filter[[length(filter)]]
    stopifnot(is.numeric(last))

    if (length(filter) == 2) {
      condition <- format_condition(filter[[1]])
      if (length(condition) == 1 && !grepl("\n", condition, fixed = TRUE)) {
        return(c(
          paste0("The ", ordinal(last), element, " matching the following condition:"),
          " " = paste0("{.code ", condition, "}")
        ))
      }
    }

    paste0("The ", ordinal(last), element, " matching a custom condition")
  }
}

format_query <- function(selector, element_name) {
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

  paste(element_name, "with", text)
}

format_selector_multiple <- function(selector, first = FALSE, element_name = NULL, of = NULL, with = NULL) {
  if (is.null(element_name)) {
    element_name <- if (!is.null(of)) {
      paste(" children of", of, "element")
    } else {
      paste0(
        if (first) "" else " child",
        " elements"
      )
    }
  }
  
  filter <- selector$filter
  
  selector$filter <- NULL
  
  element <- if (is.null(with)) {
    format_query(selector, element_name)
  } else {
    paste(element_name, with)
  }
  
  if (length(filter) == 0) {
    paste0("The", element)
  } else if (length(filter) == 1) {
    if (is.numeric(filter[[1]])) {
      format_ordinal(filter[[1]], element)
    } else {
      condition <- format_condition(filter[[1]])
      if (length(condition) == 1 && !grepl("\n", condition, fixed = TRUE)) {
        c(
          paste0("The", element, " matching the following condition:"),
          " " = paste0("{.code ", condition, "}")
        )
      } else {
        paste0("The", element, " matching a custom condition")
      }
    }
  } else {
    last <- filter[[length(filter)]]

    if (is.numeric(last)) {
      if (length(filter) == 2) {
        condition <- format_condition(filter[[1]])
        if (length(condition) == 1 && !grepl("\n", condition, fixed = TRUE)) {
          return(format_ordinal(last, element, c(paste0(" matching the following condition:"), " " = paste0("{.code ", condition, "}"))))
        }
      }

      format_ordinal(last, element,  " matching a custom condition")
    } else {
      paste0("The", element, " matching a custom condition")
    }
  }
}

format_ordinal <- function(x, element, condition = "") {
  first_condition <- condition
  if (all(x) >= 0) {
    c(paste0("The ", subscript_ordinal(x), element, condition[1]), condition[-1])
  } else {
    c(paste0("All", element, " except the ", subscript_ordinal(x), condition[1]), condition[-1])
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
      condition <- format_condition(filter[[1]])
      if (length(condition) == 1 && !grepl("\n", condition, fixed = TRUE)) {
        return(c(
          paste0("The ", ordinal(last), " of a combination of elements matching the following condition:"),
          " " = paste0("{.code ", condition, "}")
        ))
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
      condition <- format_condition(filter[[1]])
      if (length(condition) == 1 && !grepl("\n", condition, fixed = TRUE)) {
        c(
          paste0("The elements in a combination of elements that match the following condition:"),
          " " = paste0("{.code ", condition, "}")
        )
      } else {
        "The elements in a combination of elements that match a custom condition"
      }
    }
  } else {
    last <- filter[[length(filter)]]

    if (is.numeric(last)) {
      if (length(filter) == 2) {
        condition <- format_condition(filter[[1]])
        if (length(condition) == 1 && !grepl("\n", condition, fixed = TRUE)) {
          return(format_ordinal(last, c(paste0(" matching the following condition:"), " " = paste0("{.code ", condition, "}"))))
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
    c(paste0("The ", subscript_ordinal(x), " of a combination of elements", condition[1]), condition[-1])
  } else {
    c(paste0("All elements of a combination of elements except the ", subscript_ordinal(x), condition[1]), condition[-1])
  }
}

format_flatmap_selector <- function(selector, multiple = FALSE) {
  first <- format_element(selector$element)

  mock_selector <- list(filter = NULL)

  class(mock_selector) <- "selenider_flattened_selector"

  if (length(selector$selectors[[length(selector$selectors)]]$filter) == 0) {
    last <- selector$selectors[[length(selector$selectors)]]
    last$filter <- selector$filter
    formatted_last <- format(last, of = "any", multiple = multiple)

    if (length(selector$selectors) == 1) {
      c(first, formatted_last)
    } else {
      rest <- utils::head(selector$selectors, -1)

      mock_element <- list(
        selectors = append(rest, list(mock_selector), after = 0)
      )

      class(mock_element) <- selector$resulting_class
      
      mapped <- format_element(mock_element, inside_flatmap = TRUE, of = "each")[-1]

      c(first, mapped, formatted_last)
    }
  } else {
    mock_element <- list(
      selectors = append(selector$selectors, list(mock_selector), after = 0)
    )

    class(mock_element) <- selector$resulting_class
    
    mapped <- format_element(mock_element, inside_flatmap = TRUE, of = "each")[-1]

    filter <- selector$filter

    element <- list(
      filter = selector$filter
    )

    class(element) <- "selenider_selector"

    final <- format(element, with = "")
    c(first, mapped, final)
  }
}

# For nested flatmap calls
format_flatmap_selector_simple <- function(selector, multiple = FALSE, ...) {
  first <- format_element(selector$element, ...)

  middle <- "A transformation of each element using `html_flatmap()`"

  element <- list(
    filter = selector$filter
  )

  class(element) <- "selenider_selector"

  final <- format(element, multiple = multiple, with = "")
  c(first, middle, final)
}

format_relative_selector <- function(x, descriptor) {
  filter <- x$filter

  if (length(filter) == 1) {
    paste0("The ", ordinal(filter[[1]]), descriptor, " element") 
  } else {
    last <- filter[[length(filter)]]
    stopifnot(is.numeric(last))

    if (length(filter) == 2) {
      condition <- format_condition(filter[[1]])
      if (length(condition) == 1 && !grepl("\n", condition, fixed = TRUE)) {
        return(c(
          paste0("The ", ordinal(last), descriptor, " element matching the following condition:"), 
          paste0("{.code ", condition, "}")
        ))
      }
    }

    paste0("The ", ordinal(last), descriptor, " element with matching a custom condition")
  }
}

format_parent_selector <- function(x, of = NULL) {
  if (is.null(of)) {
    paste0("The parent of this element.")
  } else {
    paste0("The parent of ", of, " element.")
  }
}

format_condition <- function(c) {
  expr <- attr(c, "original_call")

  if (is_call(expr)) {
    expr_deparse(expr)
  } else {
    escape_squirlies(expr_deparse(expr))
  }
}

replace_names_bullets <- function(x) {
  names(x)[names(x) != " "] <- rep("*", sum(names(x) != " "))
  x
}
