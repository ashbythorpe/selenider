#' Format selenider selectors
#'
#' Turn a selenider selector into a character vector, meant to be used with
#' [cli::cli_bullets()].
#'
#' @param x A `selenider_selector` object.
#' @param first Is this selector the first selector of the element? This
#'   changes the formatting since if this is `TRUE`, the selector will
#'   not have a parent.
#' @param multiple Does this selector select multiple elements?
#' @param element_name Overrides the description of the element, not
#'   including any filters applied to it. Must start with a space. For
#'   example, the string " element" would be a valid name.
#' @param One of "each", "any" or `NULL`. Used to modify the element name to
#'   create complex bullets such as "The children of each element", and
#'   "The first child of any element with ...". This option is used
#'   when formatting a `selenider_flatmap_selector`.
#' @param ... Not used.
#' 
#' @returns 
#' A character vector of statements (e.g. "The first child element"), to
#' be used with [cli::cli_bullets()]. Most of the time, a single string will
#' be returned, but some functions (e.g. `format_flatmap_selector()`) return
#' multiple bullets. These bullets may be named, and unnamed bullets are renamed
#' with `replace_names_bullets()`.
#'
#' @noRd
#' @export
format.selenider_selector <- function(x, first = FALSE, multiple = FALSE, element_name = NULL, of = NULL, with = NULL, ...) {
  if (multiple) {
    format_selector_multiple(x, first, element_name = element_name, of = of, with = with)
  } else {
    format_selector(x, first, element_name = element_name, of = of, with = with)
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
    element_name <- if (!is.null(of)) paste(" ancestors of", of, "element") else " ancestors"
    format_selector_multiple(x, element_name = element_name, with = "")
  } else {
    element_name <- if (!is.null(of)) paste(" ancestor of", of, "element") else " ancestor"
    format_selector(x, element_name = element_name, with = "")
  }
}

#' @export
format.selenider_parent_selector <- function(x, of = NULL, ...) {
  format_parent_selector(x, of)
}

#' @export
format.selenider_sibling_selector <- function(x, multiple = FALSE, of = NULL, ...) {
  if (multiple) {
    element_name <- if (!is.null(of)) paste(" siblings of", of, "element") else " siblings"
    format_selector_multiple(x, element_name = element_name, with = "")
  } else {
    element_name <- if (!is.null(of)) paste(" sibling of", of, "element") else " sibling"
    format_selector(x, element_name = element_name, with = "")
  }
}

#' @export
format.selenider_child_selector <- function(x, multiple = FALSE, of = NULL, ...) {
  if (multiple) {
    element_name <- if (!is.null(of)) paste(" direct children of", of, "element") else " direct children"
    format_selector_multiple(x, element_name = element_name, with = "")
  } else {
    element_name <- if (!is.null(of)) paste(" direct child of", of, "element") else " direct child"
    format_selector(x, element_name = element_name, with = "")
  }
}

#' @export
format.selenider_descendant_selector <- function(x, multiple = FALSE, of = NULL, ...) {
  if (multiple) {
    element_name <- if (!is.null(of)) paste(" descendants of", of, "element") else " descendants"
    format_selector_multiple(x, element_name = element_name, with = "")
  } else {
    element_name <- if (!is.null(of)) paste(" descendant of", of, "element") else " descendant"
    format_selector(x, element_name = element_name, with = "")
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

#' @export
format.selenider_js_selector <- function(x, multiple = FALSE, ...) {
  if (multiple) {
    format_selector_multiple(x, element_name = " results of a JavaScript expression", with = "")
  } else {
    if (length(x$filter) == 0) {
      "The result of a JavaScript expression."
    } else {
      format_selector(x, element_name = " result of a JavaScript expression", with = "")
    }
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
  } else if (with == "") {
    element_name
  } else {
    paste(element_name, with)
  }

  if (length(filter) == 1) {
    stopifnot(is.numeric(filter[[1]]))
    paste0("The ", ordinal(filter[[1]]), element, ".")
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

    paste0("The ", ordinal(last), element, " matching a custom condition.")
  }
}

format_query <- function(selector, element_name) {
  selector$to_be_filtered <- NULL
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
  } else if (with == "") {
    element_name
  } else {
    paste(element_name, with)
  }
  
  if (length(filter) == 0) {
    paste0("The", element, ".")
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
        paste0("The", element, " matching a custom condition.")
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

      format_ordinal(last, element,  " matching a custom condition.")
    } else {
      paste0("The", element, " matching a custom condition.")
    }
  }
}

format_ordinal <- function(x, element, condition = ".") {
  first_condition <- condition
  if (all(x >= 0)) {
    c(paste0("The ", subscript_ordinal(x), element, condition[1]), condition[-1])
  } else {
    c(paste0("All", element, " except the ", subscript_ordinal(abs(x)), condition[1]), condition[-1])
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
    "The first of a combination of elements."
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

    paste0("The ", ordinal(last), " of a combination of elements matching a custom condition.")
  }
}

format_flattened_selector_multiple <- function(selector) {
  filter <- selector$filter

  if (length(filter) == 0) {
    "A combination of elements."
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
        "The elements in a combination of elements that match a custom condition."
      }
    }
  } else {
    last <- filter[[length(filter)]]

    if (is.numeric(last)) {
      if (length(filter) == 2) {
        condition <- format_condition(filter[[1]])
        if (length(condition) == 1 && !grepl("\n", condition, fixed = TRUE)) {
          return(format_ordinal_flattened(last, c(paste0(" matching the following condition:"), " " = paste0("{.code ", condition, "}"))))
        }
      }

      format_ordinal_flattened(last, " matching a custom condition.")
    } else {
      "The elements in a combination of elements that match a custom condition."
    }
  }
}

format_ordinal_flattened <- function(x, condition = ".") {
  if (all(x >= 0)) {
    element <- if (length(x) == 1) " element" else " elements"
    c(paste0("The ", subscript_ordinal(x), " of a combination of elements", condition[1]), condition[-1])
  } else {
    c(paste0("All elements of a combination of elements except the ", subscript_ordinal(abs(x)), condition[1]), condition[-1])
  }
}

format_flatmap_selector <- function(selector, multiple = FALSE) {
  first <- format_elements(selector$element)

  mock_selector <- list(filter = list(1))

  class(mock_selector) <- "selenider_flattened_selector"

  if (length(selector$selectors[[length(selector$selectors)]]$filter) == 0) {
    last <- selector$selectors[[length(selector$selectors)]]
    last$filter <- selector$filter
    determiner <- if (length(last$filter) == 0) "each" else "any"
    formatted_last <- format(last, of = determiner, multiple = multiple, inside_flatmap = TRUE)

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
    
    mapped <- if ("selenider_elements" %in% selector$resulting_class) {
      format_elements(mock_element, inside_flatmap = TRUE, of = "each")[-1]
    } else {
      format_element(mock_element, inside_flatmap = TRUE, of = "each")[-1]
    }

    final <- format_final_filter(selector$filter, multiple = multiple)

    c(first, mapped, final)
  }
}

# For nested flatmap calls
format_flatmap_selector_simple <- function(selector, multiple = FALSE, ...) {
  first <- format_elements(selector$element, ...)

  middle <- "A transformation of each element using `elem_flatmap()`."

  final <- format_final_filter(selector$filter, multiple = multiple)

  c(first, middle, final)
}

format_final_filter <- function(filter, multiple) {
  if (length(filter) > 0) {
    element <- list(
      filter = filter
    )

    class(element) <- "selenider_selector"
    
    format(element, with = "", multiple = multiple, first = TRUE)
  } else {
    character()
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

  paste0("{'", escape_single_quotes(expr_deparse(expr)), "'}")
}

# Replace names with "*", keeping names if they are " "
replace_names_bullets <- function(x) {
  if (is.null(names(x))) {
    names(x) <- rep("*", length(x))
  } else {
    names(x)[names(x) != " "] <- rep("*", sum(names(x) != " "))
  }
  x
}
