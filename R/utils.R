get_with_timeout <- function(timeout, .f, ...) {
  if (timeout == 0) {
    .f(...)
  } else {
    end <- Sys.time() + timeout

    while (Sys.time() <= end) {
      res <- .f(...)

      if (!is.null(res)) {
        return(res)
      }
    }

    NULL
  }
}

elem_unique <- function(x, uses_selenium) {
  if (uses_selenium) {
    selenium_intersect(x)
  } else {
    Reduce(intersect, x)
  }
}

selenium_intersect <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  } else if (length(x) == 1) {
    return(x[[1]])
  }

  result <- list()

  shortest_index <- which.min(lengths(x))
  
  repeat {
    if (any(lengths(x) == 0)) {
      break
    }

    current <- x[[shortest_index]][[1]]
    x[[shortest_index]] <- x[[shortest_index]][-1]

    intersection <- TRUE
    for (i in seq_along(x)) {
      if (i == shortest_index) {
        next
      }

      current_list <- x[[i]]
      
      found <- FALSE
      for (j in seq_along(current_list)) {
        to_compare <- current_list[[j]]

        if (tryCatch(current$compareElement(to_compare), error = function(e) FALSE)) {
          x[[i]] <- x[[i]][-j]
          found <- TRUE
          break
        }
      }

      if (!found) {
        intersection <- FALSE
        break
      }
    }

    if (intersection) {
      result <- append(result, list(current))
    }
  }

  result
}

# Adapted from scales::ordinal()
ordinal <- function(x) {
  res <- character(length(x))
  res[x == 1] <- "first"
  res[x == 2] <- "second"
  res[x == 3] <- "third"
  res[res == ""] <- ordinal_numbers(x[res == ""])
  res
}

ordinal_numbers <- function(x) {
  rules <- list(
    st = "(?<!1)1$",
    nd = "(?<!1)2$",
    rd = "(?<!1)3$",
    th = "(?<=1)[123]$",
    th = "[0456789]$",
    th = "."
  )
  
  out <- utils::stack(lapply(rules, grep, x = x, perl = TRUE))
  out <- out[!duplicated(out$values), ] # only first result should be considered
  paste0(
    x,
    out$ind[order(out$values)]
  )
}

call_insert <- function(call, elem_name, quo = TRUE) {
  if (quo) {
    new_call <- call2(
      as.list(quo_get_expr(call))[[1]], 
      parse_expr(elem_name), 
      !!!call_args(call)
    )

    new_quosure(new_call, quo_get_env(call))
  } else {
    new_call <- call2(
      as.list(call)[[1]], 
      parse_expr(elem_name), 
      !!!call_args(call)
    )

    new_call
  }
}

escape_squirlies <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  gsub("}", "}}", x, fixed = TRUE)
}

is_multiple_elements <- function(x) {
  !(inherits_any(x, c("webElement", "remoteDriver", "mock_element", "mock_client", "ChromoteSession")) || (is.numeric(x) && length(x) == 1))
}

uses_selenium <- function(x) {
  inherits_any(x, c("remoteDriver", "mock_client")) || (!is.null(x$client) && inherits_any(x$client, c("remoteDriver", "mock_client")))
}

execute_js_fn <- function(fn, x, driver) {
  if (uses_selenium(driver)) {
    script <- paste0("let fn = ", fn, ";", "return fn(arguments[0]);")
    driver$executeScript(script, list(x))
  } else {
    script <- paste0("function() { return (", fn, ")(this) }")
    x$driver$Runtime$callFunctionOn(fn, chromote_object_id(x))$result$value
  }
}

is_windows <- function() .Platform$OS.type == "windows"

is_mac <- function() Sys.info()[['sysname']] == 'Darwin'

is_linux <- function() Sys.info()[['sysname']] == 'Linux'

