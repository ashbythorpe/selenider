% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/html_filter.R
\name{html_filter}
\alias{html_filter}
\alias{html_find}
\alias{[.selenider_elements}
\alias{[[.selenider_elements}
\title{Extract a subset of HTML elements}
\usage{
html_filter(x, ...)

html_find(x, ...)

\method{[}{selenider_elements}(x, i)

\method{[[}{selenider_elements}(x, i)
}
\arguments{
\item{x}{A \code{selenider_elements} object.}

\item{...}{<\code{\link[rlang:dyn-dots]{dynamic-dots}}> Conditions (functions or function
calls) that are used to filter the elements of \code{x}.}

\item{i}{A number (or for \code{[}, a vector of one or more numbers) used to select
elements by position.}
}
\value{
\code{html_filter()} and \code{[} return a \code{selenider_elements} object, since they can result
in multiple elements.
\code{html_find()} and \code{[[} return a single \code{selenider_element} object.
}
\description{
Operators to extract a subset of elements, or a single element, from
a selenider element collection.

\code{html_filter()} and \code{html_find()} allow you to use conditions to filter HTML
elements (see \code{\link[=is_present]{is_present()}} and other conditions). \code{html_find()} returns the \emph{first}
element that satisfies one or more conditions, while \code{html_filter()} returns every
element that satisfies these conditions.

\code{[} and \code{[[} with a numeric subscript can be used on an element collection to filter
the elements by position. \code{[} returns a single element at a specified location, while
\code{[[} returns a collection of the elements at more than one position.
}
\details{
As with the \code{\link[=html_element]{html_element()}} and \code{\link[=html_elements]{html_elements()}} functions, these functions are
lazy, meaning that the elements are not fetched and filtered until they are needed.

Conditions can be functions or function calls (see \code{\link[=html_expect]{html_expect()}} for more details).
}
\examples{
session <- mock_selenider_session()

# Gives the same result as s()
ss(".class1")[[1]]

ss(".class1")[1:5]

ss(".class1") |>
  html_filter(is_visible)

ss(".class1") |>
  html_find(is_visible)

# The above is equivalent to:
elems <- ss(".class1") |>
  html_filter(is_visible)

elems[[1]]

# In R >= 4.3.0, we can instead do:
# ss(".class1") |>
#   html_filter(is_visible) |>
#   _[[1]]

}
\seealso{
\itemize{
\item \code{\link[=html_elements]{html_elements()}} and \code{\link[=ss]{ss()}} to get elements to filter.
\item \code{\link[=is_present]{is_present()}} and other conditions for conditions to filter by.
}
}
