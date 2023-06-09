% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/html_elements.R
\name{html_elements}
\alias{html_elements}
\alias{html_elements.selenider_session}
\alias{html_elements.selenider_element}
\title{Get multiple HTML elements}
\usage{
html_elements(x, ...)

\method{html_elements}{selenider_session}(
  x,
  css = NULL,
  xpath = NULL,
  id = NULL,
  class_name = NULL,
  name = NULL,
  link_text = NULL,
  ...
)

\method{html_elements}{selenider_element}(
  x,
  css = NULL,
  xpath = NULL,
  id = NULL,
  class_name = NULL,
  name = NULL,
  link_text = NULL,
  ...
)
}
\arguments{
\item{x}{A selenider session or element.}

\item{...}{Arguments passed to methods.}

\item{css}{A css selector.}

\item{xpath}{An XPath.}

\item{id}{The id of the element you want to select.}

\item{class_name}{The class name of the element you want to select.}

\item{name}{The name attribute of the element you want to select.}

\item{link_text}{The link text of the link element that you would like to
select.}
}
\value{
A \code{selenider_elements} object.
}
\description{
Find every available HTML element using a CSS selector, an XPath, or a
variety of other methods.
}
\details{
If more than one method is used to select an element (e.g. \code{css} and
\code{xpath}), the first element which satisfies every condition will be found.
}
\examples{
session <- mock_selenider_session()

session |>
  html_elements(".class1")

# Or:
ss(".class1")

session |>
  html_element(".class1") |>
  html_elements(".class2")
  
# The above can be shortened to:
s(".class1") |>
  html_elements(".class2")

}
\seealso{
\itemize{
\item \code{\link[=ss]{ss()}} to quickly select multiple elements without specifying the session.
\item \code{\link[=html_element]{html_element()}} to select multiple elements.
\item \code{\link[=selenider_session]{selenider_session()}} to begin a session.
}
}
