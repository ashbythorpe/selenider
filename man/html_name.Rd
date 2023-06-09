% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/properties.R
\name{html_name}
\alias{html_name}
\title{Get tag name of an element}
\usage{
html_name(x, timeout = NULL)
}
\arguments{
\item{x}{A \code{selenider_element} object.}

\item{timeout}{The time to wait for \code{x} to exist.}
}
\value{
A string
}
\description{
Get the tag name (e.g. \code{"p"} for a \verb{<p>} tag) of a \code{selenider_element} object.
}
\examples{
session <- mock_selenider_session()

s(".class1") |>
  html_name()

}
\seealso{
Other properties: 
\code{\link{html_attr}()},
\code{\link{html_css_property}()},
\code{\link{html_text}()}
}
\concept{properties}
