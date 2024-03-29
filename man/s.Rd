% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{s}
\alias{s}
\alias{ss}
\title{Select HTML elements}
\usage{
s(css = NULL, xpath = NULL, id = NULL, class_name = NULL, name = NULL)

ss(css = NULL, xpath = NULL, id = NULL, class_name = NULL, name = NULL)
}
\arguments{
\item{css}{A css selector.}

\item{xpath}{An XPath.}

\item{id}{The id of the element you want to select.}

\item{class_name}{The class name of the element you want to select.}

\item{name}{The name attribute of the element you want to select.}
}
\value{
\code{s()} returns a \code{selenider_element} object.
\code{ss()} returns a \code{selenider_elements} object.
}
\description{
Both \code{s()} and \code{ss()} allow you to select elements without specifying a
session object.

\code{s()} selects a single element, being a shorthand for \code{\link[=find_element]{find_element()}}
on the current session.

\code{ss()} selects multiple elements, being a shorthand for \code{\link[=find_elements]{find_elements()}}.
}
\details{
Both functions allow the starting point for chains of selectors to be made
more concise. Both use \code{\link[=get_session]{get_session()}} to get the global session object.
}
\examples{
\dontshow{if (selenider::selenider_available(online = FALSE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
html <- "
<div>
<p id='id1' class='inner'></p>
<div class='child'>
<p class='inner'></p>
</div>
</div>
"

session <- minimal_selenider_session(html)

s("#id1")

# This is the equivalent of:
find_element(session, "#id1")

ss(".inner")

# This is the equivalent of:
find_element(session, ".inner")

# This provides a more concise way to begin a chain of selectors
s("div") |>
  find_element(".child") |>
  find_element(".inner")
\dontshow{\}) # examplesIf}
}
\seealso{
\itemize{
\item \code{\link[=find_element]{find_element()}} and \code{\link[=find_elements]{find_elements()}}
\item \code{\link[=selenider_session]{selenider_session()}} to begin a session.
}
}
