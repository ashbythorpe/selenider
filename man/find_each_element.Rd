% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collections.R
\name{find_each_element}
\alias{find_each_element}
\alias{find_all_elements}
\title{Find HTML children from a collection}
\usage{
find_each_element(
  x,
  css = NULL,
  xpath = NULL,
  id = NULL,
  class_name = NULL,
  name = NULL
)

find_all_elements(
  x,
  css = NULL,
  xpath = NULL,
  id = NULL,
  class_name = NULL,
  name = NULL
)
}
\arguments{
\item{x}{A \code{selenider_elements} object.}

\item{css}{A CSS selector.}

\item{xpath}{An XPath.}

\item{id}{The id of the elements you want to select.}

\item{class_name}{The class name of the elements you want to select.}

\item{name}{The name attribute of the elements you want to select.}
}
\value{
A \code{selenider_elements} object.
}
\description{
Find HTML child elements from elements in a collection. Provides
a convenient way to operate on a collection of elements.

\code{find_each_element()} finds the first child element of each element in
the collection.

\code{find_all_elements()} finds every child element of every element in the
collection.
}
\details{
\code{find_each_element()} will usually preserve the length of the input, since
for each element in the collection, one new element will be found. However,
if an element in the collection cannot be found, it will not be included in
the resulting collection.

\code{find_each_element(x, ...)} is roughly equivalent to:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{x |>
  as.list() |>
  lapply(\\(x) find_element(x, ...)) |>
  elem_flatten()
}\if{html}{\out{</div>}}

Similarly, \code{find_all_elements(x, ...)} is roughly equivalent to:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{x |>
  as.list() |>
  lapply(\\(x) find_elements(x, ...)) |>
  elem_flatten()
}\if{html}{\out{</div>}}
}
\examples{
\dontshow{if (selenider::selenider_available(online = FALSE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
html <- "
<div id='div1'>
  <p>Text 1</p>
  <button>Button 1</button>
</div>
<div id='div2'>
  <p>Text 2</p>
</div>
<div id='div3'>
  <p>Text 3</p>
</div>
<div id='div4'>
  <p>Text 4</p>
</div>
"

session <- minimal_selenider_session(html)

divs <- ss("div")

# Get the <p> tag inside each div.
divs |>
  find_each_element("p")


# Get the <button> tag in the first div as well.
divs |>
  find_all_elements("*")
\dontshow{\}) # examplesIf}
}
\seealso{
\itemize{
\item \code{\link[=as.list]{as.list()}} to iterate over an element collection.
\item \code{\link[=elem_flatten]{elem_flatten()}} to combine multiple
\code{selenider_element}/\code{selenider_elements} objects into a single object.
}
}
