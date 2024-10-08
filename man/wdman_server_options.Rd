% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/session-options.R
\name{wdman_server_options}
\alias{wdman_server_options}
\alias{rselenium_client_options}
\title{RSelenium options}
\usage{
wdman_server_options(
  version = "latest",
  driver_version = "latest",
  port = 4444L,
  check = TRUE,
  verbose = FALSE,
  retcommand = FALSE,
  ...
)

rselenium_client_options(
  port = 4444L,
  host = "localhost",
  path = "/wd/hub",
  version = "",
  platform = "ANY",
  javascript = TRUE,
  native_events = TRUE,
  extra_capabilities = list()
)
}
\arguments{
\item{version}{The version of Selenium server to use.}

\item{driver_version}{The version of the browser-specific driver to use.}

\item{port}{The port to run selenium client/server on.}

\item{check, verbose, retcommand, ...}{Passed into \code{\link[wdman:selenium]{wdman::selenium()}}.}

\item{host}{The host to connect to the selenium server over.}

\item{path, platform, javascript, native_events, extra_capabilities}{Passed into \code{\link[RSelenium:remoteDriver-class]{RSelenium::remoteDriver()}}.}
}
\value{
An options object that can be passed into \code{\link[=selenium_options]{selenium_options()}}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}
Instruct selenider to use RSelenium instead of selenium. Passed into
\code{\link[=selenium_options]{selenium_options()}}. This is not recommended, since RSelenium does not
support the latest version of Selenium, and wdman (the server manager that
RSelenium) uses, is not compatible with the latest version of Chrome.
}
\details{
In \code{\link[=selenium_options]{selenium_options()}}, you can supply options to configure a server and
client run by RSelenium instead of selenium.
Instead of \code{selenium_server_options()}, you can use \code{wdman_server_options()}
to allow \code{wdman} to run the Selenium server using \code{\link[wdman:selenium]{wdman::selenium()}}.

Instead of using \code{selenium_client_options()}, you can use
\code{rselenium_client_options()} to control the creation of an
\code{\link[RSelenium:remoteDriver-class]{RSelenium::remoteDriver()}} object instead.

Note that the \code{driver} option of \code{selenider_session()} also accepts these
objects in place of their selenium equivalents.
}
