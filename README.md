
<!-- README.md is generated from README.Rmd. Please edit that file -->

# selenider

<!-- badges: start -->
<!-- badges: end -->

selenider aims to make web testing and scraping using Selenium much
simpler. It is inspired by Java’s [Selenide](https://selenide.org/) and
Python’s [Selene](https://yashaka.github.io/selene/).

It provides the following features:

- All elements are lazy, meaning that they will only actually collect an
  element when absolutely necessary.
- Automatic implicit waiting, meaning that functions will automatically
  wait a certain amount of time (by default, 4 seconds) before
  performing some action on it.
- An API that is designed for use with the pipe operator (`|>` or
  `%>%`).
- A quick and concise way to specify test expectations.

## Installation

You can install the development version of selenider like so:

``` r
# install.packages("remotes")
remotes::install_github("ashbythorpe/selenider")
```

## Usage

To use selenider, you first have to create a session.

``` r
library(selenider)

session <- selenider_session("firefox")

session
#> A selenider session object
#> • Open for 0s
#> • Browser: "firefox"
#> • Port: 4567
#> • Currently opened: Nothing
#> • Timeout: 4s
```

This session object is stored globally, so we don’t need to pass it into
any subsequent functions.

To navigate to a website, use `open_url()`

``` r
open_url("https://www.google.com/")
```

Use `html_element()` to select an element. By default, CSS selectors are
used, but other options are available.

``` r
html_element(session, ".myclass")
#> A selenider element selecting:
#> The first element with css selector ".myclass"

html_element(session, xpath = "//div/a")
#> A selenider element selecting:
#> The first element with xpath "//div/a"
```

However, there is a more concise way, that allows you to skip specifying
the session, using `s()`. The above code can be translated to:

``` r
s(".myclass")
#> A selenider element selecting:
#> The first element with css selector ".myclass"

s("//div/a")
#> A selenider element selecting:
#> The first element with xpath "//div/a"
```

These functions can be chained to find child elements, providing a much
nicer syntax than complex XPaths.

``` r
s(".myclass") |>
  html_element(class_name = "mydiv") |>
  html_element("#mychild")
#> A selenider element selecting:
#> • The first element with css selector ".myclass"
#> • The first child element with class name "mydiv"
#> • The first child element with css selector "#mychild"
```

If you want to select all matching HTML elements rather than just one,
use `ss()` or `html_elements()`.

``` r
# Select all <div> elements
ss("div")
#> A collection of selenider elements selecting:
#> The elements with css selector "div"

s(".myclass") |>
  html_element(".mychild") |>
  html_elements(".multiplechild")
#> A selenider element selecting:
#> • The first element with css selector ".myclass"
#> • The first child element with css selector ".mychild"
#> • The child elements with css selector ".multiplechild"
```

Note that unless we do something with these elements, they will not
actually be collected from the website.

(Todo: Element specific actions e.g. click)

selenider provides a concise testing interface using the `html_expect()`
function. Provide an element, and one or more conditions, and the
function will wait until all the conditions are met. If the session’s
timeout is reached, then the function will provide an informative error
message.

``` r
# Check that an element exists
s(".myclass") |>
  html_expect(exists)

# Whoops! This element is not visible!
s(".invisible") |> 
  html_expect(is_visible)
#> Error in `html_expect()`:
#> ! Condition failed after waiting for 4 seconds:
#> `is_visible`
#> Run `rlang::last_trace()` to see where the error occurred.

# Check that an element is visible and enabled
s(".myclass") |>
  html_element("#mychild") |>
  html_expect(is_visible, is_enabled)
```

And (`&&`), or (`||`) and not (`!`) can be used as if the conditions
were logical values.

``` r
# Check that an element doesn't exist
s(".myclass") |>
  html_expect(!exists)

# Check that an element is visible or enabled
s(".myclass") |>
  html_expect(is_visible || is_enabled)
```

`html_expect()` accepts both functions (`exists`) and calls
(`exists()`), allowing expectations to be defined for multiple elements.
Note that the first argument to `html_expect()` can be an element *or* a
condition.

``` r
elem_1 <- s(".class1")

elem_2 <- s(".class2")

# Test that either the first or second element exists
html_expect(exists(elem_1) || exists(elem_2))
```

(Todo: Link to documentation of conditions)
