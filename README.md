
<!-- README.md is generated from README.Rmd. Please edit that file -->

# selenider

<!-- badges: start -->

[![R-CMD-check](https://github.com/ashbythorpe/selenider/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ashbythorpe/selenider/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ashbythorpe/selenider/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ashbythorpe/selenider?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/selenider)](https://CRAN.R-project.org/package=selenider)
<!-- badges: end -->

Traditionally, automating a web browser is often unreliable, especially
when using R. Programmers are forced to write verbose code, utilising
inconsistent workarounds (such as using `Sys.sleep()` to wait for
something to happen).

selenider aims to make web testing and scraping in R much simpler,
providing a wrapper for either
[chromote](https://rstudio.github.io/chromote/) or
[selenium](https://ashbythorpe.github.io/selenium-r/). It is inspired by
Java’s [Selenide](https://selenide.org/) and Python’s
[Selene](https://yashaka.github.io/selene/).

Code reliability and reproducibility are essential when writing R code.
selenider provides features to make your scripts work every time they
are run, without any extra code:

- Lazy elements: selenider will only try to find an element on the page
  when it is absolutely necessary. Your definitions of HTML elements are
  separated from their existence on the page, only allowing the two to
  converge when absolutely necessary. In selenider, HTML elements are
  stored as the directions to the element on the page, rather than the
  element itself. This is much more reliable than the alternative since
  the webpage can constantly change, resulting in elements becoming
  invalid between their creation and use (e.g. the dreaded
  `StaleElementReferenceException` in Selenium).
- Automatic waiting: selenider will automatically wait for your code to
  work (e.g. waiting for an input to exist and be clickable before
  actually clicking it), allowing you to write scripts as if your
  website always responds instantly to your interactions.

selenider’s other main focus is its API. Its design choices result in
concise yet expressive code that is easy to read and easy to write:

- A global session object results in shorter, more declarative code. It
  also allows the session to be created at the beginning of your script
  or test, and closed at the end.
- All functions are designed for use with the pipe operator (`|>` or
  `%>%`); elements can be selected, tested and operated on in a single
  pipeline.
- `elem_expect()` is a powerful way to specify test expectations, with a
  simple but extensible syntax and informative error messages.
- selenider is compatible with automated testing frameworks like
  [testthat](https://testthat.r-lib.org) and
  [shinytest2](https://rstudio.github.io/shinytest2/).

## Installation

``` r
# Install selenider from CRAN
install.packages("selenider")

# Or the development version from Github
# install.packages("remotes")
remotes::install_github("ashbythorpe/selenider")
```

Additionally, you must install
[chromote](https://rstudio.github.io/chromote/) or
[selenium](https://ashbythorpe.github.io/selenium-r/). We recommend
chromote, as it is quicker and easier to get up and running.

``` r
# Either:
install.packages("chromote")

# Or:
install.packages("selenium")
```

If you are using selenium, you must also have
[Java](https://www.oracle.com/java/technologies/downloads/) installed.

Finally, you must have a web browser installed. For chromote, [Google
Chrome](https://www.google.com/chrome/) is required. For selenium, any
browser can be used, but [Firefox](https://www.mozilla.org/firefox/new/)
is recommended.

## Usage

``` r
library(selenider)
```

The following code navigates to the [R project
website](https://www.r-project.org/), finds the link to the CRAN mirror
list, checks that the link is correct, and clicks the link element.

``` r
open_url("https://www.r-project.org/")
#> Can't find an existing selenider session.
#> ℹ Creating a new session.

s(".row") |>
  find_element("div") |>
  find_elements("a") |>
  elem_find(has_text("CRAN")) |>
  elem_expect(attr_contains("href", "cran.r-project.org")) |>
  elem_click()
```

Now that we’re in the mirror list page, let’s find the link to every
CRAN mirror in the UK.

``` r
s("dl") |>
  find_elements("dt") |>
  elem_find(has_text("UK")) |>
  find_element(xpath = "./following-sibling::dd") |>
  find_elements("tr") |>
  elem_expect(has_at_least(1)) |>
  as.list() |>
  lapply(
    \(x) x |>
      find_element("a") |>
      elem_attr("href")
  )
#> [[1]]
#> [1] "https://www.stats.bris.ac.uk/R/"
#> 
#> [[2]]
#> [1] "https://cran.ma.imperial.ac.uk/"
#> 
#> [[3]]
#> [1] "https://anorien.csc.warwick.ac.uk/CRAN/"
```

## Vignettes

- Get started with selenider, and learn the basics:
  `vignette("selenider")`.
- Use selenider with testthat, shinytest2 and Github Actions:
  `vignette("unit-testing", package = "selenider")`.
- Use selenider with rvest:
  `vignette("with-rvest", package = "selenider")`
