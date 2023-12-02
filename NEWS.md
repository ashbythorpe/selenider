# selenider 0.2.0

## New features

* `elem_select()` is a new function for interacting with `<select>`/`<option>`
  elements, allowing you to easily select by value, text and index.
* New `current_url()` returns the URL of the current page.
## Major changes

* Previously, `selenider` used [RSelenium](https://docs.ropensci.org/RSelenium/)
  as a Selenium backend. However, this package did not work with latest version
  of Selenium, or the latest version of Google Chrome. It now uses 
  [selenium](https://ashbythorpe.github.io/selenium-r/), which is compatible
  with both. This should make `selenider` more reliable, and more likely to
  work out of the box when using Selenium.
* Printing HTML elements now prints the actual HTML contained by the element.
  Previously, it printed the element in "lazy" form, describing how the element
  was to be obtained. While this new change makes viewing elements much easier,
  it also means that to print an element, it needs to exist in the DOM. Use
  `print_lazy()` to use the previous behaviour when printing.

## Minor changes and bug fixes

* `take_screenshot()` with `file = NULL` now views the screenshot correctly.
* `elem_set_value()` now correctly invisibly returns its first argument `x`,
  allowing it to be used in a pipeline.
* `find_element()` and `find_elements()` no longer accept the `link_text`
  argument, as this was not working with chromote. Use `elem_find()` or
  `elem_filter()` instead.
* `elem_get_value()` now uses the JavaScript `value` property of an element,
  not the `"value"` attribute, meaning that it works in more cases.

# selenider 0.1.2

* Responded to CRAN review.

# selenider 0.1.1

* Fixed issue with empty vignette.

# selenider 0.1.0

* Initial CRAN submission.
