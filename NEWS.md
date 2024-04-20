# selenider 0.4.0

- `find_each_element()` and `find_all_elements()` are new functions that can
  be used to find elements using an element collection. They are designed to
  replace `elem_flatmap()`, which is now deprecated and defunct.
- `element_list()` is now deprecated in favour of
  `as.list.selenider_elements()`.
- `print_lazy()` is now deprecated, as it offers little benefit and is complex
  to maintain.
- `ChromoteSession$check_active()` is used to make sure that the session is
  still active before performing an action.
- The `timeout` argument to `open_url()`, `back()`, `forward()`, and
  `reload()`/`refresh()` allows you to specify how long to wait for a page to
  load. They default to 60 seconds.

# selenider 0.3.0

- Moved all session-specific options in `selenider_session()` to an option object
  (either `chromote_options()` or `selenium_options()`), which can be passed to
  the new `options` argument of `selenider_session()`. This means that a few
  arguments to `selenider_session()` have been deprecated and moved into
  option objects. Old code should be changed as follows:

  - `selenider_session(view = TRUE)` should be changed to:  
    `selenider_session(options = chromote_options(view = TRUE))`
  - `selenider_session(selenium_manager = FALSE)` should be changed to:  
     `selenider_session(
  options = selenium_options(server_options = wdman_server_options())
)
    `
  - `selenider_session(quiet = FALSE)` should be changed to:  
     `selenider_session(options = selenium_options(
  server_options = selenium_server_options(verbose = TRUE))
))
   `

  Additionally, `create_chromote_session()`, `create_selenium_server()`, and
  `create_selenium_client()` are now deprecated. This is because you now
  have access to all the options of these functions in the `options` argument
  of `selenider_session()`.

- Improved reliability of functions when they use chromote, with help of code
  from the [puppeteer](https://github.com/puppeteer/puppeteer) project. Most
  notably:

  - `elem_click()` and friends now work if part of the element is not visible.
  - `elem_value()` and `elem_set_value()` now work on a wider variety of
    elements, including `<select>` elements.

- `elem_focus()` is a new function that allows you to focus an element.
- `selenider_session()` now uses the `withr.hook_source` option to ensure
  that `withr` works inside `source()`.

# selenider 0.2.0

## New features

- `elem_select()` is a new function for interacting with `<select>`/`<option>`
  elements, allowing you to easily select by value, text and index.
- New `current_url()` returns the URL of the current page.

## Major changes

- Previously, `selenider` used [RSelenium](https://docs.ropensci.org/RSelenium/)
  as a Selenium backend. However, this package did not work with the latest version
  of Selenium, or the latest version of Google Chrome. It now uses
  [selenium](https://ashbythorpe.github.io/selenium-r/), which is compatible
  with both. This should make `selenider` more reliable, and more likely to
  work out of the box when using Selenium.
- Printing HTML elements now prints the actual HTML contained by the element.
  Previously, it printed the element in "lazy" form, describing how the element
  was to be obtained. While this new change makes viewing elements much easier,
  it also means that to print an element, it needs to exist in the DOM. Use
  `print_lazy()` to use the previous behaviour when printing.

## Minor changes and bug fixes

- `take_screenshot()` with `file = NULL` now views the screenshot correctly.
- `elem_set_value()` now correctly invisibly returns its first argument `x`,
  allowing it to be used in a pipeline.
- `find_element()` and `find_elements()` no longer accept the `link_text`
  argument, as this was not working with chromote. Use `elem_find()` or
  `elem_filter()` instead.
- `elem_get_value()` now uses the JavaScript `value` property of an element,
  not the `"value"` attribute, meaning that it works in more cases.

# selenider 0.1.2

- Responded to CRAN review.

# selenider 0.1.1

- Fixed issue with empty vignette.

# selenider 0.1.0

- Initial CRAN submission.
