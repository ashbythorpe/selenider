# selenider (development version)

* `take_screenshot()` with `file = NULL` now views the screenshot correctly.
* `current_url()` is a new function that returns the current URL.
* `elem_set_value()` now correctly invisibly returns its first argument `x`.
* `selenium` is now used as a backend instead of `RSelenium`.
* Removed `link_text` as an argument in `find_element()`/`find_elements()`,
  as it does not work with chromote.
* Change print method to print the HTML value of an element/element collection.
* Updated `elem_value()` to get the use JavaScript (e.g. `x.value`), 
  instead of the `value` attribute.
* `elem_select()` is a new function for interacting with `<select>`/`<option>`
  elements.

# selenider 0.1.2

* Responded to CRAN review.

# selenider 0.1.1

* Fixed issue with empty vignette.

# selenider 0.1.0

* Initial CRAN submission.
