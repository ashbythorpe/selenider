test_that("Conditions work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_true(is_present(s("#toggle_div")))

  expect_false(is_absent(s("#toggle_div")))

  expect_false(is_in_dom(s(".random-class")))

  expect_true(is_absent(s(".random-class")))

  expect_error(is_visible(s(".random-class")), class = "selenider_error_absent_element")

  expect_false(is_visible(s(".toggleable")))
  expect_true(is_invisible(s(".toggleable")))

  expect_true(is_displayed(s(".buttons")))
  expect_false(is_hidden(s(".buttons")))

  enabled_button <- find_elements(s(".buttons"), "button")[[1]]
  disabled_button <- find_elements(s(".buttons"), "button")[[2]]

  expect_true(is_enabled(enabled_button))
  expect_false(is_disabled(enabled_button))

  expect_false(is_enabled(disabled_button))
  expect_true(is_disabled(disabled_button))

  expect_true(has_name(s(".toggleable"), "div"))
  expect_false(has_name(s(".toggleable"), "biv"))

  # To get the text in an element in selenium, the element must be visible.
  elem_click(s("#toggle_div"))

  element <- find_element(s(".toggleable"), "p")

  expect_true(has_text(element, "Hello!"))
  expect_true(has_text(element, "ell"))
  expect_false(has_text(element, "Goodbye!"))

  expect_true(has_exact_text(element, "Hello!"))
  expect_false(has_exact_text(element, "ell"))

  buttons <- elem_children(s(".buttons"))
  expect_true(has_attr(buttons[[1]], "disabled", NA))

  expect_false(has_attr(buttons[[1]], "disabled", ""))
  expect_false(has_attr(buttons[[2]], "disabled", "Something"))

  elem_click(s("#toggle_div"))

  expect_true(attr_contains(s(".toggleable"), "style", "display"))
  expect_false(attr_contains(s(".toggleable"), "style", "color"))

  submit_button <- find_element(s(".actions-form"), "input[type='submit']")

  expect_true(has_value(submit_button, "Submit"))

  expect_true(has_css_property(s(".toggleable"), "display", "none"))
  expect_false(has_css_property(s(".toggleable"), "display", "block"))
})
