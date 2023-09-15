test_that("elem_expect() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_snapshot(elem_expect(is_present, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect(s(".toggleable")), error = TRUE)

  expect_snapshot(elem_expect(s(".random-class"), is_present, testthat = FALSE, timeout = 0), error = TRUE)
  expect_snapshot(elem_expect(s(".random-class"), is_present, testthat = FALSE, timeout = 0.1), error = TRUE)

  expect_snapshot(elem_expect(s(".toggleable"), !is_in_dom, testthat = FALSE, timeout = 0.1), error = TRUE)

  expect_snapshot(elem_expect(s(".random-class"), !is_absent, testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect(s(".toggleable"), !(!(is_absent)), testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect(s(".random-class"), !(!(!(is_absent))), testthat = FALSE, timeout = 0.1), error = TRUE)

  expect_snapshot(elem_expect(s(".random-class"), is_visible, testthat = FALSE, timeout = 0.1), error = TRUE)

  expect_snapshot(elem_expect(s(".toggleable"), is_visible, testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect(s(".buttons"), is_hidden, testthat = FALSE, timeout = 0.1), error = TRUE)

  enabled_button <- find_elements(s(".buttons"), "button")[[1]]
  disabled_button <- find_elements(s(".buttons"), "button")[[2]]

  expect_snapshot(elem_expect(enabled_button, is_disabled, testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect(disabled_button, is_enabled, testthat = FALSE, timeout = 0.1), error = TRUE)

  expect_snapshot(elem_expect(s(".random-class"), has_name("p"), testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect(s(".toggleable"), has_name("biv"), testthat = FALSE, timeout = 0.1), error = TRUE)

  elem_click(s("#toggle_div"))

  element <- find_element(s(".toggleable"), "p")

  expect_snapshot(elem_expect(element, has_text("Goodbye!"), testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect(element, has_exact_text("ell"), testthat = FALSE, timeout = 0.1), error = TRUE)

  buttons <- elem_children(s(".buttons"))

  expect_snapshot(elem_expect(buttons[[1]], has_attr("disabled", ""), testthat = FALSE, timeout = 0.1), error = TRUE)

  elem_click(s("#toggle_div"))

  expect_snapshot(elem_expect(s(".toggleable"), attr_contains("style", "color"), testthat = FALSE, timeout = 0.1), error = TRUE)

  submit_button <- find_element(s(".actions-form"), "input[type='submit']")

  expect_snapshot(elem_expect(submit_button, has_value("Don't submit"), testthat = FALSE, timeout = 0.1), error = TRUE)

  expect_snapshot(elem_expect(s(".toggleable"), has_css_property("display", "block"), testthat = FALSE, timeout = 0.1), error = TRUE)

  expect_snapshot(elem_expect(s(".random-class"), function(x) elem_name(x) == "biv", testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect(s(".toggleable"), function(x) elem_name(x) == "biv", testthat = FALSE, timeout = 0.1), error = TRUE)

  expect_snapshot(elem_expect(elem_children(s(".random-class")), function(x) elem_name(x[[1]]) == "biv", testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect(ss(".random-class"), function(x) elem_name(x[[1]]) == "biv", testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect(ss(".toggleable"), function(x) elem_name(x[[1]]) == "biv", testthat = FALSE, timeout = 0.1), error = TRUE)

  expect_snapshot(elem_expect(ss(".random-class"), has_at_least(1), testthat = FALSE, timeout = 0.1), error = TRUE)
  expect_snapshot(elem_expect(ss(".toggleable"), has_length(2), testthat = FALSE, timeout = 0.1), error = TRUE)
})

test_that("elem_expect() test failures work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_failure(elem_expect(s(".random-class"), is_present, timeout = 0.1))

  expect_snapshot(show_failure(elem_expect(s(".random-class"), is_present, timeout = 0.1)))

  expect_failure(elem_expect(s(".random-class"), is_visible, timeout = 0.1))

  expect_snapshot(show_failure(elem_expect(s(".random-class"), is_visible, timeout = 0.1)))
})

test_that("elem_wait_until() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_false(elem_wait_until(s(".random-class"), is_present, timeout = 0))
  expect_true(elem_wait_until(s(".toggleable"), is_present, timeout = 0))
})
