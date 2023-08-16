test_that("selenider_element properties work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_equal(html_name(s("#toggle_div")), "button")
  expect_equal(html_name(s(".actions-form")), "form")

  expect_equal(html_text(s("#toggle_div")), "Toggle div")
  expect_equal(html_value(s("input[type='submit']")), "Submit")

  expect_equal(html_attr(html_find(html_elements(s(".actions-test"), "input"), has_value("Submit")), "type"), "submit")
  expect_equal(html_attr(html_children(s(".actions-form"))[[1]], "class"), "actions-test")

  expect_equal(html_attrs(html_element(s(".actions-test"), "input")), list(type = "text", class = "actions-input"))

  expect_equal(html_value(s(".actions-input")), NA_character_)

  set_value(s(".actions-input"), "Input")

  expect_equal(html_value(s(".actions-input")), "Input")

  set_value(s(".actions-input"), 10)

  expect_equal(html_value(s(".actions-input"), ptype = integer()), 10L)

  expect_equal(html_css_property(s("#form-output"), "color"), "rgb(255, 0, 0)")
})
