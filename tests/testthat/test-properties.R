test_that("selenider_element properties work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_equal(elem_name(s("#toggle_div")), "button")
  expect_equal(elem_name(s(".actions-form")), "form")

  expect_equal(elem_text(s("#toggle_div")), "Toggle div")
  expect_equal(elem_value(s("input[type='submit']")), "Submit")

  expect_equal(elem_attr(elem_find(find_elements(s(".actions-test"), "input"), has_value("Submit")), "type"), "submit")
  expect_equal(elem_attr(elem_children(s(".actions-form"))[[1]], "class"), "actions-test")

  expect_mapequal(elem_attrs(find_element(s(".actions-test"), "input")), list(type = "text", class = "actions-input"))

  expect_equal(elem_value(s(".actions-input")), NA_character_)

  elem_set_value(s(".actions-input"), "Input")

  expect_equal(elem_value(s(".actions-input")), "Input")

  elem_set_value(s(".actions-input"), 10)

  expect_equal(elem_value(s(".actions-input"), ptype = integer()), 10L)

  # Computed style
  expect_true(elem_css_property(s("#form-output"), "color") %in% c("rgb(255, 0, 0)", "rgba(255, 0, 0, 1)"))
})
