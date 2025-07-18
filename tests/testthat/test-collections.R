test_that("find_each_element() and find_all_elements() work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- find_all_elements(
    elem_flatten(s(".toggleable"), s(".buttons")),
    "*"
  )

  elem_expect(elements, has_length(3), timeout = 0)

  elements <- find_each_element(
    elem_flatten(s(".toggleable"), s(".buttons")),
    "*"
  )

  elem_expect(elements, has_length(2), timeout = 0)

  elements <- elem_filter(find_all_elements(ss("div"), "input"), has_value("Submit"))

  elem_expect(elements, has_length(1))

  elements <- elem_filter(find_each_element(ss("div"), "input[type='submit']"), has_value("Submit"))

  elem_expect(elements, has_length(1))

  element <- elem_find(find_all_elements(ss("div"), "input"), has_value("Submit"))

  elem_expect(element, is_present)

  element <- elem_find(find_each_element(ss("div"), "input[type='submit']"), has_value("Submit"))

  elem_expect(element, is_present)
})

test_that("element_list() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- find_all_elements(
    elem_flatten(s(".toggleable"), s(".buttons")),
    "*"
  )

  elements_list <- as.list(elements)

  expect_equal(
    length(elements),
    length(elements_list)
  )

  expect_equal(
    elements[[1]],
    elements_list[[1]]
  )

  expect_equal(
    elements[[3]],
    elements_list[[3]]
  )

  expect_equal(
    vapply(elements_list, elem_name, FUN.VALUE = character(1)),
    c("p", "button", "button")
  )
})
