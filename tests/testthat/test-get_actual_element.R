test_that("get_actual_element() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  element <- s(".toggleable")

  actual_element <- get_actual_element(element)

  if (uses_selenium(session$driver)) {
    expect_s4_class(actual_element, "webElement")
  } else {
    expect_type(actual_element, "integer")
  }
})

test_that("get_actual_elements() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- ss("p")

  actual_elements <- get_actual_elements(elements)

  expect_type(actual_elements, "list")
  if (uses_selenium(session$driver)) {
    expect_s4_class(actual_elements[[1]], "webElement")
  } else {
    expect_type(actual_elements[[1]], "integer")
  }
})
