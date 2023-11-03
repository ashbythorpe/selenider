test_that("get_actual_element() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  element <- s(".toggleable")

  actual_element <- get_actual_element(element)

  if (session$session == "chromote") {
    expect_type(actual_element, "integer")
  } else if (session$session == "selenium") {
    expect_true(is_r6(actual_element))
    expect_s3_class(actual_element, "WebElement")
  } else {
    expect_s4_class(actual_element, "webElement")
  }
})

test_that("get_actual_elements() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- ss("p")

  actual_elements <- get_actual_elements(elements)

  if (session$session == "chromote") {
    expect_type(actual_elements[[1]], "integer")
  } else if (session$session == "selenium") {
    expect_true(is_r6(actual_elements[[1]]))
    expect_s3_class(actual_elements[[1]], "WebElement")
  } else {
    expect_s4_class(actual_elements[[1]], "webElement")
  }
})
