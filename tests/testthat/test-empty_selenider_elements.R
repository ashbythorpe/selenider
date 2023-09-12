test_that("Printing empty element collections works", {
  session <- selenider_test_session()

  empty_elements <- suppressWarnings(ss(".class")[1:5][6])

  expect_s3_class(empty_elements, "empty_selenider_elements")

  expect_true(elements_is_empty(empty_elements))

  expect_snapshot(print(empty_elements))

  expect_equal(get_elements(empty_elements), list())
})
