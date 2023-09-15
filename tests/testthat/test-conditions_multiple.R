test_that("Collection conditions work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- elem_children(s(".buttons"))

  expect_false(has_length(elements, 1))
  expect_true(has_length(elements, 2))
  expect_false(has_length(elements, 3))

  expect_true(has_at_least(elements, 1))
  expect_true(has_at_least(elements, 2))
  expect_false(has_at_least(elements, 3))
})
