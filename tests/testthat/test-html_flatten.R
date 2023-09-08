test_that("html_flatten() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  element_1 <- s(".toggleable")
  buttons <- html_children(s(".buttons"))
  element_2 <- s(".actions-input")

  flattened <- html_flatten(element_1, buttons, element_2)

  expect_true(
    flattened[[1]] == element_1
  )

  expect_true(
    flattened[[2]] == buttons[[1]]
  )

  expect_true(
    flattened[[3]] == buttons[[2]]
  )

  expect_true(
    flattened[[4]] == element_2
  )

  flattened <- html_flatten(list(element_1, buttons), element_2)

  expect_true(
    flattened[[1]] == element_1
  )

  expect_true(
    flattened[[2]] == buttons[[1]]
  )

  expect_true(
    flattened[[3]] == buttons[[2]]
  )

  expect_true(
    flattened[[4]] == element_2
  )
})
