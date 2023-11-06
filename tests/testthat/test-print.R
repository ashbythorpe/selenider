test_that("Printing elements works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_snapshot(print(s(".toggleable"), width = 80))

  expect_snapshot(print(s("#toggle_div"), width = 80))

  expect_snapshot(print(elem_children(s(".actions-test")), width = 80))

  expect_snapshot(print(elem_children(s(".actions-test")), width = 80, n = 3))
})
