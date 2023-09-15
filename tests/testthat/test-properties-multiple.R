test_that("Getting the length of an element collection works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_equal(
    elem_size(elem_children(s(".buttons"))),
    2L
  )

  expect_equal(
    length(find_elements(s(".actions-form"), "input")),
    2L
  )
})
