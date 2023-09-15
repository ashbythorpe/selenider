test_that("elem_flatmap() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- elem_flatmap(
    elem_flatten(s(".toggleable"), s(".buttons")),
    function(x) elem_children(x)
  )

  elem_expect(elements, has_length(3))

  elements <- elem_flatmap(
    elem_flatten(s(".toggleable"), s(".buttons")),
    function(x) elem_children(x)[[1]]
  )

  elem_expect(elements, has_length(2))
})

test_that("element_list() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- elem_flatmap(
    elem_flatten(s(".toggleable"), s(".buttons")),
    function(x) elem_children(x)
  )

  elements_list <- as.list(elements)

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
