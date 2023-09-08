test_that("html_flatmap() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- html_flatmap(
    html_flatten(s(".toggleable"), s(".buttons")),
    function(x) html_children(x)
  )

  html_expect(elements, has_length(3))

  elements <- html_flatmap(
    html_flatten(s(".toggleable"), s(".buttons")),
    function(x) html_children(x)[[1]]
  )

  html_expect(elements, has_length(2))
})

test_that("element_list() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elements <- html_flatmap(
    html_flatten(s(".toggleable"), s(".buttons")),
    function(x) html_children(x)
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
    vapply(elements_list, html_name, FUN.VALUE = character(1)),
    c("p", "button", "button")
  )
})
