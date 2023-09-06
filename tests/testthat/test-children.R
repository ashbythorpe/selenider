test_that("DOM-relative functions work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  element <- s(".actions-form")

  ancestors <- html_ancestors(element)
  
  html_expect(ancestors, has_length(5))

  parent <- html_parent(element)

  expect_true(parent == s("#main"))

  siblings <- html_siblings(element)

  expect_true(siblings[[1]] == s(".page-header"))

  children <- html_children(element)

  html_expect(children, has_length(1))

  expect_true(children[[1]] == s(".actions-test"))

  descendants <- html_descendants(element)

  html_expect(descendants, has_length(6))

  expect_true(descendants[[1]] == s(".actions-test"))
  expect_true(descendants[[2]] == s(".actions-button"))
})
