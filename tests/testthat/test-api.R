test_that("s() and ss() work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_equal(s(".toggleable"), find_element(session, ".toggleable"))
  expect_equal(ss(".toggleable"), find_elements(session, ".toggleable"))

  expect_true(s(".toggleable") == find_element(session, ".toggleable"))
})
