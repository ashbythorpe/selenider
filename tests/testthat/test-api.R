test_that("s() and ss() work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  testthat::expect_equal(s(".toggleable"), html_element(session, ".toggleable"))
  expect_equal(ss(".toggleable"), html_elements(session, ".toggleable"))

  expect_true(s(".toggleable") == html_element(session, ".toggleable"))
})
