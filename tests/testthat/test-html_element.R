test_that("html_element() work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_true(
    html_element(session, "div", class_name = "actions-test") ==
      html_element(session, ".actions-test")
  )
})
