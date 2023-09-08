test_that("html_elements() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_true(
    html_find(html_elements(session, "p", xpath = ".//div/p"), has_text("Hello")) ==
      html_element(html_element(session, ".toggleable"), "p")
  )
})
