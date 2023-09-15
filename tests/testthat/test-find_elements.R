test_that("find_elements() works", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  elem_click(s("#toggle_div"))

  expect_true(
    elem_find(find_elements(session, "p", xpath = ".//div/p"), has_text("Hello")) ==
      find_element(find_element(session, ".toggleable"), "p")
  )
})
