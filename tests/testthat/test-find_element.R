test_that("find_element() work", {
  session <- selenider_test_session()

  open_url("https://ashbythorpe.github.io/selenider/articles/test-site.html")

  expect_true(
    find_element(session, "div", class_name = "actions-test") ==
      find_element(session, ".actions-test")
  )

  expect_true(
    find_element(session, xpath = "//button[@id = 'toggle_div']") ==
      find_element(session, "#toggle_div")
  )
})
