test_that("Printing session works", {
  session <- selenider_test_session()

  if (uses_selenium(session$driver)) {
    skip("Printing not consistent with Selenium drivers")
  }

  expect_snapshot(print(session, .time = as.difftime(10, units = "secs")))
})
