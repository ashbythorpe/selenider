test_that("find_port_from_server() works", {
  session <- selenider_test_session()

  if (!uses_selenium(session$driver)) {
    skip("Test only works when using Selenium")
  }

  expect_equal(
    find_port_from_server(session$driver$server),
    session$driver$client$port
  )
})

test_that("Printing session works", {
  session <- selenider_test_session()

  if (uses_selenium(session$driver)) {
    skip("Printing not consistent with Selenium drivers")
  }

  expect_snapshot(print(session, .time = as.difftime(10, units = "secs")))
})
